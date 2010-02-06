# GrandPA, a LedBar lighting controller.
#
# Copyright (c) 2010 aszlig <"^[0-9]+$"@regexmail.net>
#
# LastWatch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# LastWatch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GrandPA. If not, see <http://www.gnu.org/licenses/>.

"""
This is our controller, which sends out DMX packets and draws the current state
of the fixtures to the interface.
"""
import logging
import threading
import Queue
import socket
import array

from grandpa import dmx
from grandpa import fixture

class Controller(threading.Thread):
    def __init__(self, root):
        """
        Initialize the controller and set up the queue for the specific chaser
        threads, works like this:
            - chaser is running as a while loop and sends down the event
        """
        threading.Thread.__init__(self)
        self.do_quit = threading.Event()
        self.queue = Queue.Queue()

        self.root = root

        self.chasers = {}
        self.bars = []

        self.connected = False

        self.packet = [0] * 512

        self.dmx_lock = threading.Lock()

    def add_bar(self, addr, fixtype, iface_sections):
        b = fixture.Bar(addr, fixtype, iface_sections)
        self.bars.append(b)

    def dim_update(self):
        self.queue.put((None, None))

    def selection2sections(self, selection):
        sects = []

        for b, bar in enumerate(selection):
            for s, section in enumerate(bar):
                if not section:
                    continue

                sects.append(self.bars[b].sections[s])

        return sects

    def add_chaser(self, chaser, selection):
        if chaser in self.chasers:
            return

        sects = self.selection2sections(selection)

        c = chaser(self.queue, sects)
        c.start()

        if not chaser.is_cue:
            self.chasers[chaser] = c

    def remove_chaser(self, chaser):
        if chaser not in self.chasers:
            return

        c = self.chasers.pop(chaser)
        c.stop()

    def is_running_chaser(self, chaser):
        return chaser in self.chasers

    def set_speed(self, chaser, speed):
        chaser = self.chasers.get(chaser, None)
        if chaser is None:
            return

        chaser.set_speed(speed)

    def dmx_init(self):
        try:
            self.dmx = dmx.DMX('/dev/ttyUSB0')
        except dmx.DMXError, e:
            self.root.status.set_dmx_error(e[0])
            self.connected = False
            return

        self.connected = True
        self.root.status.reset_dmx_error()

    def dmxout_single(self, addr, values):
        start = addr-1
        end = start+len(values)
        self.packet[start:end] = values

    def dmxflush(self):
        self.dmx_lock.acquire()

        if not self.connected:
            self.dmx_init()

        if self.connected:
            try:
                self.dmx.send(self.packet)
            except dmx.DMXError, e:
                self.root.status.set_dmx_error(e[0])
                self.connected = False

        self.dmx_lock.release()

    def dmxout(self, addr, values):
        for a, v in zip(addr, values):
            self.dmxout_single(a, v)

        self.dmxflush()

    def process(self):
        while not self.do_quit.isSet():
            try:
                addr, values = self.queue.get(timeout=1)
            except ValueError:
                self.queue.task_done()
                break
            except Queue.Empty:
                continue

            if addr == 'exception':
                self.chasers.pop(values)
                self.root.menu.fail_chaser(values)
            elif addr is None:
                for bar in self.bars:
                    addr, values = bar.dmxout()
                    self.dmxout_single(addr, values)
                self.dmxflush()
            else:
                self.dmxout(addr, values)

            self.queue.task_done()

        self.cleanup()

    def run(self):
        try:
            self.process()
        except:
            logging.exception("Controller died!")

    def stop(self):
        self.do_quit.set()
        self.queue.put('quit')
        self.join()
        self.cleanup()

    def cleanup(self):
        for chaser in self.chasers:
            c = self.chasers.get(chaser)
            c.stop()
            c.join()

        self.chasers = {}
