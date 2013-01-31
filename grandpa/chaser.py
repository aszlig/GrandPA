# GrandPA, a LedBar lighting controller.
#
# Copyright (c) 2010 aszlig <"^[0-9]+$"@regexmail.net>
#
# GrandPA is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GrandPA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GrandPA. If not, see <http://www.gnu.org/licenses/>.

import sys
import time
import threading
import logging

import fixture

from grandpa.color import Color


class ChaserError(RuntimeError):
    pass


class Chaser(threading.Thread):
    label = None
    is_cue = False
    speed = None

    def __init__(self, queue, sections):
        threading.Thread.__init__(self)
        self.queue = queue

        self.quit = threading.Event()
        self.notify = threading.Event()

        self.restart_event = threading.Event()

        self.sections = sections
        self.sectlen = len(sections)

        self.last_wait = None

        if self.sectlen == 0:
            self.end_chaser()

    def init_sections(self):
        self.all_sections = fixture.Section(None, None)

    def propagate_sections(self):
        if not self.all_sections.has_changed():
            return

        for s in self.sections:
            s.color.patch_color(self.all_sections.color)

        self.all_sections.color.reset_changed()

    def dmxout(self):
        bars = set([s.bar for s in self.sections])

        addrs = []
        vals = []
        for addr, val in [bar.dmxout() for bar in bars]:
            addrs.append(addr)
            vals.append(val)

        self.queue.put((addrs, vals))

    def update_visuals(self):
        for s in self.sections:
            s.visual.refresh()

    def handle_exception(self):
        logging.exception("Chaser died: %s", self)
        self.queue.put(('exception', self.__class__))

    def run(self):
        if self.quit.isSet():
            return

        self.init_sections()

        if hasattr(self, 'setup'):
            try:
                self.setup()
            except:
                self.handle_exception()
                return

        while not self.quit.isSet():
            try:
                self.next()
            except:
                self.handle_exception()
                return
            self.propagate_sections()

            self.dmxout()

            self.update_visuals()

            if self.is_cue:
                break

    def wait(self, seconds, frames=None):
        if self.speed is None:
            sec = seconds
        else:
            sec = self.speed

        if frames:
            sec /= frames

        if self.last_wait:
            sec -= time.time() - self.last_wait

        self.notify.wait(sec)
        self.last_wait = time.time()
        self.notify.clear()

        if self.restart_event.isSet():
            if hasattr(self, 'restart'):
                self.restart()
            self.restart_event.clear()

    def set_speed(self, speed):
        self.speed = speed
        self.restart_event.set()
        self.notify.set()

    def end_chaser(self):
        self.queue.put(('quit', self.__class__))
        self.quit.set()

    def stop(self):
        self.quit.set()
        self.notify.set()

    def next(self):
        raise ChaserError("No next method defined.")
