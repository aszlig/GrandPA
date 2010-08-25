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

import time

import style
from grandpa.chaser import Chaser
import grandpa.chasers

from grandpa import locking

class Menu(object):
    COLUMNS = 6

    def __init__(self, root, window):
        self.root = root
        self.outer = window
        self.outer.box()

        # window within box
        y, x = self.outer.getmaxyx()
        inner = self.outer.derwin(y - 2, x - 2, 1, 1)
        self.menu = inner

        y, x = self.menu.getmaxyx()
        self.listlen = y
        self.listwidth = (x - x % self.COLUMNS) / self.COLUMNS

        self.pointer = 0

        self.chasers = self.get_chaserlist()
        self.active_chasers = []

        self.chaser_speeds = {}

        self.update()

        self.refresh()

    def get_chaserlist(self, cls=Chaser):
        if cls.label is not None:
            classes = [cls]
        else:
            classes = []

        for sub in cls.__subclasses__():
            classes += self.get_chaserlist(sub)

        return classes

    def get_chaser(self):
        """
        Returns the currently selected chaser class.
        """
        try:
            return self.chasers[self.pointer]
        except IndexError:
            return None

    def activate(self):
        """
        Toggle-activate a chaser.
        """
        chaser = self.get_chaser()
        if chaser is None:
            return

        if self.root.controller.is_running_chaser(chaser):
            self.root.controller.remove_chaser(chaser)
            self.active_chasers.remove(chaser)
        else:
            s = self.root.tavern.get_selection_struct()
            self.root.controller.add_chaser(chaser, s)
            if not chaser.is_cue:
                self.active_chasers.append(chaser)

        self.update()

    def remove_chaser(self, chaser):
        try:
            self.active_chasers.remove(chaser)
        except ValueError:
            pass
        self.update()

    def reset(self):
        """
        Stop all chasers and set colors to full brightness.
        """
        self.root.controller.set_fullbright()

        while len(self.active_chasers):
            chaser = self.active_chasers.pop()
            self.root.controller.remove_chaser(chaser)

        self.update()

    def learn_speed(self):
        curtime = time.time()

        chaser = self.get_chaser()

        if chaser in self.chaser_speeds:
            speed, last_time = self.chaser_speeds.get(chaser)
            speed = curtime - last_time

            if chaser in self.active_chasers:
                self.root.controller.set_speed(chaser, speed)
        else:
            speed = None

        last_time = curtime

        self.chaser_speeds[chaser] = speed, last_time

    def up(self):
        if self.pointer == 0:
            self.pointer = self.listlen * self.COLUMNS - 1
        else:
            self.pointer -= 1
        self.update()

    def down(self):
        if self.pointer >= self.listlen * self.COLUMNS - 1:
            self.pointer = 0
        else:
            self.pointer += 1
        self.update()

    def left(self):
        self.pointer -= self.listlen
        if self.pointer < 0:
            self.pointer += self.listlen * self.COLUMNS
        self.update()

    def right(self):
        self.pointer += self.listlen
        if self.pointer >= self.listlen * self.COLUMNS - 1:
            self.pointer -= self.listlen * self.COLUMNS
        self.update()

    def pageup(self):
        newval = self.pointer - self.pointer % self.listlen
        if newval < 0:
            return
        self.pointer = newval
        self.update()

    def pagedown(self):
        newval = self.pointer + (self.listlen - self.pointer % self.listlen) - 1
        if newval >= self.listlen * self.COLUMNS:
            return
        self.pointer = newval
        self.update()

    def select(self, number):
        if not 0 <= number < self.listlen * self.COLUMNS:
            return

        self.pointer = number
        self.update()

    def update(self):
        clist = iter(self.chasers)

        for n in range(self.listlen * self.COLUMNS):
            try:
                chaser = clist.next()
            except StopIteration:
                chaser = None

            if chaser is not None:
                label = "%2d. %s" % (n, chaser.label)
            else:
                label = "%2d." % n

            label += " " * (self.listwidth - len(label))

            if self.pointer == n:
                if chaser is not None and chaser.is_cue:
                    attr = style.attr('list_cue_highlight')
                elif chaser is not None and chaser in self.active_chasers:
                    attr = style.attr('list_active_highlight')
                else:
                    attr = style.attr('list_highlight')
            else:
                if chaser is not None and chaser.is_cue:
                    attr = style.attr('list_cue')
                elif chaser is not None and chaser in self.active_chasers:
                    attr = style.attr('list_active_normal')
                else:
                    attr = style.attr('list_normal')

            self.menu.attron(attr)
            self.menu.addstr(n % self.listlen,
                             self.listwidth * (n // self.listlen),
                             label)
            self.menu.attroff(attr)

        return

        for n, c in enumerate(self.chasers):
            item = c[:self.listwidth]
            self.menu.addstr(n, 0, item)
            self.listlen

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        if hard:
            self.outer.redrawwin()
            self.menu.redrawwin()
        else:
            self.outer.refresh()
            self.menu.refresh()
        locking.refresh_lock.release()
