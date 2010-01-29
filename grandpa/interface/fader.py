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

import threading
import curses

from grandpa import locking

class Fader(object):
    def __init__(self, root, window):
        self.root = root

        self.outer = window
        self.outer.box()

        y, x = self.outer.getmaxyx()
        inner = self.outer.derwin(y - 2, x - 2, 1, 1)
        self.fader = inner
        self.faderlen = y

        self.faderval = 255

        self.dyndims = []

        self.update_lock = threading.RLock()
        self.update()

        self.refresh()

    def _correct(self):
        if self.faderval > 255:
            self.faderval = 255
        elif self.faderval < 0:
            self.faderval = 0

    def set(self, value):
        self.update_lock.acquire()
        self.faderval = value
        self._correct()
        self.update()
        self.update_lock.release()

    def adjust(self, value):
        self.update_lock.acquire()
        self.faderval += value
        self._correct()
        self.update()
        self.update_lock.release()

    def update(self):
        self.update_lock.acquire()

        bottom = int(self.faderlen / 255.0 * self.faderval)
        top = self.faderlen - bottom

        self.fader.vline(0, 0, ' ', top)
        if bottom > 2:
            self.fader.vline(top, 0, curses.ACS_BLOCK, bottom)

        # only refresh the inner window
        locking.refresh_lock.acquire()
        self.fader.refresh()
        locking.refresh_lock.release()

        self.root.tavern.update_dyndimmer(self.faderval)
        self.root.controller.dim_update()

        self.update_lock.release()

    def refresh(self):
        locking.refresh_lock.acquire()
        self.outer.refresh()
        self.fader.refresh()
        locking.refresh_lock.release()
