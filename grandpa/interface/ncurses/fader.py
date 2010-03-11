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

import style

from grandpa import locking

class FadeControl(object):
    def __init__(self):
        self.faders = []
        self.active_fader = None

    def add_fader(self, fader):
        self.faders.append(fader)

        if len(self.faders) == 1:
            self.switch_fader()

    def switch_fader(self):
        try:
            self.active_fader.deactivate()
        except AttributeError:
            pass

        try:
            i = self.faders.index(self.active_fader)
            self.active_fader = self.faders[i+1]
        except (ValueError, IndexError):
            self.active_fader = self.faders[0]

        self.active_fader.activate()

    def set(self, value):
        if self.active_fader is None:
            return
        self.active_fader.set(value)

    def adjust(self, value):
        if self.active_fader is None:
            return
        self.active_fader.adjust(value)

class Fader(object):
    def __init__(self, root, window):
        self.root = root

        self.is_active = False

        self.outer = window
        self.draw_box()

        y, x = self.outer.getmaxyx()
        inner = self.outer.derwin(y - 2, x - 2, 1, 1)
        self.fader = inner
        self.faderlen = y

        self.faderval = self.startval

        self.update_lock = threading.RLock()
        self.update()

        self.refresh()

    def activate(self):
        self.is_active = True
        self.draw_box()
        self.refresh()

    def deactivate(self):
        self.is_active = False
        self.draw_box()
        self.refresh()

    def draw_box(self):
        if self.is_active:
            attr = style.attr('fader_active')
        else:
            attr = style.attr('fader_inactive')

        self.outer.attron(attr)
        self.outer.box()
        self.outer.attroff(attr)

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

        if hasattr(self, 'set_faderval'):
            self.set_faderval(self.faderval)

        self.update_lock.release()

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        if hard:
            self.outer.redrawwin()
            self.fader.redrawwin()
        else:
            self.outer.refresh()
            self.fader.refresh()
        locking.refresh_lock.release()

class DimmerFader(Fader):
    startval = 255

    def set_faderval(self, value):
        self.root.tavern.update_dyndimmer(self.faderval)
        self.root.controller.dim_update()

class FadetimeFader(Fader):
    startval = 0
