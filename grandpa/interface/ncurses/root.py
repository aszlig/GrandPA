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

from grandpa import controller

from status import StatusLine
from tavern import Tavern
from menu import Menu
from fader import Fader
from clock import Clock
import fb

from grandpa import locking

class Root(object):
    # must be divisible by 3
    BAR_LENGTH = 39

    def __init__(self, win, config):
        self.win = win
        self.config = config

        root_my, root_mx = win.getmaxyx()

        # the dmx/chaser controller
        self.controller = controller.Controller(self)

        # viewport
        self.view_win = self.viewport()
        view_y, view_x = self.view_win.getparyx()
        view_max_y, view_max_x = self.view_win.getmaxyx()

        # clock
        clock_win = self.view_win.derwin(3, 41, int(view_max_y / 2 - 1.5),
                                         int(view_max_x / 2 - 15))

        self.clock = Clock(clock_win)

        # status line
        status_win = win.derwin(1, root_mx, root_my - 1, 0)
        self.status = StatusLine(self, status_win)

        # menu
        menu_height = view_y
        menu_width = root_mx
        menu_win = win.derwin(menu_height, menu_width, 0, 0)
        self.menu = Menu(self, menu_win)

        # tavern with bars
        try:
            fbdev = fb.Framebuffer(root_mx, root_my)
        except Exception, e:
            fbdev = None
            self.status.set_error(e[0])
        self.tavern = Tavern(self, fbdev)
        self.tavern.setstage(config['stage'])
        self.tavern.refresh()

        # dimmer fader
        fader_width = 3
        fader_height = root_my - menu_height - 1
        fader_x = root_mx - 3
        fader_y = menu_height
        fader_win = win.derwin(fader_height, fader_width, fader_y, fader_x)
        self.fader = Fader(self, fader_win)

        self.controller.start()
        self.clock.start()

    def stop(self):
        self.clock.stop()
        self.controller.stop()

    def viewport(self):
        """
        Stage simulation.
        """
        max_y, max_x = self.win.getmaxyx()

        width = int(max_x * 0.75)
        height = int(max_y * 0.75)

        x = int(max_x / 2 - width / 2)
        # towards the status bar
        y = max_y - height

        view = self.win.derwin(height, width, y, x)
        return view

    def getch(self):
        return self.win.getch()

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        self.tavern.refresh(hard=hard)
        self.status.refresh(hard=hard)
        self.menu.refresh(hard=hard)
        if hard:
            self.win.redrawwin()
            self.view_win.redrawwin()
        else:
            self.view_win.refresh()
        locking.refresh_lock.release()
