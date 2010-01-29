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

from status import StatusLine
from tavern import Tavern
from menu import Menu
from fader import Fader

# must be divisible by 3
BAR_LENGTH = 39

refresh_lock = threading.RLock()

class Root(object):
    def __init__(self, rootwin, config):
        self.root = rootwin
        self.config = config

        self.view = self.viewport()
        self.status = StatusLine(self)

        view_y, view_x = self.view.getparyx()
        root_my, root_mx = rootwin.getmaxyx()

        # menu
        menu_height = view_y
        menu_width = root_mx
        menu_win = rootwin.derwin(menu_height, menu_width, 0, 0)
        self.menu = Menu(self, menu_win)

        self.tavern = Tavern(self)
        self.tavern.setstage(config['stage'])
        self.tavern.refresh()

        # dimmer fader
        fader_width = 3
        fader_height = root_my - menu_height - 1
        fader_x = root_mx - 3
        fader_y = menu_height
        fader_win = rootwin.derwin(fader_height, fader_width, fader_y, fader_x)
        self.fader = Fader(self, fader_win)

    def viewport(self):
        """
        Stage simulation.
        """
        max_y, max_x = self.root.getmaxyx()

        width = int(max_x * 0.75)
        height = int(max_y * 0.75)

        x = int(max_x / 2 - width / 2)
        # towards the status bar
        y = max_y - height

        view = self.root.derwin(height, width, y, x)
        return view

    def refresh(self):
        refresh_lock.acquire()
        self.tavern.refresh()
        self.status.refresh()
        self.menu.refresh()
        refresh_lock.release()
