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

import style

from grandpa import locking

class StatusLine(object):
    def __init__(self, root, win):
        self.root = root
        self.win = win
        self.width = win.getmaxyx()[1]

        self.cmd = ''
        self.error = None
        self.stack = 0

        self.update_lock = threading.Lock()

        self.update()

    def update(self):
        self.update_lock.acquire()

        if self.error is None:
            attr = style.attr('statusbar')
        else:
            attr = style.attr('statusbar_error')

        line = 'Command: [%10s]' % self.cmd[:10]
        line += '  Stack:'

        for s in xrange(4):
            if s == self.stack:
                line += " <%d>" % (s+1)
            else:
                line += "  %d " % (s+1)

        if self.error is not None:
            err = ' Error: %s' % self.error
        else:
            err = ''

        line += err.rjust(self.width - len(line) - 1)

        self.win.addnstr(0, 0, line, self.width - 1, attr)

        self.update_lock.release()

    def set_stack(self, number):
        self.stack = number
        self.update()

    def reset_error(self):
        self.error = None
        self.update()
        self.refresh()

    def set_error(self, msg):
        self.error = msg
        self.update()
        self.refresh()

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        if hard:
            self.win.redrawwin()
        else:
            self.win.refresh()
        locking.refresh_lock.release()
