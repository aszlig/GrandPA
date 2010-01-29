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

from grandpa import style

import root

class StatusLine(object):
    def __init__(self, root):
        max_y, max_x = root.root.getmaxyx()
        self.status = root.root.derwin(1, max_x, max_y - 1, 0)
        self.width = max_x

        self.cmd = ''
        self.dmxerror = None
        self.stack = 0

        self.update_lock = threading.Lock()

        self.update()

    def update(self):
        self.update_lock.acquire()

        if self.dmxerror is None:
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

        if self.dmxerror is not None:
            err = ' DMX Error: %s' % self.dmxerror
        else:
            err = ''

        line += err.rjust(self.width - len(line) - 1)

        self.status.addnstr(0, 0, line, self.width - 1, attr)

        self.update_lock.release()

    def set_stack(self, number):
        self.stack = number
        self.update()

    def reset_dmx_error(self):
        self.dmxerror = None
        self.update()
        self.refresh()

    def set_dmx_error(self, msg):
        self.dmxerror = msg
        self.update()
        self.refresh()

    def refresh(self):
        root.refresh_lock.acquire()
        self.status.refresh()
        root.refresh_lock.release()
