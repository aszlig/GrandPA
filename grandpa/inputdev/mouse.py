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
import threading
import _mouse

class Mouse(threading.Thread):
    def __init__(self, fader):
        threading.Thread.__init__(self)
        self.do_quit = threading.Event()

        self.fader = fader

        _mouse.open()

    def run(self):
        # XXX: remove this pesky workaround as soon as we found the critical
        #      section for update/refresh distortion.
        time.sleep(1)

        while not self.do_quit.isSet():
            pos = _mouse.getpos(timeout=1)
            if pos is None:
                continue

            if pos[4] == _mouse.BUTTON1_PRESSED:
                self.fader.set(255)
                continue
            elif pos[4] == _mouse.BUTTON2_PRESSED:
                self.fader.set(0)
                continue

            dy = pos[3]

            if self.do_quit.isSet():
                break

            self.fader.adjust(-dy * 20)

        _mouse.close()

    def stop(self):
        self.do_quit.set()
        self.join()
