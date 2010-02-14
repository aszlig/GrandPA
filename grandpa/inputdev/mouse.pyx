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

cdef extern from "gpm.h":
    enum Gpm_Etype:
        GPM_MOVE   = 1
        GPM_DRAG   = 2
        GPM_DOWN   = 4
        GPM_UP     = 8
        GPM_SINGLE = 16
        GPM_DOUBLE = 32
        GPM_TRIPLE = 64
        GPM_MFLAG  = 128
        GPM_HARD   = 256
        GPM_ENTER  = 512
        GPM_LEAVE  = 1024

    enum Gpm_Margin:
        GPM_TOP = 1
        GPM_BOT = 2
        GPM_LFT = 4
        GPM_RGT = 8

    ctypedef struct Gpm_Connect:
        unsigned short eventMask, defaultMask
        unsigned short minMod, maxMod
        int pid
        int vc

    ctypedef struct Gpm_Event:
        unsigned char buttons, modifiers
        unsigned short vc
        short dx, dy, x, y
        Gpm_Etype type
        int clicks
        Gpm_Margin margin
        short wdx, wdy

    int GPM_DOWN
    int GPM_B_LEFT
    int GPM_B_MIDDLE
    int GPM_B_RIGHT
    int GPM_DOWN
    int GPM_B_LEFT
    int GPM_B_MIDDLE
    int GPM_B_RIGHT

    int Gpm_Open(Gpm_Connect*, int)
    int Gpm_Close()
    int Gpm_GetEvent(Gpm_Event*)

    int gpm_fd

cdef extern from "sys/select.h":
    cdef struct timeval:
        pass

    ctypedef struct fd_set:
        pass

    void FD_SET(int, fd_set*)
    void FD_ZERO(fd_set*)

    int select(int, fd_set*, fd_set*, fd_set*, timeval*) with gil

BUTTON1_PRESSED  = 1
BUTTON2_PRESSED  = 2
BUTTON3_PRESSED  = 4

BUTTON1_RELEASED = 8
BUTTON2_RELEASED = 16
BUTTON3_RELEASED = 32

class Mouse(threading.Thread):
    def __init__(self, fader):
        threading.Thread.__init__(self)
        self.do_quit = threading.Event()

        self.fader = fader

        self.open()

    def open(self):
        cdef Gpm_Connect connection

        connection.eventMask = ~0
        connection.defaultMask = 0
        connection.minMod = 0
        connection.maxMod = ~0

        if Gpm_Open(&connection, 0) == -1:
            raise OSError("Cannot open mouse.")

    def getpos(self, timeout=0):
        cdef Gpm_Event ev
        cdef fd_set gpmwatch
        cdef timeval tv

        cdef int result
        cdef int event
        cdef int buttons

        if timeout > 0:
            FD_ZERO(&gpmwatch);
            FD_SET(gpm_fd, &gpmwatch);

            with nogil:
                result = select(gpm_fd + 1, &gpmwatch, NULL, NULL, &tv)
            if result == -1:
                raise OSError("Cannot select.")
            elif result == 0:
                return None

        event = Gpm_GetEvent(&ev)

        if event <= 0:
            raise OSError("Cannot get GPM event.")

        buttons = 0

        etype = ev.type & 0x0f
        if etype == GPM_DOWN:
            if ev.buttons & GPM_B_LEFT:
                buttons |= BUTTON1_PRESSED
            if ev.buttons & GPM_B_MIDDLE:
                buttons |= BUTTON3_PRESSED
            if ev.buttons & GPM_B_RIGHT:
                buttons |= BUTTON2_PRESSED;
        elif etype == GPM_DOWN:
            if ev.buttons & GPM_B_LEFT:
                buttons |= BUTTON1_RELEASED
            if ev.buttons & GPM_B_MIDDLE:
                buttons |= BUTTON3_RELEASED
            if ev.buttons & GPM_B_RIGHT:
                buttons |= BUTTON2_RELEASED

        return (ev.x, ev.y, ev.dx, ev.dy, buttons)

    def close(self):
        Gpm_Close()

    def run(self):
        while not self.do_quit.isSet():
            pos = self.getpos(timeout=1)
            if pos is None:
                continue

            if pos[4] == BUTTON1_PRESSED:
                self.fader.set(255)
                continue
            elif pos[4] == BUTTON2_PRESSED:
                self.fader.set(0)
                continue

            dy = pos[3]

            if self.do_quit.isSet():
                break

            self.fader.adjust(-dy * 20)

        self.close()

    def stop(self):
        self.do_quit.set()
        self.join()
