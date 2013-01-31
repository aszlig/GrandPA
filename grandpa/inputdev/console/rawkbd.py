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

import os
import sys
import array
import termios
import fcntl

# from linux/kd.h:
KDGKBMODE = 0x4B44  # get current keyboard mode
KDSKBMODE = 0x4B45  # set current keyboard mode

K_MEDIUMRAW = 0x02

termstate = {}


def set_raw_kbd():
    fd = os.open('/dev/tty', os.O_RDONLY)

    kbdmode = array.array('h', [0])

    fcntl.ioctl(fd, KDGKBMODE, kbdmode)
    fcntl.ioctl(fd, KDSKBMODE, K_MEDIUMRAW)

    term = termios.tcgetattr(sys.stdin.fileno())

    newterm = list(term)
    c_iflag = term[0]
    c_cc = term[6]

    # unset some iflags
    newterm[0] &= ~(termios.ISTRIP | termios.INLCR | termios.IGNCR |
                    termios.ICRNL | termios.IUCLC | termios.IXON)
    newterm[0] &= ~(termios.ECHO | termios.ICANON | termios.IEXTEN |
                    termios.ISIG | termios.XCASE)

    # unset control characters and buffering of them
    newterm[6][termios.VMIN] = 0
    newterm[6][termios.VTIME] = 0

    termios.tcsetattr(sys.stdin.fileno(), termios.TCSANOW, newterm)

    termstate['fd'] = fd
    termstate['kbdmode'] = kbdmode
    termstate['term'] = term


def unraw():
    """
    Temporarily unraw the keyboard.
    """
    fcntl.ioctl(termstate['fd'], KDSKBMODE, termstate['kbdmode'][0])
    fd = sys.stdin.fileno()
    os.read(fd, 1)
    fcntl.ioctl(termstate['fd'], KDSKBMODE, K_MEDIUMRAW)


def restore_kbd():
    termios.tcsetattr(sys.stdin.fileno(), termios.TCSANOW, termstate['term'])
    fcntl.ioctl(termstate['fd'], KDSKBMODE, termstate['kbdmode'][0])
    os.close(termstate['fd'])
