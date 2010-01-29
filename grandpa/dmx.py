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

import os
import serial

class DMXError(Exception):
    pass

class DMX(object):
    def __init__(self, port):
        baudrate = 57600 # 115200

        if not os.path.exists(port):
            raise DMXError("Serial port at %s not found!" % port)

        try:
            self.device = serial.Serial(port=port, baudrate=baudrate, timeout=1)
        except serial.SerialException, e:
            raise DMXError(e)

    def send(self, packet):
        packet_size = len(packet) + 1

        data = [0x7E, # start of message
                6,    # send DMX
                packet_size        & 0xFF,
                (packet_size >> 8) & 0xFF]

        data += [0] + packet

        data.append(0xE7) # end of message

        self.device.write(''.join([chr(c) for c in data]))

    def close(self):
        self.device.close()
