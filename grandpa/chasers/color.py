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

from grandpa.chaser import Chaser

class Color(Chaser):
    is_cue = True

    def next(self):
        self.all_sections.color.set_channels(*self.code)

class Blackout(Color):
    label = 'Blackout'
    code = (0, 0, 0)

class Red(Color):
    label = 'Red'
    code = (255, 0, 0)

class Green(Color):
    label = 'Green'
    code = (0, 255, 0)

class Blue(Color):
    label = 'Blue'
    code = (0, 0, 255)

class Congo(Color):
    label = 'Congo'
    code = (50, 0, 74)

class Cyan(Color):
    label = 'Cyan'
    code = (0, 255, 255)

class Pink(Color):
    label = 'Pink'
    code = (255, 0, 255)

class Yellow(Color):
    label = 'Yellow'
    code = (255, 255, 0)

class White(Color):
    label = 'White'
    code = (255, 255, 255)
