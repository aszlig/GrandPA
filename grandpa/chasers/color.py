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

import random

from grandpa.chaser import Chaser
from grandpa.color import Color as BaseColor


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


class Cyan(Color):
    label = 'Cyan'
    code = (0, 255, 255)


class Magenta(Color):
    label = 'Magenta'
    code = (255, 0, 127)


class Pink(Color):
    label = 'Pink'
    code = (255, 0, 255)


class Yellow(Color):
    label = 'Yellow'
    code = (255, 255, 0)


class Congo(Color):
    label = 'Congo'
    code = (167, 0, 255)


class Coral(Color):
    label = 'Coral'
    code = (255, 127, 80)


class Orange(Color):
    label = 'Orange'
    code = (255, 127, 0)


class MidnightBlue(Color):
    label = 'Midnight blue'
    code = (25, 25, 112)


class White(Color):
    label = 'White'
    code = (255, 255, 255)


class DistortedColorFade(Chaser):
    label = 'Distorted color fade'
    frames = 1

    def setup(self):
        self.step = int(255.0 / self.frames)
        self.jumper = iter(self.sectionjump())

    def sectionjump(self):
        while True:
            for s in self.sections:
                red = random.randint(0, 255)
                green = random.randint(0, 255)
                blue = random.randint(0, 255)

                s.color.set_channels(red, green, blue)
            yield

    def next(self):
        self.jumper.next()
        self.wait(0.5, frames=self.frames)


class SwapBlue(Chaser):
    label = 'Swap blue'
    frames = 1

    def setup(self):
        self.step = int(255.0 / self.frames)
        self.jumper = iter(self.sectionjump())
        self.active = False
        self.initial = self.sections[0].color.copy()

    def sectionjump(self):
        while True:
            self.active = not self.active

            if self.active:
                color = BaseColor(0, 0, 255)
            else:
                color = self.initial

            for s in self.sections:
                s.color.set_color(color)
            yield

    def next(self):
        self.jumper.next()
        self.wait(0.5, frames=self.frames)
