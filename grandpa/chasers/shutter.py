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

import random
import logging

from grandpa.chaser import Chaser

class SimpleShutter(Chaser):
    label = 'Simple shutter'

    def setup(self):
        self.i = 0

    def restart(self):
        self.i = 0

    def next(self):
        if self.i == 0:
            self.all_sections.color.alpha = 255
        else:
            self.all_sections.color.alpha = 0

        self.wait(0.3, frames=10)

        self.i += 1
        if self.i >= 10:
            self.i = 0

class DistortedShutter(Chaser):
    label = 'Distorted shutter'

    def setup(self):
        self.i = 0

    def restart(self):
        self.i = 0

    def next(self):
        if self.i == 0:
            sects = random.sample(xrange(self.sectlen), int(self.sectlen * 0.25))
            for n, s in enumerate(self.sections):
                if n in sects:
                    s.color.alpha = 255
                else:
                    s.color.alpha = 0
        else:
            self.all_sections.color.alpha = 0

        self.wait(0.3, frames=10)

        self.i += 1
        if self.i >= 10:
            self.i = 0

class ChainShutter(Chaser):
    label = 'Chain shutter'
    chains = 3

    def setup(self):
        self.i = 0
        self.siterator = self.sectiter()

    def restart(self):
        self.i = 0

    def sectiter(self):
        while True:
            for i in xrange(int(self.sectlen / 3)):
                cur = i * 3
                yield self.sections[cur:cur+3]

    def next(self):
        if self.i <= self.chains:
            for s in self.siterator.next():
                s.color.alpha = 255
        else:
            self.all_sections.color.alpha = 0

        self.wait(0.3, frames=10)

        self.i += 1
        if self.i >= 10:
            self.i = 0
