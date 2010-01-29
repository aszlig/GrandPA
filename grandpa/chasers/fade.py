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

from grandpa.chaser import Chaser

class SimpleFade(Chaser):
    label = 'Simple section fade'
    frames = 30
    length = 8

    def setup(self):
        self.jumper = iter(self.sectionjump())

    def sectionjump(self):
        x = list(range(self.length))
        mod = 255 / self.frames

        while True:
            for frame in xrange(self.frames):
                first = self.sections[x[0]]
                first.color.alpha -= mod

                last = self.sections[x[-1]]
                last.color.alpha += mod

                for n, i in enumerate(x[1:-1]):
                    section = self.sections[i]
                    section.color.alpha -= int(mod / (n + 1))
                yield

            x = [n + 1 if n + 1 <self.sectlen else 0
                 for n in x]

    def next(self):
        self.jumper.next()
        self.wait(0.3, frames=self.frames)

class DistortedFade(Chaser):
    label = 'Distorted section fade'
    frames = 30

    def setup(self):
        self.step = int(255.0 / self.frames)
        self.jumper = iter(self.sectionjump())

    def sectionjump(self):
        while True:
            decisions = []
            sects = random.sample(xrange(self.sectlen), int(self.sectlen * 0.5))

            for n, s in enumerate(self.sections):
                if n in sects:
                    decisions.append((s.color, self.step))
                else:
                    decisions.append((s.color, -self.step))

            for dummy in xrange(self.frames):
                for color, val in decisions:
                    color.alpha += val
                yield

    def next(self):
        self.jumper.next()
        self.wait(0.3, frames=self.frames)

if __name__ == '__main__':
    s = SectionFade(None, [1,2,3,4,5,6,7,8,9])
    i = iter(s.sectionjump())
    for n in xrange(60):
        print i.next()
