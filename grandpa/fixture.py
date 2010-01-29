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

from grandpa.color import Color

class Section(object):
    def __init__(self, bar, visual):
        self.bar = bar
        self.visual = visual

        self.color = Color()
        #self.basecolor = Color()

    def has_changed(self):
        return self.color.has_changed

class Bar(object):
    def __init__(self, addr, fixtype, sections):
        self.addr = addr
        self.fixtype = fixtype

        self.sections = [Section(self, s) for s in sections]

    def make_colors(self):
        colors = []
        for s in self.sections:
            s.visual.dimmer_lock.acquire()
            s.visual.color.set_color(s.color)
            if s.visual.dimmer is None:
                colors += [0, 0, 0]
            else:
                colors += [int(c / 255.0 * s.visual.dimmer)
                           for c in s.color.to_tuple()]
            s.visual.dimmer_lock.release()
        return colors

    def dmxout(self):
        colors = self.make_colors()
        if self.fixtype == 'eurolite':
            # ctrl, dimmer, shutter
            values = [10, 255, 0] + colors
        elif self.fixtype == 'americandj':
            # shutter, dimmer
            values = colors + [0, 255]
        elif self.fixtype == 'test':
            # my moving head for testing the dmx controller
            values = [0, 0, 0, 0, 0, 255] + colors[:3] + [0, 0, 0, 0]

        return self.addr, values
