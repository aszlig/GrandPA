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

"""
This is the Tavern which holds our visual bars. The Tavern itself doesn't have
any window itself, but updates the bars.
"""

from grandpa import stage
from grandpa import dyndim
from grandpa import locking

import fixture

class Tavern(object):
    def __init__(self, root, fbdev=None):
        self.root = root
        self.bars = []
        self.selected = set()
        self.fbdev = fbdev

    def newbar(self, x, y, inverted=False):
        num = len(self.bars) + 1
        bar = fixture.Bar(self.root, num, x, y, inverted, self.fbdev)
        self.bars.append(bar)

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        for b in self.bars:
            b.refresh(hard=hard)
        locking.refresh_lock.release()

    def get_chaser_fixtures(self):
        """
        Get all sections currently selected and return a fixture list.
        """
        fixtures = []

        for b in self.bars:
            sections = [s for s in b.sections if s.selected]
            if len(sections) == 0:
                continue

            f = fixture.Fixture(sections)
            fixtures.append(f)

        return fixtures

    def get_selection_struct(self, sectfun=None):
        """
        Get a list representiation from a selection.
        Every selection is represented as a boolean value.
        """
        if sectfun is None:
            sectfun = lambda x: x.selected

        struct = []

        for b in self.bars:
            sect = []
            for s in b.sections:
                sect.append(bool(sectfun(s)))

            struct.append(sect)

        return struct

    def set_selection_struct(self, struct, barfun=None, sectfun=None):
        """
        Set a selection or any other attribute to sections/bars from a list
        representiation.
        """
        if sectfun is None:
            sectfun = lambda x: setattr(x, 'selected', True)

        for n, s in enumerate(struct):
            bar = self.bars[n]

            for i, b in enumerate(s):
                sect = bar.sections[i]
                if b:
                    sectfun(sect)

            if all(s) and barfun is not None:
                barfun(bar)

    def update_dyndimmer(self, value):
        """
        Update dimmer values from current fader state.
        """
        for b in self.bars:
            for s in b.sections:
                if s.dimmer is not None:
                    s.dimmer = value
                    s.refresh()

    def spawn_dyndim(self, *args, **kwargs):
        return dyndim.DynamicDimmer(*args, **kwargs)

    def get_bar(self, number):
        """
        Get Bar object by number.
        """
        try:
            bar = self.bars[number]
        except IndexError:
            return None

        return bar

    def select_bar(self, bar):
        """
        Select a Bar object within the Tavern.
        """
        self.selected.add(bar)
        bar.selected = True

    def deselect_bar(self, bar):
        """
        Deselect a Bar object within the Tavern.
        """
        try:
            self.selected.remove(bar)
        except KeyError:
            pass
        bar.selected = False

    def activate_bar(self, number, section=None):
        """
        Select a Bar within the Tavern and also select sections.
        """
        bar = self.get_bar(number)
        self.select_bar(bar)

        if section is not None:
            cur = bar.sections[section]
            cur.selected = True

    def deactivate_bar(self, number, section=None):
        """
        Deselect a Bar within the Tavern and also deselect sections.
        """
        bar = self.get_bar(number)

        if section is not None:
            cur = bar.sections[section]
            cur.selected = False

        if not any([s.selected for s in bar.sections]):
            self.deselect_bar(bar)

    def toggle_bar(self, number, section=None):
        """
        Invert selection.
        """
        bar = self.get_bar(number)

        # toggle sections plus Bar object
        if section is not None:
            cur = bar.sections[section]
            cur.selected = not cur.selected

            if any([s.selected for s in bar.sections]):
                self.select_bar(bar)
            else:
                self.deselect_bar(bar)
            return

        # toggle bars, without sections
        if bar in self.selected:
            self.deselect_bar(bar)
        else:
            self.select_bar(bar)

    def set_full_dimmer(self, value):
        """
        Set dimmer value for all bars and all sections.
        """
        for b in self.bars:
            for s in b.sections:
                s.dimmer = value

    def setstage(self, setting):
        height, width = self.root.view_win.getmaxyx()
        cls = getattr(stage, setting)
        self.stage = cls()
        self.stage.set_view(self, self.root.BAR_LENGTH, width, height)

        # associate controller with all bars and interface sections
        for n, b in enumerate(self.bars):
            addr, fixtype = self.stage.num2fixture(n + 1)
            sects = b.sections
            self.root.controller.add_bar(addr, fixtype, sects)
