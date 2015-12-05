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


class Stage(object):
    def num2fixture(self, number):
        """
        Convert fixture number to DMX start address and fixture type.
        """
        return self.FIXTURE_MAPPING.get(number, None)


class Rockfabrik(Stage):
    FIXTURE_MAPPING = {
        9: (0x12d, 'eurolite'),
        1: (0x139, 'eurolite'),
        6: (0x145, 'eurolite'),
        4: (0x151, 'eurolite'),
        2: (0x15d, 'eurolite'),
        8: (0x169, 'eurolite'),
        3: (0x175, 'eurolite'),
        7: (0x181, 'eurolite'),
        11: (0x18d, 'eurolite'),
        10: (0x199, 'eurolite'),
        12: (0x1a5, 'eurolite'),
        5: (0x1b1, 'eurolite'),
    }

    def set_flashers(self):
        """
        Return the default settings for all flashers.
        """
        return [
            ['XXX XXX XXX XXX XXX XXX ... ... ... ... ... ...',
             'X.. X.. X.. X.. X.. X.. ... ... ... ... ... ...',
             '.X. .X. .X. .X. .X. .X. ... ... ... ... ... ...',
             '..X ..X ..X ..X ..X ..X ... ... ... ... ... ...',
             '... XXX ... ... XXX ... ... ... ... ... ... ...',
             '... ... ... ... ... ... X.. X.. X.. X.. X.. X..',
             '... ... ... ... ... ... .X. .X. .X. .X. .X. .X.',
             '... ... ... ... ... ... ..X ..X ..X ..X ..X ..X',
             '... ... ... ... ... ... XXX XXX XXX XXX XXX XXX'],

            ['XXX XXX XXX XXX XXX XXX ... ... ... ... ... ...',
             'XXX ... ... ... ... XXX ... ... ... ... ... ...',
             '... XXX ... ... XXX ... ... ... ... ... ... ...',
             '... ... XXX XXX ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... XXX ... ... XXX ... ...',
             '... ... ... ... ... ... ... XXX ... ... XXX ...',
             '... ... ... ... ... ... ... ... XXX ... ... XXX',
             '... ... ... ... ... ... XXX XXX XXX XXX XXX XXX'],

            ['XXX XXX XXX XXX XXX XXX ... ... ... ... ... ...',
             'XXX ... ... XXX ... ... ... ... ... ... ... ...',
             '... XXX ... ... XXX ... ... ... ... ... ... ...',
             '... ... XXX ... ... XXX ... ... ... ... ... ...',
             'X.X X.X X.X X.X X.X X.X ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... XXX XXX ... ...',
             '... ... ... ... ... ... ... XXX ... ... XXX ...',
             '... ... ... ... ... ... XXX ... ... ... ... XXX',
             '... ... ... ... ... ... XXX XXX XXX XXX XXX XXX'],

            ['XXX XXX XXX XXX XXX XXX ... ... ... ... ... ...',
             'X.. ..X X.. ..X X.. ..X ... ... ... ... ... ...',
             '..X X.. ..X X.. ..X X.. ... ... ... ... ... ...',
             '.X. .X. .X. .X. .X. .X. ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... XXX XXX XXX ... ... ...',
             '... ... ... ... ... ... ... ... ... XXX XXX XXX',
             'XXX XXX XXX ... ... ... ... ... ... ... ... ...',
             '... ... ... XXX XXX XXX ... ... ... ... ... ...'],
        ]

    def set_view(self, tavern, bar_length, width, height):
        """
        The view for the Rockfabrik stage design.
        """
        midx, midy = width / 2, height / 2

        bar_width = bar_length + 2
        bar_height = 3

        bar_midx = bar_width / 2
        bar_midy = bar_height / 2

        mid_barcount = 6
        mid_area = min(abs(height - 10), height)
        mid_spacing = (mid_area - (bar_height * mid_barcount))
        mid_spacing /= mid_barcount - 1
        mid_start = midy - mid_area / 2
        mid_bar_and_spacing = bar_height + mid_spacing

        for barnum in range(6):
            offset = mid_start + mid_bar_and_spacing * barnum
            tavern.newbar(int(midx - bar_midx), int(offset), inverted=True)

        for offset, inverted in [(0, True), (width - bar_width, False)]:
            tavern.newbar(offset, 0, view_inverted=inverted)
            tavern.newbar(offset, int(midy - bar_midy), view_inverted=inverted)
            if offset == 0:
                tavern.newbar(offset, height - bar_height, inverted=inverted)
            else:
                tavern.newbar(offset, height - bar_height,
                              view_inverted=inverted)
