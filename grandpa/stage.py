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

import math


class Stage(object):
    def num2fixture(self, number):
        """
        Convert fixture number to DMX start address and fixture type.
        """
        return self.FIXTURE_MAPPING.get(number, None)


class Rockfabrik(Stage):
    FIXTURE_MAPPING = {
        3: (0x12d, 'eurolite'),
        2: (0x139, 'eurolite'),
        1: (0x145, 'eurolite'),
        12: (0x151, 'eurolite'),
        11: (0x15d, 'eurolite'),
        10: (0x169, 'eurolite'),
        9: (0x175, 'eurolite'),
        8: (0x181, 'eurolite'),
        7: (0x18d, 'eurolite'),
        6: (0x199, 'eurolite'),
        5: (0x1a5, 'eurolite'),
        4: (0x1b1, 'eurolite'),
    }

    def set_flashers(self):
        """
        Return the default settings for all flashers.
        """
        return [
            ['XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX',
             'X.. X.. X.. X.. X.. X.. X.. X.. X.. X.. X.. X..',
             '.X. .X. .X. .X. .X. .X. .X. .X. .X. .X. .X. .X.',
             '..X ..X ..X ..X ..X ..X ..X ..X ..X ..X ..X ..X',
             '... XXX ... ... XXX ... ... XXX ... ... XXX ...'],

            ['XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX',
             'XXX ... ... ... ... XXX XXX ... ... ... ... XXX',
             '... XXX ... ... XXX ... ... XXX ... ... XXX ...',
             '... ... XXX XXX ... ... ... ... XXX XXX ... ...',
             'XXX XX. X.. X.. XX. XXX XXX XX. X.. X.. XX. XXX'],

            ['XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX',
             'XXX ... ... XXX ... ... XXX ... ... XXX ... ...',
             '... XXX ... ... XXX ... ... XXX ... ... XXX ...',
             '... ... XXX ... ... XXX ... ... XXX ... ... XXX',
             'X.. X.. X.. X.. X.. X.. X.. X.. X.. X.. X.. X..'],

            ['... ... ... ... ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... ... ... ... ...',
             '... ... ... ... ... ... ... ... ... ... ... ...'],
        ]

    def set_view(self, tavern, bar_length, width, height):
        """
        The view for the Rockfabrik stage design.
        """
        midx, midy = int(width / 2), int(height / 2)

        radius_vertical = midy
        radius_horizontal = midx

        bar_midx = int((bar_length + 2) / 2)
        bar_midy = 1.5

        angles = [6, 15, 29]

        all_angles = list(angles)
        all_angles += reversed(map(lambda x: (90 - x) + 90, angles))
        all_angles += map(lambda x: x + 180, angles)
        all_angles += reversed(map(lambda x: (90 - x) + 270, angles))

        for degree in all_angles:
            angle = degree * math.pi / 180.0

            r = 0.5 / math.sqrt(
                (math.cos(angle) / float(radius_horizontal * 2)) ** 2 +
                (math.sin(angle) / float(radius_vertical * 2)) ** 2
            )

            cx = midx + r * math.cos(angle)
            cy = midy + r * math.sin(angle)

            x = int(cx - bar_midx)
            y = int(cy - bar_midy)

            if 90 < degree <= 270:
                # left
                x += 15
            else:
                # right
                x -= 15

            if degree > 180:
                # upper
                y += 5
            else:
                # lower
                y -= 5

            # ensure that we don't leave the window

            if y > (height - 3):
                y = height - 3
            elif y < 0:
                y = 0

            if x > (width - bar_length - 2):
                x = width - bar_length - 2
            elif x < 0:
                x = 0

            if 90 < degree <= 270:
                tavern.newbar(x, y, inverted=True)
            else:
                tavern.newbar(x, y)
