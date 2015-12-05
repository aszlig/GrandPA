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

COLORMAP = {
    (0xaa, 0x00, 0x00): 'bright_red',       # dark red
    (0x00, 0xaa, 0x00): 'japanese_laurel',  # dark green
    (0x00, 0x00, 0xaa): 'dark_blue',        # dark blue
    (0x00, 0xaa, 0xaa): 'persian_green',    # dark cyan
    (0xaa, 0x00, 0xaa): 'flirt',            # dark magenta
    (0xaa, 0x55, 0x00): 'chelsea_gem',      # dark yellow
    (0xaa, 0xaa, 0xaa): 'silver_chalice',   # dark white

    (0xff, 0x55, 0x55): 'persimmon',        # light red
    (0x55, 0xff, 0x55): 'screaming_green',  # light green
    (0x55, 0x55, 0xff): 'dodger_blue',      # light blue
    (0x55, 0xff, 0xff): 'aquamarine',       # light cyan
    (0xff, 0x55, 0xff): 'pink_flamingo',    # light magenta
    (0xff, 0xff, 0x55): 'gorse',            # light yellow
    (0xff, 0xff, 0xff): 'white',            # light white

    (0x55, 0x55, 0x55): 'emperor',          # light black
}


class Color(object):
    def __init__(self, red=0, green=0, blue=0, alpha=255):
        self.__red = int(red)
        self.__green = int(green)
        self.__blue = int(blue)
        self.__alpha = int(alpha)

        self.reset_changed()

    def __correct_channel(self, value):
        if value > 255:
            return 255
        elif value < 0:
            return 0
        return value

    def _set_red(self, val):
        self._changed[0] = True
        self.__red = self.__correct_channel(val)
    red = property(lambda x: x.__red, _set_red)

    def _set_green(self, val):
        self._changed[1] = True
        self.__green = self.__correct_channel(val)
    green = property(lambda x: x.__green, _set_green)

    def _set_blue(self, val):
        self._changed[2] = True
        self.__blue = self.__correct_channel(val)
    blue = property(lambda x: x.__blue, _set_blue)

    def _set_alpha(self, val):
        self._changed[3] = True
        self.__alpha = self.__correct_channel(val)
    alpha = property(lambda x: x.__alpha, _set_alpha)

    def __set_channels_direct(self, red, green, blue, alpha):
        if red is not None:
            self.__red = int(red)
        if green is not None:
            self.__green = int(green)
        if blue is not None:
            self.__blue = int(blue)
        if alpha is not None:
            self.__alpha = int(alpha)

    @property
    def has_changed(self):
        return any(self._changed)

    def __repr__(self):
        return "Color(%d, %d, %d, %d)" % self.to_tuple(alpha=True)

    def __add__(self, color):
        """
        Alpha blending between own instance and color.
        """
        (sr, sg, sb, sa) = self.to_tuple(floats=True, alpha=True)
        (dr, dg, db, da) = color.to_tuple(floats=True, alpha=True)

        if sa <= 0:
            return Color(dr * 255, dg * 255, db * 255, da * 255)

        a = 1.0 - (1.0 - da) * (1.0 - sa)
        r = dr * da / a + sr * sa * (1.0 - da) / a
        g = dg * da / a + sg * sa * (1.0 - da) / a
        b = db * da / a + sb * sa * (1.0 - da) / a

        return Color(r * 255, g * 255, b * 255, a * 255)

    def __nonzero__(self):
        return self.__red > 0 or self.green > 0 or self.blue > 0

    def _convert(self, converter, floats=False, alpha=False, use_alpha=None):
        chans = [self.__red, self.__green, self.__blue]

        if alpha:
            chans.append(self.__alpha)
        else:
            chans = [c / 255.0 * self.__alpha for c in chans]

            if use_alpha is not None:
                chans = [c / 255.0 * use_alpha for c in chans]

        if floats:
            chans = [c / 255.0 for c in chans]
        else:
            chans = [int(c) for c in chans]

        return converter(chans)

    def to_tuple(self, *args, **kwargs):
        return self._convert(tuple, *args, **kwargs)

    def to_list(self, *args, **kwargs):
        return self._convert(list, *args, **kwargs)

    def copy(self):
        return Color(*self.to_tuple(alpha=True))

    def set_channels(self, red, green, blue, alpha=255):
        """
        Set colors and/or the alpha value.
        """
        self.red = int(red)
        self.green = int(green)
        self.blue = int(blue)
        self.alpha = int(alpha)

    def set_color(self, color):
        if isinstance(color, Color):
            self.red = color.red
            self.green = color.green
            self.blue = color.blue
            self.alpha = color.alpha
        else:
            self.set_channels(*color)

    def get_name(self):
        """
        Get color name for GrandPA curses attributes.
        """
        cur_color = self.to_tuple()
        highest = max(cur_color)
        if highest <= 0:
            return None

        if highest > 170:
            mod = 255.0 / highest
        elif highest > 85:
            mod = 170.0 / highest
        else:
            mod = 85 / highest

        red, green, blue = [c * mod for c in cur_color]

        def _keyfunc(color):
            r = red - color[0]
            g = green - color[1]
            b = blue - color[2]

            # calculate the euclidean distance
            return r * r + g * g + b * b

        match = min(COLORMAP, key=_keyfunc)
        return COLORMAP.get(match)

    def patch_color(self, color, in_place=False):
        """
        Patch differences to color *IN PLACE*.
        """
        if not color.has_changed:
            return

        diff = zip(color._changed, color.to_tuple(alpha=True))
        new = [None if not f else c for f, c in diff]
        self.__set_channels_direct(*new)

    def reset_changed(self):
        self._changed = [False] * 4

    def fade_to(self, color, steps=255):
        step = int(steps / 255)
        col = color.copy()
        for alpha in xrange(0, 255, step):
            col.alpha = alpha
            yield self.__add__(col)

if __name__ == '__main__':
    c1 = Color(255, 255, 255)
    c2 = Color(0, 0, 0)

    c2.red = 22
    c2.alpha = 0

    c1.patch_color(c2)
    print c1
    raise SystemExit

    print Color(100, 200, 0).get_name()
    print Color(100, 200, 0).get_name()

    for c in Color(255, 0, 0, 20).fade_to(Color(0, 0, 255)):
        print c
