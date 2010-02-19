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

import curses

class Attr(object):
    def __init__(self, fg=0, bg=0, attr=0):
        self.fg = fg
        self.bg = bg
        self.attr = attr
        self.pairnum = 0

    def cursesattr(self):
        return curses.color_pair(self.pairnum) | self.attr

class Style(object):
    def __init__(self):
        self._attrs = {}
        for attr in dir(self):
            a = getattr(self, attr)
            if isinstance(a, Attr):
                self._attrs[attr] = a

    def init_pairs(self):
        for n, attr in enumerate(self._attrs.values()):
            curses.init_pair(n + 1, attr.fg, attr.bg)
            attr.pairnum = n + 1

    def attr(self, name):
        if name is None:
            return 0

        attr = self._attrs.get(name)
        return attr.cursesattr()

class Default(Style):
    # colors
    color_bright_red        = Attr(curses.COLOR_RED)
    color_japanese_laurel   = Attr(curses.COLOR_GREEN)
    color_dark_blue         = Attr(curses.COLOR_BLUE)
    color_persian_green     = Attr(curses.COLOR_CYAN)
    color_flirt             = Attr(curses.COLOR_MAGENTA)
    color_chelsea_gem       = Attr(curses.COLOR_YELLOW)
    color_silver_chalice    = Attr(curses.COLOR_WHITE)

    color_persimmon         = Attr(curses.COLOR_RED,     attr=curses.A_BOLD)
    color_screaming_green   = Attr(curses.COLOR_GREEN,   attr=curses.A_BOLD)
    color_dodger_blue       = Attr(curses.COLOR_BLUE,    attr=curses.A_BOLD)
    color_aquamarine        = Attr(curses.COLOR_CYAN,    attr=curses.A_BOLD)
    color_pink_flamingo     = Attr(curses.COLOR_MAGENTA, attr=curses.A_BOLD)
    color_gorse             = Attr(curses.COLOR_YELLOW,  attr=curses.A_BOLD)
    color_white             = Attr(curses.COLOR_WHITE,   attr=curses.A_BOLD)

    color_emperor           = Attr(curses.COLOR_BLACK,   attr=curses.A_BOLD)

    # bar boxes
    bar_selected          = Attr(curses.COLOR_CYAN)
    bar_deselected        = Attr(curses.COLOR_BLACK, attr=curses.A_BOLD)
    bar_activated         = Attr(curses.COLOR_WHITE, attr=curses.A_BOLD)

    # section numbers
    section_selected      = Attr(curses.COLOR_CYAN,  attr=curses.A_BOLD)
    section_deselected    = Attr(curses.COLOR_BLACK, attr=curses.A_BOLD)
    section_activated     = Attr(curses.COLOR_WHITE, attr=curses.A_BOLD)

    # status bar
    statusbar             = Attr(curses.COLOR_WHITE, curses.COLOR_BLUE,    curses.A_BOLD)
    statusbar_error       = Attr(curses.COLOR_WHITE, curses.COLOR_RED,     curses.A_BOLD)

    # cue/chaser menu
    list_cue              = Attr(curses.COLOR_GREEN,  attr=curses.A_BOLD)
    list_normal           = Attr(curses.COLOR_RED,    attr=curses.A_BOLD)
    list_cue_highlight    = Attr(curses.COLOR_GREEN,  curses.COLOR_YELLOW, curses.A_BOLD)
    list_highlight        = Attr(curses.COLOR_RED,    curses.COLOR_GREEN,  curses.A_BOLD)
    list_active_normal    = Attr(curses.COLOR_YELLOW, attr=curses.A_BOLD)
    list_active_highlight = Attr(curses.COLOR_YELLOW, curses.COLOR_GREEN,  curses.A_BOLD)

    # obvious ;-)
    clock_number          = Attr(curses.COLOR_CYAN,   attr=curses.A_BOLD)
    clock_dots            = Attr(curses.COLOR_CYAN)

theme = Default()
attr = theme.attr
