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

import threading
from functools import partial

import curses

from grandpa import style
from grandpa.color import Color

import root

class Section(object):
    """
    Represents a visual section where the dimmer gets delegated to DMXout.
    """
    DIMCHARS = [
        ' ', '+', '*', 'ACS_DIAMOND', 'ACS_BULLET', 'ACS_BOARD', 'ACS_PLUS', 'ACS_CKBOARD', 'ACS_LANTERN', 'ACS_BLOCK'
    ]

    def __init__(self, window, length):
        self.window = window
        self.length = length

        self.selected = False

        self.dimmer_lock = threading.RLock()
        self._dimmer = None

        dimchars = []
        for dc in self.DIMCHARS:
            if dc.startswith('ACS_'):
                dimchars.append(getattr(curses, dc))
            else:
                dimchars.append(dc)
        self.dimchars = dimchars

        self.color_lock = threading.Lock()
        self._color = Color()

    def _set_dimmer(self, val):
        self.dimmer_lock.acquire()
        self._dimmer = val
        self.dimmer_lock.release()
    def _get_dimmer(self):
        self.dimmer_lock.acquire()
        ret = self._dimmer
        self.dimmer_lock.release()
        return ret
    dimmer = property(_get_dimmer, _set_dimmer)

    def _set_color(self, val):
        self.color_lock.acquire()
        self._color.set_color(val)
        self.color_lock.release()
    def _get_color(self):
        self.color_lock.acquire()
        ret = self._color
        self.color_lock.release()
        return ret
    color = property(_get_color, _set_color)

    def update(self):
        self.dimmer_lock.acquire()
        if self.dimmer is None:
            dim = 0
        else:
            dim = int(max(self.color.to_tuple()) / 255.0
                      * self.dimmer / 255.0 *
                      len(self.dimchars))
        self.dimmer_lock.release()

        try:
            dimchar = self.dimchars[dim]
        except IndexError:
            dimchar = self.dimchars[len(self.dimchars) - 1]

        colname = self.color.get_name()
        if colname is not None:
            attr = style.attr('color_%s' % colname)
        else:
            attr = 0

        self.window.hline(0, 0, dimchar, self.length, attr)

    def refresh(self):
        root.refresh_lock.acquire()
        self.update()
        self.window.refresh()
        root.refresh_lock.release()

class Bar(object):
    def __init__(self, window, number, x, y, inverted=False):
        self.selected = False
        self.inverted = inverted

        self.bar = window.derwin(3, root.BAR_LENGTH + 2, y, x)
        self.number = number

        sectsize = root.BAR_LENGTH / 3

        # initialize dimmer sections
        for s in xrange(3):
            sect = self.bar.derwin(1, sectsize + 1, 1, 1 + sectsize * s)
            setattr(self, 'section%d' % (s + 1), Section(sect, sectsize))

        if self.inverted:
            self.sections = (self.section3, self.section2, self.section1)
        else:
            self.sections = (self.section1, self.section2, self.section3)

        self.draw_box()

    def draw_box(self):
        if self.selected:
            attr = style.attr('bar_selected')
        else:
            attr = style.attr('bar_deselected')

        self.bar.attron(attr)
        self.bar.box()
        self.bar.attroff(attr)

        for n in xrange(3):
            if self.inverted:
                sectnum = 2 - n
            else:
                sectnum = n

            if self.sections[sectnum].dimmer is not None:
                attr = style.attr('section_activated')
            elif self.sections[sectnum].selected:
                attr = style.attr('section_selected')
            else:
                attr = style.attr('section_deselected')

            self.bar.attron(attr)
            self.bar.addstr(0, 2 + n * (root.BAR_LENGTH / 3),
                            " %d.%d " % (self.number, sectnum + 1))
            self.bar.addch(curses.ACS_DARROW)
            self.bar.addch(' ')
            self.bar.attroff(attr)

    def refresh(self):
        root.refresh_lock.acquire()
        self.draw_box()

        for s in self.sections:
            s.refresh()

        self.bar.refresh()
        root.refresh_lock.release()
