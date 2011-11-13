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

import style
from grandpa.color import Color

from grandpa import locking


class Section(object):
    """
    Represents a visual section where the dimmer gets delegated to DMXout.
    """
    DIMCHARS = [
        ' ', '+', '*', 'ACS_DIAMOND', 'ACS_BULLET', 'ACS_BOARD',
        'ACS_PLUS', 'ACS_CKBOARD', 'ACS_LANTERN', 'ACS_BLOCK',
    ]

    def __init__(self, length, sect=None, rect=None, bar=None):
        if sect is None and rect is None:
            raise RuntimeError("Section was initialized without rect or sect")

        self.sect = sect
        self.rect = rect

        self.bar = bar

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

        if self.sect is not None:
            if self.dimmer is None:
                dim = 0
            else:
                dim = int(max(self.color.to_tuple()) / 255.0
                          * self.dimmer / 255.0 *
                          len(self.dimchars))

            try:
                dimchar = self.dimchars[dim]
            except IndexError:
                dimchar = self.dimchars[len(self.dimchars) - 1]

            colname = self.color.get_name()
            if colname is not None:
                attr = style.attr('color_%s' % colname)
            else:
                attr = 0

            self.sect.hline(0, 0, dimchar, self.length, attr)
        elif self.rect is not None:
            if self.dimmer is None:
                dim = 0
            else:
                dim = self.dimmer
            color = self.color.to_tuple(use_alpha=dim)
            self.rect.fill(*color)

        self.dimmer_lock.release()

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        self.update()
        if self.sect is not None:
            if hard:
                self.sect.redrawwin()
            else:
                self.sect.refresh()
        locking.refresh_lock.release()


class Bar(object):
    def __init__(self, root, number, x, y, inverted=False, fbdev=None):
        self.root = root
        self.selected = False
        self.inverted = inverted

        self.win = root.view_win.derwin(3, root.BAR_LENGTH + 2, y, x)
        self.number = number

        sectsize = root.BAR_LENGTH / 3

        if fbdev is not None:
            bar_y, bar_x = self.win.getbegyx()

        # initialize dimmer sections
        for s in xrange(3):
            if fbdev is None:
                sect = self.win.derwin(1, sectsize + 1, 1, 1 + sectsize * s)
                setattr(self, 'section%d' % (s + 1),
                        Section(sectsize, bar=self, sect=sect))
            else:
                rect = fbdev.newrect(bar_x + 1 + sectsize * s, bar_y + 1,
                                     sectsize, 1)
                setattr(self, 'section%d' % (s + 1),
                        Section(sectsize, bar=self, rect=rect))

        if self.inverted:
            self.sections = (self.section3, self.section2, self.section1)
        else:
            self.sections = (self.section1, self.section2, self.section3)

        self.update()

    def update(self):
        if self.selected:
            attr = style.attr('bar_selected')
        else:
            attr = style.attr('bar_deselected')

        self.win.attron(attr)
        self.win.box()
        self.win.attroff(attr)

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

            self.win.attron(attr)
            self.win.addstr(0, 2 + n * (self.root.BAR_LENGTH / 3),
                            " %d.%d " % (self.number, sectnum + 1))
            self.win.addch(curses.ACS_DARROW)
            self.win.addch(' ')
            self.win.attroff(attr)

    def refresh(self, hard=False):
        locking.refresh_lock.acquire()
        self.update()

        for s in self.sections:
            s.refresh(hard=hard)

        if hard:
            self.win.redrawwin()
        else:
            self.win.refresh()
        locking.refresh_lock.release()
