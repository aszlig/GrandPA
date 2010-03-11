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

import style
from root import Root
from grandpa.inputdev.console import rawkbd
from grandpa.inputdev.console import keys
from grandpa.inputdev.console import mouse

def curses_wrapper(scr, config, options):
    curses.raw()

    curses.start_color()

    style.theme.init_pairs()

    curses.curs_set(0)

    root = Root(scr, config, options)
    root.refresh()

    try:
        keyhandler = keys.Keys(root)
        keyhandler.start()
    except:
        root.stop()
        raise

    try:
        mousehandler = mouse.Mouse(root.fadectrl)
    except Exception, e:
        has_mouse = False
        root.status.set_error(e[0])
    else:
        mousehandler.start()
        has_mouse = True

    while keyhandler.is_active.get():
        root.refresh()
        keyhandler.is_active.task_done()

    if has_mouse:
        mousehandler.stop()

    root.stop()

def startup(config, options):
    rawkbd.set_raw_kbd()
    try:
        curses.wrapper(curses_wrapper, config, options)
    finally:
        rawkbd.restore_kbd()
