import curses

import style
from root import Root
from grandpa.inputdev import rawkbd
from grandpa.inputdev import keys
from grandpa.inputdev import mouse

def curses_wrapper(scr, config):
    curses.raw()

    curses.start_color()

    style.theme.init_pairs()

    curses.curs_set(0)

    root = Root(scr, config)
    root.refresh()

    try:
        keyhandler = keys.Keys(root)
        keyhandler.start()
    except:
        root.stop()
        raise

    try:
        mousehandler = mouse.Mouse(root.fader)
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

def startup(config):
    rawkbd.set_raw_kbd()
    try:
        curses.wrapper(curses_wrapper, config)
    finally:
        rawkbd.restore_kbd()
