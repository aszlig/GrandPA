import os

try:
    import pyglet
    pyglet.window.get_platform()
except:
    INTERFACE = 'ncurses'
else:
    INTERFACE = 'pyglet'
