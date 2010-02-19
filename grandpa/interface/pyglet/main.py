import pyglet

from root import Root

def startup(config):
    window = Root(width=1680, height=1050)

    pyglet.app.run()
