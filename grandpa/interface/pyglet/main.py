import pyglet

from root import Root

def startup(config, options):
    window = Root(width=1680, height=1050)

    pyglet.app.run()
