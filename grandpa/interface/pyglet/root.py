import math

import pyglet
from pyglet.gl import *

import keys

import bar

class Root(pyglet.window.Window):
    def __init__(self, *args, **kwargs):
        super(Root, self).__init__(*args, **kwargs)

        glEnable(GL_DEPTH_TEST)
        glEnable(GL_CULL_FACE)

        self.angle = 0

        self.keyhandler = keys.Keys(self)

        self.set_view()

        self.rot = 1.0
        self.zoom = -1.0

        self.pause = True
        self.mousegrab = True

        self.set_mouse_visible(False)
        self.set_exclusive_mouse(True)

        self.fps_drawer = pyglet.clock.ClockDisplay()

        pyglet.clock.schedule(self.update)

    def toggle_mousegrab(self):
        self.mousegrab = not self.mousegrab
        self.set_exclusive_mouse(self.mousegrab)

    def set_view(self):
        # XXX: this should be merged with grandpa.stage
        radius_vertical = 1.0
        radius_horizontal = 3.0

        angles = [6, 15, 35]

        all_angles = list(angles)
        all_angles += reversed(map(lambda x: (90 - x) + 90, angles))
        all_angles += map(lambda x: x + 180, angles)
        all_angles += reversed(map(lambda x: (90 - x) + 270, angles))

        self.bars = []

        for degree in all_angles:
            angle = degree * math.pi / 180.0

            r = 0.5 / math.sqrt((math.cos(angle) / float(radius_horizontal * 2)) ** 2 +
                                (math.sin(angle) / float(radius_vertical * 2)) ** 2)

            x = r * math.cos(angle)
            y = r * math.sin(angle)

            if 90 < degree <= 270:
                # left
                x += 0.15
            else:
                # right
                x -= 0.15

            if degree > 180:
                # upper
                y += 0.05
            else:
                # lower
                y -= 0.05

            self.bars.append(bar.Bar(x, y, 0))

    def ortho_begin(self):
        glMatrixMode(GL_PROJECTION)
        glPushMatrix()
        glLoadIdentity()
        glOrtho(0, self.width, 0, self.height, -1, 1)
        glMatrixMode(GL_MODELVIEW)

    def ortho_end(self):
        glMatrixMode(GL_PROJECTION)
        glPopMatrix()
        glMatrixMode(GL_MODELVIEW)

    def toggle_pause(self):
        self.pause = not self.pause

    def update(self, dt):
        if not self.pause:
            self.angle += (dt * 80)
            self.angle %= 360

        # FIXME: remove this ugly test effect ;-)
        pos1 = getattr(self, 'pos1', 0)
        pos2 = getattr(self, 'pos2', 0)
        for i, bar in enumerate(self.bars):
            for j, color in enumerate(bar.colors):
                if ((i+1) * 3 + (j+1)) % 6 == int(pos1):
                    bar.colors[j] = [0.0, 1.0, 1.0]
                elif ((i+1) * 3 + (j+1)) % 9 == int(pos2):
                    bar.colors[j] = [1.0, 0.0, 0.0]
                else:
                    bar.colors[j] = [0.0, 0.0, 1.0]
        pos1 += dt * 10
        if pos1 >= 6:
            pos1 = 0

        self.pos1 = pos1

        pos2 += dt * 23
        if pos2 >= 6:
            pos2 = 0

        self.pos2 = pos2

    def on_resize(self, width, height):
        glViewport(0, 0, width, height)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluPerspective(60., width / float(height), 0.1, 1000.)
        glMatrixMode(GL_MODELVIEW)

    def correct(self, num, minval, maxval):
        if num > maxval:
            return maxval
        elif num < minval:
            return minval

        return num

    def on_mouse_motion(self, x, y, dx, dy):
        self.angle += (dx / 2.0) % 360
        self.angle %= 360
        self.zoom += (dy / 20.0)

        #self.angle = self.correct(self.angle, 0.0, 360.0)
        self.zoom = self.correct(self.zoom, -80.0, -1.0)

    def on_mouse_drag(self, x, y, dx, dy, buttons, mods):
        self.on_mouse_motion(x, y, dx, dy)

    def on_draw(self):
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        glPushMatrix()

        glLoadIdentity()
        glTranslatef(0, self.rot, self.zoom)
        glRotatef(20, 0, 1, 0)
        glRotatef(self.angle, 0, 0, 1)
        [b.draw() for b in self.bars]

        glPopMatrix()

        self.ortho_begin()
        self.fps_drawer.draw()
        self.ortho_end()

    def on_key_press(self, sym, mod):
        self.keyhandler.press(sym, mod)

    def on_key_release(self, sym, mod):
        self.keyhandler.release(sym, mod)
