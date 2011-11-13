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

import random
from pyglet.gl import *


class Bar(object):
    SECTIONS = 3

    def __init__(self, x, y, z, ratio=16 / 3):
        width = .2 * ratio
        height = .2 / ratio

        vertices = [
            -width+x, -height+y, -height+z, # 0
            -width+x, -height+y,  height+z, # 1 FP
            -width+x,  height+y, -height+z, # 2
            -width+x,  height+y,  height+z, # 3 FP
             width+x, -height+y, -height+z, # 4
             width+x, -height+y,  height+z, # 5 FP
             width+x,  height+y, -height+z, # 6
             width+x,  height+y,  height+z, # 7 FP
        ]

        indices = [
            0, 1, 3, 2,
            4, 5, 1, 0,
            0, 2, 6, 4,
            6, 7, 5, 4,
            2, 3, 7, 6,
            #5, 7, 3, 1, # FP
        ]

        self.bar_dlist = self.make_dlist(vertices, indices, (0.3, 0.3, 0.3))

        sect_vertices = self.gen_sect_vertices(x, y, z, width, height)
        self.init_sects(sect_vertices)

        self.colors = [
            [1.0, 0.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 0.0, 1.0],
        ]

    def update_color(self, section, color):
        self.colors[section] = color

    def make_dlist(self, vertices, indices, color=None):
        verts = (GLfloat * len(vertices))(*vertices)
        inds = (GLuint * len(indices))(*indices)

        dlist = glGenLists(1)
        glNewList(dlist, GL_COMPILE)

        if color is not None:
            glColor3f(*color)
            #glMaterialfv(GL_FRONT_AND_BACK,
            #             GL_EMISSION,
            #             (GLfloat * 4)(*color))

        glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT)
        glEnableClientState(GL_VERTEX_ARRAY)
        glEnableClientState(GL_NORMAL_ARRAY)
        glVertexPointer(3, GL_FLOAT, 0, verts)
        glNormalPointer(GL_FLOAT, 0, verts)
        glDrawElements(GL_QUADS, len(inds), GL_UNSIGNED_INT, inds)
        glPopClientAttrib()

        glEndList()

        return dlist

    def gen_sect_vertices(self, x, y, z, width, height):
        sect_vertices = []

        barlen = width * 2.0
        for i in xrange(self.SECTIONS):

            sect_x_neg = -width + barlen / self.SECTIONS * i
            sect_x_pos = -width + barlen / self.SECTIONS * (i + 1)

            vertices = [
                sect_x_neg+x, -height+y,  height+z,
                sect_x_neg+x,  height+y,  height+z,
                sect_x_pos+x, -height+y,  height+z,
                sect_x_pos+x,  height+y,  height+z,
            ]

            sect_vertices.append(vertices)

        return sect_vertices

    def init_sects(self, vertices):
        self.sections = []

        for i in xrange(self.SECTIONS):
            indices = [2, 3, 1, 0]
            dlist = self.make_dlist(vertices[i], indices)
            self.sections.append(dlist)

    def draw(self):
        glCallList(self.bar_dlist)

        for i, sect in enumerate(self.sections):
            #glMaterialfv(GL_FRONT_AND_BACK,
            #             GL_EMISSION,
            #             (GLfloat * 4)(*(self.colors[i])))

            glColor3f(*(self.colors[i]))
            glCallList(sect)
