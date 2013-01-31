#!/usr/bin/env python

# GrandPA, a LedBar lighting controller.
#
# Copyright (c) 2010 aszlig <"^[0-9]+$"@regexmail.net>
#
# GrandPA is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GrandPA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GrandPA. If not, see <http://www.gnu.org/licenses/>.

from distutils.core import setup, Extension
from Cython.Distutils import build_ext

mouse = Extension('grandpa.inputdev.console.mouse',
                  libraries=['gpm'],
                  sources=['grandpa/inputdev/console/mouse.pyx'])

fb = Extension('grandpa.interface.ncurses.fb',
               sources=['grandpa/interface/ncurses/fb.pyx'])

setup(name='GrandPA',
      version='0.5',
      description='The GrandPA LedBar lighting controller',
      author='aszlig',
      author_email='aszlig@redmoonstudios.org',
      cmdclass={'build_ext': build_ext},
      ext_modules=[mouse, fb],
      scripts=['bin/grandpa'],
      packages=['grandpa', 'grandpa.interface', 'grandpa.interface.ncurses',
                'grandpa.interface.pyglet', 'grandpa.inputdev',
                'grandpa.inputdev.console', 'grandpa.chasers'])
