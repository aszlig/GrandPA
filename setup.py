#!/usr/bin/env python
from distutils.core import setup, Extension
from Cython.Distutils import build_ext

mouse = Extension('grandpa.inputdev.mouse',
                  libraries = ['gpm'],
                  sources = ['grandpa/inputdev/mouse.pyx'])

fb = Extension('grandpa.interface._fb',
               sources = ['ext/fb.c'])

setup(name = 'GrandPA',
      version = '0.5',
      description = 'The GrandPA LedBar lighting controller',
      author = 'aszlig',
      author_email = 'aszlig@redmoonstudios.org',
      cmdclass = {'build_ext': build_ext},
      ext_modules = [mouse, fb],
      scripts = ['bin/grandpa'],
      packages = ['grandpa', 'grandpa.interface', 'grandpa.inputdev',
                  'grandpa.chasers'])
