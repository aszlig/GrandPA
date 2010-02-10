#!/usr/bin/env python
from distutils.core import setup, Extension

mouse = Extension('grandpa.inputdev._mouse',
                  libraries = ['gpm'],
                  sources = ['ext/mouse.c'])

fb = Extension('grandpa.interface._fb',
                  sources = ['ext/fb.c'])

setup(name = 'GrandPA',
      version = '0.5',
      description = 'The GrandPA LedBar lighting controller',
      author = 'aszlig',
      author_email = 'aszlig@redmoonstudios.org',
      ext_modules = [mouse, fb],
      scripts = ['bin/grandpa'],
      packages=['grandpa', 'grandpa.interface', 'grandpa.inputdev',
                'grandpa.chasers'])
