#!/usr/bin/env python
from distutils.core import setup, Extension

mouse = Extension('grandpa.inputdev._mouse',
                  libraries = ['gpm'],
                  sources = ['ext/mouse.c'])

setup(name = 'GrandPA',
      version = '0.5',
      description = 'The GrandPA LedBar lighting controller',
      author = 'aszlig',
      author_email = 'aszlig@redmoonstudios.org',
      ext_modules = [mouse],
      scripts = ['bin/grandpa'],
      packages=['grandpa', 'grandpa.interface', 'grandpa.inputdev',
                'grandpa.chasers'])
