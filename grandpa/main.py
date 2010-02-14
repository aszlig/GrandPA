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

import os
import logging
import optparse
import curses
import shelve

import style
import interface
import inputdev.rawkbd
import inputdev.keys
import inputdev.mouse

class Grandpa(object):
    DEFAULT_CONFIG = {
        'stage': 'Rockfabrik'
    }

    DEFAULT_LOGFILE = os.path.expanduser('~/.grandpa/logfile')
    DEFAULT_CFGFILE = os.path.expanduser('~/.grandpa/config')

    def main(self):
        p = optparse.OptionParser()

        p.add_option('-c', '--config', metavar='CFGFILE',
                     dest='cfgfile', default=self.DEFAULT_CFGFILE,
                     help="The configuration file, default: %default")

        p.add_option('-l', '--log', metavar='LOGFILE',
                     dest='logfile', default=self.DEFAULT_LOGFILE,
                     help="The logfile, default: %default")

        options, args = p.parse_args()

        # create base directory
        for fname in (options.logfile, options.cfgfile):
            path = os.path.dirname(fname)
            if not os.path.exists(path):
                os.makedirs(path)

        self.init_logging(options.logfile)
        self.init_config(options.cfgfile)

        self.startapp()

    def init_logging(self, logfile):
        logging.basicConfig(level=logging.DEBUG,
                            format='%(asctime)s %(levelname)s %(message)s',
                            filename=logfile,
                            filemode='w')

    def init_config(self, cfgfile):
        config = shelve.open(cfgfile, protocol=2)

        for k, v in self.DEFAULT_CONFIG.iteritems():
            config.setdefault(k, v)

        self.config = config

    def curses_wrapper(self, scr):
        curses.raw()

        curses.start_color()

        style.theme.init_pairs()

        curses.curs_set(0)

        root = interface.Root(scr, self.config)
        root.refresh()

        try:
            keyhandler = inputdev.keys.Keys(root)
            keyhandler.start()
        except:
            root.stop()
            raise

        try:
            mousehandler = inputdev.mouse.Mouse(root.fader)
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

    def startapp(self):
        inputdev.rawkbd.set_raw_kbd()
        try:
            curses.wrapper(self.curses_wrapper)
        finally:
            inputdev.rawkbd.restore_kbd()
