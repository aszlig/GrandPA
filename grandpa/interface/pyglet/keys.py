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

from pyglet.window import key

KEYMAP = {
    key.Q: 'quit',
    key.SPACE: 'pause',
    (key.MOD_CTRL, key.M): 'toggle_mousegrab',
}

class Keys(object):
    def __init__(self, root):
        self.root = root

    def press(self, sym, mod):
        attr = None

        for key, action in KEYMAP.iteritems():
            k_mod = 0
            if isinstance(key, int):
                k_sym = key
            else:
                k_mod, k_sym = key

            if (mod == 0 or mod & k_mod) and sym == k_sym:
                attr = getattr(self, 'do_%s' % action)
                break

        if attr is not None:
            attr()

    def release(self, sym, mod):
        pass

    def do_quit(self):
        self.root.close()

    def do_pause(self):
        self.root.toggle_pause()

    def do_toggle_mousegrab(self):
        self.root.toggle_mousegrab()
