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
