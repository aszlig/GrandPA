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

import logging
import threading
import Queue

import rawkbd

KEYMAP = {
    'quit': 0x2d,  # q

    'select_all': 0x1e,  # a
    'select_odd': 0x1f,  # o
    'select_even': 0x20,  # e
    'select_none': 0x21,  # u
    'select_invert': 0x22,  # i

    'select_group1': 0x10,  # ,
    'select_group2': 0x11,  # .

    'select_direct': [
        # f1-f12
        0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40,
        0x41, 0x42, 0x43, 0x44, 0x57, 0x58,
    ],

    'stack_1': 0x2d,  # q
    'stack_2': 0x2e,  # j
    'stack_3': 0x2f,  # k
    'stack_4': 0x30,  # x

    'flash_0': 0x39,  # space
    'flash_1': 0x31,  # b
    'flash_2': 0x32,  # m
    'flash_3': 0x33,  # w
    'flash_4': 0x34,  # v

    'flash_5': 0x23,  # d
    'flash_6': 0x24,  # h
    'flash_7': 0x25,  # t
    'flash_8': 0x26,  # n

    'numbers': [
        # 0-9
        0x0b, 0x02, 0x03, 0x04, 0x05,
        0x06, 0x07, 0x08, 0x09, 0x0a,
    ],

    'reset_chasers': 0x6f,  # delete

    'list_activate': 0x1c,  # enter
    'list_up': 0x67,  # up
    'list_down': 0x6c,  # down
    'list_left': 0x69,  # left
    'list_right': 0x6a,  # right
    'list_pageup': 0x68,  # page up
    'list_pagedown': 0x6d,  # page down
    'list_direct': 0x12,  # .

    'learn_speed': 0x0f,  # tab
    'switch_fader': 0x15,  # f

    'superstrobe': 0x0e,  # backspace
    'record_chaser': 0x18,  # r

    'toggle_clock': 0x17,  # c

    'cancel_cmd': 0x01,  # esc

    'redraw': 0x19,  # l
}


class Keys(threading.Thread):
    def __init__(self, root):
        threading.Thread.__init__(self)
        self.is_active = Queue.Queue()
        self.root = root

        self.effect = None

    def run(self):
        try:
            self.process()
        except:
            logging.exception("Keyboard handler died!")
        finally:
            self.is_active.put(False)

    def pressed(self, mapkey, keycode=None):
        if keycode is None:
            keycode = self.keycode

        key = KEYMAP.get(mapkey, None)

        if key is None:
            return False
        elif isinstance(key, int):
            return keycode == key
        elif keycode in key:
            return True

        return False

    def released(self, mapkey):
        return self.pressed(mapkey, self.keycode - 0x80)

    def process_modifiers(self):
        if self.keycode in (0x2a, 0x36):
            self.mod_shift = True
        elif self.keycode in (0xaa, 0xb6):
            self.mod_shift = False
        elif self.keycode in (0x1d, 0x61):
            self.mod_ctrl = True
        elif self.keycode in (0x9d, 0xe1):
            self.mod_ctrl = False
        elif self.keycode in (0x38, 0x64):
            self.mod_meta = True
        elif self.keycode in (0xb8, 0xe4):
            self.mod_meta = False

    def decode_flasher(self, chars):
        filtered = map(lambda c: c == "X", filter(lambda c: c in "X.", chars))
        return [filtered[i:i + 3] for i in range(0, len(filtered), 3)]

    def init_selects(self):
        if hasattr(self.root.tavern.stage, 'set_flashers'):
            return [[self.decode_flasher(flasher) for flasher in stack]
                    for stack in self.root.tavern.stage.set_flashers()]

        selects = []
        for x in xrange(4):
            flashers = []
            for y in xrange(9):
                flashers.append([])

            selects.append(flashers)

        return selects

    def process(self):
        self.keycode = None
        self.mod_shift = False
        self.mod_ctrl = False
        self.mod_meta = False

        self.cur = 0

        self.stacks = {
            'stack_1': 0,
            'stack_2': 1,
            'stack_3': 2,
            'stack_4': 3,
        }

        self.dimmers = {
            'flash_0': 0,
            'flash_1': 1,
            'flash_2': 2,
            'flash_3': 3,
            'flash_4': 4,
            'flash_5': 5,
            'flash_6': 6,
            'flash_7': 7,
            'flash_8': 8,
        }

        self.selects = self.root.config.get('selections', self.init_selects())
        self.keep_dimmer = [False] * 9

        buf = ''
        while not (self.pressed('quit') and self.mod_meta):
            reset = False

            if buf != self.root.status.cmd:
                self.root.status.cmd = buf
                self.root.status.update()

            self.buf = buf

            self.is_active.put(True)
            self.is_active.join()

            # prevent keyboard repeat
            old_keycode = self.keycode
            while True:
                self.keycode = self.root.getch()
                if self.keycode == old_keycode:
                    continue
                break

            self.process_modifiers()

            if self.pressed('numbers'):
                num = KEYMAP['numbers'].index(self.keycode)
                buf += str(num)
                continue

            # flasher
            for fkey in self.dimmers.keys():
                if self.pressed(fkey) or self.released(fkey):
                    self.flasher(fkey)
                    continue

            # stacks
            for skey in self.stacks.keys():
                if self.pressed(skey):
                    self.setstack(self.stacks[skey])
                    continue

            # superstrobe
            if self.pressed('superstrobe'):
                self.root.tavern.set_full_dimmer(255)
                self.root.controller.superstrobe(True)
                continue
            elif self.released('superstrobe'):
                self.root.tavern.set_full_dimmer(None)
                self.root.controller.superstrobe(False)
                continue

            # menu
            if self.pressed('list_activate'):
                self.root.menu.activate()
            elif self.pressed('learn_speed'):
                self.root.menu.learn_speed()
            elif self.pressed('list_up'):
                self.root.menu.up()
            elif self.pressed('list_down'):
                self.root.menu.down()
            elif self.pressed('list_left'):
                self.root.menu.left()
            elif self.pressed('list_right'):
                self.root.menu.right()
            elif self.pressed('list_pageup'):
                self.root.menu.pageup()
            elif self.pressed('list_pagedown'):
                self.root.menu.pagedown()
            elif self.pressed('list_direct') and len(buf) > 0:
                self.root.menu.select(int(buf))
                reset = True
            elif self.pressed('reset_chasers'):
                self.root.menu.reset()

            # selector
            elif self.pressed('select_direct'):
                num = KEYMAP['select_direct'].index(self.keycode)
                self.selector(toggle=True, function=lambda x: x == num)
                reset = True
            elif self.pressed('select_all'):
                self.selector()
                reset = True
            elif self.pressed('select_odd'):
                self.selector(function=lambda x: x % 2 == 0)
                reset = True
            elif self.pressed('select_even'):
                self.selector(function=lambda x: x % 2 == 1)
                reset = True
            elif self.pressed('select_none'):
                self.selector(select=False)
                reset = True
            elif self.pressed('select_invert'):
                self.selector(toggle=True)
                reset = True
            elif self.pressed('select_group1'):
                self.selector(function=lambda x: x < 6)
                reset = True
            elif self.pressed('select_group2'):
                self.selector(function=lambda x: x >= 6)
                reset = True

            # misc
            elif self.pressed('switch_fader'):
                self.root.fadectrl.switch_fader()
            elif self.pressed('toggle_clock'):
                self.root.clock.toggle()
            elif self.pressed('redraw') and self.mod_ctrl:
                self.root.refresh(hard=True)
            elif self.pressed('cancel_cmd'):
                if self.mod_ctrl:
                    self.unraw()
                    self.mod_ctrl = False
                else:
                    reset = True

            if reset:
                buf = ''

    def unraw(self):
        rawkbd.unraw()
        self.root.refresh(hard=True)

    def setselect(self, dimmer):
        for num in xrange(len(self.root.tavern.bars)):
            for sect in xrange(3):
                self.root.tavern.deactivate_bar(num, sect)

        self.root.tavern.set_selection_struct(self.selects[self.cur][dimmer])

    def record_chaser(self):
        self.root.tavern.record_chaser()

    def setflash(self, dimmer, active):
        """
        Set dimmer to fader value or zero for a particular dimmer setting.
        """
        if active:
            value = self.root.dimfader.faderval
        else:
            value = None

        dd = None
        if self.root.ftfader.faderval > 0:
            dd = self.root.tavern.spawn_dyndim(
                to=value,
                speed=self.root.ftfader.faderval,
                controller=self.root.controller
            )
            f = lambda x: dd.add_sect(x)
        else:
            f = lambda x: setattr(x, 'dimmer', value)

        self.root.tavern.set_selection_struct(self.selects[self.cur][dimmer],
                                              sectfun=f, barfun=f)

        if dd is not None:
            dd.start()
        else:
            self.root.controller.dim_update()

    def saveflash(self, dim):
        """
        Store the current selections in a local dimmer setting.
        """
        self.selects[self.cur][dim] = self.root.tavern.get_selection_struct()
        self.root.config['selections'] = self.selects

    def flasher(self, flashkey):
        """
        Handle all flash keys at once, including things like:
            - Stay flashed if shift is hold while the flash key is pressed.
            - Stay flashed while shift and flash key is still hold.
            - Flash/Unflash if no shift key is involved.
        """
        dimmer = self.dimmers[flashkey]

        if self.pressed(flashkey):
            if self.mod_ctrl:
                self.saveflash(dimmer)
                return

            if self.mod_meta:
                self.setselect(dimmer)
                return

            if self.keep_dimmer[dimmer]:
                # deactivate dimmer after being locked on
                self.setflash(dimmer, False)
                self.keep_dimmer[dimmer] = False
            else:
                # activate and keep dimmer if shift is hold
                self.setflash(dimmer, True)
                if self.mod_shift:
                    self.keep_dimmer[dimmer] = True
        elif self.released(flashkey) and not self.keep_dimmer[dimmer]:
            # key release, but we want the dimmer to
            # stay on in case shift is hold
            if self.mod_shift:
                self.keep_dimmer[dimmer] = True
                self.setflash(dimmer, True)
            else:
                self.setflash(dimmer, False)

    def setstack(self, stack):
        """
        Switch to the specified stack and update statusbar.
        """
        self.root.status.set_stack(stack)
        self.cur = stack

    def selector(self, select=True, toggle=False, function=None):
        """
        Select bars/sections based on a function which returns True for the bar
        to be selected and False for a bar where the selection shouldn't be
        changed.

        The toggle argument inverts the selection returned by the function,
        while the select argument controls whether the selection should be
        activated (True) or deactivated (False).
        """
        buf = self.buf

        if function is None:
            function = lambda x: True

        if toggle:
            do = 'toggle_bar'
        elif select:
            do = 'activate_bar'
        else:
            do = 'deactivate_bar'

        cmd = getattr(self.root.tavern, do)

        for bar in filter(function, range(0, 12)):
            if buf == '0':
                cmd(bar)
            elif len(buf) >= 1 and buf.isdigit():
                for sect in buf:
                    if 1 <= int(sect) <= 3:
                        cmd(bar, int(sect) - 1)
            else:
                for sect in xrange(3):
                    cmd(bar, sect)
