import time
import threading

class DynamicDimmer(threading.Thread):
    def __init__(self, to=0, speed=0, controller=None):
        threading.Thread.__init__(self)

        self.dim_to = 0 if to is None else to
        self.dim_speed = speed
        self.controller = controller
        controller.dyndims.append(self)

        self.quit_event = threading.Event()

        self.sects = []

    def add_sect(self, sect):
        currdim = getattr(sect, 'dimmer', 0)
        if currdim is None:
            currdim = 0
        item = (sect, currdim)

        # attach ourself to section dyndim
        sect.dyndim = self

        self.sects.append(item)

    def run(self):
        first_refresh = False

        for percent in xrange(21):
            for sect, startval in self.sects:
                if sect.dyndim is not self:
                    continue
                endval = self.dim_to
                current = (endval - startval) / 20.0 * percent
                sect.dimmer = int(startval + current)
                if sect.dimmer == 0:
                    sect.dimmer = None
                if (not first_refresh and endval > 0) or sect.dimmer is None:
                    if hasattr(sect, 'bar'):
                        sect.bar.refresh()
                sect.refresh()
            if percent > 1:
                first_refresh = True
            self.controller.dim_update()
            self.quit_event.wait(0.1 / 255.0 * self.dim_speed)
            if self.quit_event.isSet():
                return

    def stop(self):
        self.quit_event.set()
