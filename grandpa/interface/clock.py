import time
import threading

from grandpa import style
from grandpa import locking

NUMBERS = {
    '1': ["    ",
          "/|  ",
          " |  "],

    '2': ["_   ",
          " )  ",
          "/_  "],

    '3': ["_   ",
          "_)  ",
          "_)  "],

    '4': ["    ",
          "|_|_",
          "  | "],

    '5': [" _  ",
          "|_  ",
          " _) "],

    '6': [" _  ",
          "|_  ",
          "|_) "],

    '7': ["__  ",
          " /  ",
          "/   "],

    '8': [" _  ",
          "(_) ",
          "(_) "],

    '9': [" _  ",
          "(_| ",
          "  | "],

    '0': [" _  ",
          "| | ",
          "|_| "],

    ':': ["    ",
          " o  ",
          " o  "],
}


class Clock(threading.Thread):
    def __init__(self, win):
        threading.Thread.__init__(self)
        self.do_quit = threading.Event()
        self.is_active = threading.Event()
        self.sleeper = threading.Event()
        self.win = win

    def run(self):
        while not self.do_quit.isSet():
            self.update()
            self.sleeper.clear()
            self.sleeper.wait(1)

    def update(self):
        if not self.is_active.isSet():
            return

        ts = time.strftime('%H:%M:%S')

        for n in xrange(3):
            for i, char in enumerate(ts):
                try:
                    line = NUMBERS[char][n]
                except IndexError:
                    continue

                if char == ':':
                    attr = style.attr('clock_dots')
                else:
                    attr = style.attr('clock_number')

                self.win.addstr(n, i * 4, line, attr)
        
        self.refresh()

    def toggle(self):
        """
        Switch clock on/off
        """
        if self.is_active.isSet():
            self.is_active.clear()
            self.refresh(clear=True)
        else:
            self.is_active.set()
            self.sleeper.set()

    def refresh(self, clear=False, hard=False):
        locking.refresh_lock.acquire()
        if clear:
            self.win.erase()
        if hard:
            self.win.redrawwin()
        else:
            self.win.refresh()
        locking.refresh_lock.release()

    def stop(self):
        self.do_quit.set()
        self.sleeper.set()
        self.join()
