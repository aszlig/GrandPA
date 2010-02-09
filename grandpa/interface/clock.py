import time
import threading
import logging

from grandpa import style
from grandpa import locking

NUMBERS = {
    "1": ["    ",
          "/|  ",
          " |  "],

    "2": ["_   ",
          " )  ",
          "/_  "],

    "3": ["_   ",
          "_)  ",
          "_)  "],

    "4": ["    ",
          "|_|_",
          "  | "],

    "5": [" _  ",
          "|_  ",
          " _) "],

    "6": [" _  ",
          "|_  ",
          "|_) "],

    "7": ["__  ",
          " /  ",
          "/   "],

    "8": [" _  ",
          "(_) ",
          "(_) "],

    "9": [" _  ",
          "(_| ",
          "  | "],

    "0": [" _  ",
          "| | ",
          "|_| "],

    ":": ["    ",
          " o  ",
          " o  "],
}


class Clock(threading.Thread):
    def __init__(self, win):
        threading.Thread.__init__(self)
        self.do_quit = threading.Event()
        self.win = win

    def run(self):
        while not self.do_quit.isSet():
            self.update()
            self.do_quit.wait(1)

    def update(self):
        ts = time.strftime('%H:%M:%S')

        for n in xrange(3):
            line = ''

            for i, char in enumerate(ts):
                try:
                    line += NUMBERS[char][n]
                except IndexError:
                    pass

            self.win.addstr(n, 0, line, style.attr('clock'))
        
        self.refresh()

    def refresh(self):
        locking.refresh_lock.acquire()
        self.win.refresh()
        locking.refresh_lock.release()

    def stop(self):
        self.do_quit.set()
        self.join()
