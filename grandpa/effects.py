import threading
import Queue


class Recorder(threading.Thread):
    def __init__(self, fixtures):
        threading.Thread.__init__(self)

        self.do_quit = threading.Event()
        self.queue = Queue.Queue()

        self.fixtures = fixtures

        self.initial_colors = []
        for f in fixtures:
            self.initial_colors.append(f.color.copy())
            if not hasattr(f, 'recorder'):
                err = "Another recorder is already running on %r." % f
                raise RecorderClash(err)
            else:
                f.recorder = self

    def push_change(self, fixture):
        # TODO: calculate delta to initial_colors
        self.queue.put(fixture)

    def stop(self):
        self.queue.put('quit')
        self.join()

    def run(self):
        while True:
            value = self.queue.get()
            if value == 'quit':
                break
