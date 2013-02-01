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

cdef extern from "fcntl.h":
    int open(char*, int)
    int close(int)

    int O_RDWR

cdef extern from "sys/ioctl.h":
    int ioctl(int, int, ...)

cdef extern from "sys/mman.h":
    void *mmap(void*, int, int, int, int, int)
    int munmap(void*, unsigned long)

    int PROT_READ, PROT_WRITE, MAP_SHARED
    void *MAP_FAILED

cdef extern from "linux/fb.h":
    cdef struct fb_fix_screeninfo:
        unsigned long line_length

    cdef struct fb_var_screeninfo:
        unsigned long xoffset, yoffset
        unsigned long xres, yres
        unsigned long bits_per_pixel

    int FBIOGET_FSCREENINFO
    int FBIOGET_VSCREENINFO

class FBError(Exception):
    pass

cdef class Rect(object):
    cdef unsigned int left, top, width, height
    cdef object handle

    def __init__(self, handle, left, top, width, height):
        self.handle = handle

        self.left = left
        self.top = top
        self.width = width
        self.height = height

    def fill(self, red=0, green=0, blue=0, alpha=0):
        color = (red, green, blue, alpha)
        coords = (self.left, self.top, self.width, self.height)
        return self.handle._fill(color, coords)

cdef class Framebuffer(object):
    cdef int fd
    cdef fb_fix_screeninfo fixed
    cdef fb_var_screeninfo var
    cdef unsigned long buffersize
    cdef char *fbdev

    cdef unsigned int termsize_x, termsize_y
    cdef unsigned int char_width, char_height

    def __init__(self, width, height):
        self.termsize_x = width
        self.termsize_y = height

        self.fd = open("/dev/fb0", O_RDWR)
        if self.fd == -1:
            raise FBError("Cannot open framebuffer device.")

        if ioctl(self.fd, FBIOGET_FSCREENINFO, &self.fixed) == -1:
            raise FBError("Unable to determine fixed screen information.")
        if ioctl(self.fd, FBIOGET_VSCREENINFO, &self.var) == -1:
            raise FBError("Unable to determine variable screen information.")

        self.buffersize  = self.var.xres * self.var.yres * self.var.bits_per_pixel / 8;
        self.char_width  = self.var.xres / self.termsize_x
        self.char_height = self.var.yres / self.termsize_y

        self.fbdev = <char*>mmap(NULL, self.buffersize, PROT_READ | PROT_WRITE,
                                 MAP_SHARED, self.fd, 0)

        if <void*>self.fbdev == MAP_FAILED:
            raise FBError("Can't mmap() framebuffer device.")

    def close(self):
        munmap(self.fbdev, self.buffersize);
        close(self.fd);

    def _fill(self, color, coords):
        cdef unsigned long left, top, width, height
        cdef unsigned char red, green, blue, alpha
        cdef long offset
        cdef void *tmp

        red, green, blue, alpha = color
        left, top, width, height = coords

        for y in range(top, top + height):
            for x in range(left, left + width):
                offset = (
                    (x + self.var.xoffset) * (self.var.bits_per_pixel / 8) +
                    (y + self.var.yoffset) * self.fixed.line_length
                )

                self.fbdev[offset] = blue
                self.fbdev[offset + 1] = green
                self.fbdev[offset + 2] = red
                self.fbdev[offset + 3] = alpha

    def newrect(self, unsigned int left,  unsigned int top,
                      unsigned int width, unsigned int height):

        cdef l, t, w, h

        if left + width > self.termsize_x:
            raise FBError("Right edge exceeds terminal size.")
        if top + height > self.termsize_y:
            raise FBError("Bottom edge exceeds terminal size.")

        l = left * self.char_width
        t = top * self.char_height
        w = width * self.char_width;
        h = height * self.char_height;

        return Rect(self, l, t, w, h)
