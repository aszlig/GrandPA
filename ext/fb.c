/*
 * GrandPA, a LedBar lighting controller.
 *
 * Copyright (c) 2010 aszlig <"^[0-9]+$"@regexmail.net>
 *
 * LastWatch is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * LastWatch is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GrandPA. If not, see <http://www.gnu.org/licenses/>.
 */

#include <Python.h>
#include <structmember.h>

#include <fcntl.h>
#include <linux/fb.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

static PyObject *FBError;

typedef struct {
	PyObject_HEAD
	int fd;
	char *fbdev;
	long int buffersize;

	struct fb_fix_screeninfo fixed;
	struct fb_var_screeninfo var;
} FBHandle;

static PyObject *fb_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
	FBHandle *self;
	self = (FBHandle*)type->tp_alloc(type, 0);
	return (PyObject*)self;
}

static int fb_init(FBHandle *self, PyObject *args, PyObject *kwargs)
{
	if ((self->fd = open("/dev/fb0", O_RDWR)) == -1) {
		PyErr_SetString(FBError, "Cannot open framebuffer device.");
		return -1;
	}

	if (ioctl(self->fd, FBIOGET_FSCREENINFO, &self->fixed) == -1) {
		PyErr_SetString(FBError, "Unable to determine fixed screen information.");
		return -1;
	}

	if (ioctl(self->fd, FBIOGET_VSCREENINFO, &self->var) == -1) {
		PyErr_SetString(FBError, "Unable to determine variable screen information.");
		return -1;
	}

	self->buffersize = self->var.xres * self->var.yres * self->var.bits_per_pixel / 8;

	if ((self->fbdev = (char *)mmap(0, self->buffersize, PROT_READ | PROT_WRITE, MAP_SHARED, self->fd, 0)) == MAP_FAILED) {
		PyErr_SetString(FBError, "Can't mmap() framebuffer device.");
		return -1;
	}

	return 0;
}

static void fb_close(FBHandle *self)
{
	if (munmap(self->fbdev, self->buffersize) == -1) {
		PyErr_SetString(FBError, "Can't munmap() framebuffer device.");
		return;
	}

	if (close(self->fd) == -1) {
		PyErr_SetString(FBError, "Cannot close framebuffer device");
		return;
	}

	self->ob_type->tp_free((PyObject*)self);
}

static PyObject *fb_fillbar(FBHandle *self, PyObject *args)
{
	unsigned int red = 0, green = 0, blue = 0, alpha = 0;
	long int offset;
	int x, y;

	if (!PyArg_ParseTuple(args, "iii|i", &red, &green, &blue, &alpha)) {
		return NULL;
	}

	/* XXX: currently stubs */
	for (y = 300; y <= 332; y++) {
		for (x = 300; x <= 540; x++) {
			offset = (x + self->var.xoffset) * (self->var.bits_per_pixel / 8) +
				 (y + self->var.yoffset) * self->fixed.line_length;

			*(self->fbdev + offset) = blue;
			*(self->fbdev + offset + 1) = green;
			*(self->fbdev + offset + 2) = red;
			*(self->fbdev + offset + 3) = alpha;
		}
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyMemberDef fbhandle_members[] = {
	{"buffersize", T_INT, offsetof(FBHandle, buffersize), 0, "Screen size in bytes."},
	{NULL}
};

static PyMethodDef fbhandle_methods[] = {
	//{"makebar", (PyCFunction)fb_close, METH_VARARGS, "Create a virtual region to be filled with arbitrary colors."},
	{"fillbar", (PyCFunction)fb_fillbar, METH_VARARGS, "Fill a bar with the specified color (RGB or RGBA)."},
	{NULL}
};

static PyTypeObject FBHandleType = {
	PyObject_HEAD_INIT(NULL)
	0,                         /*ob_size*/
	"fb.Framebuffer",          /*tp_name*/
	sizeof(FBHandle),          /*tp_basicsize*/
	0,                         /*tp_itemsize*/
	(destructor)fb_close,      /*tp_dealloc*/
	0,                         /*tp_print*/
	0,                         /*tp_getattr*/
	0,                         /*tp_setattr*/
	0,                         /*tp_compare*/
	0,                         /*tp_repr*/
	0,                         /*tp_as_number*/
	0,                         /*tp_as_sequence*/
	0,                         /*tp_as_mapping*/
	0,                         /*tp_hash */
	0,                         /*tp_call*/
	0,                         /*tp_str*/
	0,                         /*tp_getattro*/
	0,                         /*tp_setattro*/
	0,                         /*tp_as_buffer*/
	Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /*tp_flags*/
	"Framebuffer handle",      /* tp_doc */
	0,                         /* tp_traverse */
	0,                         /* tp_clear */
	0,                         /* tp_richcompare */
	0,                         /* tp_weaklistoffset */
	0,                         /* tp_iter */
	0,                         /* tp_iternext */
	fbhandle_methods,          /* tp_methods */
	fbhandle_members,          /* tp_members */
	0,                         /* tp_getset */
	0,                         /* tp_base */
	0,                         /* tp_dict */
	0,                         /* tp_descr_get */
	0,                         /* tp_descr_set */
	0,                         /* tp_dictoffset */
	(initproc)fb_init,         /* tp_init */
	0,                         /* tp_alloc */
	fb_new,                    /* tp_new */
};

static PyMethodDef fb_methods[] = {
	//{"open", (PyCFunction)fb_open, METH_NOARGS, "Open framebuffer device."},
	{NULL}
};

PyMODINIT_FUNC init_fb(void)
{
	PyObject *module;

	if (PyType_Ready(&FBHandleType) < 0) {
		return;
	}

	if ((module = Py_InitModule("_fb", fb_methods)) == NULL) {
		return;
	}

	FBError = PyErr_NewException("_fb.FBError", NULL, NULL);

	Py_INCREF(FBError);
	PyModule_AddObject(module, "FBError", FBError);

	Py_INCREF(&FBHandleType);
	PyModule_AddObject(module, "Framebuffer", (PyObject*)&FBHandleType);
}
