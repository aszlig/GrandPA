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
#include <sys/time.h>
#include <gpm.h>

#define BUTTON1_PRESSED 1
#define BUTTON2_PRESSED 2
#define BUTTON3_PRESSED 4

#define BUTTON1_RELEASED 8
#define BUTTON2_RELEASED 16
#define BUTTON3_RELEASED 32

static PyObject *mouse_open(PyObject* self)
{
	Gpm_Connect connection;
	int ret;

	connection.eventMask = ~0;
	connection.defaultMask = 0;
	connection.minMod = 0;
	connection.maxMod = ~0;

	Py_BEGIN_ALLOW_THREADS
	ret = Gpm_Open(&connection, 0);
	Py_END_ALLOW_THREADS

	if (ret == -1) {
		return PyErr_SetFromErrno(PyExc_OSError);
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static char mouse_getpos__doc__[] =
"Get the current position of the mouse pointer as a tuple which looks like this:\n\
 (dx, dy, x, y, button)\n\
 Where x/y are the current absolute coordinates and dx/dy are the X/Y coordinates\
 relative to the last position. The fifth value is the current button state:\n\
 BUTTON_LEFT, BUTTON_MIDDLE, BUTTON_RIGHT\n";
static PyObject *mouse_getpos(PyObject *self, PyObject *args, PyObject *kwargs)
{
	Gpm_Event ev;

	static char *kwlist[] = {"timeout", NULL};
	int timeout = 0;

	fd_set gpmwatch;
	struct timeval tv;
	int result;

	int buttons;

	if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|i", kwlist, &timeout)) {
		return NULL;
	}

	if (timeout > 0) {
		FD_ZERO(&gpmwatch);
		FD_SET(gpm_fd, &gpmwatch);

		tv.tv_sec = timeout;
		tv.tv_usec = 0;

		Py_BEGIN_ALLOW_THREADS
		result = select(gpm_fd + 1, &gpmwatch, NULL, NULL, &tv);
		Py_END_ALLOW_THREADS

		if (result == -1) {
			return PyErr_SetFromErrno(PyExc_OSError);
		} else if(result == 0) {
			Py_INCREF(Py_None);
			return Py_None;
		}
	}

	Py_BEGIN_ALLOW_THREADS
	result = Gpm_GetEvent(&ev);
	Py_END_ALLOW_THREADS

	if (result <= 0) {
		return PyErr_SetFromErrno(PyExc_OSError);
	}

	buttons = 0;

	switch (ev.type & 0x0f) {
		case (GPM_DOWN):
			if (ev.buttons & GPM_B_LEFT) {
				buttons |= BUTTON1_PRESSED;
			}
			if (ev.buttons & GPM_B_MIDDLE) {
				buttons |= BUTTON3_PRESSED;
			}
			if (ev.buttons & GPM_B_RIGHT) {
				buttons |= BUTTON2_PRESSED;
			}
			break;

		case (GPM_UP):
			if (ev.buttons & GPM_B_LEFT) {
				buttons |= BUTTON1_RELEASED;
			}
			if (ev.buttons & GPM_B_MIDDLE) {
				buttons |= BUTTON3_RELEASED;
			}
			if (ev.buttons & GPM_B_RIGHT) {
				buttons |= BUTTON2_RELEASED;
			}
			break;
	}

	return Py_BuildValue("(iiiii)", ev.x, ev.y, ev.dx, ev.dy, buttons);
}

static PyObject *mouse_close(PyObject *self)
{
	Py_BEGIN_ALLOW_THREADS
	Gpm_Close();
	Py_END_ALLOW_THREADS

	Py_INCREF(Py_None);
	return Py_None;
}

static PyMethodDef mouse_methods[] = {
	{"open", (PyCFunction)mouse_open, METH_NOARGS, "Connect to GPM."},
	{"getpos", (PyCFunction)mouse_getpos, METH_KEYWORDS, mouse_getpos__doc__},
	{"close", (PyCFunction)mouse_close, METH_NOARGS, "Close the connection to GPM."},
	{NULL, NULL}
};

PyMODINIT_FUNC init_mouse(void)
{
	PyObject* module;

	module = Py_InitModule("_mouse", mouse_methods);

	PyModule_AddIntConstant(module, "BUTTON1_PRESSED",  BUTTON1_PRESSED);
	PyModule_AddIntConstant(module, "BUTTON2_PRESSED",  BUTTON2_PRESSED);
	PyModule_AddIntConstant(module, "BUTTON3_PRESSED",  BUTTON3_PRESSED);
	PyModule_AddIntConstant(module, "BUTTON1_RELEASED", BUTTON1_RELEASED);
	PyModule_AddIntConstant(module, "BUTTON2_RELEASED", BUTTON2_RELEASED);
	PyModule_AddIntConstant(module, "BUTTON3_RELEASED", BUTTON3_RELEASED);
}
