/*
 * glib.i -- SWIG interface file for type translation of glib types
 *
 * Copyright (C) 2008 ParIT Worker Co-operative <paritinfo@parit.ca>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 * @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
 */

%typemap(in) gint8, gint16, gint32, gint64, gint, gshort, glong {
    $1 = ($1_type)PyInt_AsLong($input);
}

%typemap(out) gint8, gint16, gint32, gint64, gint, gshort, glong {
    $result = PyInt_FromLong($1);
}

%typemap(in) guint8, guint16, guint32, guint64, guint, gushort, gulong {
    $1 = ($1_type)PyLong_AsUnsignedLong($input);
}

%typemap(out) guint8, guint16, guint32, guint64, guint, gushort, gulong {
    $result = PyLong_FromUnsignedLong($1);
}

%typemap(in) gfloat, gdouble {
    $1 = ($1_type)PyFloat_AsDouble($input);
}

%typemap(out) gfloat, gdouble {
    $result = PyFloat_FromDouble($1);
}

%typemap(in) gchar * {
    $1 = ($1_type)PyString_AsString($input);
}

%typemap(out) gchar * {
    $result = PyString_FromString($1);
}

%typemap(in) gboolean {
    if ($input == Py_True)
        $1 = TRUE;
    else if ($input == Py_False)
        $1 = FALSE;
    else
    {
        PyErr_SetString(
            PyExc_ValueError,
            "Python object passed to a gboolean argument was not True "
            "or False" );
        return NULL;
    }
}

%typemap(out) gboolean {
    if ($1 == TRUE)
    {
        Py_INCREF(Py_True);
        $result = Py_True;
    }
    else if ($1 == FALSE)
    {
        Py_INCREF(Py_False);
        $result = Py_False;
    }
    else
    {
        PyErr_SetString(
            PyExc_ValueError,
            "function returning gboolean returned a value that wasn't "
            "TRUE or FALSE.");
        return NULL;        
    }
}
