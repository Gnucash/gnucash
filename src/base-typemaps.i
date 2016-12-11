/** @file 
    @brief interface file for SWIG, used by python-bindings and scheme/guile.
    @addtogroup python_bindings
*/
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/


/* Not sure why SWIG doesn't figure this out. */
typedef int gint;
typedef gint64 time64;
typedef unsigned int guint;
typedef double gdouble;
typedef float gfloat;
typedef void * gpointer;

%typemap(newfree) gchar * "g_free($1);"

#if defined(SWIGGUILE)
typedef char gchar;

%typemap (out) char * {
  $result = SCM_UNSPECIFIED;
  if ($1) {
    $result = scm_from_utf8_string((const char *)$1);
  }
  if (!$1 || !scm_is_true($result)) {
    $result = scm_c_make_string(0, SCM_UNDEFINED);
  }
}
%typemap(in) GNCPrintAmountInfo "$1 = gnc_scm2printinfo($input);"
%typemap(out) GNCPrintAmountInfo "$result = gnc_printinfo2scm($1);"

%typemap(in) gboolean "$1 = scm_is_true($input) ? TRUE : FALSE;"
%typemap(out) gboolean "$result = $1 ? SCM_BOOL_T : SCM_BOOL_F;"

%typemap(in) Timespec "$1 = gnc_timepair2timespec($input);"
%typemap(out) Timespec "$result = gnc_timespec2timepair($1);"

%typemap(in) GDate "$1 = gnc_timepair_to_GDate($input);"

%typemap(in) GncGUID "$1 = gnc_scm2guid($input);"
%typemap(out) GncGUID "$result = gnc_guid2scm($1);"
%typemap(in) GncGUID * (GncGUID g) " g = gnc_scm2guid($input); $1 = &g; "
%typemap(out) GncGUID * " $result = ($1) ? gnc_guid2scm(*($1)): SCM_BOOL_F; "

%typemap(in) gnc_numeric "$1 = gnc_scm_to_numeric($input);"
%typemap(out) gnc_numeric "$result = gnc_numeric_to_scm($1);"

%typemap(in) gint64 " $1 = scm_to_int64($input); "
%typemap(out) gint64 " $result = scm_from_int64($1); "

%typemap(in) time64 " $1 = scm_to_int64($input); "
%typemap(out) time64 " $result = scm_from_int64($1); "
%typemap(in) time64 * (time64 t) "t = scm_to_int64($input); $1 = &t;"
%typemap(out) time64 * " $result = ($1) ? scm_from_int64(*($1)) : SCM_BOOL_F; "

%typemap(in) struct tm * {
    SCM tm = $input;
    struct tm t = {
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 0)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 1)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 2)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 3)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 4)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 5)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 6)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 7)),
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 8)),
#ifdef HAVE_STRUCT_TM_GMTOFF
        scm_to_int(SCM_SIMPLE_VECTOR_REF(tm, 9)),
        scm_to_locale_string(SCM_SIMPLE_VECTOR_REF(tm, 10)),
#endif
    };
    $1 = &t;
 }

%typemap(out) struct tm * {
    SCM tm = scm_c_make_vector(11, SCM_UNDEFINED);
    struct tm* t = $1;
    SCM_SIMPLE_VECTOR_SET(tm, 0, scm_from_int(t->tm_sec));
    SCM_SIMPLE_VECTOR_SET(tm, 1, scm_from_int(t->tm_min));
    SCM_SIMPLE_VECTOR_SET(tm, 2, scm_from_int(t->tm_hour));
    SCM_SIMPLE_VECTOR_SET(tm, 3, scm_from_int(t->tm_mday));
    SCM_SIMPLE_VECTOR_SET(tm, 4, scm_from_int(t->tm_mon));
    SCM_SIMPLE_VECTOR_SET(tm, 5, scm_from_int(t->tm_year));
    SCM_SIMPLE_VECTOR_SET(tm, 6, scm_from_int(t->tm_wday));
    SCM_SIMPLE_VECTOR_SET(tm, 7, scm_from_int(t->tm_yday));
    SCM_SIMPLE_VECTOR_SET(tm, 8, scm_from_int(t->tm_isdst));
#ifdef HAVE_STRUCT_TM_GMTOFF
    SCM_SIMPLE_VECTOR_SET(tm, 9, scm_from_long(t->tm_gmtoff));
    SCM_SIMPLE_VECTOR_SET(tm, 10, scm_from_locale_string(t->tm_zone));
#else
    SCM_SIMPLE_VECTOR_SET(tm, 9, scm_from_long(0));
    SCM_SIMPLE_VECTOR_SET(tm, 10, scm_from_locale_string("GMT"));
#endif
    $result = tm;
 }

%define GLIST_HELPER_INOUT(ListType, ElemSwigType)
%typemap(in) ListType * {
  SCM list = $input;
  GList *c_list = NULL;

  while (!scm_is_null(list)) {
        void *p;

        SCM p_scm = SCM_CAR(list);
        if (scm_is_false(p_scm) || scm_is_null(p_scm))
           p = NULL;
        else
           p = SWIG_MustGetPtr(p_scm, ElemSwigType, 1, 0);

        c_list = g_list_prepend(c_list, p);
        list = SCM_CDR(list);
  }

  $1 = g_list_reverse(c_list);
}
%typemap(out) ListType * {
  SCM list = SCM_EOL;
  GList *node;

  for (node = $1; node; node = node->next)
    list = scm_cons(SWIG_NewPointerObj(node->data,
       ElemSwigType, 0), list);

  $result = scm_reverse(list);
}
%enddef
#elif defined(SWIGPYTHON) /* Typemaps for Python */
%typemap(in) gint8, gint16, gint32, gint64, gshort, glong {
    $1 = ($1_type)PyInt_AsLong($input);
}

%typemap(out) gint8, gint16, gint32, gint64, gshort, glong {
    $result = PyInt_FromLong($1);
}

%typemap(in) guint8, guint16, guint32, guint64, gushort, gulong {
    $1 = ($1_type)PyLong_AsUnsignedLong($input);
}

%typemap(out) guint8, guint16, guint32, guint64, gushort, gulong {
    $result = PyLong_FromUnsignedLong($1);
}

%typemap(in) gchar * {
    $1 = ($1_ltype)PyString_AsString($input);
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
        $result = Py_True;
        Py_INCREF($result);
    }
    else if ($1 == FALSE)
    {
        $result = Py_False;
        Py_INCREF($result);
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

%typemap(in) GSList *, QofQueryParamList * {
    $1 = NULL;
    /* Check if is a list */
    if (PyList_Check($input)) {
        int i;
        int size = PyList_Size($input);
        for (i = size-1; i >= 0; i--) {
            PyObject *o = PyList_GetItem($input, i);
            if (PyString_Check(o)) {
                $1 = g_slist_prepend($1,PyString_AsString(PyList_GetItem($input, i)));
            } else {
                PyErr_SetString(PyExc_TypeError, "list must contain strings");
                g_slist_free($1);
                return NULL;
            }
        }
    } else {
        PyErr_SetString(PyExc_TypeError, "not a list");
        return NULL;
    }
}

%typemap(out) GList *, CommodityList *, SplitList *, AccountList *, LotList *,
    MonetaryList *, PriceList *, EntryList * {
    guint i;
    gpointer data;
    PyObject *list = PyList_New(0);
    for (i = 0; i < g_list_length($1); i++)
    {
        data = g_list_nth_data($1, i);
        if (GNC_IS_ACCOUNT(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_Account, 0));
        else if (GNC_IS_SPLIT(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_Split, 0));
        else if (GNC_IS_TRANSACTION(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_Transaction, 0));
        else if (GNC_IS_COMMODITY(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_gnc_commodity, 0));
        else if (GNC_IS_COMMODITY_NAMESPACE(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_gnc_commodity_namespace, 0));
        else if (GNC_IS_LOT(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_GNCLot, 0));
        else if (GNC_IS_PRICE(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_GNCPrice, 0));
        else if (GNC_IS_INVOICE(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncInvoice, 0));
        else if (GNC_IS_ENTRY(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncEntry, 0));
        else if (GNC_IS_CUSTOMER(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncCustomer, 0));
        else if (GNC_IS_VENDOR(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncVendor, 0));
        else if (GNC_IS_EMPLOYEE(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncEmployee, 0));
        else if (GNC_IS_JOB(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncJob, 0));
        else if (GNC_IS_TAXTABLE(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p__gncTaxTable, 0));
        else if ($1_descriptor == $descriptor(MonetaryList *))
            PyList_Append(list, SWIG_NewPointerObj(data, $descriptor(gnc_monetary *), 0));
        else
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_void, 0));
    }
    $result = list;
}
#endif
