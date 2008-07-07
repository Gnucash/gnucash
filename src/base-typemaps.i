/* Not sure why SWIG doesn't figure this out. */
typedef int gint;
typedef int time_t;
typedef unsigned int guint;
typedef double gdouble;
typedef float gfloat;
typedef char * URLType;
typedef void * gpointer;

%typemap(newfree) gchar * "g_free($1);"

#if defined(SWIGGUILE)
typedef char gchar;

%typemap (out) char * {
  $result = scm_makfrom0str((const char *)$1);
  if (!SCM_NFALSEP($result)) {
    $result = scm_makstr(0, 0);
  }
}
%typemap(in) GNCPrintAmountInfo "$1 = gnc_scm2printinfo($input);"
%typemap(out) GNCPrintAmountInfo "$result = gnc_printinfo2scm($1);"

%typemap(in) gboolean "$1 = SCM_NFALSEP($input) ? TRUE : FALSE;"
%typemap(out) gboolean "$result = $1 ? SCM_BOOL_T : SCM_BOOL_F;"

%typemap(in) Timespec "$1 = gnc_timepair2timespec($input);"
%typemap(out) Timespec "$result = gnc_timespec2timepair($1);"

%typemap(in) GUID "$1 = gnc_scm2guid($input);"
%typemap(out) GUID "$result = gnc_guid2scm($1);"
%typemap(in) GUID * (GUID g) " g = gnc_scm2guid($input); $1 = &g; "
%typemap(out) GUID * " $result = ($1) ? gnc_guid2scm(*($1)): SCM_UNDEFINED; "

%typemap(in) gnc_numeric "$1 = gnc_scm_to_numeric($input);"
%typemap(out) gnc_numeric "$result = gnc_numeric_to_scm($1);"

%typemap(in) gint64 " $1 = gnc_scm_to_gint64($input); "
%typemap(out) gint64 " $result = gnc_gint64_to_scm($1); "

%define GLIST_HELPER_INOUT(ListType, ElemSwigType)
%typemap(in) ListType * {
  SCM list = $input;
  GList *c_list = NULL;

  while (!SCM_NULLP(list)) {
        void *p;

        SCM p_scm = SCM_CAR(list);
        if (SCM_FALSEP(p_scm) || SCM_NULLP(p_scm))
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

%typemap(out) GList *, CommodityList *, SplitList *, AccountList *, LotList * {
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
        else if (GNC_IS_LOT(data))
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_GNCLot, 0)); 
        else
            PyList_Append(list, SWIG_NewPointerObj(data, SWIGTYPE_p_void, 0)); 
    }
    $result = list;
}
#endif
