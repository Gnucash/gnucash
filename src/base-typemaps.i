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

/* Not sure why SWIG doesn't figure this out. */
typedef void * gpointer;
typedef int gint;
typedef int time_t;
typedef unsigned int guint;
typedef double gdouble;
typedef char * URLType;
typedef char gchar;

%typemap(newfree) gchar * "g_free($1);"
%typemap (out) char * {
  $result = scm_makfrom0str((const char *)$1);
  if (!SCM_NFALSEP($result)) {
    $result = scm_makstr(0, 0);
  }
}
%typemap(in) GNCPrintAmountInfo "$1 = gnc_scm2printinfo($input);"
%typemap(out) GNCPrintAmountInfo "$result = gnc_printinfo2scm($1);"


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


