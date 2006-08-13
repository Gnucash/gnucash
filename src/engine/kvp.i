%module sw_kvp
%{
/* Includes the header in the wrapper code */
#include <qof.h>
#include <kvp-scm.h>
//#include <Transaction.h>
#include "glib-helpers.h"
%}

/* Convert from Guile --> C */
%typemap(in) GUID {
    $1 = gnc_scm2guid($input);
}

/* Convert from C --> Guile */
%typemap(out) GUID {
    $result = gnc_guid2scm($1);
}


/* Parse the header file to generate wrappers */
//%include <kvp-scm.h>
//%include <Transaction.h>

void gnc_kvp_frame_delete_at_path(KvpFrame *frame, GSList *key_path);

void kvp_frame_set_slot_path_gslist(
   KvpFrame *frame, const KvpValue *new_value, GSList *key_path);

%typemap(in) GSList *key_path {
  $1 = gnc_scm_to_gslist_string($input);
}

%typemap(out) KvpValue * {
  $result = gnc_kvp_value_ptr_to_scm($1);
}

KvpValue * kvp_frame_get_slot_path_gslist (KvpFrame *frame, GSList *key_path);

%typemap(in) GSList *key_path ""
%typemap(out) KvpValue * ""

%inline %{
KvpFrame * gnc_book_get_slots(QofBook *book) {
   qof_instance_get_slots(QOF_INSTANCE(book));
}
%}