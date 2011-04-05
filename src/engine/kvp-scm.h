#ifndef KVP_SCM_H
#define KVP_SCM_H

#include "qof.h"
#include <libguile.h>

int gnc_kvp_value_ptr_p(SCM arg);
KvpValue* gnc_scm_to_kvp_value_ptr(SCM kvpval);
SCM gnc_kvp_value_ptr_to_scm(KvpValue* val);
void gnc_kvp_frame_delete_at_path(KvpFrame *frame, GSList *key_path);

#endif /* KVP_SCM_H */

