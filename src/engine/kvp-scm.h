#ifndef KVP_SCM_H
#define KVP_SCM_H

#include "qof.h"
#include <libguile.h>

KvpValue* gnc_scm_to_kvp_value_ptr(SCM kvpval);
SCM gnc_kvp_value_ptr_to_scm(KvpValue* val);

#endif /* KVP_SCM_H */

