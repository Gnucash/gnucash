#ifndef KVP_SCM_H
#define KVP_SCM_H

#include <kvp_frame.h>
#include <libguile.h>

int gnc_kvp_value_ptr_p(SCM arg);
kvp_value* gnc_scm_to_kvp_value_ptr(SCM kvpval);
SCM gnc_kvp_value_ptr_to_scm(kvp_value* val);

#endif /* KVP_SCM_H */

