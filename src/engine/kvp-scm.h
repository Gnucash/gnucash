#ifndef KVP_SCM_H
#define KVP_SCM_H

#ifdef __cplusplus
extern "C"
{
#endif
#include <qof.h>
#include <libguile.h>

KvpValue* gnc_scm_to_kvp_value_ptr(SCM kvpval);
SCM gnc_kvp_value_ptr_to_scm(KvpValue* val);

#ifdef __cplusplus
}
#endif
#endif /* KVP_SCM_H */

