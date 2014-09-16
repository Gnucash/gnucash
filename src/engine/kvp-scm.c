#include "config.h"

#include "qof.h"
#include <libguile.h>
#include "engine-helpers-guile.h"

#include "kvp-scm.h"
#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include "swig-runtime.h"

/* NOTE: There are some problems with this approach. Currently,
 *       guids are stored simply as strings in scheme, so some
 *       strings could be mistaken for guids, although that is
 *       unlikely. The general problem is distinguishing kvp
 *       types based only on the scheme type.
 */

KvpValue *
gnc_scm_to_kvp_value_ptr(SCM val)
{
    if (scm_is_number(val))
    {
        /* in guile 1.8 (exact? ) only works on numbers */
        if (scm_is_exact (val) && gnc_gh_gint64_p(val))
        {
            return kvp_value_new_gint64(scm_to_int64(val));
        }
        else
        {
            return kvp_value_new_double(scm_to_double(val));
        }
    }
    else if (gnc_numeric_p(val))
    {
        return kvp_value_new_gnc_numeric(gnc_scm_to_numeric(val));
    }
    else if (gnc_guid_p(val))
    {
        GncGUID tmpguid = gnc_scm2guid(val);
        return kvp_value_new_guid(&tmpguid);
    }
    else if (gnc_timepair_p(val))
    {
        Timespec ts = gnc_timepair2timespec(val);
        return kvp_value_new_timespec(ts);
    }
    else if (scm_is_string(val))
    {
        gchar *newstr;
        KvpValue *ret;
        newstr = gnc_scm_to_utf8_string (val);
        ret = kvp_value_new_string(newstr);
        g_free (newstr);
        return ret;
    }
    else if (SWIG_IsPointerOfType(val, SWIG_TypeQuery("_p_KvpFrame")))
    {
#define FUNC_NAME G_STRFUNC
        KvpFrame *frame = SWIG_MustGetPtr(val, SWIG_TypeQuery("_p_KvpFrame"),
                                          1, 0);
#undef FUNC_NAME
        return kvp_value_new_frame (frame);
    }
    /* FIXME: add list handler here */
    return NULL;
}

SCM
gnc_kvp_value_ptr_to_scm(KvpValue* val)
{
    const gchar *string;
    switch (kvp_value_get_type(val))
    {
    case KVP_TYPE_GINT64:
        return scm_from_int64(kvp_value_get_gint64(val));
        break;
    case KVP_TYPE_DOUBLE:
        return scm_from_double (kvp_value_get_double(val));
        break;
    case KVP_TYPE_NUMERIC:
        return gnc_numeric_to_scm(kvp_value_get_numeric(val));
        break;
    case KVP_TYPE_STRING:
        string = kvp_value_get_string(val);
        return string ? scm_from_utf8_string(string) : SCM_BOOL_F;
        break;
    case KVP_TYPE_GUID:
    {
        GncGUID *tempguid = kvp_value_get_guid(val);
        return gnc_guid2scm(*tempguid);
    }
    break;
    case KVP_TYPE_TIMESPEC:
        return gnc_timespec2timepair(kvp_value_get_timespec(val));
        break;

    case KVP_TYPE_FRAME:
    {
        KvpFrame *frame = kvp_value_get_frame(val);
        if (frame)
            return SWIG_NewPointerObj(frame, SWIG_TypeQuery("_p_KvpFrame"), 0);
    }
    break;
    case KVP_TYPE_GDATE:
        return gnc_timespec2timepair(gdate_to_timespec(kvp_value_get_gdate(val)));

        /* FIXME: handle types below */
    case KVP_TYPE_GLIST:
    default:
	break;
    }
    return SCM_BOOL_F;
}

void
gnc_kvp_frame_delete_at_path (KvpFrame *frame, GSList *key_path)
{
    kvp_frame_set_slot_path_gslist (frame, NULL, key_path);
}
