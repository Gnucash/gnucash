#include "config.h"

#include <kvp_frame.h>
#include <g-wrap-wct.h>
#include <libguile.h>
#include <engine-helpers.h>

#include "kvp-scm.h"

int
gnc_kvp_value_ptr_p(SCM arg)
{
    return TRUE;
}

/* NOTE: There are some problems with this approach. Currently,
 *       guids are stored simply as strings in scheme, so some
 *       strings could be mistaken for guids, although that is
 *       unlikely. The general problem is distinguishing kvp
 *       types based only on the scheme type. */
kvp_value*
gnc_scm_to_kvp_value_ptr(SCM val)
{
    if(gh_exact_p (val) && gnc_gh_gint64_p(val))
    {
        return kvp_value_new_gint64(gnc_scm_to_gint64(val));
    }
    else if(gh_number_p(val))
    {
        return kvp_value_new_double(gh_scm2double(val));
    }
    else if(gnc_numeric_p(val))
    {
        return kvp_value_new_gnc_numeric(gnc_scm_to_numeric(val));
    }
    else if(gnc_guid_p(val))
    {
        GUID tmpguid = gnc_scm2guid(val);
        return kvp_value_new_guid(&tmpguid);
    }
    else if(gnc_timepair_p(val))
    {
        Timespec ts = gnc_timepair2timespec(val);
	return kvp_value_new_timespec(ts);
    }
    else if(gh_string_p(val))
    {
        char *newstr;
        kvp_value *ret;
        newstr = gh_scm2newstr(val, NULL);
        ret = kvp_value_new_string(newstr);
        free(newstr);
        return ret;
    }
    else if(gw_wcp_p(val) &&
	    gw_wcp_is_of_type_p(gh_eval_str("<gnc:kvp-frame*>"), val))
    {
        kvp_frame *frame = gw_wcp_get_ptr(val);
        return kvp_value_new_frame (frame);
    }
    /* FIXME: add binary handler here when it's figured out */
    /* FIXME: add list handler here */
    return NULL;
}

SCM
gnc_kvp_value_ptr_to_scm(kvp_value* val)
{
    switch(kvp_value_get_type(val))
    {
    case KVP_TYPE_GINT64:
        return gnc_gint64_to_scm(kvp_value_get_gint64(val));
        break;
    case KVP_TYPE_DOUBLE:
        return gh_double2scm(kvp_value_get_double(val));
        break;
    case KVP_TYPE_NUMERIC:
        return gnc_numeric_to_scm(kvp_value_get_numeric(val));
        break;
    case KVP_TYPE_STRING:
        return gh_str02scm(kvp_value_get_string(val));
        break;
    case KVP_TYPE_GUID:
    {
        GUID *tempguid = kvp_value_get_guid(val);
        return gnc_guid2scm(*tempguid);
    }
        break;
    case KVP_TYPE_TIMESPEC:
        return gnc_timespec2timepair(kvp_value_get_timespec(val));
	break;

    case KVP_TYPE_FRAME:
    {
        kvp_frame *frame = kvp_value_get_frame(val);
	if (frame)
	  return gw_wcp_assimilate_ptr (frame,
					gh_eval_str("<gnc:kvp-frame*>"));
    }
        break;

    /* FIXME: handle types below */
    case KVP_TYPE_BINARY:
        break;
    case KVP_TYPE_GLIST:
        break;
    }
    return SCM_BOOL_F;
}

void
gnc_kvp_frame_delete_at_path (kvp_frame *frame, GSList *key_path)
{
  kvp_value *val;

  val = kvp_frame_get_slot_path_gslist (frame, key_path);
  if (!val) return;
  if (kvp_value_get_type (val) == KVP_TYPE_FRAME) {
    frame = kvp_value_get_frame (val);
    kvp_frame_delete (frame);
  }
}
