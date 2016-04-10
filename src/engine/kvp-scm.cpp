#include <kvp_frame.hpp>
#include <libguile.h>

extern "C"
{
#include "config.h"

#include <qof.h>
#include "engine-helpers-guile.h"

#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include "swig-runtime.h"
#include "kvp-scm.h"
}

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
            return new KvpValue{scm_to_int64(val)};
        }
        else
        {
            return new KvpValue{scm_to_double(val)};
        }
    }
    else if (gnc_numeric_p(val))
    {
        return new KvpValue{gnc_scm_to_numeric(val)};
    }
    else if (gnc_guid_p(val))
    {
        auto guid = gnc_scm2guid(val);
        auto tmpguid = guid_copy(&guid);
        return new KvpValue{tmpguid};
    }
    else if (gnc_timepair_p(val))
    {
        Timespec ts = gnc_timepair2timespec(val);
        return new KvpValue{ts};
    }
    else if (scm_is_string(val))
    {
        return new KvpValue{gnc_scm_to_utf8_string(val)};
    }
    else if (SWIG_IsPointerOfType(val, SWIG_TypeQuery("_p_KvpFrame")))
    {
#define FUNC_NAME G_STRFUNC
        auto vp_frame = SWIG_MustGetPtr(val,
                                        SWIG_TypeQuery("_p_KvpFrame"), 1, 0);
        KvpFrame *frame = static_cast<KvpFrame*>(vp_frame);
#undef FUNC_NAME
        return new KvpValue{frame};
    }
    /* FIXME: add list handler here */
    return NULL;
}

SCM
gnc_kvp_value_ptr_to_scm(KvpValue* val)
{
    if (val == nullptr) return SCM_BOOL_F;
    
    switch (val->get_type())
    {
    case KvpValue::Type::INT64:
        return scm_from_int64(val->get<int64_t>());
        break;
    case KvpValue::Type::DOUBLE:
        return scm_from_double (val->get<double>());
        break;
    case KvpValue::Type::NUMERIC:
        return gnc_numeric_to_scm(val->get<gnc_numeric>());
        break;
    case KvpValue::Type::STRING:
    {
        auto string = val->get<const char*>();
        return string ? scm_from_utf8_string(string) : SCM_BOOL_F;
        break;
    }
    case KvpValue::Type::GUID:
    {
        auto tempguid = val->get<GncGUID*>();
        return gnc_guid2scm(*tempguid);
    }
    break;
    case KvpValue::Type::TIMESPEC:
        return gnc_timespec2timepair(val->get<Timespec>());
        break;

    case KvpValue::Type::FRAME:
    {
        auto frame = val->get<KvpFrame*>();
        if (frame != nullptr)
            return SWIG_NewPointerObj(frame, SWIG_TypeQuery("_p_KvpFrame"), 0);
    }
    break;
    case KvpValue::Type::GDATE:
        return gnc_timespec2timepair(gdate_to_timespec(val->get<GDate>()));

        /* FIXME: handle types below */
    case KvpValue::Type::GLIST:
    default:
	break;
    }
    return SCM_BOOL_F;
}
