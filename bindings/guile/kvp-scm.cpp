#include <guid.hpp>
#include <kvp-frame.hpp>
#include <libguile.h>

extern "C"
{
#include <config.h>

#include <qof.h>
#include "swig-runtime.h"
#include "guile-mappings.h"
#include "engine-helpers-guile.h"
#include "gnc-guile-utils.h"
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
    if (scm_is_rational(val))
    {
        if (scm_is_exact(val) &&
            (scm_is_signed_integer(val, INT64_MIN, INT64_MAX) ||
             scm_is_unsigned_integer(val, INT64_MIN, INT64_MAX)))
        {
            return new KvpValue{scm_to_int64(val)};
        }
        else if (scm_is_exact(val) &&
                 (scm_is_signed_integer(scm_numerator(val),
                                       INT64_MIN, INT64_MAX) ||
                  scm_is_unsigned_integer(scm_numerator(val),
                                          INT64_MIN, INT64_MAX)) &&
                 (scm_is_signed_integer(scm_denominator(val),
                                        INT64_MIN, INT64_MAX) ||
                  (scm_is_unsigned_integer(scm_denominator(val),
                                           INT64_MIN, INT64_MAX))))
        {
            return new KvpValue{gnc_scm_to_numeric(val)};
        }
        else
        {
            return new KvpValue{scm_to_double(val)};
        }
    }
    else if (gnc_guid_p(val))
    {
        auto guid = gnc_scm2guid(val);
        auto tmpguid = guid_copy(&guid);
        return new KvpValue{tmpguid};
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
    case KvpValue::Type::FRAME:
    {
        auto frame = val->get<KvpFrame*>();
        if (frame != nullptr)
            return SWIG_NewPointerObj(frame, SWIG_TypeQuery("_p_KvpFrame"), 0);
    }
    break;
    case KvpValue::Type::GLIST:
    default:
	break;
    }
    return SCM_BOOL_F;
}
