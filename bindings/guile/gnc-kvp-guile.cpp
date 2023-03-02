#include <guid.hpp>
#include <kvp-frame.hpp>
#include <libguile.h>
#include <numeric>

#include <config.h>

#include <qof.h>
#include "swig-runtime.h"
#include "guile-mappings.h"
#include "gnc-engine-guile.h"
#include "gnc-guile-utils.h"
#include "gnc-kvp-guile.h"

/* NOTE: There are some problems with this approach. Currently,
 *       guids are stored simply as strings in scheme, so some
 *       strings could be mistaken for guids, although that is
 *       unlikely. The general problem is distinguishing kvp
 *       types based only on the scheme type.
 */

static bool scm_is_list_of_string_pairs (SCM val)
{
    for (; !scm_is_null (val); val = scm_cdr (val))
    {
        if (!(scm_is_pair (val) && scm_is_pair (scm_car (val)) &&
              scm_is_string (scm_caar (val))))
            return false;
    }
    return true;
}

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
    else if (!scm_is_null (val) && scm_is_list_of_string_pairs (val))
    {
        auto frame = new KvpFrame;
        for (; !scm_is_null (val); val = scm_cdr (val))
        {
            auto key_str = scm_to_utf8_stringn (scm_caar (val), nullptr);
            auto val_scm = scm_cdar (val);
            auto prev = frame->set ({key_str}, gnc_scm_to_kvp_value_ptr (val_scm));
            g_free (key_str);
            // there is a pre-existing key-value
            if (prev)
                delete prev;
        }
        return new KvpValue (frame);
    }
    else if (!scm_is_null (val) && scm_is_list (val))
    {
        GList *kvplist = nullptr;
        for (; !scm_is_null (val); val = scm_cdr (val))
        {
            auto elt = gnc_scm_to_kvp_value_ptr (scm_car (val));
            kvplist = g_list_prepend (kvplist, elt);
        }
        return new KvpValue (g_list_reverse (kvplist));
    }
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
        return tempguid ? gnc_guid2scm(*tempguid) : SCM_BOOL_F;
    }
    break;
    case KvpValue::Type::FRAME:
    {
        auto frame { val->get<KvpFrame*>() };
        auto acc = [](const auto& rv, const auto& iter)
        {
            auto key_scm { scm_from_utf8_string (iter.first) };
            auto val_scm { gnc_kvp_value_ptr_to_scm (iter.second) };
            return scm_acons (key_scm, val_scm, rv);
        };
        return frame ? scm_reverse (std::accumulate (frame->begin(), frame->end(), SCM_EOL, acc)) : SCM_BOOL_F;
    }
    break;
    case KvpValue::Type::GLIST:
    {
        SCM lst = SCM_EOL;
        for (GList *n = val->get<GList*>(); n; n = n->next)
        {
            auto elt = gnc_kvp_value_ptr_to_scm (static_cast<KvpValue*>(n->data));
            lst = scm_cons (elt, lst);
        }
        return scm_reverse (lst);
    }
    default:
	break;
    }
    return SCM_BOOL_F;
}
