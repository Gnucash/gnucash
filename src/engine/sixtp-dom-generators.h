#ifndef _SIXTP_DOM_GENERATORS_H_
#define _SIXTP_DOM_GENERATORS_H_

#include "config.h"

#include <glib.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-generators.h"
#include "GNCId.h"
#include "gnc-commodity.h"
#include "date.h"
#include "gnc-numeric.h"

xmlNodePtr guid_to_dom_tree(const char *tag, GUID* gid);
xmlNodePtr commodity_ref_to_dom_tree(const char *tag, const gnc_commodity *c);
xmlNodePtr timespec_to_dom_tree(const char *tag, const Timespec *spec);
gchar * timespec_nsec_to_string(const Timespec *ts);
gchar * timespec_sec_to_string(const Timespec *ts);
xmlNodePtr gnc_numeric_to_dom_tree(const char *tag, const gnc_numeric *num);

#endif /* _SIXTP_DOM_GENERATORS_H_ */
