#ifndef _SIXTP_DOM_GENERATORS_H_
#define _SIXTP_DOM_GENERATORS_H_

#include "config.h"

#include <glib.h>

#include "gnc-xml-helper.h"

#include "sixtp-dom-generators.h"
#include "GNCId.h"


xmlNodePtr guid_to_dom_tree(GUID* gid);

#endif /* _SIXTP_DOM_GENERATORS_H_ */
