#ifndef __QUERY_USER_H__
#define __QUERY_USER_H__

#include "gnc-common.h"
#include <guile/gh.h>

/* Only implemented in GNOME version right now. */
int queryBox(const char *text,
             int default_answer,
             gncBoolean yes_allowed,
             gncBoolean ok_allowed,
             gncBoolean no_allowed,
             gncBoolean cancel_allowed);

SCM gnc_choose_item_from_list_dialog(const char *title, SCM list_items);

#endif

