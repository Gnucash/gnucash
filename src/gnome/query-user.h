#ifndef __QUERY_USER_H__
#define __QUERY_USER_H__

#include <guile/gh.h>

enum
{
  GNC_QUERY_YES = -1,
  GNC_QUERY_NO = -2,
  GNC_QUERY_CANCEL = -3
};


gncBoolean gnc_verify_dialog_parented(GtkWindow *parent, const char *message,
                                      gncBoolean yes_is_default);

void gnc_info_dialog(const char *message);
void gnc_info_dialog_parented(GtkWindow *parent, const char *message);

void gnc_warning_dialog(const char *message);
void gnc_warning_dialog_parented(GtkWindow *parent, const char *message);

void gnc_error_dialog_parented(GtkWindow *parent, const char *message);

SCM gnc_choose_item_from_list_dialog(const char *title, SCM list_items);

#endif
