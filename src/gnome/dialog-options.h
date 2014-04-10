#ifndef __OPTIONS_DIALOG_H__
#define __OPTIONS_DIALOG_H__

#include <gnome.h>
#include <guile/gh.h>

void gnc_show_options_dialog();

/* private */

GtkWidget *_gnc_options_dialog_add_page_(const char label[]);
void       _gnc_options_dialog_add_item_(GtkBox *page_w, SCM item);

void       _gnc_options_dialog_item_refresh_ui_(SCM item);
SCM        _gnc_options_dialog_item_get_ui_value_(SCM item);

#endif /* __OPTIONS_DIALOG_H__ */














