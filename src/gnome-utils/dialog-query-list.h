/*
 * dialog-query-list.h -- a simple dialog to display a querylist and
 *                        allow users to select items (or close the list)
 *
 * Created By:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef GNC_DIALOG_QUERY_LIST_H
#define GNC_DIALOG_QUERY_LIST_H

#include "Query.h"

typedef struct _DialogQueryList DialogQueryList;

typedef void (*GNCDisplayListCB)(gpointer obj, gpointer user_data);
typedef struct {
  const char *		label;
  GNCDisplayListCB	cb_fcn;
} GNCDisplayListButton;

DialogQueryList *
gnc_dialog_query_list_new (GList *param_list, Query *q);

void gnc_dialog_query_list_set_title (DialogQueryList *dql, const char *title);
void gnc_dialog_query_list_set_label (DialogQueryList *dql, const char *label);
void gnc_dialog_query_list_set_buttons (DialogQueryList *dql,
					GNCDisplayListButton *buttons,
					gpointer user_data);
void gnc_dialog_query_list_set_numerics (DialogQueryList *dql, gboolean abs,
					 gboolean inv_sort);

void gnc_dialog_query_list_refresh (DialogQueryList *dql);
void gnc_dialog_query_list_destroy (DialogQueryList *dql);

DialogQueryList *
gnc_dialog_query_list_create (GList *param_list, Query *q,
			      const char *title, const char *label,
			      gboolean abs, gboolean inv_sort,
			      GNCDisplayListButton *buttons, gpointer user_data);


#endif /* GNC_DIALOG_QUERY_LIST_H */
