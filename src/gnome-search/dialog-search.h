/*
 * dialog-search.h -- Search Dialog
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _GNC_DIALOG_SEARCH_H
#define _GNC_DIALOG_SEARCH_H

#include "GNCId.h"
#include "QueryNew.h"

typedef struct _GNCSearchWindow GNCSearchWindow;

/* The two types of callbacks.  
 *
 * Return TRUE if you want to keep the search dialog open, return
 * FALSE if you want the search dialog to close.
 *
 * In the first callback, the obj_p will be a pointer to the selected
 * item (if one is selected).  The callback may change the value if they
 * wish (note that the display will not adjust to the new selected item)
 *
 * In the second callback, the query is the property of the search
 * dialog; the callback should copy it if they want to keep it.
 *
 * The third callback will create a new item, and if the item is created,
 * it will destroy the search dialog.
 */
typedef gboolean (*GNCSearchCallback) (gpointer *obj_p, gpointer user_data);
typedef gboolean (*GNCSearchResultCB) (QueryNew *query, gpointer user_data,
				       gpointer *result);
typedef gpointer (*GNCSearchNewItemCB) (gpointer user_data);

typedef struct {
  const char *		label;
  GNCSearchCallback	cb_fcn;
} GNCSearchCallbackButton;

/* Caller MUST supply _EITHER_ a result_callback or a list of callback
 * buttons.  The caller MUST NOT supply both.
 *
 * The param_list is the property of the dialog but will NOT be destroyed.
 * The start_query is the property of the caller and will only be copied.
 * The show_start_query, if it exists, will become the property of the
 * dialog and will be automatically destroyed.  
 */
GNCSearchWindow * gnc_search_dialog_create (GNCIdTypeConst obj_type,
					    GList *param_list,
					    QueryNew *start_query,
					    QueryNew *show_start_query,
					    GNCSearchCallbackButton *callbacks,
					    GNCSearchResultCB result_callback,
					    GNCSearchNewItemCB new_item_cb,
					    gpointer user_data);
void gnc_search_dialog_destroy (GNCSearchWindow *sw);
void gnc_search_dialog_test (void);


/* Use this function to choose (and return) an object.
 *
 * The param_list is the property of the dialog but will NOT be destroyed.
 * the start_query is the property of the caller and will only be copied.
 * the show_start_query, if it exists, will become the property of the
 * dialog and will be automatically destroyed.
 */
gpointer gnc_search_dialog_choose_object (GNCIdTypeConst obj_type,
					  GList *param_list,
					  QueryNew *start_query,
					  QueryNew *show_start_query,
					  GNCSearchCallbackButton *callbacks,
					  GNCSearchResultCB result_callback,
					  GNCSearchNewItemCB new_item_cb,
					  gpointer user_data);

#endif
