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
 */
typedef gboolean (*GNCSearchCallback)(gpointer *obj_p, gpointer user_data);
typedef gboolean (*GNCSearchResultCB) (QueryNew *query, gpointer user_data,
				       gpointer *result);

typedef struct {
  const char *		label;
  GNCSearchCallback	cb_fcn;
} GNCSearchCallbackButton;

/* Caller MUST supply _EITHER_ a result_callback or a list of callback
 * buttons.  The caller MUST NOT supply both.
 *
 * The start_query is the property of the caller.
 */
GNCSearchWindow * gnc_search_dialog_create (GNCIdTypeConst obj_type,
					    QueryNew *start_query,
					    gboolean show_start_results,
					    GNCSearchCallbackButton *callbacks,
					    GNCSearchResultCB result_callback,
					    gpointer user_data);
void gnc_search_dialog_destroy (GNCSearchWindow *sw);
void gnc_search_dialog_test (void);


/* Use this function to choose (and return) an object. */
gpointer gnc_search_dialog_choose_object (GNCIdTypeConst obj_type,
					  QueryNew *start_query,
					  gboolean show_start_results,
					  GNCSearchCallbackButton *callbacks,
					  GNCSearchResultCB result_callback,
					  gpointer user_data);

#endif
