/*
 * dialog-search.h -- Search Dialog
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef _GNC_DIALOG_SEARCH_H
#define _GNC_DIALOG_SEARCH_H

#include "GNCId.h"
#include "QueryNew.h"

typedef struct _GNCSearchWindow GNCSearchWindow;

/* The two types of callbacks.
 *
 * In the first callback, (used in the callback button list) the obj_p
 * argument will be a pointer to the selected item (if one is
 * selected).  The callback may change the value if they wish (note
 * that the display will not adjust to the new selected item)
 *
 * In the second callback, the query is the property of the search
 * dialog; the callback should copy it if they want to keep it.  The
 * result will be a pointer to the selected item (if one is selected)
 * and the callback may change the value.
 */
typedef void (*GNCSearchCallback) (gpointer *obj_p, gpointer user_data);
typedef void (*GNCSearchResultCB) (QueryNew *query, gpointer user_data,
                                   gpointer *result);

/*
 * This callback will create a new item and return a handle to the
 * newly created item (even if it is not completely finished).  It
 * will be added to the query, but not selected.  This means the GncGUID
 * must be set.
 */
typedef gpointer (*GNCSearchNewItemCB) (gpointer user_data);

/* Free the general user_data object */
typedef void (*GNCSearchFree) (gpointer user_data);

/* This callback is called when (if) the user clicks the 'select'
 * button.  The search dialog will close when this callback function
 * returns.
 */
typedef void (*GNCSearchSelectedCB) (gpointer selected_object,
                                     gpointer user_data);

typedef struct
{
    const char *		label;
    GNCSearchCallback	cb_fcn;
} GNCSearchCallbackButton;

/* Caller MUST supply _EITHER_ a result_callback or a list of callback
 * buttons.  The caller MUST NOT supply both.
 *
 * Both the param_list and display_list are the property of the dialog
 * but will NOT be destroyed..  They should be a GList of
 * GNCSearchParam objects.  The display_list defines which paramters
 * of the found transactions are printed, and how.
 *
 * The start_query is the property of the caller and will only be copied.
 * The show_start_query, if it exists, will become the property of the
 * dialog and will be automatically destroyed.
 *
 * The user_data becomes the property of the search dialog and will
 * be freed via the callback when the dialog is closed.
 *
 * the type_label (if non-null) is the TRANSLATED string to use for
 * the type of object being searched.  This will be put in the Title
 * as well as into the "New" button.  If this string is NULL then
 * the dialog will use the obj_type instead.
 */
GNCSearchWindow *
gnc_search_dialog_create (GNCIdTypeConst obj_type, const gchar *title,
                          GList *param_list,
                          GList *display_list,
                          QueryNew *start_query, QueryNew *show_start_query,
                          GNCSearchCallbackButton *callbacks,
                          GNCSearchResultCB result_callback,
                          GNCSearchNewItemCB new_item_cb,
                          gpointer user_data, GNCSearchFree free_user_data,
                          const gchar *gconf_section,
                          const gchar *type_label);

void gnc_search_dialog_destroy (GNCSearchWindow *sw);
void gnc_search_dialog_raise (GNCSearchWindow *sw);

/* Register an on-close signal with the Search Dialog */
guint gnc_search_dialog_connect_on_close (GNCSearchWindow *sw,
        GCallback func,
        gpointer user_data);

/* Un-register the signal handlers with the Search Dialog */
void gnc_search_dialog_disconnect (GNCSearchWindow *sw, gpointer user_data);

/*
 * Set the select callback with this Search Window; setting it to NULL
 * will effectively clear it out.  If this is set, then a 'select'
 * button will show up.  If allow_clear is TRUE, then also allow
 * a 'clear' button which would allow the selected_cb to be called
 * with "NULL".
 */
void gnc_search_dialog_set_select_cb (GNCSearchWindow *sw,
                                      GNCSearchSelectedCB selected_cb,
                                      gpointer user_data,
                                      gboolean allow_clear);

/* Test the dialog */
void gnc_search_dialog_test (void);

#endif
