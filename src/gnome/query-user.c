/********************************************************************\
 * query-user.c -- functions for creating dialogs for GnuCash       * 
 * Copyright (C) 1998,1999 Linas Vepstas                            *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <gnome.h>
 
#include "top-level.h"

#include "messages.h"
#include "query-user.h"
#include "util.h"


/********************************************************************
 gnc_foundation_query_dialog

 A flexible user dialog that allows you to set up which buttons you
 want at the bottom, and whatever contents you want inside the main
 portion of the window.

 This function creates a dialog with the requested buttons. Clicking
 yes or ok will set *dialog_result to GNC_QUERY_YES, no to
 GNC_QUERY_NO and cancel to GNC_QUERY_CANCEL. These values are all
 negative; positive values are reserved for the caller.

 Each of the *_allowed arguments indicates whether or not the dialog
 should contain a button of that type. default_answer may be set to
 GNC_QUERY_YES for "yes" (or OK), GNC_QUERY_NO for "no", and
 GNC_QUERY_CANCEL for "cancel".  If you allow both yes and OK buttons,
 and set GNC_QUERY_YES as the default answer, which button is the
 default is undefined, but the result is the same either way, and why
 would you be doing that anyhow?

 The function returns the created dialog, or NULL if there was a
 problem.

*/

typedef struct {
  int delete_result;
  int * dialog_result;
} FoundationCBData;

static void
foundation_query_yes_cb(GtkWidget *w, gpointer data) {
  int *result = (int *) data; 
  *result = GNC_QUERY_YES;
}

static void
foundation_query_no_cb(GtkWidget *w, gpointer data) {
  int *result = (int *) data; 
  *result = GNC_QUERY_NO;
}

static void
foundation_query_cancel_cb(GtkWidget *w, gpointer data) {
  int *result = (int *) data; 
  *result = GNC_QUERY_CANCEL;
}

static gboolean
foundation_query_delete_cb(GtkWidget *w, GdkEvent *event, gpointer data) {
  FoundationCBData * cb_data = (FoundationCBData *) data;
  *(cb_data->dialog_result) = cb_data->delete_result;
  return(FALSE);
}

static gint
foundation_query_close_cb(GtkWidget *w, gpointer data) {
  free(data);
  return(FALSE);
}

GtkWidget *
gnc_foundation_query_dialog(const gchar *title,
                            GtkWidget *contents,
                            int default_answer,
                            gncBoolean yes_allowed,
                            gncBoolean ok_allowed,
                            gncBoolean no_allowed,
                            gncBoolean cancel_allowed,
                            int *dialog_result) {

  FoundationCBData * cb_data;
  GtkWidget *query_dialog = NULL;

  const gchar *button_names[5] = {NULL, NULL, NULL, NULL, NULL};
  GtkSignalFunc button_func[5] = {NULL, NULL, NULL, NULL, NULL};

  int button_count = 0;
  int default_button = 0;
  int delete_result = 0;


  /* Validate our arguments */
  assert(yes_allowed || ok_allowed || no_allowed || cancel_allowed);
  assert((default_answer == GNC_QUERY_YES) ||
	 (default_answer == GNC_QUERY_NO) ||
	 (default_answer == GNC_QUERY_CANCEL));
  assert((default_answer != GNC_QUERY_YES) || (yes_allowed || ok_allowed));
  assert((default_answer != GNC_QUERY_NO) || no_allowed);
  assert((default_answer != GNC_QUERY_CANCEL) || cancel_allowed);
  assert(dialog_result != NULL);

  /* Setup buttons */
  if(yes_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_YES;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_yes_cb);
    if(GNC_QUERY_YES == default_answer) default_button = button_count;
    delete_result = GNC_QUERY_YES;
    button_count++;
  }  
  if(ok_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_OK;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_yes_cb);
    if(GNC_QUERY_YES == default_answer) default_button = button_count;
    delete_result = GNC_QUERY_YES;
    button_count++;
  }  
  if(no_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_NO;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_no_cb);
    if(GNC_QUERY_NO == default_answer) default_button = button_count;
    delete_result = GNC_QUERY_NO;
    button_count++;
  }
  if(cancel_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_CANCEL;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_cancel_cb);
    if(GNC_QUERY_CANCEL == default_answer) default_button = button_count;
    delete_result = GNC_QUERY_CANCEL;
    button_count++;
  }

  /* Allocate our resources */
  cb_data = malloc(sizeof(FoundationCBData));
  if (cb_data == NULL)
    return NULL;

  query_dialog = gnome_dialog_newv(title, button_names);

  if (query_dialog == NULL) {
    free(cb_data);
    return NULL;
  }

  /* Connect button signals */
  {
    int i;
    for(i = 0; i < button_count; i++) {
      gnome_dialog_button_connect(GNOME_DIALOG(query_dialog), i,
                                  GTK_SIGNAL_FUNC(button_func[i]),
                                  (gpointer) dialog_result);
    }
  }

  /* Setup the delete and close callbacks */
  cb_data->delete_result = delete_result;
  cb_data->dialog_result = dialog_result;

  gtk_signal_connect(GTK_OBJECT(query_dialog),
		     "delete_event",
		     GTK_SIGNAL_FUNC(foundation_query_delete_cb),
		     (gpointer) cb_data);

  gtk_signal_connect(GTK_OBJECT(query_dialog),
		     "close",
		     GTK_SIGNAL_FUNC(foundation_query_close_cb),
		     (gpointer) cb_data);

  /* Setup window settings */
  gtk_window_set_modal(GTK_WINDOW(query_dialog), GNC_T);
  gnome_dialog_set_default(GNOME_DIALOG(query_dialog), default_button);
  gnome_dialog_set_close(GNOME_DIALOG(query_dialog), TRUE);
  gnome_dialog_set_parent(GNOME_DIALOG(query_dialog),
			  GTK_WINDOW(gnc_get_ui_data()));

  /* Add in the user-supplied widget */
  if(contents)
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(query_dialog)->vbox),
                       contents, FALSE, FALSE, 0);

  return query_dialog;
}


/********************************************************************\
 * gnc_verify_dialog                                                *
 *   display a message, and asks the user to press "Yes" or "No"    *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   text     - the message to display                        *
 * Return: none                                                     *
\********************************************************************/
gncBoolean
gnc_verify_dialog( const char *message,
		   gncBoolean yes_is_default ) {
  GtkWidget *verify_box = NULL;
  
  verify_box = gnome_message_box_new(message,
				     GNOME_MESSAGE_BOX_QUESTION,
				     GNOME_STOCK_BUTTON_YES,
				     GNOME_STOCK_BUTTON_NO,
				     NULL);

  gnome_dialog_set_parent(GNOME_DIALOG(verify_box),
			  GTK_WINDOW(gnc_get_ui_data()));

  gnome_dialog_set_default(GNOME_DIALOG(verify_box),
			   yes_is_default ? 0 : 1);

  return (gnome_dialog_run_and_close(GNOME_DIALOG(verify_box)) == 0);
}


/********************************************************************\
 * gnc_info_dialog                                                  * 
 *   displays an information dialog box                             * 
 *                                                                  * 
 * Args:   message - the information message to display             * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_info_dialog(const char *message) {
  GtkWidget *info_box = NULL;
  
  info_box = gnome_ok_dialog_parented(message,
				      GTK_WINDOW(gnc_get_ui_data()));

  gnome_dialog_run_and_close(GNOME_DIALOG(info_box));
}

/********************************************************************\
 * gnc_warning_dialog                                               * 
 *   displays a warning dialog box                                  * 
 *                                                                  * 
 * Args:   message - the warning message to display                 * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_warning_dialog(const char *message)
{
  GtkWidget *warning_box = NULL;
  
  warning_box = gnome_warning_dialog_parented(message,
					      GTK_WINDOW(gnc_get_ui_data()));

  gnome_dialog_run_and_close(GNOME_DIALOG(warning_box));
}

/********************************************************************\
 * gnc_error_dialog                                                 * 
 *   displays an error dialog box                                   * 
 *                                                                  * 
 * Args:   message - the error message to display                   * 
 * Return: none                                                     * 
\********************************************************************/
void 
gnc_error_dialog(const char *message) {
  GtkWidget *error_box = NULL;
  
  error_box = gnome_error_dialog_parented(message,
					  GTK_WINDOW(gnc_get_ui_data()));

  gnome_dialog_run_and_close(GNOME_DIALOG(error_box));
}

/********************************************************************
 gnc_choose_one_from_list_dialog

 This function is primarily intended to be called from scheme.

 Returns ('result . value) if the user selects an item, 'cancel if the
 user selects cancel, and #f if there was a failure of some kind.

 list_items must be a scheme list of pairs.  Each pair must contain a
 list item name (as a string) followed by a thunk to call when that
 item is selected.  The thunk should take no arguments.  If the thunk
 does not return #f, then the dialog box will be closed and this
 function will return that thunk's result in a pair ('result . value).
 
*/

typedef struct {
  char *name;
  SCM closure;
  int *dialog_result;
  SCM *scm_result;
  GnomeDialog *dialog;
} ChooseItemCBData;


static void
gnc_choose_item_cb(GtkWidget *w, gpointer p)
{
  if(p == NULL)
    return;

 {
   ChooseItemCBData *d = (ChooseItemCBData *) p;
   SCM result = gh_call0(d->closure);
   
   if(result != SCM_BOOL_F) {
     *(d->dialog_result) = 1;
     *(d->scm_result) = result;
     gnome_dialog_close(d->dialog);
   }
 }
}

SCM
gnc_choose_item_from_list_dialog(const char *title, SCM list_items)
{
  ChooseItemCBData *cb_data = NULL;
  int dialog_result = 0;
  SCM scm_result = SCM_BOOL_F;
  int result;
  unsigned long num_items;
  GtkWidget *vbox;
  GtkWidget *query_box;
  SCM listcursor;
  gncBoolean status_ok;
  unsigned long i;

  if(title == NULL)
    return SCM_BOOL_F;
  if(!gh_list_p(list_items))
    return SCM_BOOL_F;

  num_items = gh_length(list_items);
  
  cb_data = (ChooseItemCBData *) malloc(sizeof(ChooseItemCBData) * num_items);

  if(cb_data == NULL)
    return SCM_BOOL_F;

  /* Convert the scm data to C callback structs */
  i = 0;
  status_ok = GNC_T;
  listcursor = list_items;
  while(status_ok && !gh_null_p(listcursor)) {
    SCM scm_item = gh_car(listcursor);

    if(!gh_pair_p(scm_item)) {
      fprintf(stderr, "Dying: not a pair.\n");
      status_ok = GNC_F;
    } else {
      SCM item_scm_name = gh_car(scm_item);
      SCM item_scm_thunk = gh_cdr(scm_item);
      
      if(!(gh_string_p(item_scm_name) && gh_procedure_p(item_scm_thunk))) {
        fprintf(stderr, "Dying: bad pair item(s).\n");
        status_ok = GNC_F;
      } else {
        cb_data[i].name = gh_scm2newstr(item_scm_name, NULL);
        cb_data[i].closure = item_scm_thunk;
        cb_data[i].dialog_result = &dialog_result;
        cb_data[i].scm_result = &scm_result;

        if(!cb_data[i].name) {
          fprintf(stderr, "Dying: no C name.\n");
          status_ok = GNC_F;
        } else {
          listcursor = gh_cdr(listcursor);
          i++;
        }
      }
    }
  }

  if(!status_ok) {
    fprintf(stderr, "Dying after copy.\n");
    free(cb_data);
    return SCM_BOOL_F;
  }

  /* Build the list */
  vbox = gtk_vbox_new(TRUE, 0);

  for(i = 0; i < num_items; i++) {
    GtkWidget *b = gtk_button_new_with_label(cb_data[i].name);
    if(b == NULL) {
      status_ok = GNC_F;
    } else {
      gtk_signal_connect(GTK_OBJECT(b), "clicked",
                         GTK_SIGNAL_FUNC(gnc_choose_item_cb),
                         (gpointer) &(cb_data[i]));
      gtk_box_pack_start(GTK_BOX(vbox), b, TRUE, TRUE, 0);
      gtk_widget_show(b);
    }
  }

  if(!status_ok) {
    fprintf(stderr, "Dying after buttons.\n");
    gtk_widget_unref(vbox);
    free(cb_data);
    return SCM_BOOL_F;
  }

  gtk_widget_show(vbox);

  query_box = gnc_foundation_query_dialog(title,
					  vbox,
					  GNC_QUERY_CANCEL, /* cancel */
					  GNC_F, /* yes_allowed */
					  GNC_F, /* ok_allowed */
					  GNC_F, /* no_allowed */
					  GNC_T, /* cancel_allowed */
					  &dialog_result);

  if(query_box == NULL) {
    free(cb_data);
    return SCM_BOOL_F;
  }

  for (i = 0; i < num_items; i++)
    cb_data[i].dialog = GNOME_DIALOG(query_box);

  gnome_dialog_run_and_close(GNOME_DIALOG(query_box));

  switch(dialog_result) {
    case 1:
      result = gh_cons(gh_symbol2scm("result"), scm_result);
      break;
    case GNC_QUERY_YES:
    case GNC_QUERY_NO:
      result = SCM_BOOL_F;
      break;
    case GNC_QUERY_CANCEL:
      result = gh_symbol2scm("cancel");
      break;
    default:
      result = SCM_BOOL_F;
      break;
  }

  free(cb_data);

  return(result);
}
