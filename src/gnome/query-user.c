
#include <gnome.h>
 
#include "config.h"
#include "messages.h"
#include "top-level.h"
#include "query-user.h"

/********************************************************************
 gnc_foundation_query_dialog

 A flexible user dialog that allows you to set up which buttons you
 want at the bottom, and whatever contents you want inside the main
 portion of the window.

 Upon entry, dialog_result must point to an integer value of 0.  This
 function will put up a dialog with the requested buttons and will not
 return until someone has set *dialog_result to a non-zero value.
 Clicking yes will set *dialog_result to -1, no to -2 and cancel to
 -3.  The positive values are reserved for the caller.

 Each of the *_allowed arguments indicates whether or not the dialog
 should contain a button of that type.  default_answer may be set to
 -1 for "yes" (or OK), -2 for "no", and -3 for "cancel".  If you allow
 both yes and OK buttons, and set -1 as the default answer, which
 button is the default is undefined, but the result is the same either
 way, and why would be doing that anyhow?

 The function will return 0 if all went well, and non-zero if there
 was a failure of any sort.  If there was a failure, you cannot trust
 the value of *dialog_result.

 FIXME: We need more system call error checking in here!

*/

static void
foundation_query_yes_cb(GtkWidget *w, gpointer data) {
  int *result = (int *) data; 
  *result = -1;
}

static void
foundation_query_no_cb(GtkWidget *w, gpointer data) {
  int *result = (int *) data; 
  *result = -2;
}

static void
foundation_query_cancel_cb(GtkWidget *w, gpointer data) {
  int *result = (int *) data; 
  *result = -3;
}

int
gnc_foundation_query_dialog(const gchar *title,
                            GtkWidget *contents,
                            int default_answer,
                            gncBoolean yes_allowed,
                            gncBoolean ok_allowed,
                            gncBoolean no_allowed,
                            gncBoolean cancel_allowed,
                            int *dialog_result) {

  GtkWidget *parent = gnc_get_ui_data();
  GtkWidget *verify_box = NULL;

  gchar *button_names[5] = {NULL, NULL, NULL, NULL, NULL};
  int button_count = 0;
  GtkSignalFunc button_func[5] = {NULL, NULL, NULL, NULL, NULL};
  int default_button = 0;

  /* FIXME: These should be nana checks, but nana seems broken right now... */
#if 0
  I(yes_allowed || ok_allowed || no_allowed || cancel_allowed);
  I((default_answer == -1) && (yes_allowed || ok_allowed));
  I((default_answer == -2) && no_allowed);
  I((default_answer == -3) && cancel_allowed);
#endif
  
  if(yes_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_YES;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_yes_cb);
    if(-1 == default_answer) default_button = button_count;
    button_count++;
  }  
  if(ok_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_OK;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_yes_cb);
    if(-1 == default_answer) default_button = button_count;
    button_count++;
  }  
  if(no_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_NO;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_no_cb);
    if(-2 == default_answer) default_button = button_count;
    button_count++;
  }  
  if(cancel_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_CANCEL;
    button_func[button_count] = GTK_SIGNAL_FUNC(foundation_query_cancel_cb);
    if(-3 == default_answer) default_button = button_count;
    button_count++;
  }

  /* FIXME: I have no idea why gcc needs this coercion right now... */
  verify_box = gnome_dialog_newv(title, (const gchar **) button_names);
    
  // gnome_dialog_set_modal(GNOME_DIALOG(verify_box));

  gnome_dialog_set_default(GNOME_DIALOG(verify_box), default_button);
  gnome_dialog_set_close(GNOME_DIALOG(verify_box), TRUE);

  {
    int i;
    for(i = 0; i < button_count; i++) {
      gnome_dialog_button_connect(GNOME_DIALOG(verify_box), i,
                                  GTK_SIGNAL_FUNC(button_func[i]),
                                  (gpointer) dialog_result);
    }
  }
  
  if(contents) {
    gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(verify_box)->vbox),
                       contents, FALSE, FALSE, 0);
    gtk_widget_show(contents);
  }

  gtk_widget_show(verify_box);
  
  setBusyCursor(parent);
  
  while(*dialog_result == 0) {
    gtk_main_iteration();
  }

  /* User supplied value, dialog not automatically closed. */
  if(*dialog_result > 0) gnome_dialog_close(GNOME_DIALOG(verify_box));

  unsetBusyCursor(parent);
  
  return(0);
}

/********************************************************************
 queryBox

 display text, and wait for yes, no, or cancel, depending on the
 arguments.  Each of the *_allowed arguments indicates whether or not
 the dialog should contain a button of that type.  default_answer may
 be set to 1 for "yes" (or OK), 2 for "no", and 3 for "cancel".  If
 you allow both yes and OK buttons, and set 1 as the default answer,
 which button is the default is undefined, but the result is the same
 either way, and why would be doing that anyhow?

 This function returns 1 for yes (or OK), 2 for no, and 3 for cancel.
 If there was an unrecorverable error of some kind, it returns a
 negative value.
 
 NOTE: This function does not return until the dialog is closed.

*/

int
queryBox(const char *text,
         int default_answer,
         gncBoolean yes_allowed,
         gncBoolean ok_allowed,
         gncBoolean no_allowed,
         gncBoolean cancel_allowed) {

  int dialog_result = 0;
  int foundation_default;
  int result;
  GtkWidget *verify_text_widget = NULL;

  /* FIXME: These should be nana checks, but nana seems broken right now... */
#if 0
  I(yes_allowed || ok_allowed || no_allowed || cancel_allowed);
  I((default_answer == 1) && (yes_allowed || ok_allowed));
  I((default_answer == 2) && no_allowed);
  I((default_answer == 3) && cancel_allowed);
#endif

  /* Switch to what foundation expects */
  default_answer = -default_answer;
  
  verify_text_widget = gtk_label_new(text);
  if(!verify_text_widget) return -1;

  if(gnc_foundation_query_dialog(text,
                                 verify_text_widget,
                                 default_answer,
                                 yes_allowed,
                                 ok_allowed,
                                 no_allowed,
                                 cancel_allowed,
                                 &dialog_result) == 0) {

    switch(dialog_result) {
      case -1: result = 1; break;
      case -2: result = 2; break;
      case -3: result = 3; break;
      default: result = -1; break;
    }
  } else {
    result = -1;
  }

  return(result);
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
} ChooseItemCBData;


static void
gnc_choose_item_cb(GtkWidget *w, gpointer p) {
  if(!p) return;
  
 {
   ChooseItemCBData *d = (ChooseItemCBData *) p;
   SCM result = gh_call0(d->closure);
   
   if(result != SCM_BOOL_F) {
     *(d->dialog_result) = 1;
     *(d->scm_result) = result;
   }
 }
}

SCM
gnc_choose_item_from_list_dialog(const char *title, SCM list_items) {

  ChooseItemCBData *cb_data = NULL;
  int dialog_result = 0;
  SCM scm_result = SCM_BOOL_F;
  int result;
  unsigned long num_items;
  GtkWidget *vbox;
  SCM listcursor;
  gncBoolean status_ok;
  unsigned long i;

  if(!title) return SCM_BOOL_F;
  if(!gh_list_p(list_items)) return SCM_BOOL_F;

  num_items = gh_length(list_items);
  
  cb_data = (ChooseItemCBData *) malloc(sizeof(ChooseItemCBData) * num_items);

  if(!cb_data) return SCM_BOOL_F;

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
    if(!b) {
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

  if(gnc_foundation_query_dialog(title,
                                 vbox,
                                 3, /* cancel */
                                 GNC_F, /* yes_allowed */
                                 GNC_F, /* ok_allowed */
                                 GNC_F, /* no_allowed */
                                 GNC_T, /* cancel_allowed */
                                 &dialog_result) == 0) {
    switch(dialog_result) {
      case 1:  result = gh_cons(gh_symbol2scm("result"), scm_result); break;
      case -1: result = SCM_BOOL_F; break;
      case -2: result = SCM_BOOL_F; break;
      case -3: result = gh_symbol2scm("cancel"); break;
      default: result = SCM_BOOL_F; break;
    }
  } else {
    result = SCM_BOOL_F;
  }

  free(cb_data);

  return(result);
}
