/********************************************************************\
 * util.c -- utility functions that are used everywhere else for    *
 *           xacc (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <gnome.h>
 
#include "config.h"

#include "messages.h"
#include "top-level.h"
#include "cursors.h"
#include "ui-callbacks.h"
#include "util.h"

GncCursorDef gnc_cursors[GNC_CURSOR_LAST] = {
        {  NULL, GDK_ARROW },
        {  NULL, GDK_WATCH },
};


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

#if 0

/** GLOBALS *********************************************************/

extern XtAppContext app;
extern int realized;


/********************************************************************\
 * dateCB -- ensures the data the user enters in the date field     * 
 *   is in a valid format.                                          * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     *
 *         cb - the callback struct                                 * 
 * Return: none                                                     * 
\********************************************************************/
void
dateCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)cb;
  char input;

  /* TODO: ??? add support for date field accelerator keys!! */
  if( cbs->text->ptr != NULL )
    {
    input = (cbs->text->ptr)[0];
  
    switch( input )
      {
      case '/':
      case '.':
      case '-':
        /* Make sure that there is at most two separators */
        {
        String str = XmTextGetString(mw);
        int i,count=0;
        
        for( i=0; str[i] != '\0'; i++ )
          if( str[i] == input )
            count++;
        if( count >= 2 )
          cbs->doit = False;
        }
        break;

      case 0x0:
        /* if delete key (for example) is hit, then input string */
        /* will be an empty string. In such a case, allow the input */
        cbs->doit = True;
        break;

      default:
        /* only accept the input if it is a number */
        cbs->doit = isNum(input);
      }
    }
  }

/********************************************************************\
 * amountCB -- ensures the data entered in the amount field is in   * 
 *   a valid format.                                                * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     *
 *         cb - the callback struct                                 * 
 * Return: none                                                     * 
\********************************************************************/
void
amountCB(GtkWidget *w, gpointer data)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)cb;
  char input;
  
  if( cbs->text->ptr != NULL )
    {
    input = (cbs->text->ptr)[0];
  
    switch( input )
      {
      case '.':
        /* Make sure that there is only one '.' */
      {
      String str = XmTextGetString(mw);
      int i,count=0;
	
      for( i=0; str[i] != '\0'; i++ )
        if( str[i] == '.' )
          count++;
      if( count >= 1 )
        cbs->doit = False;
      }
      break;
      default:
        /* only accept the input if it is a number */
        cbs->doit = isNum(input);
      }
    }
  }

/********************************************************************\
 * noeditCB                                                         * 
 *   makes an Xbae matrix non-editable                              * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void
noeditCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XbaeMatrixEnterCellCallbackStruct *cbs = 
      (XbaeMatrixEnterCellCallbackStruct * )cb;
  
  cbs->doit = False;
  }

/********************************************************************\
 * destroyShellCB                                                   * 
 *   a callback to destroy a widget (cd, not w, because we want to  * 
 *   destroy a window, not a button!)                               * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd - the widget to destroy                               * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void 
destroyShellCB( Widget w, XtPointer cd, XtPointer cb )
  {
  Widget window = (Widget)cd;
  
  XtDestroyWidget(window);
  }
#endif


void
gnc_ui_init_cursors (void)
{
        int i;
        for (i = 0; i < GNC_CURSOR_LAST; i++)
                gnc_cursors[i].cursor  = gdk_cursor_new( gnc_cursors[i].type);
}

void
gnc_ui_shutdown_cursors(void)
{
        int i;
        for (i = 0; i < GNC_CURSOR_LAST; i++)
                gdk_cursor_destroy (gnc_cursors[i].cursor);
}

void
gnc_ui_set_cursor (GdkWindow *win, int type)
{
        if ( -1 < type  && type < GNC_CURSOR_LAST)
                if (win)
                        gdk_window_set_cursor (win, gnc_cursors[type].cursor);
}

/********************************************************************\
 * setBusyCursor                                                    * 
 *   sets the cursor to the busy watch                              * 
 *                                                                  * 
 * Args:   w - the widget over which to make cursor busy            * 
 * Return: none                                                     * 
\********************************************************************/
void 
setBusyCursor(GtkWidget *w)
{

  if (w)
          gnc_ui_set_cursor(w->window, GNC_CURSOR_BUSY);
    
}

/********************************************************************\
 * unsetBusyCursor                                                  * 
 *   sets the cursor to the default cursor                          * 
 *                                                                  * 
 * Args:   w - the widget over which to make cursor normal          * 
 * Return: none                                                     * 
\********************************************************************/
void 
unsetBusyCursor(GtkWidget *w)
{
  
  if (w)
    gnc_ui_set_cursor(w->window, GNC_CURSOR_NORMAL);

}


/********************************************************************\
 **************** VERIFYBOX STUFF ***********************************
\********************************************************************/

struct verify_callback_data {
  gboolean finished;
  int value;
};

static void
verify_cb_yes(GtkWidget *w, gpointer data) {
  struct verify_callback_data *result = (struct verify_callback_data *) data; 
  result->value = 1;
  result->finished = TRUE;
}

static void
verify_cb_no(GtkWidget *w, gpointer data) {
  struct verify_callback_data *result = (struct verify_callback_data *) data; 
  result->value = 0;
  result->finished = TRUE;
}

static void
verify_cb_cancel(GtkWidget *w, gpointer data) {
  struct verify_callback_data *result = (struct verify_callback_data *) data; 
  result->value = -1;
  result->finished = TRUE;
}

/********************************************************************
 queryBox

 display text, and wait for yes, no, or cancel, depending on the
 arguments.  Each of the *_allowed arguments indicates whether or not
 the dialog should contain a button of that type.  default_answer may
 be set to 1 for "yes" (or OK), 2 for "no", and -1 for "cancel".  If
 you allow both yes and OK buttons, and set 1 as the default answer,
 which button is the default is undefined, but the result is the same
 either way, and why would be doing that anyhow?

 This function returns 1 for yes (or OK), 0 for no, and -1 for cancel.
 
 NOTE: This function does not return until the dialog is closed.

*/

int
queryBox(const char *text,
         int default_answer,
         gncBoolean yes_allowed,
         gncBoolean ok_allowed,
         gncBoolean no_allowed,
         gncBoolean cancel_allowed) {

  GtkWidget *parent = gnc_get_ui_data();
  GtkWidget *verify_box = NULL;
  GtkWidget *verify_text = NULL;
  struct verify_callback_data result;

  gchar *button_names[5] = {NULL, NULL, NULL, NULL, NULL};
  int button_count = 0;
  GtkSignalFunc button_func[5] = {NULL, NULL, NULL, NULL, NULL};
  int default_button = 0;

  /* FIXME: These should be nana checks, but nana seems broken right now... */
#if 0
  I(yes_allowed || ok_allowed || no_allowed || cancel_allowed);
  I((default_answer == 1) && (yes_allowed || ok_allowed));
  I((default_answer == 0) && no_allowed);
  I((default_answer == -1) && cancel_allowed);
#endif
  
  if(yes_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_YES;
    button_func[button_count] = GTK_SIGNAL_FUNC(verify_cb_yes);
    if(1 == default_answer) default_button = button_count;
    button_count++;
  }  
  if(ok_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_OK;
    button_func[button_count] = GTK_SIGNAL_FUNC(verify_cb_yes);
    if(1 == default_answer) default_button = button_count;
    button_count++;
  }  
  if(no_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_NO;
    button_func[button_count] = GTK_SIGNAL_FUNC(verify_cb_no);
    if(0 == default_answer) default_button = button_count;
    button_count++;
  }  
  if(cancel_allowed) {
    button_names[button_count] = GNOME_STOCK_BUTTON_CANCEL;
    button_func[button_count] = GTK_SIGNAL_FUNC(verify_cb_cancel);
    if(-1 == default_answer) default_button = button_count;
    button_count++;
  }

  /* FIXME: I have no idea why gcc needs this coercion right now... */
  verify_box = gnome_dialog_newv(text, (const gchar **) button_names);
    
  // gnome_dialog_set_modal(GNOME_DIALOG(verify_box));

  gnome_dialog_set_default(GNOME_DIALOG(verify_box), default_button);
  gnome_dialog_set_close(GNOME_DIALOG(verify_box), TRUE);

  {
    int i;
    for(i = 0; i < button_count; i++) {
      gnome_dialog_button_connect(GNOME_DIALOG(verify_box), i,
                                  GTK_SIGNAL_FUNC(button_func[i]),
                                  (gpointer) &result);
    }
  }
  
  verify_text = gtk_label_new(text);
  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(verify_box)->vbox),
                     verify_text, FALSE, FALSE, 0);
  gtk_widget_show(verify_text);
  
  result.finished = FALSE;
  gtk_widget_show(verify_box);
  
  setBusyCursor(parent);
  
  while(!result.finished) {
    gtk_main_iteration();
  }
  
  unsetBusyCursor(parent);
  
  //gnome_dialog_close(GNOME_DIALOG(verify_box));
  
  return result.value;
}

/********************************************************************\
 * verifyBox                                                        *
 *   display a message, and asks the user to press "Ok" or "Cancel" *
 *                                                                  *
 * NOTE: This function does not return until the dialog is closed   *
 *                                                                  *
 * Args:   parent   - the parent widget                             *
 *         title    - the title of the window                       *
 *         text     - the message to display                        *
 * Return: none                                                     *
\********************************************************************/
gncBoolean
verifyBox( const char *text ) {
  return(queryBox(text, -1, FALSE, TRUE, FALSE, TRUE) == 1);
}


/********************************************************************\
*********************************************************************
\********************************************************************/

static void
error_cb_ok(GtkWidget *w, gpointer data) {
  *((gboolean *) data) = TRUE;
}
 
/********************************************************************\
 * errorBox                                                         * 
 *   displays an error dialog box                                   * 
 *                                                                  * 
 * Args:   w       - the parent widget                              * 
 *         message - the error message to display                   * 
 * Return: none                                                     * 
\********************************************************************/
void 
errorBox(const char *message) {
  GtkWidget *parent = gnc_get_ui_data();
  GtkWidget *error_box = NULL;
  GtkWidget *error_text = NULL;
  gboolean finished = FALSE;
  
  error_box = gnome_dialog_new("GnuCash: error",
                               GNOME_STOCK_BUTTON_OK,
                               NULL);
  gnome_dialog_button_connect(GNOME_DIALOG(error_box), 0,
                              GTK_SIGNAL_FUNC(error_cb_ok),
                              (gpointer) &finished);
  // gnome_dialog_set_modal(GNOME_DIALOG(error_box));
  gnome_dialog_set_default(GNOME_DIALOG(error_box), 0);
  gnome_dialog_set_close(GNOME_DIALOG(error_box), TRUE);
  
  error_text = gtk_label_new(message);
  gtk_box_pack_start(GTK_BOX(GNOME_DIALOG(error_box)->vbox),
                     error_text, FALSE, FALSE, 0);
  gtk_widget_show(error_text);
  
  gtk_widget_show(error_box);
  
  setBusyCursor(parent);
  
  while(!finished) {
    gtk_main_iteration();
  }
  
  unsetBusyCursor(parent);
}

/************************* END OF FILE ******************************\
\********************************************************************/
