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
#include "query-user.h"
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

/********************************************************************\
 * verifyBox                                                        *
 *   display a message, and asks the user to press "Yes" or "No"    *
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
  return(queryBox(text, 3, TRUE, FALSE, TRUE, FALSE) == 1);
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
