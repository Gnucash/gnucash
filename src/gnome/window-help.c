/********************************************************************\
 * HelpWindow.c -- a help window for hypertext help.                *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
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

#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <gnome.h>
#include <gtk-xmhtml/gtk-xmhtml.h>

#include <guile/gh.h>

#include "config.h"

#if HAVE_XPM
#  include <X11/xpm.h>
#endif

#include "File.h"
#include "window-help.h"
#include "top-level.h"
#include "ui-callbacks.h"
#include "messages.h"
#include "Sheet.h"
#include "util.h"
//#include "xtutil.h"

extern GtkWidget app;
static short module = MOD_HTML; 

/********************************************************************\
 *     HTML History functions                                       * 
 * hack alert -- these are gui-independent, and should be moved
 * to somewhere were the gtk, Qt gui's can make use of them
\********************************************************************/
typedef struct _HTMLHistory {
  struct _HTMLHistory *next;
  struct _HTMLHistory *last;
  char   *htmlfile;
  char   *text;
} HTMLHistory;

/* insert an htmlfile into history.  Return TRUE if this
 * is the first element in the history.  If not last element
 * in history, all next pages are deleted */
static int
historyInsert( HTMLHistory **history, const char *htmlfile )
  {
  if( (*history) != NULL )
    {
    HTMLHistory *temp;
    
    /* delete all next pages: */
    temp = (*history)->next;
    while( temp ) 
      {
      (*history)->next = temp->next;
      if (temp->htmlfile) free(temp->htmlfile);
      if (temp->text) free(temp->text);
      _free(temp);

      temp = (*history)->next;
      }
    
    /* Add new node to history: */
    temp = (HTMLHistory *)_malloc(sizeof(HTMLHistory));
    if (htmlfile) {
       temp->htmlfile = strdup (htmlfile);
    } else {
       temp->htmlfile = NULL;
    }
    temp->text = NULL;
    temp->next = NULL;
    temp->last = (*history);
    (*history)->next = temp;
    (*history) = temp;
    
    return FALSE;
    }
  else
    {
    /* This must be the first node in the history... */
    (*history) = (HTMLHistory *)_malloc(sizeof(HTMLHistory));
    if (htmlfile) {
       (*history)->htmlfile = strdup (htmlfile);
    } else {
       (*history)->htmlfile = NULL;
    }
    (*history)->text = NULL;
    (*history)->last = NULL;
    (*history)->next = NULL;
    
    /* ...so return TRUE */
    return TRUE;
    }
  }

/* Move forward in history, and return current htmlfile */
static char *
historyFwd( HTMLHistory **history )
  {
  if( (*history) != NULL ) {
    if( (*history)->next != NULL ) { 
      (*history) = (*history)->next; 
       }
    return (*history)->htmlfile;
    }
  else
    return NULL;
  }

/* Move back in history, and return current htmlfile */
static char *
historyBack( HTMLHistory **history )
  {
  if( (*history) != NULL ) {
    if( (*history)->last != NULL ) {
      (*history) = (*history)->last;
      }
    return (*history)->htmlfile;
    }
  else
    return NULL;
  }

#ifdef NOT_USED
/* Return current htmlfile */
static char *
historyCurrent( HTMLHistory **history )
  {
  if( (*history) != NULL )
    return (*history)->htmlfile;
  else
    return NULL;
  }
#endif

/* Remove all entries from history: */
static void
historyClear( HTMLHistory **history )
  {
  /* move to beginning of history: */
  while( (*history)->last != NULL )
    (*history) = (*history)->last;
  
  /* delete all next pages: */
  while( (*history)->next != NULL )
    {
    HTMLHistory *temp = (*history)->next;
    
    (*history)->next = temp->next;
    if(temp->htmlfile) free(temp->htmlfile);
    if(temp->text) free(temp->text);
    _free(temp);
    }
  
  /* delete current page: */
  if ((*history)->htmlfile) free((*history)->htmlfile);
  if ((*history)->text) free((*history)->text);
  _free(*history);
  (*history) = NULL;
  }


/********************************************************************\
 *     HTML Window stuff...                                         * 
\********************************************************************/

/** GLOBALS *********************************************************/

struct _HTMLWindow {
   GtkWidget *htmlwidget;
   GtkWidget *data_target_frame;
   GtkWidget *menu_target_frame;
   short  installed_data_frame_form_cb;
   short  installed_menu_frame_form_cb;
   HTMLHistory *history;
};

typedef struct _HTMLWindow HTMLWindow;

HTMLWindow *helpwindow = NULL;
HTMLWindow *reportwindow = NULL;

/** PROTOTYPES ******************************************************/
static void   htmlWindow( GtkWidget *parent, HTMLWindow **hwinp,
                          const char * const title, 
                          const char * const htmlfile, 
                          const char * const text);
static void   closeHtmlWin( GtkWidget *widget, gpointer data );
static void   frameCB( GtkWidget *widget, gpointer data );
static void   formCB( GtkWidget *widget, gpointer data );
static void   htmlBackCB( GtkWidget *widget,  gpointer data );
static void   htmlFwdCB( GtkWidget *widget, gpointer data );
static void   htmlAnchorCB( GtkWidget *widget, XmHTMLAnchorCallbackStruct *acbs, gpointer data );

#if HAVE_LIBXMHTML
static char * xaccJumpToLabel (GtkWidget *widget, const char * jumpfile, char * text);
static XmImageInfo * htmlReadImageProc (GtkWidget *widget, String file);
#endif /* WANT_XMHTML */

/********************************************************************\
 * reportWindow                                                     * 
 *   opens up a report window, and displays html                    * 
 *                                                                  * 
 * Args:   parent   - the parent widget                             * 
 *         title    - the title of the window                       * 
 *         htmlfile - the file name of the help file to display     * 
 * Return: none                                                     * 
\********************************************************************/
void
reportWindow( GtkWidget *parent, const char *title, const char *file)
{
  if (!reportwindow) {
    reportwindow = (HTMLWindow *) g_malloc (sizeof (HTMLWindow));
    reportwindow->htmlwidget = 0;
    reportwindow->data_target_frame = 0;
    reportwindow->menu_target_frame = 0;
    reportwindow->installed_data_frame_form_cb = 0;
    reportwindow->installed_menu_frame_form_cb = 0;
    reportwindow->history = NULL;
  } 
 
  htmlWindow (parent, &reportwindow, title, file, NULL);
}
  
/********************************************************************\
 * helpWindow                                                       * 
 *   opens up a help window, and displays html                      * 
 *                                                                  * 
 * Args:   parent   - the parent widget                             * 
 *         title    - the title of the window                       * 
 *         htmlfile - the file name of the help file to display     * 
 * Return: none                                                     * 
\********************************************************************/
void
helpWindow( GtkWidget *parent, const char *title, const char *htmlfile )
{
  if (!helpwindow) {
    helpwindow = (HTMLWindow *) malloc (sizeof (HTMLWindow));
    helpwindow->htmlwidget = 0;
    helpwindow->data_target_frame = 0;
    helpwindow->menu_target_frame = 0;
    helpwindow->installed_data_frame_form_cb = 0;
    helpwindow->installed_menu_frame_form_cb = 0;
    helpwindow->history = NULL;
  } 
 
  htmlWindow (parent, &helpwindow, title, htmlfile, NULL);
}
  
/********************************************************************\
 * helpWindow                                                       * 
 *   opens up a help window, and displays html                      * 
 *                                                                  * 
 * Args:   parent   - the parent widget                             * 
 *         title    - the title of the window                       * 
 *         htmlfile - the file name of the help file to display     * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlWindow( GtkWidget *parent, 
            HTMLWindow **hwinp,
            const char * const title, 
            const char * const htmlfile,
            const char * const htmltext)
{
  GtkWidget dialog,
         pane,
         controlform,
         buttonform,
         widget;
  int position=0;
  HTMLWindow *hw = *hwinp;
  char * text=0x0;
  
 // gnc_set_busy_cursor( parent );
  
  historyInsert (&(hw->history), htmlfile); 
  if (htmltext) text = strdup (htmltext);
  hw->history->text = text;

  /* if the help window isn't open, then open it... otherwise, load
   * new help page into current help window */
   
  /************ CREATE HTML WINDOW HERE *****************/
  /******************************************************/
  {
    GtkWidget *window;
    GtkWidget *html;
    GtkWidget *button;
    GtkWidget *hbuttonbox1;
    GtkWidget *vbox1;

    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_object_set_data (GTK_OBJECT (window), "window", window);
    gtk_window_set_title (GTK_WINDOW (window), "Gnucash Help System");
    gtk_window_set_policy (GTK_WINDOW (window), TRUE, TRUE, FALSE);

    html = gtk_xmhtml_new();
  
    hw->htmlwidget = html;
  
    text = xaccJumpToLabel( hw->htmlwidget, htmlfile, text);
    hw->history->text = text;

    vbox1 = gtk_vbox_new (FALSE, 0);
    gtk_object_set_data (GTK_OBJECT (window), "vbox1", vbox1);
    gtk_widget_show (vbox1);
    gtk_container_add (GTK_CONTAINER (window), vbox1);
    gtk_container_border_width (GTK_CONTAINER (vbox1), 6);

    /* Pack our html widget into the window */
    //gtk_container_add(GTK_CONTAINER(window), html);
    gtk_box_pack_start(GTK_BOX(vbox1), html, TRUE, TRUE, 5);
  
    hbuttonbox1 = gtk_hbutton_box_new ();
    gtk_object_set_data (GTK_OBJECT (window), "hbuttonbox1", hbuttonbox1);
    gtk_widget_show (hbuttonbox1);
    gtk_box_pack_start (GTK_BOX (vbox1), hbuttonbox1, FALSE, FALSE, 0);
    gtk_container_border_width (GTK_CONTAINER (hbuttonbox1), 5);
    gtk_button_box_set_layout (GTK_BUTTON_BOX (hbuttonbox1), GTK_BUTTONBOX_SPREAD);
  
    button = gtk_button_new_with_label ("Back");
    gtk_object_set_data (GTK_OBJECT (window), "back", button);
    gtk_widget_show (button);
    gtk_container_add (GTK_CONTAINER (hbuttonbox1), button);

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
                       GTK_SIGNAL_FUNC(htmlBackCB),
                       hw);                       
  
    button = gtk_button_new_with_label ("Contents");
    gtk_object_set_data (GTK_OBJECT (window), "contents", button);
    gtk_widget_show (button);
    gtk_container_add (GTK_CONTAINER (hbuttonbox1), button);
  
    button = gtk_button_new_with_label ("Forward");
    gtk_object_set_data (GTK_OBJECT (window), "forward", button);
    gtk_widget_show (button);
    gtk_container_add (GTK_CONTAINER (hbuttonbox1), button);

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
                       GTK_SIGNAL_FUNC(htmlFwdCB),
                       hw);                       

    hw->data_target_frame = hw->htmlwidget;
  
    gtk_widget_show(html);  
    gtk_widget_show(window);

    gtk_widget_set_usize(GTK_WIDGET(window), 675, 400);
    
    gtk_signal_connect(GTK_OBJECT(hw->htmlwidget), "activate",
                       GTK_SIGNAL_FUNC(htmlAnchorCB),
                       hw);
                       
  }

}

/********************************************************************\
 *     callback functions...                                        * 
\********************************************************************/

/* sample usage for data target frame: 
 * <frame name="report-target" src="xxx.html">
 * design hack alert -- to be general and flexible and less of a hack,
 * we should be keeping track of the named frame elements, and pushing
 * them onto a linked list.  Then, the URL that gets clicked on 
 * should contain the name of the target frame that we want to draw
 * draw to ... and we could look up that name in our list.
 */
#define DATA_TARGET "report-target"
#define MENU_TARGET "menu-target"

#if HAVE_LIBXMHTML
static void
frameCB( GtkWidget *widget, gpointer data )
{
   #if 0
  XmHTMLFrameCallbackStruct *fcs = (XmHTMLFrameCallbackStruct *) cb;
  HTMLWindow *hw = (HTMLWindow *) cd;
  
  if (XmCR_HTML_FRAMECREATE == fcs->reason) { 
     fcs->doit = True;
  } else
  if (XmCR_HTML_FRAME == fcs->reason) { 
     XtVaSetValues (fcs->html, XmNimageProc, htmlReadImageProc,  NULL);
     XtAddCallback (fcs->html, XmNactivateCallback, htmlAnchorCB, cd);
     xaccJumpToLabel (fcs->html, fcs->src, NULL); 

     /* keep track of the frame where we will be placing the reports. */
     if (!strcmp (fcs->name, DATA_TARGET)) {
        hw->data_target_frame = fcs->html;
        if (0 == hw->installed_data_frame_form_cb) {
           hw->installed_data_frame_form_cb = 1;
           XtAddCallback( hw->data_target_frame, 
               XmNformCallback, formCB, (XtPointer) hw);
        }
     } else 
     if (!strcmp (fcs->name, MENU_TARGET)) {
        hw->menu_target_frame = fcs->html;
        if (0 == hw->installed_menu_frame_form_cb) {
           hw->installed_menu_frame_form_cb = 1;
           XtAddCallback( hw->menu_target_frame, 
               XmNformCallback, formCB, (XtPointer) hw);
        }
     }
  }
  #endif
}

/********************************************************************\
\********************************************************************/
static void
formCB( GtkWidget *widget, gpointer data )
{

#if 0
  int i,n;
  XmHTMLFormCallbackStruct *fcs = (XmHTMLFormCallbackStruct *) cb;
  // HTMLWindow *hw = (HTMLWindow *) cd;
  
   PINFO ("formCB(): action=%s\n", fcs->action);
   PINFO ("formCB(): encoding=%s\n", fcs->enctype);
   PINFO ("formCB(): num keys=%d \n", fcs->ncomponents);

   DEBUGCMD ({
      n = fcs->ncomponents;
      for (i=0; i<n; i++) {
         DEBUG ("formCB(): %s=%s\n", 
             fcs->components[i].name, fcs->components[i].value);
      }
   });
 #endif
}
#endif

/********************************************************************\
 * htmlBackCB - called when user clicks "Back" button... shows last * 
 *   help page in history                                           * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlBackCB( GtkWidget *widget, gpointer data )
  {
  HTMLWindow *hw = (HTMLWindow *) data;
  char *file = historyBack(&(hw->history));
  char *text = hw->history->text;
 
#if HAVE_LIBXMHTML
  text = xaccJumpToLabel( hw->htmlwidget, file, text);
  hw->history->text = text;
#endif
  }

/********************************************************************\
 * htmlFwdCB - called when user clicks "Forward" button... shows    * 
 *   next help page in the history                                  * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlFwdCB( GtkWidget *widget, gpointer data )
  {
  HTMLWindow *hw = (HTMLWindow *) data;
  char *file = historyFwd(&(hw->history));
  char *text = hw->history->text;
#if HAVE_LIBXMHTML
  text = xaccJumpToLabel( hw->htmlwidget, file, text);
  hw->history->text = text;
#endif
  }

/********************************************************************\
 * closeHtmlWin - called when the help window is closed             * 
 *                                                                  * 
 * Args:   mw -                                                     * 
 *         cd -                                                     * 
 *         cb -                                                     * 
 * Return: none                                                     * 
\********************************************************************/
static void
closeHtmlWin( GtkWidget *widget, gpointer data )
  {
  HTMLWindow **hw = (HTMLWindow **) data;
  /* Delete the history: */
  historyClear (&((*hw)->history));
  (*hw)->history=NULL;
  (*hw)->htmlwidget=0;
  free (*hw);
  (*hw) = NULL;
  }

/********************************************************************\
 * htmlAnchorCB - called when user clicks on html anchor tag        * 
 *                                                                  * 
 * Args:   mw - the html widget that called us                      * 
 *         cd -                                                     * 
 *         cb - the anchor call-back struct                         * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlAnchorCB( GtkWidget *widget, XmHTMLAnchorCallbackStruct *acbs, gpointer data)
  {
  HTMLWindow *hw = (HTMLWindow *) data;

#if HAVE_LIBXMHTML
//   XmHTMLAnchorCallbackStruct *acbs = (XmHTMLAnchorCallbackStruct *) data;

   if(acbs->reason != XmCR_ACTIVATE) return;

   switch(acbs->url_type) {

      /* a named anchor on a page that is already displayed */
      case ANCHOR_JUMP: {
//          XmHTMLAnchorScrollToName(widget, acbs->href);
      }
      break;

      /* a local file with a possible jump to label */
      case ANCHOR_FILE_LOCAL: {
         historyInsert(&(hw->history), acbs->href);
/* hack alert -- the target widget thing is a hack */
         hw->history->text = xaccJumpToLabel (hw->data_target_frame, acbs->href, NULL);
      }
      break;

      /*  other types are unsupported, but it would be fun if they were ... */
      case ANCHOR_FTP:
         PERR(" this help window doesn't support ftp: %s\n", acbs->href);
         break;
      case ANCHOR_HTTP:
         PERR (" this help window doesn't support http: %s\n", acbs->href);
         break;
      case ANCHOR_MAILTO:
         PERR(" this help window doesn't support email: %s\n", acbs->href);
         break;
      case ANCHOR_UNKNOWN:
      default:
         PERR(" don't know this type of url: %s\n", acbs->href);
         break;
   }

#endif
  }

/********************************************************************\
 *     utility functions...                                         * 
\********************************************************************/

#if HAVE_LIBXMHTML
static char * 
xaccJumpToLabel (GtkWidget *widget, const char * jumpfile, char * text)
{
   char *label=0x0, *file=0x0;

   if (jumpfile) {
      file = strdup (jumpfile);
   
      /*  see if this anchor contains a jump */
      label = strpbrk (file, "#?");
      if (label) {
         file [label - file] = 0x0;  /* truncate # from name */
      }
   
      /* see if the anchor is an "active gnucash page" */
      if (strstr (file, ".phtml")) {
        text = gncReport (file);
      }

      /* if text to display wasn't specified, use the truncated name to read */
      if (!text) text = gncReadFile (file);
   }
   if (!text) return NULL;


   gtk_xmhtml_source(GTK_XMHTML(widget), text);
#if 0
   if (label) {
      XmHTMLAnchorScrollToName(mw, label);
   } else {
      XmHTMLTextScrollToLine(mw, 0);
}
#endif
   if (file) free (file);

   return text;
}

/********************************************************************\
 *     HTML functions...                                            * 
\********************************************************************/
/********************************************************************\
 * htmlReadImageProc                                                * 
 *                                                                  * 
 * Args:   file - the name of the html file to read                 * 
 * Return: none                                                     * 
 * Global: helpPath - the path to the help files                    * 
\********************************************************************/
static XmImageInfo *
htmlReadImageProc (GtkWidget *widget, String file) 
  {
  char  *filename;
  XmImageInfo *retval;

  /* construct absolute path -- twiddle the relative path we recieved */  
  filename = gncFindFile (file);
  
  /* use the default proc for the hard work */
//  retval = XmHTMLImageDefaultProc(widget, filename, NULL, 0);

  free(filename);
  return retval;
}
#endif /* HAVE_XMHTML */

/* ----------------------- END OF FILE ---------------------  */
