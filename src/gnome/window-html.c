/********************************************************************\
 * window-html.c -- an html window for gnucash                      *
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

#include <string.h>

#include "top-level.h"

#include "window-html.h"
#include "File.h"
#include "util.h"

static short module = MOD_HTML; 


/********************************************************************\
 *     HTML History functions                                       * 
 * hack alert -- these are gui-independent, and should be moved     *
 *               to a central location                              *
\********************************************************************/
typedef struct _HTMLHistoryNode HTMLHistoryNode;
struct _HTMLHistoryNode
{
  HTMLHistoryNode *next;
  HTMLHistoryNode *last;

  HTMLHistoryData history_data;
};

typedef struct _HTMLHistory HTMLHistory;
struct _HTMLHistory
{
  HTMLHistoryNode *current_node;
  HTMLHistoryDestroyDataFunc destroy;
};



static HTMLHistoryNode *
history_node_new(const HTMLHistoryData history_data)
{
  HTMLHistoryNode *new;

  new = malloc(sizeof(HTMLHistoryNode));
  assert(new != NULL);

  new->history_data = history_data;
  new->last = NULL;
  new->next = NULL;

  return new;
}

/* Insert a history element into history. Return TRUE if this
 * is the first element in the history. If not last element
 * in history, all next pages are deleted */
static gncBoolean
historyInsert(HTMLHistory *history, const HTMLHistoryData history_data)
{
  HTMLHistoryNode *new;
  HTMLHistoryNode *temp;

  assert(history != NULL);

  new = history_node_new(history_data);

  if (history->current_node == NULL)
  {
    history->current_node = new;
    return GNC_T;
  }

  /* delete all next pages: */
  temp = history->current_node->next;
  while(temp != NULL)
  {
    history->current_node->next = temp->next;
    if (temp->history_data != NULL)
      (history->destroy)(temp->history_data);
    free(temp);

    temp = history->current_node->next;
  }

  new->last = history->current_node;
  history->current_node->next = new;
  history->current_node = new;

  return GNC_F;
}

/* Move forward in history, and return current history data */
static HTMLHistoryData
historyFwd(HTMLHistory *history)
{
  if (history == NULL)
    return NULL;
  if (history->current_node == NULL)
    return NULL;

  if (history->current_node->next != NULL)
    history->current_node = history->current_node->next;

  return history->current_node->history_data;
}

/* Move back in history, and return current history data */
static HTMLHistoryData
historyBack(HTMLHistory *history)
{
  if (history == NULL)
    return NULL;
  if (history->current_node == NULL)
    return NULL;

  if (history->current_node->last != NULL)
    history->current_node = history->current_node->last;

  return history->current_node->history_data;
}

/* Remove all entries from history: */
static void
historyClear(HTMLHistory *history)
{
  if (history == NULL)
    return;
  if (history->current_node == NULL)
    return;

  /* move to beginning of history: */
  while(history->current_node->last != NULL )
    history->current_node = history->current_node->last;

  /* delete all next pages: */
  while(history->current_node->next != NULL )
  {
    HTMLHistoryNode *temp = history->current_node->next;

    history->current_node->next = temp->next;
    if (temp->history_data != NULL)
      (history->destroy)(temp->history_data);
    free(temp);
  }
  
  /* delete current page: */
  if (history->current_node->history_data != NULL)
    (history->destroy)(history->current_node->history_data);
  free(history->current_node);
  history->current_node = NULL;
}

static HTMLHistoryData
historyData(HTMLHistory *history)
{
  if (history == NULL)
    return NULL;
  if (history->current_node == NULL)
    return NULL;

  return history->current_node->history_data;
}

static HTMLHistory *
historyNew(HTMLHistoryDestroyDataFunc destroy)
{
  HTMLHistory *history;

  history = malloc(sizeof(HTMLHistory));
  assert(history != NULL);

  history->current_node = NULL;
  history->destroy = destroy;

  return history;
}

static void
historyDestroy(HTMLHistory *history)
{
  if (history == NULL)
    return;

  historyClear(history);
  free(history);
}


/********************************************************************\
 *     HTML Window stuff...                                         * 
\********************************************************************/

struct _HTMLWindow
{
  GtkWidget *window;
  GtkWidget *htmlwidget;

  GtkWidget *forward;
  GtkWidget *back;

  HTMLHistory *history;

  HTMLAnchorCB anchor_cb;
  HTMLJumpCB   jump_cb;
};


/** PROTOTYPES ******************************************************/
static void htmlBackCB(GtkWidget *widget,  gpointer data);
static void htmlFwdCB(GtkWidget *widget, gpointer data);
static void htmlAnchorCB(GtkWidget *widget,
                         XmHTMLAnchorCallbackStruct *acbs,
                         gpointer data);

static gboolean htmlKeyCB(GtkWidget *widget, GdkEventKey *event,
                          gpointer user_data);
static void closeHtmlWinCB(GtkWidget *widget, gpointer data);
static void destroyHtmlWinCB(GtkWidget *widget, gpointer data);

static XmImageInfo * htmlReadImageProc(GtkWidget *widget, String file,
                                       gpointer data);

static void htmlSetButtonStates(HTMLWindow *hw);


/********************************************************************\
 * gnc_html_window_history_data                                     *
 *   return the current history data for the window                 *
 *                                                                  *
 * Args: none                                                       *
 * Return: history data for the window                              * 
\********************************************************************/
HTMLHistoryData
gnc_html_window_history_data(HTMLWindow *hw)
{
  if (hw == NULL)
    return NULL;

  return historyData(hw->history);
}


/********************************************************************\
 * gnc_html_window_new                                              *
 *   g_malloc and initialize HTMLWindow structure                   *
 *                                                                  *
 * Args: none                                                       *
 * Return: g_malloc'd initialized HTMLWindow structure              * 
\********************************************************************/
HTMLWindow *
gnc_html_window_new(HTMLHistoryDestroyDataFunc destroy,
                    HTMLAnchorCB anchor_cb, HTMLJumpCB jump_cb)
{
  HTMLWindow *hw;

  hw = g_new0(HTMLWindow, 1);

  hw->history = historyNew(destroy);

  hw->anchor_cb = anchor_cb;
  hw->jump_cb = jump_cb;

  return hw;
}


/********************************************************************\
 * gnc_html_window_destroy                                          *
 *   destroy an HTMLWindow structure                                *
 *                                                                  *
 * Args: hw - the htmlwindow structure to destroy                   *
 * Return: none                                                     * 
\********************************************************************/
void
gnc_html_window_destroy(HTMLWindow *hw)
{
  if (hw == NULL)
    return;

  if (hw->window != NULL)
    gtk_widget_destroy(hw->window);
  else
  {
    historyDestroy(hw->history);
    g_free(hw);
  }
}


static GtkWidget *
create_html_toolbar(HTMLWindow *hw, GnomeUIInfo *user_buttons,
                    gint num_buttons)
{
  GnomeUIInfo *toolbar_info;
  GtkWidget *toolbar;
  gint num_start, num_end;
  gint i;

  GnomeUIInfo toolbar_start[] = 
  {
    { GNOME_APP_UI_ITEM,
      "Back", 
      "Move back one step in the history.",
      htmlBackCB, hw,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_BACK,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      "Forward", 
      "Move forward one step in the history.",
      htmlFwdCB, hw,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_FORWARD,
      0, 0, NULL
    }
  };

  GnomeUIInfo toolbar_end[] = 
  {
    { GNOME_APP_UI_ITEM,
      "Close", 
      "Close this HTML window.",
      closeHtmlWinCB, hw,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  num_start = sizeof(toolbar_start) / sizeof(GnomeUIInfo);
  num_end = sizeof(toolbar_end) / sizeof(GnomeUIInfo);

  toolbar_info = g_new0(GnomeUIInfo, num_start + num_buttons + num_end);

  for (i = 0; i < num_start; i++)
    toolbar_info[i] = toolbar_start[i];

  for (i = 0; i < num_buttons; i++)
    toolbar_info[i + num_start] = user_buttons[i];

  for (i = 0; i < num_end; i++)
    toolbar_info[i + num_start + num_buttons] = toolbar_end[i];

  toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);

  gnome_app_fill_toolbar(GTK_TOOLBAR(toolbar), toolbar_info, NULL);

  hw->back = toolbar_info[0].widget;
  hw->forward = toolbar_info[1].widget;

  g_free(toolbar_info);

  return toolbar;
}


/********************************************************************\
 * htmlWindow                                                       *
 *   opens up an html window, and displays html                     *
 *                                                                  *
 * Args:   parent   - the parent widget                             *
 *         hwp      - the htmlwindow structure pointer              *
 *         title    - the title of the window                       *
 *         history_data - the history data                          *
 *         new_buttons  - array of buttons to add to icon bar       *
 *         num_buttons  - number of buttons in list                 *
 * Return: none                                                     * 
\********************************************************************/
void
htmlWindow(GtkWidget  * parent, 
           HTMLWindow ** hwp,
           const char * const title,
           HTMLHistoryData history_data,
           GnomeUIInfo *user_buttons,
           gint num_buttons)
{
  HTMLWindow *hw = *hwp;

  historyInsert(hw->history, history_data); 

  /* If the help window is already created, just load the new
   * page into the existing widget and raise the window. */
  if (hw->htmlwidget != NULL)
  {
    gnc_html_load(hw);

    if (hw->window->window != NULL)
      gdk_window_raise(hw->window->window);

    htmlSetButtonStates(hw);

    return;
  }

  /************ CREATE HTML WINDOW HERE *****************/
  {
    GtkWidget *window;
    GtkWidget *html;
    GtkWidget *dock;
    GtkWidget *dock_item;
    GtkWidget *toolbar;

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    hw->window = window;
    gtk_window_set_title(GTK_WINDOW (window), title);
    gtk_window_set_policy(GTK_WINDOW (window), TRUE, TRUE, FALSE);
    gtk_window_set_default_size(GTK_WINDOW(window), 675, 400);

    dock = gnome_dock_new();
    gtk_container_add(GTK_CONTAINER(window), dock);

    dock_item = gnome_dock_item_new("toolbar", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    toolbar = create_html_toolbar(hw, user_buttons, num_buttons);
    gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), toolbar);

    gnome_dock_add_item (GNOME_DOCK(dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 0, 0, 0, TRUE);

    html = gtk_xmhtml_new();
    hw->htmlwidget = html;
    gnome_dock_set_client_area(GNOME_DOCK(dock), html);
    gtk_xmhtml_set_image_procs(GTK_XMHTML(html), htmlReadImageProc,
                               NULL, NULL, NULL);

    gnc_html_load(hw);

    gtk_signal_connect(GTK_OBJECT(hw->htmlwidget), "activate",
                       GTK_SIGNAL_FUNC(htmlAnchorCB), hw);

    gtk_signal_connect(GTK_OBJECT(window), "destroy",
                       GTK_SIGNAL_FUNC(destroyHtmlWinCB), hwp);

    gtk_signal_connect(GTK_OBJECT(window), "key_press_event",
                       GTK_SIGNAL_FUNC(htmlKeyCB), hw);

    gtk_widget_show_all(window);
  }

  htmlSetButtonStates(hw);
}


/********************************************************************\
 * htmlSetButtonStates - set the sensitivity of the back/forward    * 
 *                       buttons based on the history state         *
 *                                                                  * 
 * Args:   hw - the html window structure                           * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlSetButtonStates(HTMLWindow *hw)
{
  gboolean more_to_go;

  more_to_go =
    (hw->history != NULL) &&
    (hw->history->current_node != NULL) &&
    (hw->history->current_node->last != NULL);

  gtk_widget_set_sensitive(hw->back, more_to_go);

  more_to_go =
    (hw->history != NULL) &&
    (hw->history->current_node != NULL) &&
    (hw->history->current_node->next != NULL);

  gtk_widget_set_sensitive(hw->forward, more_to_go);
}


/********************************************************************\
 * htmlKeyCB - called when user presses a key                       * 
 *                                                                  * 
 * Args:   widget - the back button                                 * 
 *         data   - html window structure                           * 
 * Return: none                                                     * 
\********************************************************************/
static gboolean
htmlKeyCB( GtkWidget *widget, GdkEventKey *event, gpointer data )
{
  HTMLWindow *hw = (HTMLWindow *) data;
  GtkXmHTML *html = GTK_XMHTML(hw->htmlwidget);
  GtkAdjustment *adj;
  gfloat value;

  if (html->vsba == NULL)
    return FALSE;

  adj = GTK_ADJUSTMENT(html->vsba);

  value = adj->value;

  switch (event->keyval)
  {
    case GDK_KP_Up:
    case GDK_Up:
      value -= adj->step_increment;
      break;
    case GDK_KP_Down:
    case GDK_Down:
      value += adj->step_increment;
      break;
    case GDK_KP_Page_Up:
    case GDK_Page_Up:
      value -= adj->page_increment;
      break;
    case GDK_KP_Page_Down:
    case GDK_Page_Down:
    case GDK_space:
      value += adj->page_increment;
      break;
    case GDK_KP_Home:
    case GDK_Home:
      value = adj->lower;
      break;
    case GDK_KP_End:
    case GDK_End:
      value = adj->upper;
      break;
    case GDK_Escape:
      gtk_widget_destroy(hw->window);
      return TRUE;
    default:
      return FALSE;
  }

  value = CLAMP(value, adj->lower, adj->upper - adj->page_size);

  gtk_adjustment_set_value(adj, value);

  return TRUE;
}


/********************************************************************\
 * htmlBackCB - called when user clicks "Back" button... shows last * 
 *   help page in history                                           * 
 *                                                                  * 
 * Args:   widget - the back button                                 * 
 *         data   - html window structure                           * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlBackCB(GtkWidget *widget, gpointer data)
{
  HTMLWindow *hw = (HTMLWindow *) data;
  HTMLHistoryData history_data;

  history_data = historyBack(hw->history);
 
  gnc_html_load(hw);

  htmlSetButtonStates(hw);
}


/********************************************************************\
 * htmlFwdCB - called when user clicks "Forward" button... shows    * 
 *   next help page in the history                                  * 
 *                                                                  * 
 * Args:   widget - the forward button                              * 
 *         data   - html window structure                           * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlFwdCB(GtkWidget *widget, gpointer data)
{
  HTMLWindow *hw = (HTMLWindow *) data;
  HTMLHistoryData history_data;

  history_data = historyFwd(hw->history);

  gnc_html_load(hw);

  htmlSetButtonStates(hw);
}


/********************************************************************\
 * closeHtmlWinCB - callback for closing html window                * 
 *                                                                  * 
 * Args:   widget - the widget that called us                       * 
 *         data   - html window structure pointer                   * 
 * Return: none                                                     * 
\********************************************************************/
static void
closeHtmlWinCB(GtkWidget *widget, gpointer data)
{
  HTMLWindow *hw = (HTMLWindow *) data;

  gtk_widget_destroy(hw->window);
}


/********************************************************************\
 * destroyHtmlWinCB - called when the help window is destroyed      * 
 *                                                                  * 
 * Args:   widget - the widget that called us                       * 
 *         data   - html window structure pointer                   * 
 * Return: none                                                     * 
\********************************************************************/
static void
destroyHtmlWinCB(GtkWidget *widget, gpointer data)
{
  HTMLWindow **hwp = (HTMLWindow **) data;
  HTMLWindow *hw = *hwp;

  /* Delete the history: */
  historyDestroy(hw->history);
  hw->history = NULL;

  hw->htmlwidget = NULL;

  g_free(hw);
  *hwp = NULL;

  DEBUG("HTML window destroyed.\n");
}


/********************************************************************\
 * htmlAnchorCB - called when user clicks on html anchor tag        * 
 *                                                                  * 
 * Args:   widget - the html widget that called us                  * 
 *         acbs   - callback structure                              * 
 *         data   - html window structure                           * 
 * Return: none                                                     * 
\********************************************************************/
static void
htmlAnchorCB(GtkWidget *widget, XmHTMLAnchorCallbackStruct *acbs,
             gpointer data)
{
  HTMLWindow *hw = (HTMLWindow *) data;
  HTMLHistoryData history_data;

  if (acbs->reason != XmCR_ACTIVATE) return;

  switch(acbs->url_type)
  {
    /* a named anchor on a page that is already displayed */
    case ANCHOR_JUMP:
      XmHTMLAnchorScrollToName(widget, acbs->href);
      break;

    default:
      if (hw->anchor_cb == NULL)
        return;

      history_data = (hw->anchor_cb)(acbs, historyData(hw->history));
      if (history_data == NULL)
        return;

      historyInsert(hw->history, history_data);
      gnc_html_load(hw);
      break;
  }

  htmlSetButtonStates(hw);
}


/********************************************************************\
 * gnc_html_load - load the current location into the html window   *
 *                                                                  * 
 * Args:   hw           - the html window structure                 *
 * Return: none                                                     * 
\********************************************************************/
void
gnc_html_load(HTMLWindow *hw)
{
  HTMLHistoryData history_data;
  char *label = NULL;
  char *text = NULL;

  if (hw == NULL)
    return;
  if (hw->jump_cb == NULL)
    return;

  history_data = historyData(hw->history);

  (hw->jump_cb)(history_data, &text, &label);

  if (text == NULL)
    return;

  gtk_xmhtml_source(GTK_XMHTML(hw->htmlwidget), text);

  if (label != NULL)
    XmHTMLAnchorScrollToName(hw->htmlwidget, label);
  else
    XmHTMLTextScrollToLine(hw->htmlwidget, 0);
}


/********************************************************************\
 * htmlReadImageProc - callback function for the html widget        *
 *                     used to find an image file                   *
 *                                                                  * 
 * Args:   widget - the html widget                                 *
 *         file   - the name of the image file to read              *
 *         data   - some data, not used                             *
 * Return: none                                                     * 
\********************************************************************/
static XmImageInfo *
htmlReadImageProc (GtkWidget *widget, String file, gpointer data)
{
  char *filename;
  XmImageInfo *retval = NULL;

  /* construct absolute path -- twiddle the relative path we received */  
  filename = gncFindFile(file);
  
  /* use the default proc for the hard work */
  retval = XmHTMLImageDefaultProc(widget, filename, NULL, 0);

  free(filename);
  return retval;
}

/* ----------------------- END OF FILE ---------------------  */
