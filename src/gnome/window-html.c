/********************************************************************\
 * window-html.c -- an html window for gnucash                      *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
 * Copyright (C) 2000 Linas Vepstas                                 *
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
#include "dialog-utils.h"
#include "global-options.h"
#include "messages.h"
#include "File.h"
#include "util.h"

static short module = MOD_HTML; 


/********************************************************************\
 *     HTML History functions                                       * 
 * hack alert -- these are gui-independent, and should be moved     *
 *               to a central location                              *
\********************************************************************/

struct _HTMLData
{
  char *title;

  GnomeUIInfo *user_buttons;
  int num_user_buttons;

  HTMLUserData user_data;
  HTMLDestroyUserDataFunc destroy;
};

typedef struct _HTMLHistoryNode HTMLHistoryNode;
struct _HTMLHistoryNode
{
  HTMLHistoryNode *next;
  HTMLHistoryNode *last;

  HTMLData *data;
};

typedef struct _HTMLHistory HTMLHistory;
struct _HTMLHistory
{
  HTMLHistoryNode *current_node;
};


/********************************************************************\
 * gnc_html_data_new                                                *
 *   construct a new html data structure                            *
 *                                                                  *
 * Args:   title            - the title of the html window          *
 *         user_data        - html user data, whatever you like     *
 *         destroy          - function to destroy user data, or NULL*
 *         user_buttons     - new buttons for the toolbar           *
 *         num_user_buttons - number of new buttons                 *
 * Return: html data structure                                      *
\********************************************************************/
HTMLData *
gnc_html_data_new(const char *title, HTMLUserData user_data,
                  HTMLDestroyUserDataFunc destroy,
                  GnomeUIInfo *user_buttons,
                  int num_user_buttons)
{
  HTMLData *data;

  data = malloc(sizeof(HTMLData));
  assert(data != NULL);

  if (title != NULL)
  {
    data->title = strdup(title);
    assert(title != NULL);
  }
  else
    data->title = NULL;

  if (num_user_buttons == 0)
    data->user_buttons = NULL;
  else
  {
    int i;

    data->user_buttons = calloc(num_user_buttons, sizeof(GnomeUIInfo));
    assert(data->user_buttons != NULL);

    for (i = 0; i < num_user_buttons; i++)
      data->user_buttons[i] = user_buttons[i];
  }

  data->num_user_buttons = num_user_buttons;

  data->user_data = user_data;
  data->destroy = destroy;

  return data;
}

static void
html_data_destroy(HTMLData *data)
{
  if (data == NULL)
    return;

  if (data->title != NULL)
    free(data->title);
  data->title = NULL;

  if (data->user_buttons != NULL)
    free(data->user_buttons);
  data->user_buttons = NULL;
  data->num_user_buttons = 0;

  if ((data->destroy != NULL) &&
      (data->user_data != NULL))
    (data->destroy)(data->user_data);

  data->destroy = NULL;

  free(data);
}

static HTMLHistoryNode *
history_node_new(HTMLData *data)
{
  HTMLHistoryNode *new;

  new = malloc(sizeof(HTMLHistoryNode));
  assert(new != NULL);

  new->data = data;
  new->last = NULL;
  new->next = NULL;

  return new;
}

/* Insert a history element into history. Return TRUE if this
 * is the first element in the history. If not last element
 * in history, all next pages are deleted */
static gncBoolean
historyInsert(HTMLHistory *history, HTMLData *data)
{
  HTMLHistoryNode *new;
  HTMLHistoryNode *temp;

  assert(history != NULL);

  new = history_node_new(data);

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
    html_data_destroy(temp->data);
    temp->data = NULL;
    free(temp);

    temp = history->current_node->next;
  }

  new->last = history->current_node;
  history->current_node->next = new;
  history->current_node = new;

  return GNC_F;
}

/* Move forward in history, and return current history data */
static HTMLData *
historyFwd(HTMLHistory *history)
{
  if (history == NULL)
    return NULL;
  if (history->current_node == NULL)
    return NULL;

  if (history->current_node->next != NULL)
    history->current_node = history->current_node->next;

  return history->current_node->data;
}

/* Move back in history, and return current history data */
static HTMLData *
historyBack(HTMLHistory *history)
{
  if (history == NULL)
    return NULL;
  if (history->current_node == NULL)
    return NULL;

  if (history->current_node->last != NULL)
    history->current_node = history->current_node->last;

  return history->current_node->data;
}

/* Remove all entries from history */
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
    html_data_destroy(temp->data);
    temp->data = NULL;
    free(temp);
  }

  /* delete current page: */
  html_data_destroy(history->current_node->data);
  history->current_node->data = NULL;
  free(history->current_node);
  history->current_node = NULL;
}

static HTMLData *
historyData(HTMLHistory *history)
{
  if (history == NULL)
    return NULL;
  if (history->current_node == NULL)
    return NULL;

  return history->current_node->data;
}

static HTMLUserData
historyUserData(HTMLHistory *history)
{
  HTMLData *data;

  data = historyData(history);
  if (data == NULL)
    return NULL;

  return data->user_data;
}

static HTMLHistory *
historyNew()
{
  HTMLHistory *history;

  history = malloc(sizeof(HTMLHistory));
  assert(history != NULL);

  history->current_node = NULL;

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

  GtkWidget *toolbar;
  SCM toolbar_change_callback_id;

  HTMLHistory *history;

  HTMLAnchorCB    anchor_cb;
  HTMLJumpCB      jump_cb;
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
 * gnc_html_window_user_data                                        *
 *   return the current user data for the window                    *
 *                                                                  *
 * Args: none                                                       *
 * Return: user data for the window                                 *
\********************************************************************/
HTMLUserData
gnc_html_window_user_data(HTMLWindow *hw)
{
  if (hw == NULL)
    return NULL;

  return historyUserData(hw->history);
}


/********************************************************************\
 * gnc_html_window_get_window                                       *
 *   return the gtk window for the html window                      *
 *                                                                  *
 * Args: none                                                       *
 * Return: gtk window for html window                               *
\********************************************************************/
GtkWidget *
gnc_html_window_get_window(HTMLWindow *hw)
{
  if (hw == NULL)
    return NULL;

  return hw->window;
}


/********************************************************************\
 * gnc_html_window_new                                              *
 *   g_malloc and initialize HTMLWindow structure                   *
 *                                                                  *
 * Args: anchor_cb - callback for new html pages                    *
 *       jump_cb   - callback for html text                         *
 * Return: g_malloc'd initialized HTMLWindow structure              * 
\********************************************************************/
HTMLWindow *
gnc_html_window_new(HTMLAnchorCB anchor_cb, HTMLJumpCB jump_cb)
{
  HTMLWindow *hw;

  hw = g_new0(HTMLWindow, 1);

  hw->history = historyNew();

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


static void
gnc_html_window_fill_toolbar(HTMLWindow *hw)
{
  GnomeUIInfo *toolbar_info;
  gint num_start, num_end;
  GList *children, *node;
  HTMLData *data;
  gint i;

  GnomeUIInfo toolbar_start[] = 
  {
    { GNOME_APP_UI_ITEM,
      BACK_STR,
      TOOLTIP_HTML_BACK,
      htmlBackCB, hw,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_BACK,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      FORWARD_STR,
      TOOLTIP_HTML_FORW,
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
      CLOSE_STR,
      TOOLTIP_CLOSE_HTML,
      closeHtmlWinCB, hw,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  data = historyData(hw->history);
  if (data == NULL)
    return;

  if (hw->toolbar == NULL)
    return;

  node = children = gtk_container_children(GTK_CONTAINER(hw->toolbar));
  while (node != NULL)
  {
    gtk_container_remove(GTK_CONTAINER(hw->toolbar), GTK_WIDGET(node->data));
    node = node->next;
  }
  g_list_free(children);

  num_start = sizeof(toolbar_start) / sizeof(GnomeUIInfo);
  num_end = sizeof(toolbar_end) / sizeof(GnomeUIInfo);

  toolbar_info = g_new0(GnomeUIInfo,
                        num_start + data->num_user_buttons + num_end);

  for (i = 0; i < num_start; i++)
    toolbar_info[i] = toolbar_start[i];

  for (i = 0; i < data->num_user_buttons; i++)
    toolbar_info[i + num_start] = data->user_buttons[i];

  for (i = 0; i < num_end; i++)
    toolbar_info[i + num_start + data->num_user_buttons] = toolbar_end[i];

  gnome_app_fill_toolbar(GTK_TOOLBAR(hw->toolbar), toolbar_info, NULL);

  hw->back = toolbar_info[0].widget;
  hw->forward = toolbar_info[1].widget;

  g_free(toolbar_info);
}


static void
gnc_html_toolbar_change_cb(void *data)
{
  HTMLWindow *hw = data;
  GtkToolbarStyle tbstyle;

  tbstyle = gnc_get_toolbar_style();

  gtk_toolbar_set_style(GTK_TOOLBAR(hw->toolbar), tbstyle);
}


/********************************************************************\
 * htmlWindow                                                       *
 *   opens up an html window, and displays html                     *
 *                                                                  *
 * Args:   parent   - the parent widget                             *
 *         hwp      - the htmlwindow structure pointer              *
 *         data     - the data for the window                       *
 *         user_buttons - array of buttons to add to icon bar       *
 *         num_buttons  - number of buttons in list                 *
 * Return: none                                                     * 
\********************************************************************/
void
htmlWindow(GtkWidget   *parent, 
           HTMLWindow **hwp,
           HTMLData    *data)
{
  HTMLWindow *hw = *hwp;

  historyInsert(hw->history, data); 

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
    SCM id;

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_policy(GTK_WINDOW(window), TRUE, TRUE, TRUE);
    gtk_window_set_default_size(GTK_WINDOW(window), 675, 400);
    hw->window = window;

    dock = gnome_dock_new();
    gtk_container_add(GTK_CONTAINER(window), dock);

    dock_item = gnome_dock_item_new("toolbar", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);
    gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), toolbar);
    hw->toolbar = toolbar;

    id = gnc_register_option_change_callback(gnc_html_toolbar_change_cb, hw,
                                             "General", "Toolbar Buttons");
    hw->toolbar_change_callback_id = id;

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

  gnc_html_toolbar_change_cb(hw);
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
 * Args:   widget - the widget getting the key                      *
 *         event  - the key event                                   *
 *         data   - html window structure                           * 
 * Return: boolean indicating whether we handled the event          * 
\********************************************************************/
static gboolean
htmlKeyCB(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
  HTMLWindow *hw = (HTMLWindow *) data;
  GtkXmHTML *html = GTK_XMHTML(hw->htmlwidget);
  GtkAdjustment *vadj, *hadj;
  gfloat v_value, h_value;

  vadj = GTK_ADJUSTMENT(html->vsba);
  hadj = GTK_ADJUSTMENT(html->hsba);

  v_value = vadj->value;
  h_value = hadj->value;

  switch (event->keyval)
  {
    case GDK_KP_Left:
    case GDK_Left:
      h_value -= hadj->step_increment;
      break;
    case GDK_KP_Right:
    case GDK_Right:
      h_value += hadj->step_increment;
      break;
    case GDK_KP_Up:
    case GDK_Up:
      v_value -= vadj->step_increment;
      break;
    case GDK_KP_Down:
    case GDK_Down:
      v_value += vadj->step_increment;
      break;
    case GDK_KP_Page_Up:
    case GDK_Page_Up:
      v_value -= vadj->page_increment;
      break;
    case GDK_KP_Page_Down:
    case GDK_Page_Down:
    case GDK_space:
      v_value += vadj->page_increment;
      break;
    case GDK_KP_Home:
    case GDK_Home:
      v_value = vadj->lower;
      break;
    case GDK_KP_End:
    case GDK_End:
      v_value = vadj->upper;
      break;
    case GDK_Escape:
      gtk_widget_destroy(hw->window);
      return TRUE;
    default:
      return FALSE;
  }

  v_value = CLAMP(v_value, vadj->lower, vadj->upper - vadj->page_size);
  h_value = CLAMP(h_value, hadj->lower, hadj->upper - hadj->page_size);

  gtk_adjustment_set_value(vadj, v_value);
  gtk_adjustment_set_value(hadj, h_value);

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

  historyBack(hw->history);
 
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

  historyFwd(hw->history);

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

  gnc_unregister_option_change_callback_id(hw->toolbar_change_callback_id);

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
  HTMLData *html_data;

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

      html_data = (hw->anchor_cb)(acbs, historyUserData(hw->history));
      if (html_data == NULL)
        return;

      historyInsert(hw->history, html_data);
      gnc_html_load(hw);
      break;
  }

  htmlSetButtonStates(hw);
}


/********************************************************************\
 * gnc_html_load - load the current location into the html window   *
 *                                                                  * 
 * Args:   hw - the html window structure                           *
 * Return: none                                                     * 
\********************************************************************/
void
gnc_html_load(HTMLWindow *hw)
{
  HTMLData *data;
  char *label = NULL;
  char *text = NULL;

  if (hw == NULL)
    return;
  if (hw->jump_cb == NULL)
    return;

  data = historyData(hw->history);

  gtk_window_set_title(GTK_WINDOW(hw->window), data->title);
  gnc_html_window_fill_toolbar(hw);

  htmlSetButtonStates(hw);

  (hw->jump_cb)(data->user_data, &text, &label);

  if (text == NULL)
  {
    text = "";
    label = NULL;
  }

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
