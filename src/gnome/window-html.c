/********************************************************************\
 * window-html.c -- an html window for gnucash                      *
 * Copyright (C) 1997 Robin D. Clark  <rclark@cs.hmc.edu>           *
 * Copyright (C) 1998 Linas Vepstas   <linas@linas.org>             *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
 * Copyright (C) 2000 Linas Vepstas                                 *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <gtk/gtk.h>
#include <gtkhtml/gtkhtml.h>
#include <gnome.h>
#include <regex.h>

#include "gnucash.h"
#include "messages.h"
#include "window-html.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "print-session.h"
#include "File.h"
#include "util.h"


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

  data = g_new(HTMLData, 1);

  if (title != NULL)
    data->title = g_strdup(title);
  else
    data->title = NULL;

  if (num_user_buttons == 0)
    data->user_buttons = NULL;
  else
  {
    int i;

    data->user_buttons = g_new0(GnomeUIInfo, num_user_buttons);

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

  g_free(data->title);
  data->title = NULL;

  g_free(data->user_buttons);
  data->user_buttons = NULL;
  data->num_user_buttons = 0;

  if ((data->destroy != NULL) &&
      (data->user_data != NULL))
    (data->destroy)(data->user_data);

  data->destroy = NULL;

  g_free(data);
}

static HTMLHistoryNode *
history_node_new(HTMLData *data)
{
  HTMLHistoryNode *new;

  new = g_new(HTMLHistoryNode, 1);

  new->data = data;
  new->last = NULL;
  new->next = NULL;

  return new;
}

/* Insert a history element into history. Return TRUE if this
 * is the first element in the history. If not last element
 * in history, all next pages are deleted */
static gboolean
historyInsert(HTMLHistory *history, HTMLData *data)
{
  HTMLHistoryNode *new;
  HTMLHistoryNode *temp;

  assert(history != NULL);

  new = history_node_new(data);

  if (history->current_node == NULL)
  {
    history->current_node = new;
    return TRUE;
  }

  /* delete all next pages: */
  temp = history->current_node->next;
  while(temp != NULL)
  {
    history->current_node->next = temp->next;
    html_data_destroy(temp->data);
    temp->data = NULL;
    g_free(temp);

    temp = history->current_node->next;
  }

  new->last = history->current_node;
  history->current_node->next = new;
  history->current_node = new;

  return FALSE;
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
    g_free(temp);
  }

  /* delete current page: */
  html_data_destroy(history->current_node->data);
  history->current_node->data = NULL;
  g_free(history->current_node);
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

  history = g_new(HTMLHistory, 1);

  history->current_node = NULL;

  return history;
}

static void
historyDestroy(HTMLHistory *history)
{
  if (history == NULL)
    return;

  historyClear(history);
  g_free(history);
}


/********************************************************************\
 *     HTML Window stuff...                                         * 
\********************************************************************/

struct _HTMLWindow
{
  GtkWidget * window;
  GtkWidget * scroller;
  GtkWidget * htmlwidget;

  GtkWidget * forward;
  GtkWidget * back;

  GtkWidget * dock;

  SCM       toolbar_change_callback_id;

  char      * null_text;
  char      * null_label;

  HTMLHistory   * history;

  HTMLAnchorCB  anchor_cb;
  HTMLJumpCB    jump_cb;
};


/** PROTOTYPES ******************************************************/

static void htmlBackCB(GtkWidget *widget, gpointer data);
static void htmlFwdCB(GtkWidget *widget, gpointer data);
static void htmlPrintCB(GtkWidget * widget, gpointer data);

static void htmlAnchorCB(GtkHTML * html, const gchar * url, gpointer data);
static void htmlUrlCB(GtkHTML * html, char * url, 
                      GtkHTMLStream * handle, gpointer data);
static gboolean htmlKeyCB(GtkWidget *widget, GdkEventKey *event,
                          gpointer user_data);

static void closeHtmlWinCB(GtkWidget *widget, gpointer data);
static void destroyHtmlWinCB(GtkWidget *widget, gpointer data);

static void htmlSetButtonStates(HTMLWindow *hw);


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

  hw->null_text = NULL;

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
gnc_html_window_destroy(HTMLWindow * hw)
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


/********************************************************************\
 * gnc_html_window_user_data                                        *
 *   return the current user data for the window                    *
 *                                                                  *
 * Args: none                                                       *
 * Return: user data for the window                                 *
\********************************************************************/

HTMLUserData
gnc_html_window_user_data(HTMLWindow * hw)
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

static void
html_window_fill_toolbar(HTMLWindow *hw)
{
  GnomeUIInfo *toolbar_info;
  gint num_start, num_end;
  GtkWidget *dock_item;
  GtkWidget *toolbar;
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
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      PRINT_STR,
      "Print HTML Window",
      htmlPrintCB, hw,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_PRINT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
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

  if (hw->dock == NULL)
    return;

  dock_item = GTK_WIDGET(gnome_dock_get_item_by_name(GNOME_DOCK(hw->dock),
                                                     "toolbar", NULL, NULL,
                                                     NULL, NULL));
  if (dock_item == NULL)
    return;

  toolbar = gnome_dock_item_get_child(GNOME_DOCK_ITEM(dock_item));
  if (toolbar != NULL)
    gtk_container_remove(GTK_CONTAINER(dock_item), toolbar);

  toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);
  gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
  gtk_container_add(GTK_CONTAINER(dock_item), toolbar);
  gtk_widget_show(toolbar);

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

  gnome_app_fill_toolbar(GTK_TOOLBAR(toolbar), toolbar_info, NULL);

  hw->back = toolbar_info[0].widget;
  hw->forward = toolbar_info[1].widget;

  g_free(toolbar_info);
}


static void
html_toolbar_change_cb(void *data)
{
  HTMLWindow *hw = data;
  GtkToolbarStyle tbstyle;
  GnomeDockItem *dock_item;
  GtkWidget *toolbar;

  tbstyle = gnc_get_toolbar_style();

  dock_item = gnome_dock_get_item_by_name(GNOME_DOCK(hw->dock), "toolbar",
                                          NULL, NULL, NULL, NULL);
  if (dock_item == NULL)
    return;

  toolbar = gnome_dock_item_get_child(dock_item);
  if (toolbar == NULL)
    return;

  gtk_toolbar_set_style(GTK_TOOLBAR(toolbar), tbstyle);
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
    GtkWidget * window;
    GtkWidget * html;
    GtkWidget * dock;
    GtkWidget * dock_item;
    GtkWidget * toolbar;
    GtkWidget * scroll;
    GtkWidget * frame;
    SCM id;

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_policy(GTK_WINDOW(window), TRUE, TRUE, TRUE);
    gtk_window_set_default_size(GTK_WINDOW(window), 675, 400);
    hw->window = window;

    dock = gnome_dock_new();
    gtk_container_add(GTK_CONTAINER(window), dock);
    hw->dock = dock;

    dock_item = gnome_dock_item_new("toolbar", GNOME_DOCK_ITEM_BEH_EXCLUSIVE);

    toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);
    gtk_container_set_border_width(GTK_CONTAINER(toolbar), 2);
    gtk_container_add(GTK_CONTAINER(dock_item), toolbar);

    gnome_dock_add_item (GNOME_DOCK(dock), GNOME_DOCK_ITEM(dock_item),
                         GNOME_DOCK_TOP, 0, 0, 0, TRUE);

    id = gnc_register_option_change_callback(html_toolbar_change_cb, hw,
                                             "General", "Toolbar Buttons");
    hw->toolbar_change_callback_id = id;

    frame  = gtk_frame_new(NULL);
    scroll = gtk_scrolled_window_new(NULL, NULL);
    html   = gtk_html_new();
    hw->htmlwidget = html;
    hw->scroller   = scroll;
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroll),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);  

    gtk_signal_connect (GTK_OBJECT(html), "url_requested",
                        GTK_SIGNAL_FUNC (htmlUrlCB), 
                        (gpointer)hw);

    gtk_signal_connect (GTK_OBJECT(html), "link_clicked",
                        GTK_SIGNAL_FUNC (htmlAnchorCB), 
                        (gpointer)hw);

    gtk_signal_connect (GTK_OBJECT(html), "key_press_event", 
                        GTK_SIGNAL_FUNC(htmlKeyCB), hw);

    gtk_signal_connect(GTK_OBJECT(window), "destroy",
                       GTK_SIGNAL_FUNC(destroyHtmlWinCB), hwp);

    gtk_container_add(GTK_CONTAINER(frame), scroll);
    gtk_container_add(GTK_CONTAINER(scroll), html);  

    gtk_html_load_empty(GTK_HTML(html));

    gnome_dock_set_client_area(GNOME_DOCK(dock), frame); 
    gtk_widget_realize(GTK_WIDGET(html));    
    gtk_widget_show_all(window);
    gnc_html_load(hw);
  }

  html_toolbar_change_cb(hw);
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
  HTMLWindow    * hw = (HTMLWindow *) data;

  GtkAdjustment * vadj = 
    gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(hw->scroller));
  GtkAdjustment * hadj = 
    gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(hw->scroller));

  gfloat        v_value = vadj->value;
  gfloat        h_value = hadj->value;

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
 * htmlPrintCB
 * callback for printing from toolbar button
\********************************************************************/
static void 
htmlPrintCB(GtkWidget * widget, gpointer data) {
  HTMLWindow * hw = (HTMLWindow *)data;
  gnc_html_print(hw);
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
  HTMLWindow **hwp = data;
  HTMLWindow *hw = *hwp;

  /* Delete the history: */
  historyDestroy(hw->history);
  hw->history = NULL;

  hw->htmlwidget = NULL;

  gnc_unregister_option_change_callback_id(hw->toolbar_change_callback_id);

  g_free(hw);
  *hwp = NULL;
}


/********************************************************************\
 * htmlAnchorCB - called when user clicks on html anchor tag        * 
 * the URL needs to be processed and the user callback called.      *
\********************************************************************/

static void
htmlAnchorCB(GtkHTML * html, const gchar * url, gpointer data) {
  HTMLWindow * hw = (HTMLWindow *)data;
  HTMLData   * html_data;
  char       * url_location;
  char       * url_label;
  int        url_type;
  
  /*  printf("in htmlAnchorCB: url='%s'\n", url); */

  url_type = gnc_html_parse_url(hw, url, &url_location, &url_label);

  switch(url_type) {
  case  URL_TYPE_JUMP:
    gtk_html_jump_to_anchor(html, url_label);
    g_free(url_location);
    g_free(url_label);
    break;

  default:
    if (hw->anchor_cb == NULL) {
      g_free(url_location);
      g_free(url_label);
      return;
    }
    
    html_data = (hw->anchor_cb)(url_type, url_location, url_label,
                                historyUserData(hw->history));
    g_free(url_location);
    g_free(url_label);

    if (html_data == NULL)
      return;
    
    historyInsert(hw->history, html_data);
    gnc_html_load(hw);    
    break;
  }
}


/********************************************************************\
 * htmlUrlCB - called by the GtkHTML widget when a URL needs to be  *
 * loaded.  At this point, the StreamHandle is already open.        *
\********************************************************************/

static void 
htmlUrlCB(GtkHTML * html, char * url, GtkHTMLStream * handle, 
          gpointer user_data) {
  HTMLWindow * hw = (HTMLWindow *)user_data;

  char    * text=NULL;
  char    * location=NULL;
  char    * label=NULL;
  int     fsize;

  URLType type;

  if(!url) return;

  if(!strcmp(url, "")) {
    gtk_html_write(GTK_HTML(hw->htmlwidget), handle, 
                   hw->null_text, (hw->null_text ? 
                                   strlen(hw->null_text) : 0));
    gtk_html_end(GTK_HTML(hw->htmlwidget), handle, GTK_HTML_STREAM_OK);
    
    if(hw->null_label) {
      gtk_html_jump_to_anchor(GTK_HTML(hw->htmlwidget), 
                              hw->null_label);
    }
  }
  else {
    type = gnc_html_parse_url(hw, url, &location, &label);
    fsize = gncReadFile(location, &text);
    if(text == NULL) {
      gtk_html_end(html, handle, GTK_HTML_STREAM_OK);    
    }
    else {
      gtk_html_write(GTK_HTML(hw->htmlwidget), handle, 
                     text, fsize);
      gtk_html_end(GTK_HTML(hw->htmlwidget), handle, GTK_HTML_STREAM_OK); 
      if(label) {
        gtk_html_jump_to_anchor(GTK_HTML(hw->htmlwidget), label);
      }
      g_free(text);
    }
  }
}


/********************************************************************\
 * gnc_html_parse_url
 * this takes a URL and the HTMLWindow context and determines the 
 * protocol type, location, and possible anchor name from the URL.
\********************************************************************/

URLType
gnc_html_parse_url(HTMLWindow * html, const gchar * url, 
                   char ** url_location, char ** url_label) {
  char        uri_rexp[] = "^(([^:]*):)?([^#]+)?(#(.*))?$";
  regex_t     compiled;
  regmatch_t  match[6];
  char        * protocol=NULL, * path=NULL, * label=NULL;
  int         found_protocol=0, found_path=0, found_label=0;  
  URLType     retval;   

  regcomp(&compiled, uri_rexp, REG_EXTENDED);

  if(!regexec(&compiled, url, 6, match, 0)) {
    if(match[2].rm_so != -1) {
      protocol = g_new0(char, match[2].rm_eo - match[2].rm_so + 1);
      strncpy(protocol, url + match[2].rm_so, 
              match[2].rm_eo - match[2].rm_so);
      protocol[match[2].rm_eo - match[2].rm_so] = 0;
      found_protocol = 1;      
    }
    if(match[3].rm_so != -1) {
      path = g_new0(char, match[3].rm_eo - match[3].rm_so + 1);
      strncpy(path, url+match[3].rm_so, 
              match[3].rm_eo - match[3].rm_so);
      path[match[3].rm_eo - match[3].rm_so] = 0;
      found_path = 1;
    }
    if(match[5].rm_so != -1) {
      label = g_new0(char, match[5].rm_eo - match[5].rm_so + 1);
      strncpy(label, url+match[5].rm_so, 
              match[5].rm_eo - match[5].rm_so);
      label[match[5].rm_eo - match[5].rm_so] = 0;
      found_label = 1;
    }
  }

  if(found_protocol) {
    if(!strcmp(protocol, "file")) {
      retval = URL_TYPE_FILE;
    }
    else if(!strcmp(protocol, "http")) {
      retval = URL_TYPE_HTTP;
    }
    else if(!strcmp(protocol, "ftp")) {
      retval = URL_TYPE_FTP;
    }
    else if(!strcmp(protocol, "https")) {
      retval = URL_TYPE_SECURE;
    }
    else {
      retval = URL_TYPE_OTHER;
    }
  }
  else if(found_label && !found_path) {
    retval = URL_TYPE_JUMP;
  }
  else {
    retval = URL_TYPE_FILE; /* FIXME BG */
  }

  g_free(protocol);

  switch(retval) {
  case URL_TYPE_FILE:
    *url_location = path;
    break;

  case URL_TYPE_JUMP:
    *url_location = NULL;
    g_free(path);
    break;

  case URL_TYPE_OTHER:
  default:
    * url_location = path;
    break;
  }

  * url_label = label;
  return retval;
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
  HTMLData      * data;
  GtkHTMLStream * handle;
  char * text=NULL, * label=NULL;

  if (hw == NULL)
    return;
  if (hw->jump_cb == NULL)
    return;

  data = historyData(hw->history);

  gtk_window_set_title(GTK_WINDOW(hw->window), data->title);

  html_window_fill_toolbar(hw);

  htmlSetButtonStates(hw);

  (hw->jump_cb)(data->user_data, &hw->null_text, &hw->null_label);

  if (text == NULL)
  {
    text = "";
    label = NULL;
  }
  handle = gtk_html_begin(GTK_HTML(hw->htmlwidget));
  htmlUrlCB(GTK_HTML(hw->htmlwidget), "", handle, (gpointer)hw);
}

void 
gnc_url_show(URLType type, char * location, char * label) {
  char * full_url;
  char proto_tag[8];

  switch(type) {
  case URL_TYPE_FILE:
  case URL_TYPE_JUMP:
    strcpy(proto_tag, "file:");
    break;
  case URL_TYPE_HTTP:
    strcpy(proto_tag, "http:");
    break;
  case URL_TYPE_FTP:
    strcpy(proto_tag, "ftp:");
    break;
  case URL_TYPE_SECURE:
    strcpy(proto_tag, "https:");
    break;
  default:
    strcpy(proto_tag, "");
  }

  full_url = g_new0(char, 
                    (proto_tag ? strlen(proto_tag) : 0) + 
                    (location ? strlen(location) : 0) +
                    (label ? strlen(label) : 0) + 1);
  strcpy(full_url, proto_tag);
  if(location) strcat(full_url, location);
  if(label) strcat(full_url, label);

  gnome_url_show(full_url);
}

/********************************************************************
 * gnc_html_print : print an html window 
 ********************************************************************/

void
gnc_html_print(HTMLWindow * hw) {
  PrintSession * ps = gnc_print_session_create();
  
  gtk_html_print(GTK_HTML(hw->htmlwidget),
                 GNOME_PRINT_CONTEXT(ps->meta));
  gnc_print_session_done(ps);
  gnc_ui_print_dialog_create(ps);
}
