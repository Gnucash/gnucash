/********************************************************************\
 * dialog-utils.c -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999-2000 Linas Vepstas                            *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "account-tree.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-commodity.h"
#include "messages.h"
#include "EuroUtils.h"
#include "Group.h"
#include "gnc-dir.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"


/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_GUI;


/********************************************************************\
 * gnc_ui_source_menu_create                                        *
 *   create the menu of stock quote sources                         *
 *                                                                  *
 * Args:    account - account to use to set default choice          *
 * Returns: the menu                                                *
 \*******************************************************************/
GtkWidget *
gnc_ui_source_menu_create(Account *account)
{
  gint i;
  GtkMenu   *menu;
  GtkWidget *item;
  GtkWidget *omenu;
  GNCAccountType type;

  menu = GTK_MENU(gtk_menu_new());
  gtk_widget_show(GTK_WIDGET(menu));

  for (i = 0; i < NUM_SOURCES; i++)
  {
    item = gtk_menu_item_new_with_label(gnc_get_source_name(i));
    gtk_widget_show(item);
    gtk_menu_append(menu, item);
  }

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);
  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu), GTK_WIDGET(menu));
  gnc_option_menu_init(omenu);

  return omenu;
}

/********************************************************************\
 * price quote timezone handling
 */

static gchar *
known_timezones[] =
{
  "Asia/Tokyo",
  "Australia/Sydney",
  "America/New_York",
  "America/Chicago",
  "Europe/London",
  "Europe/Paris",
  NULL
};

guint
gnc_find_timezone_menu_position(const gchar *timezone)
{
  /* returns 0 on failure, position otherwise. */
  gboolean found = FALSE;
  guint i = 0;
  while(!found && known_timezones[i]) {
    if(safe_strcmp(timezone, known_timezones[i]) != 0) {
      i++;
    } else {
      found = TRUE;
    }
  }
  if(found) return i + 1;
  return 0;
}

gchar *
gnc_timezone_menu_position_to_string(guint pos)
{
  if(pos == 0) return NULL;
  return known_timezones[pos - 1];
}

GtkWidget *
gnc_ui_quote_tz_menu_create(Account *account)
{
  gint i;
  GtkMenu   *menu;
  GtkWidget *item;
  GtkWidget *omenu;
  gchar     **itemstr;

  /* add items here as needed, but bear in mind that right now these
     must be timezones that GNU libc understands.  Also, I'd prefer if
     we only add things here we *know* we need.  That's because in
     order to be portable to non GNU OSes, we may have to support
     whatever we add here manually on those systems. */

  menu = GTK_MENU(gtk_menu_new());
  gtk_widget_show(GTK_WIDGET(menu));

  item = gtk_menu_item_new_with_label(_("Use local time"));
  /* set user data to non NULL so we can detect this item specially. */
  gtk_object_set_user_data(GTK_OBJECT(item), (gpointer) 1);
  gtk_widget_show(item);
  gtk_menu_append(menu, item);
 
  for(itemstr = &known_timezones[0]; *itemstr; itemstr++) {
    item = gtk_menu_item_new_with_label(*itemstr);
    gtk_widget_show(item);
    gtk_menu_append(menu, item);
  }

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);
  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu), GTK_WIDGET(menu));
  gnc_option_menu_init(omenu);

  return omenu;
}

/* =========================================================== */

static void
gnc_option_menu_cb(GtkWidget *w, gpointer data)
{
  GNCOptionCallback cb;
  gpointer _index;
  gint index;

  cb = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_cb");

  _index = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_index");
  index = GPOINTER_TO_INT(_index);

  cb(w, index, data);
}

static void
option_menu_destroy_cb (GtkObject *obj, gpointer data)
{
  GtkTooltips *tips = data;

  gtk_object_unref (GTK_OBJECT (tips));
}

/********************************************************************\
 * gnc_ui_create_option_button                                      *
 *   create an option button given the option structure             *
 *                                                                  *
 * Args: option_info - the option structure to use                  *
 *       num_options - the number of options                        *
 * Returns: void                                                    *
 \*******************************************************************/
GtkWidget *
gnc_build_option_menu(GNCOptionInfo *option_info, gint num_options)
{
  GtkTooltips *tooltips;
  GtkWidget *omenu;
  GtkWidget *menu;
  GtkWidget *menu_item;
  gint i;

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);

  menu = gtk_menu_new();
  gtk_widget_show(menu);

  tooltips = gtk_tooltips_new();

  gtk_object_ref (GTK_OBJECT (tooltips));
  gtk_object_sink (GTK_OBJECT (tooltips));

  for (i = 0; i < num_options; i++)
  {
    menu_item = gtk_menu_item_new_with_label(option_info[i].name);
    gtk_tooltips_set_tip(tooltips, menu_item, option_info[i].tip, NULL);
    gtk_widget_show(menu_item);

    gtk_object_set_data(GTK_OBJECT(menu_item),
                        "gnc_option_cb",
                        option_info[i].callback);

    gtk_object_set_data(GTK_OBJECT(menu_item),
                        "gnc_option_index",
                        GINT_TO_POINTER(i));

    gtk_object_set_data(GTK_OBJECT(menu_item),
                        "gnc_option_menu",
                        omenu);

    if (option_info[i].callback != NULL)
      gtk_signal_connect(GTK_OBJECT(menu_item), "activate",
                         GTK_SIGNAL_FUNC(gnc_option_menu_cb),
                         option_info[i].user_data);

    gtk_menu_append(GTK_MENU(menu), menu_item);
  }

  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu), menu);

  gtk_signal_connect (GTK_OBJECT (omenu), "destroy",
                      GTK_SIGNAL_FUNC (option_menu_destroy_cb), tooltips);

  return omenu;
}


/********************************************************************\
 * gnc_get_pixmap                                                   *
 *   returns a GnomePixmap widget given a pixmap filename           *
 *                                                                  *
 * Args: none                                                       *
 * Returns: GnomePixmap widget or NULL if there was a problem       *
 \*******************************************************************/
GtkWidget *
gnc_get_pixmap (const char *name)
{
  GtkWidget *pixmap;
  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = g_strconcat (GNC_PIXMAP_DIR, "/", name, NULL);
  pixmap = gnome_pixmap_new_from_file (fullname);
  g_free (fullname);

  return pixmap;
}


/********************************************************************\
 * gnc_get_imlib_image                                              *
 *   returns a GdkImlibImage object given a pixmap filename         *
 *                                                                  *
 * Args: none                                                       *
 * Returns: GnomePixmap widget or NULL if there was a problem       *
 \*******************************************************************/
GdkImlibImage *
gnc_get_gdk_imlib_image (const char *name)
{
  GdkImlibImage *image;

  char *fullname;

  g_return_val_if_fail (name != NULL, NULL);

  fullname = g_strconcat (GNC_PIXMAP_DIR, "/", name, NULL);
  image = gdk_imlib_load_image (fullname);
  g_free (fullname);

  return image;
}


/********************************************************************\
 * gnc_get_toolbar_style                                            *
 *   returns the current toolbar style for gnucash toolbars         *
 *                                                                  *
 * Args: none                                                       *
 * Returns: toolbar style                                           *
 \*******************************************************************/
GtkToolbarStyle
gnc_get_toolbar_style(void)
{
  GtkToolbarStyle tbstyle = GTK_TOOLBAR_BOTH;
  char *style_string;

  style_string = gnc_lookup_multichoice_option("General",
                                               "Toolbar Buttons",
                                               "icons_and_text");

  if (safe_strcmp(style_string, "icons_and_text") == 0)
    tbstyle = GTK_TOOLBAR_BOTH;
  else if (safe_strcmp(style_string, "icons_only") == 0)
    tbstyle = GTK_TOOLBAR_ICONS;
  else if (safe_strcmp(style_string, "text_only") == 0)
    tbstyle = GTK_TOOLBAR_TEXT;

  if (style_string != NULL)
    free(style_string);

  return tbstyle;
}

/********************************************************************
 * gnc_get_mdi_mode                                                 *
 * returns the current Gnome MDI mode preference                    *
 ********************************************************************/
GnomeMDIMode 
gnc_get_mdi_mode(void) {
  GnomeMDIMode mode = GNOME_MDI_DEFAULT_MODE;
  char * mode_string = gnc_lookup_multichoice_option("General",
                                                     "Application MDI mode",
                                                     "");
  if(!safe_strcmp(mode_string, "mdi-notebook")) {
    mode = GNOME_MDI_NOTEBOOK;
  }
  else if(!safe_strcmp(mode_string, "mdi-toplevel")) {
    mode = GNOME_MDI_TOPLEVEL;    
  }
  else if(!safe_strcmp(mode_string, "mdi-modal")) {
    mode = GNOME_MDI_MODAL;    
  }
  else if(!safe_strcmp(mode_string, "mdi-default")) {
    mode = GNOME_MDI_DEFAULT_MODE;    
  }

  if(mode_string) free(mode_string);
  return mode;
}


/********************************************************************\
 * gnc_get_deficit_color                                            *
 *   fill in the 3 color values for the color of deficit values     *
 *                                                                  *
 * Args: color - color structure                                    *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_get_deficit_color(GdkColor *color)
{
  color->red   = 50000;
  color->green = 0;
  color->blue  = 0;
}


/********************************************************************\
 * gnc_set_label_color                                              *
 *   sets the color of the label given the value                    *
 *                                                                  *
 * Args: label - gtk label widget                                   *
 *       value - value to use to set color                          *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_set_label_color(GtkWidget *label, gnc_numeric value)
{
  gboolean deficit;
  GdkColormap *cm;
  GtkStyle *style;

  if (!gnc_color_deficits())
    return;

  cm = gtk_widget_get_colormap(GTK_WIDGET(label));
  style = gtk_widget_get_style(GTK_WIDGET(label));

  style = gtk_style_copy(style);

  deficit = gnc_numeric_negative_p (value);

  if (deficit)
  {
    gnc_get_deficit_color(&style->fg[GTK_STATE_NORMAL]);
    gdk_colormap_alloc_color(cm, &style->fg[GTK_STATE_NORMAL], FALSE, TRUE);
  }
  else
    style->fg[GTK_STATE_NORMAL] = style->black;

  gtk_widget_set_style(label, style);

  gtk_style_unref(style);
}


/********************************************************************\
 * gnc_get_window_size                                              *
 *   returns the window size to use for the given option prefix,    *
 *   if window sizes are being saved, otherwise returns 0 for both. *
 *                                                                  *
 * Args: prefix - the option name prefix                            *
 *       width  - pointer to width                                  *
 *       height - pointer to height                                 *
 * Returns: nothing                                                 *
 \*******************************************************************/
void
gnc_get_window_size(const char *prefix, int *width, int *height)
{
  int w, h;
  char *name;

  if (gnc_lookup_boolean_option("General", "Save Window Geometry", TRUE))
  {
    name = g_strconcat(prefix, "_width", NULL);
    w = gnc_lookup_number_option("__gui", name, 0.0);
    g_free(name);

    name = g_strconcat(prefix, "_height", NULL);
    h = gnc_lookup_number_option("__gui", name, 0.0);
    g_free(name);
  }
  else
  {
    w = 0;
    h = 0;
  }

  if (width != NULL)
    *width = w;

  if (height != NULL)
    *height = h;
}


/********************************************************************\
 * gnc_save_window_size                                             *
 *   save the window size into options whose names are determined   *
 *   by the string prefix.                                          *
 *                                                                  *
 * Args: prefix - determines the options used to save the values    *
 *       width  - width of the window to save                       *
 *       height - height of the window to save                      *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_save_window_size(const char *prefix, int width, int height)
{
  char *name;
  gboolean save;

  save = gnc_lookup_boolean_option("General", "Save Window Geometry", FALSE);

  name = g_strconcat(prefix, "_width", NULL);
  if (save)
    gnc_set_number_option("__gui", name, width);
  else
    gnc_set_option_default("__gui", name);
  g_free(name);

  name = g_strconcat(prefix, "_height", NULL);
  if (save)
    gnc_set_number_option("__gui", name, height);
  else
    gnc_set_option_default("__gui", name);
  g_free(name);
}


/********************************************************************\
 * gnc_fill_menu_with_data                                          *
 *   fill the user data values in the menu structure with the given *
 *   value. The filling is done recursively.                        *
 *                                                                  *
 * Args: info - the menu to fill                                    *
 *       data - the value to fill with                              *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_fill_menu_with_data(GnomeUIInfo *info, gpointer data)
{
  if (info == NULL)
    return;

  while (1)
  {
    switch (info->type)
    {
      case GNOME_APP_UI_RADIOITEMS:
      case GNOME_APP_UI_SUBTREE:
      case GNOME_APP_UI_SUBTREE_STOCK:
        gnc_fill_menu_with_data((GnomeUIInfo *) info->moreinfo, data);
        break;
      case GNOME_APP_UI_ENDOFINFO:
        return;
      default:
        info->user_data = data;
        break;
    }

    info++;
  }
}


void
gnc_option_menu_init(GtkWidget * w)
{
  GtkWidget * menu;
  GtkWidget * active;
  int i;

  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(w));

  for(i = 0; i < g_list_length(GTK_MENU_SHELL(menu)->children); i++)
  {
    gtk_option_menu_set_history(GTK_OPTION_MENU(w), i);
    active = gtk_menu_get_active(GTK_MENU(menu));
    gtk_object_set_data(GTK_OBJECT(active), 
                        "option_index",
                        GINT_TO_POINTER(i));
  }

  gtk_option_menu_set_history(GTK_OPTION_MENU(w), 0);
}


int
gnc_option_menu_get_active(GtkWidget * w)
{
  GtkWidget * menu;
  GtkWidget * menuitem;

  menu     = gtk_option_menu_get_menu(GTK_OPTION_MENU(w));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));

  return GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
                                             "option_index"));
}


/********************************************************************\
 * gnc_window_adjust_for_screen                                     *
 *   adjust the window size if it is bigger than the screen size.   *
 *                                                                  *
 * Args: window - the window to adjust                              *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_window_adjust_for_screen(GtkWindow * window)
{
  gint screen_width;
  gint screen_height;
  gint width;
  gint height;

  if (window == NULL)
    return;

  g_return_if_fail(GTK_IS_WINDOW(window));
  if (GTK_WIDGET(window)->window == NULL)
    return;

  screen_width = gdk_screen_width();
  screen_height = gdk_screen_height();
  gdk_window_get_size(GTK_WIDGET(window)->window, &width, &height);

  if ((width <= screen_width) && (height <= screen_height))
    return;

  width = MIN(width, screen_width - 10);
  width = MAX(width, 0);

  height = MIN(height, screen_height - 10);
  height = MAX(height, 0);

  gdk_window_resize(GTK_WIDGET(window)->window, width, height);
  gtk_widget_queue_resize(GTK_WIDGET(window));
}


typedef struct
{
  int row;
  int col;
  gboolean checked;
} GNCCListCheckNode;

typedef struct
{
  GdkPixmap *on_pixmap;
  GdkPixmap *off_pixmap;
  GdkBitmap *mask;

  GList *pending_checks;
} GNCCListCheckInfo;

static void
free_check_list (GList *list)
{
  GList *node;

  for (node = list; node; node = node->next)
    g_free (node->data);

  g_list_free (list);
}

static void
check_realize (GtkWidget *widget, gpointer user_data)
{
  GNCCListCheckInfo *check_info = user_data;
  GdkGCValues gc_values;
  GtkCList *clist;
  gint font_height;
  gint check_size;
  GdkColormap *cm;
  GtkStyle *style;
  GList *list;
  GList *node;
  GdkGC *gc;

  if (check_info->mask)
    return;

  style = gtk_widget_get_style (widget);

  font_height = style->font->ascent + style->font->descent;
  check_size = (font_height > 0) ? font_height - 3 : 9;

  check_info->mask = gdk_pixmap_new (NULL, check_size, check_size, 1);

  check_info->on_pixmap = gdk_pixmap_new (widget->window,
                                          check_size, check_size, -1);

  check_info->off_pixmap = gdk_pixmap_new (widget->window,
                                           check_size, check_size, -1);

  gc_values.foreground = style->white;
  gc = gtk_gc_get (1, gtk_widget_get_colormap (widget),
                   &gc_values, GDK_GC_FOREGROUND);

  gdk_draw_rectangle (check_info->mask, gc, TRUE,
                      0, 0, check_size, check_size);

  gtk_gc_release (gc);

  gc = style->base_gc[GTK_STATE_NORMAL];

  gdk_draw_rectangle (check_info->on_pixmap, gc, TRUE,
                      0, 0, check_size, check_size);
  gdk_draw_rectangle (check_info->off_pixmap, gc, TRUE,
                      0, 0, check_size, check_size);

  cm = gtk_widget_get_colormap (widget);

  gc_values.foreground.red = 0;
  gc_values.foreground.green = 65535 / 2;
  gc_values.foreground.blue = 0;

  gdk_colormap_alloc_color (cm, &gc_values.foreground, FALSE, TRUE);

  gc = gdk_gc_new_with_values (widget->window, &gc_values, GDK_GC_FOREGROUND);

  gdk_draw_line (check_info->on_pixmap, gc,
                 1, check_size / 2,
                 check_size / 3, check_size - 5);
  gdk_draw_line (check_info->on_pixmap, gc,
                 1, check_size / 2 + 1,
                 check_size / 3, check_size - 4);
        
  gdk_draw_line (check_info->on_pixmap, gc,
                 check_size / 3, check_size - 5,
                 check_size - 3, 2);
  gdk_draw_line (check_info->on_pixmap, gc,
                 check_size / 3, check_size - 4,
                 check_size - 3, 1);

  gdk_gc_unref (gc);

  clist = GTK_CLIST (widget);

  list = check_info->pending_checks;
  check_info->pending_checks = NULL;

  /* reverse so we apply in the order of the calls */
  list = g_list_reverse (list);

  for (node = list; node; node = node->next)
  {
    GNCCListCheckNode *cl_node = node->data;

    gnc_clist_set_check (clist, cl_node->row, cl_node->col, cl_node->checked);
  }

  free_check_list (list);
}

static void
check_unrealize (GtkWidget *widget, gpointer user_data)
{
  GNCCListCheckInfo *check_info = user_data;

  if (check_info->mask)
    gdk_bitmap_unref (check_info->mask);

  if (check_info->on_pixmap)
    gdk_pixmap_unref (check_info->on_pixmap);

  if (check_info->off_pixmap)
    gdk_pixmap_unref (check_info->off_pixmap);

  check_info->mask = NULL;
  check_info->on_pixmap = NULL;
  check_info->off_pixmap = NULL;
}

static void
check_destroy (GtkWidget *widget, gpointer user_data)
{
  GNCCListCheckInfo *check_info = user_data;

  free_check_list (check_info->pending_checks);
  check_info->pending_checks = NULL;

  g_free (check_info);
}

static GNCCListCheckInfo *
gnc_clist_add_check (GtkCList *list)
{
  GNCCListCheckInfo *check_info;
  GtkObject *object;

  object = GTK_OBJECT (list);

  check_info = gtk_object_get_data (object, "gnc-check-info");
  if (check_info)
  {
    PWARN ("clist already has check");
    return check_info;
  }

  check_info = g_new0 (GNCCListCheckInfo, 1);

  gtk_object_set_data (object, "gnc-check_info", check_info);

  gtk_signal_connect (object, "realize",
                      GTK_SIGNAL_FUNC (check_realize), check_info);
  gtk_signal_connect (object, "unrealize",
                      GTK_SIGNAL_FUNC (check_unrealize), check_info);
  gtk_signal_connect (object, "destroy",
                      GTK_SIGNAL_FUNC (check_destroy), check_info);

  return check_info;
}


void
gnc_clist_set_check (GtkCList *list, int row, int col, gboolean checked)
{
  GNCCListCheckInfo *check_info;
  GdkPixmap *pixmap;

  g_return_if_fail (GTK_IS_CLIST (list));

  check_info = gtk_object_get_data (GTK_OBJECT (list), "gnc-check_info");
  if (!check_info)
    check_info = gnc_clist_add_check (list);

  if (!GTK_WIDGET_REALIZED (GTK_WIDGET (list)))
  {
    GNCCListCheckNode *node;

    node = g_new0 (GNCCListCheckNode, 1);

    node->row = row;
    node->col = col;
    node->checked = checked;

    check_info->pending_checks =
      g_list_prepend (check_info->pending_checks, node);

    return;
  }

  pixmap = checked ? check_info->on_pixmap : check_info->off_pixmap;

  if (checked)
    gtk_clist_set_pixmap (list, row, col, pixmap, check_info->mask);
  else
    gtk_clist_set_text (list, row, col, "");
}

void
gnc_clist_columns_autosize (GtkCList *list)
{
  GtkStyle *style;
  GdkFont *font;
  gint i;

  if (!list) return;
  g_return_if_fail (GTK_IS_CLIST (list));

  style = gtk_widget_get_style (GTK_WIDGET(list));
  if (!style)
    return;

  font = style->font;
  if (!font)
    return;

  for (i = 0; TRUE; i++)
  {
    GtkWidget *widget;
    char *title;
    gint width;

    widget = gtk_clist_get_column_widget (list, i);
    if (!widget)
      break;

    if (!GTK_IS_LABEL (widget))
      continue;

    gtk_label_get (GTK_LABEL (widget), &title);

    width = gdk_string_width (font, title);
    gtk_clist_set_column_min_width (list, i, width + 5);
  }

  gtk_clist_columns_autosize (list);
}
