/* 
 * gnc-tree-view-common.c -- common utilities for manipulating a
 *                     GtkTreeView within gnucash
 *
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <libgnome/libgnome.h>
#include <string.h>

#include "gnc-tree-view-common.h"
#include "gnc-engine-util.h"
#include "gnc-gconf-utils.h"
#include "gnc-gnome-utils.h"

#define COLUMN_NUM "column"
#define TREE_NAME  "tree-name"

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/

static gint
view_column_find_by_name(gnc_view_column *defaults,
			 const gchar *name)
{
  gint i;

  g_return_val_if_fail (name != NULL, -1);

  for (i = 0; defaults[i].pref_name != NULL; i++) {
    if (strcmp(defaults[i].pref_name, name) == 0)
      return i;
  }
  return -1;
}


/************************************************************/
/*                    Tree View Creation                    */
/************************************************************/

void
gnc_tree_view_common_create_columns (GtkTreeView *view,
				     const gchar *tree_name,
				     const gchar *stock_icon,
				     gnc_view_column *defaults)
{
  GtkTreeModel *s_model;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  gboolean is_sortable;
  gint i;

  ENTER("view %p, name %s, icon %s, defaults %p",
	view, tree_name, stock_icon, defaults);
  g_object_set_data(G_OBJECT(view), TREE_NAME, g_strdup(tree_name));

  /*
   * Get the model
   */
  s_model = gtk_tree_view_get_model(view);
  is_sortable = GTK_IS_TREE_MODEL_SORT(s_model);

  /*
   * Create the first column.  This column is special in that it will
   * have the disclosure triangle to expand the tree, and it may have
   * an icon in it.
   */
  column = gtk_tree_view_column_new ();
  g_object_set_data(G_OBJECT(column), COLUMN_NUM, GINT_TO_POINTER(0));
  gtk_tree_view_column_set_title (column, gettext(defaults[0].field_name));
  if (stock_icon) {
    renderer = gtk_cell_renderer_pixbuf_new ();
    g_object_set (renderer, "stock-id", stock_icon, NULL);
    gtk_tree_view_column_pack_start (column, renderer, FALSE);
  }
  renderer = gtk_cell_renderer_text_new ();
  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  gtk_tree_view_column_add_attribute (column,
				      renderer,
				      "text", defaults[0].column);
  if (defaults[0].visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
    gtk_tree_view_column_add_attribute (column, renderer,
					"visible", defaults[0].visibility_column);
  if (defaults[0].color_column != GNC_TREE_VIEW_COLUMN_COLOR_NONE)
    gtk_tree_view_column_add_attribute (column, renderer,
					"foreground", defaults[0].color_column);
  if (defaults[0].align_column != GNC_TREE_VIEW_COLUMN_ALIGN_NONE)
    gtk_tree_view_column_add_attribute (column, renderer,
					"xalign", defaults[0].align_column);
  gtk_tree_view_append_column (view, column);

  /*
   * Set the attributes of this first column
   */
  g_object_set(G_OBJECT(column),
	       "visible",     TRUE,
	       "alignment",   defaults[0].label_align,
	       "min-width",   50,  /* DRH - Should be based on title width */
	       "resizable",   TRUE,
	       "reorderable", TRUE,
	       "sizing",      defaults[0].sizing,
	       NULL);
  gtk_tree_view_set_expander_column (view, column);
  if (is_sortable) {
    gtk_tree_view_column_set_sort_column_id (column, 0);
    if (defaults[0].sort_fn)
      gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE(s_model), 0,
				       defaults[0].sort_fn,
				       NULL /* user_data */,
				       NULL /* destroy fn */);
  }
  DEBUG("Created %s column", defaults[0].pref_name);


  /*
   * Now create any additional columns.  These columns may contain
   * regular text or toggle buttons.
   */
  for (i = 1; defaults[i].pref_name != NULL; i++) {
    if (defaults[i].is_toggle) {
      renderer = gtk_cell_renderer_toggle_new ();
      column = gtk_tree_view_column_new_with_attributes (gettext(defaults[i].field_name),
							 renderer,
							 "active", defaults[i].column,
							 NULL);
      if (defaults[i].toggle_edited)
	g_signal_connect (G_OBJECT (renderer), "toggled",
			  G_CALLBACK (defaults[i].toggle_edited),
			  view);
    } else {
      renderer = gtk_cell_renderer_text_new ();
      column = gtk_tree_view_column_new_with_attributes (gettext(defaults[i].field_name),
							 renderer,
							 "text", defaults[i].column,
							 NULL);
      if (defaults[i].color_column != GNC_TREE_VIEW_COLUMN_COLOR_NONE)
	gtk_tree_view_column_add_attribute (column, renderer,
					    "foreground", defaults[i].color_column);
      if (defaults[i].align_column != GNC_TREE_VIEW_COLUMN_ALIGN_NONE)
	gtk_tree_view_column_add_attribute (column, renderer,
					    "xalign", defaults[i].align_column);
    }
    if (defaults[i].visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
      gtk_tree_view_column_add_attribute (column, renderer,
					  "visible", defaults[i].visibility_column);

    g_object_set_data(G_OBJECT(column), COLUMN_NUM, GINT_TO_POINTER(i));
    gtk_tree_view_append_column (view, column);

    /*
     * Set the attributes of this column
     */
    g_object_set(G_OBJECT(column),
		 "visible",     TRUE,
		 "alignment",   defaults[i].label_align,
		 "min-width",   50,  /* DRH - Should be based on title width */
		 "resizable",   TRUE,
		 "reorderable", TRUE,
		 "sizing",      defaults[i].sizing,
		 NULL);

    if (is_sortable) {
      gtk_tree_view_column_set_sort_column_id (column, i);
      if (defaults[i].sort_fn)
	gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE(s_model), i,
					 defaults[i].sort_fn,
					 NULL /* user_data */,
					 NULL /* destroy fn */);
    }

    DEBUG("Created %s column", defaults[i].pref_name);
  }
  LEAVE(" ");
}



/************************************************************/
/*         Commodity Tree View Get/Save Settings            */
/************************************************************/

static void
gnc_tree_view_common_save_sort_info (GtkTreeView *view,
				     const gchar *section,
				     GConfClient *client,
				     gnc_view_column *defaults)
{
  GtkTreeViewColumn *column;
  GtkTreeModel *s_model;
  gboolean      has_sort_column;
  gint          sort_column_id;
  GtkSortType   order;
  GEnumClass   *enum_class;
  GEnumValue   *enum_value;
  const gchar  *col_name, *enum_name;

  s_model = gtk_tree_view_get_model (view);
  has_sort_column = gtk_tree_sortable_get_sort_column_id (GTK_TREE_SORTABLE(s_model),
							  &sort_column_id,
							  &order);

  if (has_sort_column) {
    column = gtk_tree_view_get_column (view, sort_column_id);
    col_name = defaults[sort_column_id].pref_name;
  } else {
    col_name = "none"; 
    order = GTK_SORT_ASCENDING;
  }

  enum_class = g_type_class_ref (GTK_TYPE_SORT_TYPE);
  enum_value = g_enum_get_value (enum_class, order);
  enum_name  = enum_value->value_nick;

  gnc_gconf_client_set_string (client, NULL, section, "sort_column", col_name);
  gnc_gconf_client_set_string (client, NULL, section, "sort_order", enum_name);
}

void
gnc_tree_view_common_save_settings (GtkTreeView *view,
				    const gchar *section,
				    gnc_view_column *defaults)
{
  GtkTreeViewColumn *column;
  GSList *column_names = NULL;
  gchar *key;
  const gchar *tree_name;
  const gchar *name;
  gint i, num;
  GConfClient    * client;
  GError         * error;

  g_return_if_fail (GTK_IS_TREE_VIEW(view));
  g_return_if_fail (section != NULL);
  g_return_if_fail (defaults != NULL);

  tree_name = g_object_get_data(G_OBJECT(view), TREE_NAME);
  ENTER("view %p, name %s, section %s, defaults %p",
	view, tree_name, section, defaults);

  client = gconf_client_get_default ();

  for (i = 0; defaults[i].pref_name != NULL; i++) {
    column = gtk_tree_view_get_column (view, i);
    num = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), COLUMN_NUM));

    /* Save up the column names */
    name = defaults[num].pref_name;
    column_names = g_slist_append(column_names, (gpointer)name);

    /* Remember whether the column is visible */
    key = g_strdup_printf("%s/%s_visible", section, name);
    gnc_gconf_client_set_bool (client, key, NULL, NULL,
			       gtk_tree_view_column_get_visible (column));

    /* Remember whether the column width */
    key = g_strdup_printf("%s/%s_width", section, name);
    gnc_gconf_client_set_int (client, key, NULL, NULL,
			      gtk_tree_view_column_get_width (column));
  }

  gnc_tree_view_common_save_sort_info (view, section, client, defaults);

  /* Remember whether the rules hint is active */
  key = g_strdup_printf("%s/rules_hint", section);
  gnc_gconf_client_set_bool (client, key, NULL, NULL,
			     gtk_tree_view_get_rules_hint (view));

  key = g_strdup_printf("%s/column_order", section);
  if (!gconf_client_set_list (client, key, GCONF_VALUE_STRING,
			      column_names, &error)) {
    DEBUG("Failed to save order: %s", error->message);
    g_error_free (error);
  }
  g_free(key);
  LEAVE(" ");
}

static void
gnc_tree_view_common_restore_sort_info (GtkTreeView *view,
					const gchar *section,
					GConfClient *client,
					gnc_view_column *defaults)
{
  GtkTreeModel *s_model;
  gchar        *value;
  gint          sort_column_id = -1;
  GtkSortType   order = GTK_SORT_ASCENDING;
  GEnumClass   *enum_class;
  GEnumValue   *enum_value;

  if (!gnc_gconf_client_get_string (client, NULL, section, "sort_column", &value))
    return;
  if (value) {
    sort_column_id = view_column_find_by_name(defaults, value);
    g_free(value);
  }

  if (!gnc_gconf_client_get_string (client, NULL, section, "sort_order", &value))
    return;
  if (value) {
    enum_class = g_type_class_ref (GTK_TYPE_SORT_TYPE);
    enum_value = g_enum_get_value_by_nick (enum_class, value);
    if (enum_value)
      order = enum_value->value;
    g_free(value);
  }

  s_model = gtk_tree_view_get_model (view);
  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(s_model),
					sort_column_id,
					order);
}

void
gnc_tree_view_common_restore_settings (GtkTreeView *view,
				       const gchar *section,
				       gnc_view_column *defaults)
{
  GtkTreeViewColumn *column, *prev;
  const gchar *col_name;
  const gchar *tree_name;
  gchar *key;
  GSList *column_names, *tmp, *next;
  gboolean visible;
  gint i, width;
  GConfClient  *client;
  GError       *error = NULL;

  g_return_if_fail (GTK_IS_TREE_VIEW(view));
  g_return_if_fail (section != NULL);
  g_return_if_fail (defaults != NULL);

  tree_name = g_object_get_data(G_OBJECT(view), TREE_NAME);
  ENTER("view %p, name %s, section %s, defaults %p",
	view, tree_name, section, defaults);

  client = gconf_client_get_default ();

  /* Get the column order information first.  This can be used as a
   * key to see if 1) there is any information present in the users
   * gconf files, and 2) the gconf schema have been installed.  If the
   * result is a null list, neither of the two is present and this
   * routine should bail now. We'll actually swap the columns around
   * after setting up all the other information. */
  key = g_strdup_printf("%s/column_order", section);
  column_names =
    gconf_client_get_list (client, key, GCONF_VALUE_STRING, &error);
  g_free(key);
  if (column_names == NULL)
    return;

  /* Restore whether the rules hint is visible */
  key = g_strdup_printf("%s/rules_hint", section);
  if (gnc_gconf_client_get_bool (client, key, NULL, NULL, &visible)) {
    DEBUG("Setting rules hint to %d", visible);
    gtk_tree_view_set_rules_hint (view, visible);
  }

  /* Restore the sort information, if any */
  gnc_tree_view_common_restore_sort_info (view, section, client, defaults);

  /* Restore any per-column information */
  for (i = 0; defaults[i].pref_name != NULL; i++) {
    column = gtk_tree_view_get_column (view, i);
    col_name = defaults[i].pref_name;
    DEBUG("Processing column %s", col_name);

    /* visible */
    key = g_strdup_printf("%s/%s_visible", section, col_name);
    if (gnc_gconf_client_get_bool (client, key, NULL, NULL, &visible)) {
      DEBUG("  Setting %s column visibility to %d", col_name, visible);
      gtk_tree_view_column_set_visible (column, visible);
    }

    /* column width */
    key = g_strdup_printf("%s/%s_width", section, col_name);
    if (gnc_gconf_client_get_int (client, key, NULL, NULL, &width)) {
      DEBUG("  Would set %s column width to %d", col_name, width);
      // gtk_tree_view_column_set_width (column, width);
    }
  }

  /* Now swap the columns around the way the user had them. */
  DEBUG("Processing column order");

  /* First, convert from names to pointers */
  for (tmp = column_names; tmp; tmp = next) {
    next = g_slist_next(tmp);
    i = view_column_find_by_name(defaults, tmp->data);
    DEBUG("Processing %s column for order", (gchar *)tmp->data);
    g_free(tmp->data);
    if (i != -1) {
      column = gtk_tree_view_get_column (view, i);
      tmp->data = column;
    } else {
      column_names = g_slist_delete_link(column_names, tmp);
    }
  }
    
  /* Then reorder the columns */
  prev = NULL;
  for (tmp = column_names; tmp; tmp = g_slist_next(tmp)) {
    column = tmp->data;
    gtk_tree_view_move_column_after (view, column, prev);
    prev = column;
  }

  g_slist_free(column_names);

  LEAVE(" ");
}
