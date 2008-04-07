/********************************************************************\
 * gnc-gtk-utils.c -- utility functions based on glib functions     *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include "gnc-gtk-utils.h"

#define LAST_INDEX "last_index"
#define CHANGED_ID "changed_id"


/** Find an entry in the GtkComboBoxEntry by its text value, and set
 *  the widget to that value.  This function also records the index of
 *  that text value for use when the user leaves the widget.
 *
 *  @param cbe A pointer to a GtkComboBoxEntry widget.
 *
 *  @param text The entry text to find in the model of the combo box
 *  entry. */
void
gnc_cbe_set_by_string(GtkComboBoxEntry *cbe,
		      const gchar *text)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  gchar *tree_string;
  gint column, index, id;
  gboolean match;

  model = gtk_combo_box_get_model(GTK_COMBO_BOX(cbe));
  if (!gtk_tree_model_get_iter_first(model, &iter)) {
    /* empty tree */
    gtk_combo_box_set_active(GTK_COMBO_BOX(cbe), -1);
    return;
  }

  column = gtk_combo_box_entry_get_text_column(cbe);
  do {
    gtk_tree_model_get(model, &iter, column, &tree_string, -1);
    match = g_utf8_collate(text, tree_string) == 0;
    g_free(tree_string);
    if (!match)
      continue;

    /* Found a matching string */
    id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cbe), CHANGED_ID));
    g_signal_handler_block(cbe, id);
    gtk_combo_box_set_active_iter(GTK_COMBO_BOX(cbe), &iter);
    g_signal_handler_unblock(cbe, id);

    index = gtk_combo_box_get_active(GTK_COMBO_BOX(cbe));
    g_object_set_data(G_OBJECT(cbe), LAST_INDEX, GINT_TO_POINTER(index));
    return;
  } while (gtk_tree_model_iter_next(model, &iter));
}


/**  The GtkComboBoxEntry widget has changed its value.  If the widget
 *   now points to another valid entry string then record the index of
 *   that string for use when the user leaves the widget.
 *
 *   @param widget Unused.
 *
 *   @param cbe A pointer to a GtkComboBoxEntry widget. */
static void
gnc_cbe_changed_cb (GtkComboBox *widget,
		    GtkComboBoxEntry *cbe)
{
  gint index;

  index = gtk_combo_box_get_active(widget);
  if (index == -1)
    return;
  g_object_set_data(G_OBJECT(cbe), LAST_INDEX, GINT_TO_POINTER(index));
}


/**  The completion attached to currency edit widget has selected a
 *   match.  This function extracts the completed string from the
 *   completion code's temporary model, and uses that to set the index
 *   of that currency name for use when the user leaves the widget.
 *   This should always point to a valid currency name since the user
 *   made the selection from a list of currency names.
 *
 *   @param completion Unused.
 *
 *   @param comp_model A temporary model used by completion code that
 *   contains only the current matches.
 *
 *   @param comp_iter The iter in the completion's temporary model
 *   that represents the user selected match.
 *
 *   @param cbe A pointer to a currency entry widget. */
static gboolean
gnc_cbe_match_selected_cb (GtkEntryCompletion *completion,
			   GtkTreeModel       *comp_model,
			   GtkTreeIter        *comp_iter,
			   GtkComboBoxEntry   *cbe)
{
  gint column;
  gchar *text;

  column = gtk_combo_box_entry_get_text_column(cbe);
  gtk_tree_model_get(comp_model, comp_iter, column, &text, -1);
  gnc_cbe_set_by_string(cbe, text);
  g_free(text);
  return FALSE;
}


/**  The focus left the currency edit widget, so reset the widget to
 *   its last known good value.  If the widget value contained a valid
 *   currency then this is a noop.  Otherwise the widget will be reset
 *   to the last user selected currency.  This latter state will occur
 *   if the user has typed characters directly into the widget but not
 *   selected a completion.
 *
 *   @param entry Unused.
 *
 *   @param event Unused.
 *
 *   @param cbe A pointer to a currency entry widget. */
static gboolean
gnc_cbe_focus_out_cb (GtkEntry *entry,
		      GdkEventFocus *event,
		      GtkComboBoxEntry *cbe)
{
  const gchar *text;
  gint index;

  /* Make a final attempt to match the current text. */
  text = gtk_entry_get_text(entry);
  gnc_cbe_set_by_string(cbe, text);

  /* Get the last known index (which may have just been set). */
  index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cbe), LAST_INDEX));
  gtk_combo_box_set_active(GTK_COMBO_BOX(cbe), index);
  return FALSE;
}

void
gnc_cbe_add_completion (GtkComboBoxEntry *cbe)
{
  GtkEntry *entry;
  GtkEntryCompletion *completion;
  GtkTreeModel *model;

  entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(cbe)));
  completion = gtk_entry_get_completion(entry);
  if (completion)
    return;

  /* No completion yet? Set one up. */
  completion = gtk_entry_completion_new();
  model = gtk_combo_box_get_model(GTK_COMBO_BOX(cbe));
  gtk_entry_completion_set_model(completion, model);
  gtk_entry_completion_set_text_column(completion, 0);
  gtk_entry_completion_set_inline_completion(completion, TRUE);
  gtk_entry_set_completion(entry, completion);
  g_object_unref(completion);
}

void
gnc_cbe_require_list_item (GtkComboBoxEntry *cbe)
{
  GtkEntry *entry;
  GtkEntryCompletion *completion;
  GtkTreeModel *model;
  GtkTreeIter iter;
  gint index, id;

  /* Ensure completion is set up. */
  gnc_cbe_add_completion(cbe);

  /* If an item in the combo box isn't already selected, then force
   * select the first item. Take care, the combo box may not have been
   * filled yet.  */
  entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(cbe)));
  completion = gtk_entry_get_completion(entry);
  index = gtk_combo_box_get_active(GTK_COMBO_BOX(cbe));
  if (index == -1) {
    model = gtk_entry_completion_get_model(completion);
    if (gtk_tree_model_get_iter_first(model, &iter)) {
      gtk_combo_box_set_active(GTK_COMBO_BOX(cbe), 0);
      index = 0;
    }
  }
  g_object_set_data(G_OBJECT(cbe), LAST_INDEX, GINT_TO_POINTER(index));

  /* Now the signals to make sure the user can't leave the
     widget without a valid currency. */
  id = g_signal_connect(cbe, "changed",
			G_CALLBACK(gnc_cbe_changed_cb), cbe);
  g_signal_connect(completion, "match_selected",
		   G_CALLBACK(gnc_cbe_match_selected_cb), cbe);
  g_signal_connect(entry, "focus-out-event",
		   G_CALLBACK(gnc_cbe_focus_out_cb), cbe);

  g_object_set_data(G_OBJECT(cbe), CHANGED_ID, GINT_TO_POINTER(id));
}
