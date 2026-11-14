/********************************************************************\
 * dialog-object-references.c -- dialog for displaying a list of    *
 *                               objects which refer to another     *
 *                               specific object                    *
 *                                                                  *
 * Copyright (C) 2010 Phil Longstaff (plongstaff@rogers.com)        *
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
 * You should have received a copy of the GNU General Public License *
 * along with this program; if not, see                            *
 * <https://www.gnu.org/licenses/>.                                *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-ui.h"
#include "gnc-path.h"
#include "dialog-utils.h"
#include "dialog-object-references.h"

struct _GncObjectReferencesDialog
{
    GtkDialog parent_instance;
    GtkLabel *lbl_explanation;
    GtkWidget *hbox_list;
    GtkTreeView *listview;
};

enum
{
    PROP_0,
    PROP_EXPLANATION,
    PROP_OBJECT_NAMES
};

G_DEFINE_TYPE (GncObjectReferencesDialog, gnc_object_references_dialog, GTK_TYPE_DIALOG)

static void
gnc_object_references_dialog_init (GncObjectReferencesDialog *self)
{
    gtk_widget_init_template (GTK_WIDGET (self));
    gtk_widget_set_name (GTK_WIDGET (self), "gnc-id-object-reference");
    GtkListStore *store = gtk_list_store_new (1, G_TYPE_STRING);
    self->listview = GTK_TREE_VIEW (gtk_tree_view_new_with_model (GTK_TREE_MODEL (store)));
    g_object_unref (store);
    GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
    GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes ("Object", renderer, "text", 0, NULL);
    gtk_tree_view_append_column (self->listview, column);
    gtk_container_add (GTK_CONTAINER (self->hbox_list), GTK_WIDGET (self->listview));
}

static void
gnc_object_references_dialog_set_property (GObject *object, guint prop_id,
                                           const GValue *value, GParamSpec *pspec)
{
    GncObjectReferencesDialog *self = GNC_OBJECT_REFERENCES_DIALOG (object);
    switch (prop_id)
    {
    case PROP_EXPLANATION:
        gtk_label_set_text (self->lbl_explanation, g_value_get_string (value));
        break;
    case PROP_OBJECT_NAMES:
    {
        GtkListStore *store = GTK_LIST_STORE (gtk_tree_view_get_model (self->listview));
        const gchar * const *names = g_value_get_boxed (value);
        for (const gchar * const *name = names; name && *name; ++name)
        {
            GtkTreeIter iter;
            gtk_list_store_append (store, &iter);
            gtk_list_store_set (store, &iter, 0, *name, -1);
        }
        break;
    }
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
gnc_object_references_dialog_get_property (GObject *object, guint prop_id,
                                           GValue *value, GParamSpec *pspec)
{
    GncObjectReferencesDialog *self = GNC_OBJECT_REFERENCES_DIALOG (object);
    switch (prop_id)
    {
    case PROP_EXPLANATION:
        g_value_set_string (value, gtk_label_get_text (self->lbl_explanation));
        break;
    case PROP_OBJECT_NAMES:
    {
        GStrvBuilder *names = g_strv_builder_new ();
        GtkTreeModel *model = gtk_tree_view_get_model (self->listview);
        GtkTreeIter iter;
        for (gboolean valid = gtk_tree_model_get_iter_first (model, &iter); valid;
             valid = gtk_tree_model_iter_next (model, &iter))
        {
            gchar *name = NULL;
            gtk_tree_model_get (model, &iter, 0, &name, -1);
            g_strv_builder_take (names, name);
        }
        g_value_take_boxed (value, g_strv_builder_unref_to_strv (names));
        break;
    }
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
    }
}

static void
gnc_object_references_dialog_class_init (GncObjectReferencesDialogClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    object_class->set_property = gnc_object_references_dialog_set_property;
    object_class->get_property = gnc_object_references_dialog_get_property;
    g_object_class_install_property (object_class, PROP_EXPLANATION,
        g_param_spec_string ("explanation", "Explanation", "Explanation of the object references", "",
                             G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY | G_PARAM_STATIC_STRINGS));
    g_object_class_install_property (object_class, PROP_OBJECT_NAMES,
        g_param_spec_boxed ("object-names", "Object names", "Display names in list order", G_TYPE_STRV,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY | G_PARAM_STATIC_STRINGS));

    gchar *directory = gnc_path_get_gtkbuilderdir ();
    gchar *filename = g_build_filename (directory, "dialog-object-references.glade", NULL);
    gchar *contents = NULL;
    gsize length = 0;
    GError *error = NULL;
    if (!g_file_get_contents (filename, &contents, &length, &error))
        g_error ("Could not load object references dialog: %s", error->message);
    GBytes *template = g_bytes_new_take (contents, length);
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);
    gtk_widget_class_set_template (widget_class, template);
    g_bytes_unref (template);
    g_free (filename);
    g_free (directory);
    gtk_widget_class_bind_template_child (widget_class, GncObjectReferencesDialog, lbl_explanation);
    gtk_widget_class_bind_template_child (widget_class, GncObjectReferencesDialog, hbox_list);
}

GStrv
gnc_object_references_get_display_names (const GList *objects)
{
    GStrvBuilder *names = g_strv_builder_new ();
    for (const GList *node = objects; node; node = node->next)
        g_strv_builder_take (names, qof_instance_get_display_name (node->data));
    return g_strv_builder_unref_to_strv (names);
}

void
gnc_ui_object_references_show (const gchar *explanation_text, GList *objlist)
{
    GStrv names = gnc_object_references_get_display_names (objlist);
    GtkWidget *dialog = g_object_new (GNC_TYPE_OBJECT_REFERENCES_DIALOG,
                                      "explanation", explanation_text,
                                      "object-names", names, NULL);
    g_strfreev (names);
    gtk_widget_show_all (dialog);
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
}
