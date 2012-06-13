/***********************************************************************
 * dialog-reset-warnings.c -- "Resert Warnings" dialog                 *
 * Copyright (C) 2005 David Hampton                                    *
 * Copyright (C) 2011 Robert Fewell                                    *
 *                                                                     *
 * This program is free software; you can redistribute it and/or       *
 * modify it under the terms of the GNU General Public License as      *
 * published by the Free Software Foundation; either version 2 of      *
 * the License, or (at your option) any later version.                 *
 *                                                                     *
 * This program is distributed in the hope that it will be useful,     *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of      *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
 * GNU General Public License for more details.                        *
 *                                                                     *
 * You should have received a copy of the GNU General Public License   *
 * along with this program; if not, contact:                           *
 *                                                                     *
 * Free Software Foundation           Voice:  +1-617-542-5942          *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652          *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                      *
 *                                                                     *
 **********************************************************************/

#include "config.h"

#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "gnc-component-manager.h"
#include "dialog-reset-warnings.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PREFS;

#define GCONF_SECTION                   "dialogs/reset_warnings"
#define DIALOG_RESET_WARNINGS_CM_CLASS  "reset-warnings"
#define GCONF_ENTRY_LIST                "gconf_entries"
#define TIPS_STRING                     "tips"

typedef struct
{
    GtkWidget   *dialog;
    GtkWidget   *perm_vbox_label;
    GtkWidget   *perm_vbox;
    GtkWidget   *temp_vbox_label;
    GtkWidget   *temp_vbox;
    GtkWidget   *buttonbox;
    GtkWidget   *nolabel;
    GtkWidget   *applybutton;
} RWDialog;

void gnc_reset_warnings_select_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_unselect_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_response_cb (GtkDialog *dialog, gint response, gpointer user_data);
static GSList *gnc_reset_warnings_add_section (RWDialog *rw_dialog,
        const gchar *section, GtkWidget *box);
static void gnc_reset_warnings_release_entries (GSList *entries);
static void gnc_reset_warnings_update_widgets (RWDialog *rw_dialog);
static void gnc_reset_warnings_gconf_changed (GConfClient *client, guint cnxn_id,
        GConfEntry *entry, gpointer user_data);


/****************************************************
 *  Update the Dialog Widgets
 *  @internal
 *  @param rw_dialog structure.
 ****************************************************/
static void
gnc_reset_warnings_update_widgets (RWDialog *rw_dialog)
{
    GList *list, *tmp;
    gboolean any = FALSE, checked = FALSE;

    ENTER("rw_dialog %p", rw_dialog);

    list = gtk_container_get_children(GTK_CONTAINER(rw_dialog->perm_vbox));
    if (list)
    {
        gtk_widget_show_all(rw_dialog->perm_vbox_label);
        for (tmp = list; tmp; tmp = g_list_next(tmp))
        {
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(tmp->data)))
            {
                checked = TRUE;
                break;
            }
        }
        g_list_free(list);
        any = TRUE;
    }
    else
    {
        gtk_widget_hide(rw_dialog->perm_vbox_label);
    }

    list = gtk_container_get_children(GTK_CONTAINER(rw_dialog->temp_vbox));
    if (list)
    {
        gtk_widget_show_all(rw_dialog->temp_vbox_label);
        for (tmp = list; tmp; tmp = g_list_next(tmp))
        {
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(tmp->data)))
            {
                checked = TRUE;
                break;
            }
        }
        g_list_free(list);
        any = TRUE;
    }
    else
    {
        gtk_widget_hide(rw_dialog->temp_vbox_label);
    }

    if (any)
    {
        gtk_widget_show(rw_dialog->buttonbox);
        gtk_widget_hide(rw_dialog->nolabel);
        gtk_widget_set_sensitive(rw_dialog->applybutton, checked);
    }
    else
    {
        gtk_widget_hide(rw_dialog->buttonbox);
        gtk_widget_show(rw_dialog->nolabel);
        gtk_widget_set_sensitive(rw_dialog->applybutton, FALSE);
    }
    LEAVE(" ");
}


/***************************/
/*  Helper functions       */
/***************************/
static void
gnc_reset_warnings_apply_one (GtkWidget *widget,
                              GtkDialog *dialog)
{
    const char *name;

    ENTER("widget %p, dialog %p", widget, dialog);

    if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
    {
        LEAVE("not active");
        return;
    }

    name = gtk_widget_get_name(widget);
    gnc_gconf_unset(NULL, name, NULL);
    gtk_widget_destroy(widget);
    LEAVE(" ");
}


static void
gnc_reset_warnings_apply_changes (RWDialog *rw_dialog)
{
    ENTER("rw_dialog %p", rw_dialog);

    gtk_container_foreach(GTK_CONTAINER(rw_dialog->perm_vbox),
                          (GtkCallback)gnc_reset_warnings_apply_one,
                          rw_dialog->dialog);

    gtk_container_foreach(GTK_CONTAINER(rw_dialog->temp_vbox),
                          (GtkCallback)gnc_reset_warnings_apply_one,
                          rw_dialog->dialog);
    gnc_reset_warnings_update_widgets(rw_dialog);
    LEAVE(" ");
}


static void
gnc_reset_warnings_revert_changes (RWDialog *rw_dialog)
{
    GSList *entries, *tmp;
    GConfEntry *entry;

    ENTER("rw_dialog %p", rw_dialog);

    entries = g_object_get_data(G_OBJECT(rw_dialog->dialog), GCONF_ENTRY_LIST);
    for (tmp = entries; tmp; tmp = g_slist_next(tmp))
    {
        entry = tmp->data;
        gnc_gconf_set_int (NULL, entry->key,
                           gconf_value_get_int(entry->value), NULL);
    }
    LEAVE(" ");
}


/***************************/
/*    Dialog Callbacks     */
/***************************/
void
gnc_reset_warnings_response_cb (GtkDialog *dialog,
                                gint response,
                                gpointer user_data)
{
    RWDialog *rw_dialog = user_data;

    ENTER("dialog %p, response %d, user_data %p", dialog, response, user_data);

    switch (response)
    {
    case GTK_RESPONSE_APPLY:
        gnc_reset_warnings_apply_changes(rw_dialog);
        break;

    case GTK_RESPONSE_OK:
        gnc_gconf_remove_notification(G_OBJECT(rw_dialog->dialog), GCONF_WARNINGS,
                                      DIALOG_RESET_WARNINGS_CM_CLASS);
        gnc_reset_warnings_apply_changes(rw_dialog);
        gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(rw_dialog->dialog));
        gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS,
                                             rw_dialog);
        gtk_widget_destroy(GTK_WIDGET(rw_dialog->dialog));
        break;

    default:
        gnc_gconf_remove_notification(G_OBJECT(rw_dialog->dialog), GCONF_WARNINGS,
                                      DIALOG_RESET_WARNINGS_CM_CLASS);
        gnc_reset_warnings_revert_changes(rw_dialog);
        gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS,
                                             rw_dialog);
        gtk_widget_destroy(GTK_WIDGET(rw_dialog->dialog));
    }
    LEAVE("");
}


static void
gnc_reset_warnings_select_common (RWDialog *rw_dialog,
                                  gboolean selected)
{
    ENTER("rw_dialog %p, selected %d", rw_dialog, selected);

    gtk_container_foreach(GTK_CONTAINER(rw_dialog->perm_vbox),
                          (GtkCallback)gtk_toggle_button_set_active,
                          GINT_TO_POINTER(selected));

    gtk_container_foreach(GTK_CONTAINER(rw_dialog->temp_vbox),
                          (GtkCallback)gtk_toggle_button_set_active,
                          GINT_TO_POINTER(selected));
    gnc_reset_warnings_update_widgets(rw_dialog);
    LEAVE(" ");
}


void
gnc_reset_warnings_select_all_cb (GtkButton *button,
                                  gpointer user_data)
{
    RWDialog *rw_dialog = user_data;
    gnc_reset_warnings_select_common(rw_dialog, TRUE);
}


void
gnc_reset_warnings_unselect_all_cb (GtkButton *button,
                                    gpointer user_data)
{
    RWDialog *rw_dialog = user_data;
    gnc_reset_warnings_select_common(rw_dialog, FALSE);
}


/***********************************************************************
 *  This call back function adds an entry to the correct dialog box.
 *
 *  @internal
 *  @param rw_dialog, the data structure
 *  @param gconf entry.
 *  @param box, the required dialog box to update.
 ***********************************************************************/
static void
gnc_reset_warnings_add_one (RWDialog *rw_dialog, GConfEntry *entry, GtkWidget *box)
{
    const gchar *name, *schema_name, *desc, *long_desc = NULL;
    GtkWidget *checkbox;
    GConfSchema *schema = NULL;

    ENTER("rw_dialog %p, entry %p, box %p", rw_dialog, entry, box);

    name = strrchr(entry->key, '/') + 1;
    schema_name = gconf_entry_get_schema_name(entry);
    if (schema_name)
        schema = gnc_gconf_get_schema(NULL, schema_name, NULL);
    if (schema)
    {
        DEBUG("found schema %p", schema);
        desc = gconf_schema_get_short_desc(schema);
        DEBUG("description %s", desc);
        long_desc = gconf_schema_get_long_desc(schema);
        checkbox = gtk_check_button_new_with_label(desc ? desc : name);
        if (long_desc)
            gtk_widget_set_tooltip_text(checkbox, long_desc);
        gconf_schema_free(schema);
    }
    else
    {
        DEBUG("no schema");
        checkbox = gtk_check_button_new_with_label(name);
    }

    gtk_widget_set_name(checkbox, entry->key);
    g_signal_connect_swapped(G_OBJECT(checkbox), "toggled",
                             (GCallback)gnc_reset_warnings_update_widgets, rw_dialog);
    gtk_box_pack_start(GTK_BOX(box), checkbox, TRUE, TRUE, 0);
    LEAVE(" ");
}


/********************************************************************
 *  This call back function adds the gconf section
 *  to the dialog box.
 *
 *  @internal
 *  @param The reset warnings data structure
 *  @param The section in gconf.
 *  @param The required dialog box to update.
 ********************************************************************/
static GSList *
gnc_reset_warnings_add_section (RWDialog *rw_dialog, const gchar *section, GtkWidget *box)
{
    GSList *entries, *tmp;
    GConfEntry *entry;

    ENTER("rw_dialog %p, section %s, box %p", rw_dialog, section, box);

    entries = gnc_gconf_client_all_entries(section);
    for (tmp = entries; tmp; tmp = g_slist_next(tmp))
    {
        entry = tmp->data;
        if (gconf_value_get_int(entry->value) != 0)
        {
            gnc_reset_warnings_add_one(rw_dialog, entry, box);
        }
    }

    LEAVE(" ");
    return entries;
}


/****************************************
 * Reset Functions for closure
 ****************************************/
static void
gnc_reset_warnings_release_entries (GSList *entries)
{
    GSList *tmp;

    ENTER(" ");
    for (tmp = entries; tmp; tmp = g_slist_next(tmp))
    {
        gconf_entry_free(tmp->data);
    }
    g_slist_free(entries);
    LEAVE(" ");
}


static void
gnc_reset_warnings_find_remove (GtkWidget *widget,
                                const gchar *name)
{
    ENTER("widget %p, name %s", widget, name);

    if (strcmp(gtk_widget_get_name(widget), name) == 0)
    {
        DEBUG("destroying widget %s", name);
        gtk_widget_destroy(widget);
    }
    LEAVE(" ");
}


/***********************************************************************
 *  This call back function is triggered when warning gconf entries
 *  are changed.
 *
 *  @internal
 *  @param The gconf client unused.
 *  @param The gconf client connection id.
 *  @param The gconf entry.
 *  @param The user_data points to the dialog.
 ***********************************************************************/
static void
gnc_reset_warnings_gconf_changed (GConfClient *client,
                                  guint cnxn_id,
                                  GConfEntry *entry,
                                  gpointer user_data)
{
    RWDialog *rw_dialog = g_object_get_data(G_OBJECT(user_data), "dialog-structure");

    GtkWidget *box;
    GList     *list;

    ENTER("rw_dialog %p, entry %p, user_data %p", rw_dialog, entry, user_data);

    g_return_if_fail(GTK_IS_DIALOG(rw_dialog->dialog));

    DEBUG("entry key '%s', value as %p, value as int %d", entry->key, entry->value, gconf_value_get_int(entry->value));

    /* Which box is affected */
    if (strstr(entry->key, "permanent") != 0)
    {
        box = rw_dialog->perm_vbox;
    }
    else
    {
        box = rw_dialog->temp_vbox;
    }

    if (gconf_value_get_int(entry->value) != 0)
    {
        gnc_reset_warnings_add_one (rw_dialog, entry, box);
        DEBUG("added checkbox for %s", entry->key);
    }
    else
    {
        /* Don't know if we were invoked by the dialog removing the
         * warning, or if the remove happened somewhere else like
         * gconf-editor.  Can't hurt to run the widgets and try to remove
         * it.  Worst case we can't find it because its already been
         * deleted. */
        list = gtk_container_get_children(GTK_CONTAINER(box));
        g_list_foreach(list, (GFunc)gnc_reset_warnings_find_remove, entry->key);
        g_list_free(list);
    }
    gnc_reset_warnings_update_widgets(rw_dialog);
    LEAVE(" ");
}


/***********************************************************************
 *  Raise the rw dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second rw
 *  dialog.
 *
 *  @internal
 *  @param class Unused.
 *  @param component_id Unused.
 *  @param user_data A pointer to the rw structure.
 *  @param iter_data Unused.
 ***********************************************************************/
static gboolean
show_handler (const char *class, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    RWDialog *rw_dialog = user_data;

    ENTER(" ");
    if (!rw_dialog)
    {
        LEAVE("no data strucure");
        return(FALSE);
    }

    ENTER(" ");
    gtk_window_present(GTK_WINDOW(rw_dialog->dialog));
    LEAVE(" ");

    return(TRUE);
}


/****************************************************
 *  Close the reset warnings dialog.
 *  @internal
 *  @param user_data A pointer to the rw structure.
 ****************************************************/
static void
close_handler (gpointer user_data)
{
    RWDialog *rw_dialog = user_data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS, rw_dialog);
    gtk_widget_destroy(rw_dialog->dialog);
    LEAVE(" ");
}


/***********************************************/
/*     Create the Reset Warnings Dialog        */
/***********************************************/
void
gnc_reset_warnings_dialog (GtkWindow *parent)
{
    RWDialog   *rw_dialog;
    GtkWidget  *dialog;
    GtkBuilder *builder;

    GSList *perm_list, *temp_list;

    rw_dialog = g_new0 (RWDialog, 1);

    ENTER("");
    if (gnc_forall_gui_components(DIALOG_RESET_WARNINGS_CM_CLASS,
                                  show_handler, NULL))
    {
        LEAVE("existing window");
        return;
    }

    DEBUG("Opening dialog-reset-warnings.glade:");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-reset-warnings.glade", "Reset Warnings");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Reset Warnings"));

    gtk_window_set_transient_for(GTK_WINDOW (dialog), parent);

    rw_dialog->dialog = dialog;
    PINFO("rw_dialog %p, dialog %p", rw_dialog, dialog);

    /* Connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, rw_dialog);

    DEBUG("permanent");
    rw_dialog->perm_vbox_label = GTK_WIDGET(gtk_builder_get_object (builder, "perm_vbox_and_label"));
    rw_dialog->perm_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "perm_vbox"));
    perm_list = gnc_reset_warnings_add_section(rw_dialog, GCONF_WARNINGS_PERM, rw_dialog->perm_vbox);

    DEBUG("temporary");
    rw_dialog->temp_vbox_label = GTK_WIDGET(gtk_builder_get_object (builder, "temp_vbox_and_label"));
    rw_dialog->temp_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "temp_vbox"));
    temp_list = gnc_reset_warnings_add_section(rw_dialog, GCONF_WARNINGS_TEMP, rw_dialog->temp_vbox);

    rw_dialog->buttonbox = GTK_WIDGET(gtk_builder_get_object (builder, "hbuttonbox"));

    rw_dialog->nolabel = GTK_WIDGET(gtk_builder_get_object (builder, "no_warnings"));
    rw_dialog->applybutton = GTK_WIDGET(gtk_builder_get_object (builder, "applybutton"));

    g_object_set_data_full(G_OBJECT(rw_dialog->dialog), GCONF_ENTRY_LIST,
                           g_slist_concat (perm_list, temp_list),
                           (GDestroyNotify)gnc_reset_warnings_release_entries);

    /* Populate the dialog boxes with the gconf entries */
    gnc_reset_warnings_update_widgets(rw_dialog);

    /* Record the pointer to the rw data structure and claen up after */
    g_object_set_data_full(G_OBJECT(rw_dialog->dialog), "dialog-structure", rw_dialog, g_free);

    gnc_gconf_add_notification(G_OBJECT(rw_dialog->dialog), GCONF_WARNINGS,
                               gnc_reset_warnings_gconf_changed,
                               DIALOG_RESET_WARNINGS_CM_CLASS);

    gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(rw_dialog->dialog));

    gnc_register_gui_component (DIALOG_RESET_WARNINGS_CM_CLASS,
                                NULL, close_handler, rw_dialog);

    gtk_widget_show(GTK_WIDGET(rw_dialog->dialog));

    g_object_unref(G_OBJECT(builder));

    LEAVE(" ");
}
