/*
 * dialog-reset-warnings.c -- "Resert Warnings" dialog
 * Copyright (C) 2005 David Hampton
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glade/glade.h>

#include "dialog-utils.h"
#include "gnc-gconf-utils.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "gnc-component-manager.h"
#include "dialog-reset-warnings.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PREFS;

#define GCONF_SECTION			"dialogs/reset_warnings"
#define DIALOG_RESET_WARNINGS_CM_CLASS	"reset-warnings"
#define GCONF_ENTRY_LIST		"gconf_entries"
#define TIPS_STRING                     "tips"

void gnc_reset_warnings_select_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_unselect_all_cb (GtkButton *button, gpointer user_data);
void gnc_reset_warnings_response_cb (GtkDialog *dialog, gint arg1, gpointer user_data);



static void
gnc_reset_warnings_update_widgets (GtkWidget *dialog_widget)
{
    GtkWidget *box1, *box2, *nada, *buttons;
    GtkWidget *apply;
    GList *list, *tmp;
    gboolean any = FALSE, checked = FALSE;

    ENTER(" ");
    box1 = gnc_glade_lookup_widget(dialog_widget, "perm_vbox_and_label");
    box2 = gnc_glade_lookup_widget(dialog_widget, "perm_vbox");
    list = gtk_container_get_children(GTK_CONTAINER(box2));
    if (list)
    {
        gtk_widget_show_all(box1);
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
        gtk_widget_hide(box1);
    }

    box1 = gnc_glade_lookup_widget(dialog_widget, "temp_vbox_and_label");
    box2 = gnc_glade_lookup_widget(dialog_widget, "temp_vbox");
    list = gtk_container_get_children(GTK_CONTAINER(box2));
    if (list)
    {
        gtk_widget_show_all(box1);
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
        gtk_widget_hide(box1);
    }

    nada = gnc_glade_lookup_widget(dialog_widget, "no_warnings");
    buttons = gnc_glade_lookup_widget(dialog_widget, "hbuttonbox");
    apply = gnc_glade_lookup_widget(dialog_widget, "applybutton");
    if (any)
    {
        gtk_widget_show(buttons);
        gtk_widget_hide(nada);
        gtk_widget_set_sensitive(apply, checked);
    }
    else
    {
        gtk_widget_hide(buttons);
        gtk_widget_show(nada);
        gtk_widget_set_sensitive(apply, FALSE);
    }
    LEAVE(" ");
}


static void
gnc_reset_warnings_select_common (GtkButton *button,
                                  gboolean selected)
{
    GtkWidget *vbox;

    ENTER("button %p, selected %d", button, selected);
    vbox = gnc_glade_lookup_widget(GTK_WIDGET(button), "perm_vbox");
    gtk_container_foreach(GTK_CONTAINER(vbox),
                          (GtkCallback)gtk_toggle_button_set_active,
                          GINT_TO_POINTER(selected));
    vbox = gnc_glade_lookup_widget(GTK_WIDGET(button), "temp_vbox");
    gtk_container_foreach(GTK_CONTAINER(vbox),
                          (GtkCallback)gtk_toggle_button_set_active,
                          GINT_TO_POINTER(selected));
    gnc_reset_warnings_update_widgets(GTK_WIDGET(button));
    LEAVE(" ");
}


void
gnc_reset_warnings_select_all_cb (GtkButton *button,
                                  gpointer user_data)
{
    gnc_reset_warnings_select_common(button, TRUE);
}


void
gnc_reset_warnings_unselect_all_cb (GtkButton *button,
                                    gpointer user_data)
{
    gnc_reset_warnings_select_common(button, FALSE);
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
gnc_reset_warnings_apply_changes (GtkDialog *dialog)
{
    GtkWidget *vbox;

    ENTER("dialog %p", dialog);
    vbox = gnc_glade_lookup_widget(GTK_WIDGET(dialog), "perm_vbox");
    gtk_container_foreach(GTK_CONTAINER(vbox),
                          (GtkCallback)gnc_reset_warnings_apply_one,
                          dialog);
    vbox = gnc_glade_lookup_widget(GTK_WIDGET(dialog), "temp_vbox");
    gtk_container_foreach(GTK_CONTAINER(vbox),
                          (GtkCallback)gnc_reset_warnings_apply_one,
                          dialog);
    gnc_reset_warnings_update_widgets(GTK_WIDGET(dialog));
    LEAVE(" ");
}


static void
gnc_reset_warnings_revert_changes (GtkDialog *dialog)
{
    GSList *entries, *tmp;
    GConfEntry *entry;

    ENTER("dialog %p", dialog);

    entries = g_object_get_data(G_OBJECT(dialog), GCONF_ENTRY_LIST);
    for (tmp = entries; tmp; tmp = g_slist_next(tmp))
    {
        entry = tmp->data;
        gnc_gconf_set_int (NULL, entry->key,
                           gconf_value_get_int(entry->value), NULL);
    }
    LEAVE(" ");
}


void
gnc_reset_warnings_response_cb (GtkDialog *dialog,
                                gint response,
                                gpointer user_data)
{
    switch (response)
    {
    case GTK_RESPONSE_APPLY:
        gnc_reset_warnings_apply_changes(dialog);
        break;

    case GTK_RESPONSE_OK:
        gnc_gconf_remove_notification(G_OBJECT(dialog), GCONF_WARNINGS,
                                      DIALOG_RESET_WARNINGS_CM_CLASS);
        gnc_reset_warnings_apply_changes(dialog);
        gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(dialog));
        gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS,
                                             dialog);
        gtk_widget_destroy(GTK_WIDGET(dialog));
        break;

    default:
        gnc_gconf_remove_notification(G_OBJECT(dialog), GCONF_WARNINGS,
                                      DIALOG_RESET_WARNINGS_CM_CLASS);
        gnc_reset_warnings_revert_changes(dialog);
        gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS,
                                             dialog);
        gtk_widget_destroy(GTK_WIDGET(dialog));
    }
}

static void
gnc_reset_warnings_add_one (GConfEntry *entry, GtkWidget *box)
{
    const gchar *name, *schema_name, *desc, *long_desc = NULL;
    GtkWidget *checkbox;
    GConfSchema *schema = NULL;

    ENTER(" ");
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
        {
            GtkTooltips *tips;
            tips = g_object_get_data(G_OBJECT(box), TIPS_STRING);
            if (!tips)
            {
                tips = gtk_tooltips_new();
                g_object_set_data(G_OBJECT(box), TIPS_STRING, tips);
            }
            gtk_tooltips_set_tip(tips, checkbox, long_desc, NULL);
        }
        gconf_schema_free(schema);
    }
    else
    {
        DEBUG("no schema");
        checkbox = gtk_check_button_new_with_label(name);
    }

    gtk_widget_set_name(checkbox, entry->key);
    g_signal_connect_swapped(G_OBJECT(checkbox), "toggled",
                             (GCallback)gnc_reset_warnings_update_widgets,
                             box);
    gtk_box_pack_start_defaults(GTK_BOX(box), checkbox);
    LEAVE(" ");
}


static GSList *
gnc_reset_warnings_add_section (const gchar *section, GtkWidget *box)
{
    GSList *entries, *tmp;
    GConfEntry *entry;

    ENTER(" ");
    entries = gnc_gconf_client_all_entries(section);
    for (tmp = entries; tmp; tmp = g_slist_next(tmp))
    {
        entry = tmp->data;
        if (gconf_value_get_int(entry->value) != 0)
        {
            gnc_reset_warnings_add_one(entry, box);
        }
    }

    LEAVE(" ");
    return entries;
}


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
gnc_reset_warnings_gconf_changed (GConfClient *client,
                                  guint cnxn_id,
                                  GConfEntry *entry,
                                  gpointer user_data)
{
    GtkWidget *dialog, *box;
    GList *list;

    g_return_if_fail(GTK_IS_DIALOG(user_data));

    ENTER("entry %p, data %p", entry, user_data);
    dialog = GTK_WIDGET(user_data);
    DEBUG("entry key '%s', value as %p, value as int %d", entry->key, entry->value, gconf_value_get_int(entry->value));

    /* Which box is affected */
    if (strstr(entry->key, "permanent") != 0)
    {
        box = gnc_glade_lookup_widget(GTK_WIDGET(dialog), "perm_vbox");
    }
    else
    {
        box = gnc_glade_lookup_widget(GTK_WIDGET(dialog), "temp_vbox");
    }

    if (gconf_value_get_int(entry->value) != 0)
    {
        gnc_reset_warnings_add_one (entry, box);
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
    gnc_reset_warnings_update_widgets(dialog);
    LEAVE(" ");
}


static gboolean
show_handler (const char *class, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    GtkWidget *dialog;

    ENTER(" ");
    dialog = GTK_WIDGET(user_data);
    gtk_window_present(GTK_WINDOW(dialog));
    LEAVE(" ");
    return(TRUE);
}


static void
close_handler (gpointer user_data)
{
    GtkWidget *dialog = user_data;

    ENTER(" ");
    dialog = GTK_WIDGET(user_data);
    gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS, dialog);
    gtk_widget_destroy(dialog);
    LEAVE(" ");
}


void
gnc_reset_warnings_dialog (GtkWidget *main_window)
{
    GtkWidget *dialog, *box;
    GladeXML *xml;
    GSList *perm_list, *temp_list;

    ENTER("");
    if (gnc_forall_gui_components(DIALOG_RESET_WARNINGS_CM_CLASS,
                                  show_handler, NULL))
    {
        LEAVE("existing window");
        return;
    }

    DEBUG("Opening dialog-reset-warnings.glade:");
    xml = gnc_glade_xml_new("dialog-reset-warnings.glade", "Reset Warnings");
    dialog = glade_xml_get_widget(xml, "Reset Warnings");
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
                                      dialog);

    DEBUG("permanent");
    box = glade_xml_get_widget(xml, "perm_vbox");
    perm_list = gnc_reset_warnings_add_section(GCONF_WARNINGS_PERM, box);

    DEBUG("temporary");
    box = glade_xml_get_widget(xml, "temp_vbox");
    temp_list = gnc_reset_warnings_add_section(GCONF_WARNINGS_TEMP, box);

    g_object_set_data_full(G_OBJECT(dialog), GCONF_ENTRY_LIST,
                           g_slist_concat (perm_list, temp_list),
                           (GDestroyNotify)gnc_reset_warnings_release_entries);

    gnc_reset_warnings_update_widgets(dialog);

    gnc_gconf_add_notification(G_OBJECT(dialog), GCONF_WARNINGS,
                               gnc_reset_warnings_gconf_changed,
                               DIALOG_RESET_WARNINGS_CM_CLASS);

    gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(dialog));

    gnc_register_gui_component (DIALOG_RESET_WARNINGS_CM_CLASS,
                                NULL, close_handler, dialog);

    gtk_widget_show(dialog);
    LEAVE(" ");
}
