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

#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnome-utils/gnc-warnings.h"
#include "gnc-component-manager.h"
#include "dialog-reset-warnings.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PREFS;

#define GNC_PREFS_GROUP                 "dialogs.reset-warnings"
#define DIALOG_RESET_WARNINGS_CM_CLASS  "reset-warnings"
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
static void gnc_reset_warnings_add_section (RWDialog *rw_dialog,
                                            const gchar *section, GtkWidget *box);
static void gnc_reset_warnings_update_widgets (RWDialog *rw_dialog);


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
    const gchar *pref = NULL;
    const gchar *prefs_group = NULL;

    ENTER("widget %p, dialog %p", widget, dialog);

    if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
    {
        LEAVE("not active");
        return;
    }

    pref = gtk_widget_get_name(widget);
    prefs_group = g_object_get_data (G_OBJECT (widget), "prefs-group");
    if (prefs_group)
        gnc_prefs_reset (prefs_group, pref);
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
        gnc_reset_warnings_apply_changes(rw_dialog);
        gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(rw_dialog->dialog));
        gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS,
                                             rw_dialog);
        gtk_widget_destroy(GTK_WIDGET(rw_dialog->dialog));
        break;

    default:
        gnc_unregister_gui_component_by_data(DIALOG_RESET_WARNINGS_CM_CLASS,
                                             rw_dialog);
        gtk_widget_destroy(GTK_WIDGET(rw_dialog->dialog));
        break;
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
 *  This call back function adds a warning to the correct dialog box.
 *
 *  @internal
 *  @param rw_dialog, the data structure
 *  @param prefs_group the preference group that holds the warning status.
 *  @param warning a record with details for one warning.
 *  @param box, the required dialog box to update.
 ***********************************************************************/
static void
gnc_reset_warnings_add_one (RWDialog *rw_dialog, const gchar *prefs_group,
                            const GncWarningSpec *warning, GtkWidget *box)
{
    GtkWidget *checkbox;

    ENTER("rw_dialog %p, warning %p, box %p", rw_dialog, warning, box);

    checkbox = gtk_check_button_new_with_label( _(warning->warn_desc ? warning->warn_desc : warning->warn_name));
    if (warning->warn_long_desc)
        gtk_widget_set_tooltip_text(checkbox, _(warning->warn_long_desc));

    gtk_widget_set_name(checkbox, warning->warn_name);
    g_object_set_data_full (G_OBJECT (checkbox), "prefs-group", g_strdup(prefs_group),
                            (GDestroyNotify) g_free);
    g_signal_connect_swapped(G_OBJECT(checkbox), "toggled",
                             (GCallback)gnc_reset_warnings_update_widgets, rw_dialog);
    gtk_box_pack_start(GTK_BOX(box), checkbox, TRUE, TRUE, 0);
    LEAVE(" ");
}


/********************************************************************
 *  Add all warnings found in the given preference group
 *  to the dialog box.
 *
 *  @internal
 *  @param The reset warnings data structure
 *  @param The preference group.
 *  @param The required dialog box to update.
 ********************************************************************/
static void
gnc_reset_warnings_add_section (RWDialog *rw_dialog, const gchar *prefs_group, GtkWidget *box)
{
    const GncWarningSpec *warning = gnc_get_warnings();
    gint i = 0;

    ENTER("rw_dialog %p, section %s, box %p", rw_dialog, prefs_group, box);

    for (i = 0; warning[i].warn_name; i++)
    {
        if (gnc_prefs_get_int(prefs_group, warning[i].warn_name) != 0)
        {
            gnc_reset_warnings_add_one(rw_dialog, prefs_group, &warning[i], box);
        }
    }

    LEAVE(" ");
}


/***********************************************************************
 *  Raise the rw dialog to the top of the window stack.  This
 *  function is called if the user attempts to create a second rw
 *  dialog.
 *
 *  @internal
 *  @param class_name Unused.
 *  @param component_id Unused.
 *  @param user_data A pointer to the rw structure.
 *  @param iter_data Unused.
 ***********************************************************************/
static gboolean
show_handler (const char *class_name, gint component_id,
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
    gnc_reset_warnings_add_section(rw_dialog, GNC_PREFS_GROUP_WARNINGS_PERM, rw_dialog->perm_vbox);

    DEBUG("temporary");
    rw_dialog->temp_vbox_label = GTK_WIDGET(gtk_builder_get_object (builder, "temp_vbox_and_label"));
    rw_dialog->temp_vbox = GTK_WIDGET(gtk_builder_get_object (builder, "temp_vbox"));
    gnc_reset_warnings_add_section(rw_dialog, GNC_PREFS_GROUP_WARNINGS_TEMP, rw_dialog->temp_vbox);

    rw_dialog->buttonbox = GTK_WIDGET(gtk_builder_get_object (builder, "hbuttonbox"));

    rw_dialog->nolabel = GTK_WIDGET(gtk_builder_get_object (builder, "no_warnings"));
    rw_dialog->applybutton = GTK_WIDGET(gtk_builder_get_object (builder, "applybutton"));

    /* Enable the proper response buttons */
    gnc_reset_warnings_update_widgets(rw_dialog);

    /* Record the pointer to the rw data structure and clean up after */
    g_object_set_data_full(G_OBJECT(rw_dialog->dialog), "dialog-structure", rw_dialog, g_free);

    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(rw_dialog->dialog));

    gnc_register_gui_component (DIALOG_RESET_WARNINGS_CM_CLASS,
                                NULL, close_handler, rw_dialog);

    gtk_widget_show(GTK_WIDGET(rw_dialog->dialog));

    g_object_unref(G_OBJECT(builder));

    LEAVE(" ");
}
