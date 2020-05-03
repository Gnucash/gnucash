/*
 * dialog-order.c -- Dialog for Order entry
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdint.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnucash-register.h"
#include "gnucash-sheet.h"
#include "dialog-search.h"
#include "search-param.h"

#include "gncOrder.h"
#include "gncOrderP.h"

#include "gncEntryLedger.h"

#include "dialog-order.h"
#include "dialog-invoice.h"
#include "business-gnome-utils.h"
#include "dialog-date-close.h"
#include "gnc-general-search.h"

#define DIALOG_NEW_ORDER_CM_CLASS "dialog-new-order"
#define DIALOG_EDIT_ORDER_CM_CLASS "dialog-edit-order"
#define DIALOG_VIEW_ORDER_CM_CLASS "dialog-view-order"

#define GNC_PREFS_GROUP_SEARCH "dialogs.business.order-search"

void gnc_order_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_order_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_order_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_order_window_invoice_cb (GtkWidget *widget, gpointer data);
void gnc_order_window_close_order_cb (GtkWidget *widget, gpointer data);
void gnc_order_window_destroy_cb (GtkWidget *widget, gpointer data);

typedef enum
{
    NEW_ORDER,
    EDIT_ORDER,
    VIEW_ORDER
} OrderDialogType;

struct _order_select_window
{
    QofBook  *book;
    GncOwner *owner;
    QofQuery *q;
    GncOwner  owner_def;
};

struct _order_window
{
    GtkWidget *	dialog;

    GtkWidget *	id_entry;
    GtkWidget *	ref_entry;
    GtkWidget *	notes_text;
    GtkWidget *	opened_date;
    GtkWidget *	closed_date;
    GtkWidget *	active_check;

    GtkWidget * cd_label;
    GtkWidget * close_order_button;

    GtkWidget *	owner_box;
    GtkWidget *	owner_label;
    GtkWidget *	owner_choice;

    GnucashRegister *	reg;
    GncEntryLedger *	ledger;

    OrderDialogType	dialog_type;
    GncGUID		order_guid;
    gint		component_id;
    QofBook *	book;
    GncOrder *	created_order;
    GncOwner	owner;

};

static void gnc_order_update_window (OrderWindow *ow);

static GncOrder *
ow_get_order (OrderWindow *ow)
{
    if (!ow)
        return NULL;

    return gncOrderLookup (ow->book, &ow->order_guid);
}

static void gnc_ui_to_order (OrderWindow *ow, GncOrder *order)
{
    GtkTextBuffer* text_buffer;
    GtkTextIter start, end;
    gchar *text;
    time64 tt;

    /* Do nothing if this is view only */
    if (ow->dialog_type == VIEW_ORDER)
        return;

    gnc_suspend_gui_refresh ();
    gncOrderBeginEdit (order);

    gncOrderSetID (order, gtk_editable_get_chars
                   (GTK_EDITABLE (ow->id_entry), 0, -1));

    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(ow->notes_text));
    gtk_text_buffer_get_bounds (text_buffer, &start, &end);
    text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
    gncOrderSetNotes (order, text);

    gncOrderSetReference (order, gtk_editable_get_chars
                          (GTK_EDITABLE (ow->ref_entry), 0, -1));

    tt = gnc_date_edit_get_date (GNC_DATE_EDIT (ow->opened_date));
    gncOrderSetDateOpened (order, tt);

    if (ow->active_check)
        gncOrderSetActive (order, gtk_toggle_button_get_active
                           (GTK_TOGGLE_BUTTON (ow->active_check)));

    gnc_owner_get_owner (ow->owner_choice, &(ow->owner));
    gncOrderSetOwner (order, &(ow->owner));

    gncOrderCommitEdit (order);
    gnc_resume_gui_refresh ();
}

static gboolean
gnc_order_window_verify_ok (OrderWindow *ow)
{
    const char *res;

    /* Check the ID */
    res = gtk_entry_get_text (GTK_ENTRY (ow->id_entry));
    if (g_strcmp0 (res, "") == 0)
    {
        gnc_error_dialog (GTK_WINDOW (ow->dialog), "%s",
                          _("The Order must be given an ID."));
        return FALSE;
    }

    /* Check the Owner */
    gnc_owner_get_owner (ow->owner_choice, &(ow->owner));
    res = gncOwnerGetName (&(ow->owner));
    if (res == NULL || g_strcmp0 (res, "") == 0)
    {
        gnc_error_dialog (GTK_WINDOW (ow->dialog), "%s",
                          _("You need to supply Billing Information."));
        return FALSE;
    }

    return TRUE;
}

static gboolean
gnc_order_window_ok_save (OrderWindow *ow)
{
    if (!gnc_entry_ledger_check_close (ow->dialog, ow->ledger))
        return FALSE;

    if (!gnc_order_window_verify_ok (ow))
        return FALSE;

    /* Now save it off */
    {
        GncOrder *order = ow_get_order (ow);
        if (order)
        {
            gnc_ui_to_order (ow, order);

        }
        ow->created_order = order;
    }
    return TRUE;
}

void
gnc_order_window_ok_cb (GtkWidget *widget, gpointer data)
{
    OrderWindow *ow = data;

    if (!gnc_order_window_ok_save (ow))
        return;

    /* Ok, we don't need this anymore */
    ow->order_guid = *guid_null ();

    gnc_close_gui_component (ow->component_id);
}

void
gnc_order_window_cancel_cb (GtkWidget *widget, gpointer data)
{
    OrderWindow *ow = data;

    gnc_close_gui_component (ow->component_id);
}

void
gnc_order_window_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help(HF_HELP, HL_USAGE_BILL);
}

void
gnc_order_window_invoice_cb (GtkWidget *widget, gpointer data)
{
    OrderWindow *ow = data;

    /* make sure we're ok */
    if (!gnc_order_window_verify_ok (ow))
        return;

    /* Ok, go make an invoice */
    gnc_invoice_search (gtk_window_get_transient_for(GTK_WINDOW(ow->dialog)), NULL, &(ow->owner), ow->book);

    /* refresh the window */
    gnc_order_update_window (ow);
}

void
gnc_order_window_close_order_cb (GtkWidget *widget, gpointer data)
{
    OrderWindow *ow = data;
    GncOrder *order;
    GList *entries;
    char *message, *label;
    gboolean non_inv = FALSE;
    time64 t = gnc_time (NULL);

    /* Make sure the order is ok */
    if (!gnc_order_window_verify_ok (ow))
        return;

    /* Make sure the order exists */
    order = ow_get_order (ow);
    if (!order)
        return;

    /* Check that there is at least one Entry */
    if (gncOrderGetEntries (order) == NULL)
    {
        gnc_error_dialog (GTK_WINDOW (ow->dialog), "%s",
                          _("The Order must have at least one Entry."));
        return;
    }

    /* Make sure we can close the order. Are there any uninvoiced entries? */
    entries = gncOrderGetEntries (order);
    for ( ; entries ; entries = entries->next)
    {
        GncEntry *entry = entries->data;
        if (gncEntryGetInvoice (entry) == NULL)
        {
            non_inv = TRUE;
            break;
        }
    }

    if (non_inv)
    {
        /* Damn; yes.  Well, ask the user to make sure they REALLY want to
         * close this order!
         */

        message = _("This order contains entries that have not been invoiced. "
                    "Are you sure you want to close it out before "
                    "you invoice all the entries?");

        if (gnc_verify_dialog (GTK_WINDOW (ow->dialog), FALSE, "%s", message) == FALSE)
            return;
    }

    /* Ok, we can close this.  Ask for verification and set the closed date */
    message = _("Do you really want to close the order?");
    label = _("Close Date");

    if (!gnc_dialog_date_close_parented (ow->dialog, message, label, TRUE, &t))
        return;

    gncOrderSetDateClosed (order, t);

    /* save it off */
    gnc_order_window_ok_save (ow);

    /* Reset the type; change to read-only */
    ow->dialog_type = VIEW_ORDER;
    gnc_entry_ledger_set_readonly (ow->ledger, TRUE);

    /* And redisplay the window */
    gnc_order_update_window (ow);
}

void
gnc_order_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    OrderWindow *ow = data;
    GncOrder *order = ow_get_order (ow);

    gnc_suspend_gui_refresh ();

    if (ow->dialog_type == NEW_ORDER && order != NULL)
    {
        gncOrderBeginEdit (order);
        gncOrderDestroy (order);
        ow->order_guid = *guid_null ();
    }

    if (ow->ledger)
        gnc_entry_ledger_destroy (ow->ledger);
    gnc_unregister_gui_component (ow->component_id);
    gnc_resume_gui_refresh ();

    g_free (ow);
}

static int
gnc_order_owner_changed_cb (GtkWidget *widget, gpointer data)
{
    OrderWindow *ow = data;
    GncOrder *order;

    if (!ow)
        return FALSE;

    if (ow->dialog_type == VIEW_ORDER)
        return FALSE;

    gnc_owner_get_owner (ow->owner_choice, &(ow->owner));

    /* Set the Order's owner now! */
    order = ow_get_order (ow);
    gncOrderSetOwner (order, &(ow->owner));

    if (ow->dialog_type == EDIT_ORDER)
        return FALSE;

    /* Only set the reference during the New Job dialog */
    switch (gncOwnerGetType (&(ow->owner)))
    {
    case GNC_OWNER_JOB:
    {
        char const *msg = gncJobGetReference (gncOwnerGetJob (&(ow->owner)));
        gtk_entry_set_text (GTK_ENTRY (ow->ref_entry), msg ? msg : "");
        break;
    }
    default:
        gtk_entry_set_text (GTK_ENTRY (ow->ref_entry), "");
        break;
    }

    return FALSE;
}

static void
gnc_order_window_close_handler (gpointer user_data)
{
    OrderWindow *ow = user_data;

    gtk_widget_destroy (ow->dialog);
}

static void
gnc_order_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
    OrderWindow *ow = user_data;
    const EventInfo *info;
    GncOrder *order = ow_get_order (ow);

    /* If there isn't a order behind us, close down */
    if (!order)
    {
        gnc_close_gui_component (ow->component_id);
        return;
    }

    /* Next, close if this is a destroy event */
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &ow->order_guid);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (ow->component_id);
            return;
        }
    }
}

static void
gnc_order_update_window (OrderWindow *ow)
{
    GncOrder *order;
    GncOwner *owner;
    gboolean hide_cd = FALSE;

    order = ow_get_order (ow);
    owner = gncOrderGetOwner (order);

    if (ow->owner_choice)
    {
        gtk_container_remove (GTK_CONTAINER (ow->owner_box), ow->owner_choice);
        gtk_widget_destroy (ow->owner_choice);
    }

    switch (ow->dialog_type)
    {
    case VIEW_ORDER:
    case EDIT_ORDER:
        ow->owner_choice =
            gnc_owner_edit_create (ow->owner_label, ow->owner_box, ow->book,
                                   owner);
        break;
    case NEW_ORDER:
        ow->owner_choice =
            gnc_owner_select_create (ow->owner_label, ow->owner_box, ow->book,
                                     owner);
        break;
    }

    g_signal_connect (ow->owner_choice, "changed",
                      G_CALLBACK (gnc_order_owner_changed_cb),
                      ow);

    gtk_widget_show_all (ow->dialog);

    {
        GtkTextBuffer* text_buffer;
        const char *string;
        time64 tt;

        gtk_entry_set_text (GTK_ENTRY (ow->ref_entry),
                            gncOrderGetReference (order));

        string = gncOrderGetNotes (order);
        text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(ow->notes_text));
        gtk_text_buffer_set_text (text_buffer, string, -1);

        tt = gncOrderGetDateOpened (order);
        if (tt == INT64_MAX)
        {
            gnc_date_edit_set_time (GNC_DATE_EDIT (ow->opened_date),
                                    gnc_time (NULL));
        }
        else
        {
            gnc_date_edit_set_time (GNC_DATE_EDIT (ow->opened_date), tt);
        }

        /* If this is a "New Order Window" we can stop here! */
        if (ow->dialog_type == NEW_ORDER)
            return;

        tt = gncOrderGetDateClosed (order);
        if (tt == INT64_MAX)
        {
            gnc_date_edit_set_time (GNC_DATE_EDIT (ow->closed_date),
                                    gnc_time (NULL));
            hide_cd = TRUE;
        }
        else
        {
            gnc_date_edit_set_time (GNC_DATE_EDIT (ow->closed_date), tt);
        }

        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ow->active_check),
                                      gncOrderGetActive (order));

    }

    gnc_gui_component_watch_entity_type (ow->component_id,
                                         GNC_ORDER_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gnc_table_refresh_gui (gnc_entry_ledger_get_table (ow->ledger), TRUE);

    if (hide_cd)
    {
        gtk_widget_hide (ow->closed_date);
        gtk_widget_hide (ow->cd_label);
    }

    if (ow->dialog_type == VIEW_ORDER)
    {
        /* Setup viewer for read-only access */
        gtk_widget_set_sensitive (ow->id_entry, FALSE);
        gtk_widget_set_sensitive (ow->opened_date, FALSE);
        gtk_widget_set_sensitive (ow->closed_date, FALSE);
        gtk_widget_set_sensitive (ow->notes_text, FALSE); /* XXX: Should notes remain writable? */

        /* Hide the 'close order' button */
        gtk_widget_hide (ow->close_order_button);
    }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
    const GncGUID *order_guid = find_data;
    OrderWindow *ow = user_data;

    return(ow && guid_equal(&ow->order_guid, order_guid));
}

static OrderWindow *
gnc_order_new_window (GtkWindow *parent, QofBook *bookp, OrderDialogType type,
                      GncOrder *order, GncOwner *owner)
{
    OrderWindow *ow;
    GtkBuilder *builder;
    GtkWidget *vbox, *regWidget, *hbox, *date;
    GncEntryLedger *entry_ledger = NULL;
    const char * class_name;

    switch (type)
    {
    case EDIT_ORDER:
        class_name = DIALOG_EDIT_ORDER_CM_CLASS;
        break;
    case VIEW_ORDER:
    default:
        class_name = DIALOG_VIEW_ORDER_CM_CLASS;
        break;
    }

    /*
     * Find an existing window for this order.  If found, bring it to
     * the front.
     */
    if (order)
    {
        GncGUID order_guid;

        order_guid = *gncOrderGetGUID(order);
        ow = gnc_find_first_gui_component (class_name, find_handler,
                                           &order_guid);
        if (ow)
        {
            gtk_window_present (GTK_WINDOW(ow->dialog));
            gtk_window_set_transient_for (GTK_WINDOW(ow->dialog), parent);
            return(ow);
        }
    }

    /*
     * No existing order window found.  Build a new one.
     */
    ow = g_new0 (OrderWindow, 1);
    ow->book = bookp;
    ow->dialog_type = type;

    /* Save this for later */
    gncOwnerCopy (owner, &(ow->owner));

    /* Find the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-order.glade", "order_entry_dialog");
    ow->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "order_entry_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(ow->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(ow->dialog), "GncOrderDialog");

    /* Grab the widgets */
    ow->id_entry = GTK_WIDGET(gtk_builder_get_object (builder, "id_entry"));
    ow->ref_entry = GTK_WIDGET(gtk_builder_get_object (builder, "ref_entry"));
    ow->notes_text = GTK_WIDGET(gtk_builder_get_object (builder, "notes_text"));
    ow->active_check = GTK_WIDGET(gtk_builder_get_object (builder, "active_check"));
    ow->owner_box = GTK_WIDGET(gtk_builder_get_object (builder, "owner_hbox"));
    ow->owner_label = GTK_WIDGET(gtk_builder_get_object (builder, "owner_label"));

    ow->cd_label = GTK_WIDGET(gtk_builder_get_object (builder, "cd_label"));
    ow->close_order_button = GTK_WIDGET(gtk_builder_get_object (builder, "close_order_button"));


    /* Setup Date Widgets */
    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "opened_date_hbox"));
    date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (hbox), date, TRUE, TRUE, 0);
    gtk_widget_show (date);
    ow->opened_date = date;

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "closed_date_hbox"));
    date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (hbox), date, TRUE, TRUE, 0);
    gtk_widget_show (date);
    ow->closed_date = date;

    /* Build the ledger */
    switch (type)
    {
    case EDIT_ORDER:
        entry_ledger = gnc_entry_ledger_new (ow->book, GNCENTRY_ORDER_ENTRY);
        break;
    case VIEW_ORDER:
    default:
        entry_ledger = gnc_entry_ledger_new (ow->book, GNCENTRY_ORDER_VIEWER);
        break;
    }

    /* Save the entry ledger for later */
    ow->ledger = entry_ledger;

    /* Set the order for the entry_ledger */
    gnc_entry_ledger_set_default_order (entry_ledger, order);

    /* Set watches on entries */
    //  entries = gncOrderGetEntries (order);
    //  gnc_entry_ledger_load (entry_ledger, entries);

    /* Watch the order of operations, here... */
    regWidget = gnucash_register_new (gnc_entry_ledger_get_table (entry_ledger),
                                      NULL);
    ow->reg = GNUCASH_REGISTER (regWidget);
    gnucash_sheet_set_window (gnucash_register_get_sheet (ow->reg), ow->dialog);
    gnc_entry_ledger_set_parent (entry_ledger, ow->dialog);

    vbox = GTK_WIDGET(gtk_builder_get_object (builder, "ledger_vbox"));
    // gtk_box_pack_start (GTK_BOX(vbox), toolbar, FALSE, FALSE, 2);
    gtk_box_pack_start (GTK_BOX(vbox), regWidget, TRUE, TRUE, 2);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ow);

    /* Setup initial values */
    ow->order_guid = *gncOrderGetGUID (order);

    gtk_entry_set_text (GTK_ENTRY (ow->id_entry), gncOrderGetID (order));

    ow->component_id =
        gnc_register_gui_component (class_name,
                                    gnc_order_window_refresh_handler,
                                    gnc_order_window_close_handler,
                                    ow);

    gnc_table_realize_gui (gnc_entry_ledger_get_table (entry_ledger));

    /* Now fill in a lot of the pieces and display properly */
    gnc_order_update_window (ow);

    /* Maybe set the reference */
    gnc_order_owner_changed_cb (ow->owner_choice, ow);

    g_object_unref(G_OBJECT(builder));

    return ow;
}

static OrderWindow *
gnc_order_window_new_order (GtkWindow *parent, QofBook *bookp, GncOwner *owner)
{
    OrderWindow *ow;
    GtkBuilder *builder;
    GncOrder *order;
    gchar *string;
    GtkWidget *hbox, *date;

    ow = g_new0 (OrderWindow, 1);
    ow->book = bookp;
    ow->dialog_type = NEW_ORDER;

    order = gncOrderCreate (bookp);
    gncOrderSetOwner (order, owner);

    /* Save this for later */
    gncOwnerCopy (owner, &(ow->owner));

    /* Find the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-order.glade", "new_order_dialog");

    ow->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "new_order_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(ow->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(ow->dialog), "GncOrderDialog");

    g_object_set_data (G_OBJECT (ow->dialog), "dialog_info", ow);

    /* Grab the widgets */
    ow->id_entry = GTK_WIDGET(gtk_builder_get_object (builder, "entry_id"));
    ow->ref_entry = GTK_WIDGET(gtk_builder_get_object (builder, "entry_ref"));
    ow->notes_text = GTK_WIDGET(gtk_builder_get_object (builder, "text_notes"));
    ow->owner_box = GTK_WIDGET(gtk_builder_get_object (builder, "bill_owner_hbox"));
    ow->owner_label = GTK_WIDGET(gtk_builder_get_object (builder, "bill_owner_label"));

    /* Setup date Widget */
    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "date_opened_hbox"));
    date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX (hbox), date, TRUE, TRUE, 0);
    gtk_widget_show (date);
    ow->opened_date = date;

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ow);

    /* Setup initial values */
    ow->order_guid = *gncOrderGetGUID (order);
    string = gncOrderNextID(bookp);
    gtk_entry_set_text (GTK_ENTRY (ow->id_entry), string);
    g_free(string);

    ow->component_id =
        gnc_register_gui_component (DIALOG_NEW_ORDER_CM_CLASS,
                                    gnc_order_window_refresh_handler,
                                    gnc_order_window_close_handler,
                                    ow);

    /* Now fill in a lot of the pieces and display properly */
    gnc_order_update_window (ow);

    // The customer choice widget should have keyboard focus
    if (GNC_IS_GENERAL_SEARCH(ow->owner_choice))
    {
        gnc_general_search_grab_focus(GNC_GENERAL_SEARCH(ow->owner_choice));
    }

    /* Maybe set the reference */
    gnc_order_owner_changed_cb (ow->owner_choice, ow);

    g_object_unref(G_OBJECT(builder));

    return ow;
}

OrderWindow *
gnc_ui_order_edit (GtkWindow *parent, GncOrder *order)
{
    OrderWindow *ow;
    OrderDialogType type;

    if (!order) return NULL;

    type = EDIT_ORDER;
    if (gncOrderGetDateClosed (order) == INT64_MAX)
        type = VIEW_ORDER;

    ow = gnc_order_new_window (parent, gncOrderGetBook(order), type, order,
                               gncOrderGetOwner (order));

    return ow;
}

OrderWindow *
gnc_ui_order_new (GtkWindow *parent, GncOwner *ownerp, QofBook *bookp)
{
    OrderWindow *ow;
    GncOwner owner;

    if (ownerp)
    {
        switch (gncOwnerGetType (ownerp))
        {
        case GNC_OWNER_CUSTOMER:
        case GNC_OWNER_VENDOR:
        case GNC_OWNER_JOB:
            gncOwnerCopy (ownerp, &owner);
            break;
        default:
            g_warning ("Cannot deal with unknown Owner types");
            /* XXX: popup a warning? */
            return NULL;
        }
    }
    else
        gncOwnerInitJob (&owner, NULL); /* XXX: pass in the owner type? */

    /* Make sure required options exist */
    if (!bookp) return NULL;

    ow = gnc_order_window_new_order (parent, bookp, &owner);

    return ow;
}

/* Functions for order selection widgets */

static void
edit_order_cb (GtkWindow *dialog, gpointer *order_p, gpointer user_data)
{
    GncOrder *order;

    g_return_if_fail (order_p && user_data);

    order = *order_p;

    if (order)
        gnc_ui_order_edit (dialog, order);

    return;
}

static gpointer
new_order_cb (GtkWindow *dialog, gpointer user_data)
{
    struct _order_select_window *sw = user_data;
    OrderWindow *ow;

    g_return_val_if_fail (user_data, NULL);

    ow = gnc_ui_order_new (dialog, sw->owner, sw->book);
    return ow_get_order (ow);
}

static void
free_order_cb (gpointer user_data)
{
    struct _order_select_window *sw = user_data;

    g_return_if_fail (sw);

    qof_query_destroy (sw->q);
    g_free (sw);
}

GNCSearchWindow *
gnc_order_search (GtkWindow *parent, GncOrder *start, GncOwner *owner, QofBook *book)
{
    QofIdType type = GNC_ORDER_MODULE_NAME;
    struct _order_select_window *sw;
    QofQuery *q, *q2 = NULL;
    static GList *params = NULL;
    static GList *columns = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        { N_("View/Edit Order"), edit_order_cb, NULL, TRUE},
        { NULL },
    };

    g_return_val_if_fail (book, NULL);

    /* Build parameter list in reverse order */
    if (params == NULL)
    {
        params = gnc_search_param_prepend (params, _("Order Notes"), NULL, type,
                                           ORDER_NOTES, NULL);
        params = gnc_search_param_prepend (params, _("Date Closed"), NULL, type,
                                           ORDER_CLOSED, NULL);
        params = gnc_search_param_prepend (params, _("Is Closed?"), NULL, type,
                                           ORDER_IS_CLOSED, NULL);
        params = gnc_search_param_prepend (params, _("Date Opened"), NULL, type,
                                           ORDER_OPENED, NULL);
        params = gnc_search_param_prepend (params, _("Owner Name "), NULL, type,
                                           ORDER_OWNER, OWNER_NAME, NULL);
        params = gnc_search_param_prepend (params, _("Order ID"), NULL, type,
                                           ORDER_ID, NULL);
    }

    /* Build the column list in reverse order */
    if (columns == NULL)
    {
        columns = gnc_search_param_prepend (columns, _("Billing ID"), NULL, type,
                                            ORDER_REFERENCE, NULL);
        columns = gnc_search_param_prepend (columns, _("Company"), NULL, type,
                                            ORDER_OWNER, OWNER_PARENT,
                                            OWNER_NAME, NULL);
        columns = gnc_search_param_prepend (columns, _("Closed"), NULL, type,
                                            ORDER_CLOSED, NULL);
        columns = gnc_search_param_prepend (columns, _("Opened"), NULL, type,
                                            ORDER_OPENED, NULL);
        columns = gnc_search_param_prepend (columns, _("Num"), NULL, type,
                                            ORDER_ID, NULL);
    }

    /* Build the queries */
    q = qof_query_create_for (type);
    qof_query_set_book (q, book);

    /* If owner is supplied, limit all searches to orders who's owner
     * (or parent) is the supplied owner!
     */
    if (owner && gncOwnerGetGUID (owner))
    {
        QofQuery *tmp, *q3;

        q3 = qof_query_create_for (type);
        qof_query_add_guid_match (q3, g_slist_prepend
                                  (g_slist_prepend (NULL, QOF_PARAM_GUID),
                                   ORDER_OWNER),
                                  gncOwnerGetGUID (owner), QOF_QUERY_OR);
        qof_query_add_guid_match (q3, g_slist_prepend
                                  (g_slist_prepend (NULL, OWNER_PARENTG),
                                   ORDER_OWNER),
                                  gncOwnerGetGUID (owner), QOF_QUERY_OR);

        tmp = qof_query_merge (q, q3, QOF_QUERY_AND);
        qof_query_destroy (q);
        qof_query_destroy (q3);
        q = tmp;
        q2 = qof_query_copy (q);
    }

#if 0
    if (start)
    {
        if (q2 == NULL)
            q2 = qof_query_copy (q);

        qof_query_add_guid_match (q2, g_slist_prepend (NULL, QOF_PARAM_GUID),
                                  gncOrderGetGUID (start), QOF_QUERY_AND);
    }
#endif

    /* launch select dialog and return the result */
    sw = g_new0 (struct _order_select_window, 1);

    if (owner)
    {
        gncOwnerCopy (owner, &(sw->owner_def));
        sw->owner = &(sw->owner_def);
    }
    sw->book = book;
    sw->q = q;

    return gnc_search_dialog_create (parent, type, _("Find Order"),
                                     params, columns, q, q2,
                                     buttons, NULL, new_order_cb,
                                     sw, free_order_cb, GNC_PREFS_GROUP_SEARCH,
                                     NULL, "GncFindOrderDialog");
}

GNCSearchWindow *
gnc_order_search_select (GtkWindow *parent, gpointer start, gpointer book)
{
    GncOrder *o = start;
    GncOwner owner, *ownerp;

    if (!book) return NULL;

    if (o)
    {
        ownerp = gncOrderGetOwner (o);
        gncOwnerCopy (ownerp, &owner);
    }
    else
        gncOwnerInitCustomer (&owner, NULL); /* XXX */

    return gnc_order_search (parent, start, NULL, book);
}

GNCSearchWindow *
gnc_order_search_edit (GtkWindow *parent, gpointer start, gpointer book)
{
    if (start)
        gnc_ui_order_edit (parent, start);

    return NULL;
}
