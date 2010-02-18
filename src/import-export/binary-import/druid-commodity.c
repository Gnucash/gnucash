/********************************************************************
 * druid-commodity.c -- fancy importer for old Gnucash files        *
 *                       (GnuCash)                                  *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>

#include "Scrub.h"
#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "druid-commodity.h"
#include "druid-utils.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-pricedb-p.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

struct _commoditydruid
{
    GtkWidget  * window;
    GtkWidget  * druid;

    GtkWidget  * intro_page;
    GtkWidget  * finish_page;

    GHashTable * new_map;
    GHashTable * old_map;
    GList      * pages;

    int        is_modal;
};

struct _commoditydruidpage
{
    GtkWidget * page;
    char      * old_name;
    GtkWidget * new_type_combo;
    GtkWidget * new_name_entry;
    GtkWidget * new_mnemonic_entry;
};

typedef struct _commoditydruidpage CommodityDruidPage;
static CommodityDruidPage * make_commodity_druid_page(gnc_commodity * comm);

static GdkColor std_bg_color = { 0, 39835, 49087, 40092 };
static GdkColor std_logo_bg_color = { 0, 65535, 65535, 65535 };
static GdkColor std_title_color =  { 0, 65535, 65535, 65535 };

static int gnc_ui_commodity_druid_comm_check_cb(GnomeDruidPage * page,
        gpointer druid,
        gpointer user_data);

static gboolean gnc_ui_commodity_druid_cancel_cb(GnomeDruidPage * page,
        gpointer druid,
        gpointer user_data);

static void gnc_ui_commodity_druid_finish_cb(GnomeDruidPage * page,
        gpointer druid,
        gpointer user_data);


void
gnc_import_legacy_commodities(const char * filename)
{
    CommodityDruid * d;

    if (!gnc_commodity_table_has_namespace (gnc_get_current_commodities (),
                                            GNC_COMMODITY_NS_LEGACY))
        return;

    d = gnc_ui_commodity_druid_create(filename);
    d->is_modal = TRUE;

    gtk_window_set_modal(GTK_WINDOW(d->window), TRUE);
    gtk_main();
}


static gboolean
window_delete_cb(GtkWidget *widget,
                 GdkEvent  *event,
                 gpointer  user_data)
{
    CommodityDruid *cd = user_data;

    /* unload the current file (can't have out-of-date commodities) */
    gnc_file_quit ();

    gnc_ui_commodity_druid_destroy (cd);

    /* Don't delete the window, we'll handle things ourselves. */
    return TRUE;
}


/********************************************************************
 * gnc_ui_commodity_druid_create()
 ********************************************************************/

CommodityDruid *
gnc_ui_commodity_druid_create(const char * filename)
{

    CommodityDruid * d = g_new0(CommodityDruid, 1);
    GtkObject      * dobj;
    GList          * orphans, * l;
    gnc_commodity  * lost;
    gnc_commodity  * found;
    CommodityDruidPage * new_page;
    GnomeDruidPage * back_page;
    GladeXML       * xml;
    QofBook        * book = gnc_get_current_book ();

    xml = gnc_glade_xml_new ("binary-import.glade",
                             "New Commodity Format Druid");

    d->window = glade_xml_get_widget (xml, "New Commodity Format Druid");
    dobj = GTK_OBJECT(d->window);

    g_signal_connect (dobj, "delete_event",
                      G_CALLBACK (window_delete_cb), d);

    d->druid           = glade_xml_get_widget (xml, "commodity_druid");
    d->intro_page      = glade_xml_get_widget (xml, "start_page");
    d->finish_page     = glade_xml_get_widget (xml, "finish_page");
    back_page = GNOME_DRUID_PAGE(d->intro_page);

    d->is_modal = FALSE;

    glade_xml_signal_connect_data
    (xml, "gnc_ui_commodity_druid_cancel_cb",
     G_CALLBACK (gnc_ui_commodity_druid_cancel_cb), d);

    glade_xml_signal_connect_data
    (xml, "gnc_ui_commodity_druid_finish_cb",
     G_CALLBACK (gnc_ui_commodity_druid_finish_cb), d);

    d->new_map = g_hash_table_new(g_str_hash, g_str_equal);
    d->old_map = g_hash_table_new(g_str_hash, g_str_equal);
    orphans =
        gnc_commodity_table_get_commodities(gnc_get_current_commodities(),
                                            GNC_COMMODITY_NS_LEGACY);

    /* make a new list with the (saved) old mnemonic and the
     * new currency. */
    for (l = orphans; l; l = l->next)
    {
        lost = (gnc_commodity *)l->data;

        /* if the mnemonic is an ISO-4217 currency, use that as
         * the default */
        found = gnc_commodity_table_lookup(gnc_get_current_commodities(),
                                           GNC_COMMODITY_NS_CURRENCY,
                                           gnc_commodity_get_mnemonic(lost));

        /* otherwise, guess that it's a NASDAQ security. */
        if (!found)
        {
            found = gnc_commodity_new(book, gnc_commodity_get_mnemonic(lost),
                                      GNC_COMMODITY_NS_NASDAQ,
                                      gnc_commodity_get_mnemonic(lost),
                                      NULL, 100000);
        }

        g_hash_table_insert(d->new_map, (gpointer)gnc_commodity_get_mnemonic(lost),
                            (gpointer)found);
        g_hash_table_insert(d->old_map, (gpointer)gnc_commodity_get_mnemonic(lost),
                            (gpointer)lost);

        /* create a new page in the wizard for the commodity */
        new_page = make_commodity_druid_page(found);

        /* set up next/back signal handlers */
        g_signal_connect(new_page->page, "next",
                         G_CALLBACK(gnc_ui_commodity_druid_comm_check_cb),
                         d);

        g_signal_connect(new_page->page, "cancel",
                         G_CALLBACK(gnc_ui_commodity_druid_cancel_cb),
                         d);

        d->pages = g_list_append(d->pages, new_page);

        gnome_druid_insert_page(GNOME_DRUID(d->druid),
                                back_page,
                                GNOME_DRUID_PAGE(new_page->page));
        back_page = GNOME_DRUID_PAGE(new_page->page);
    }

    gnc_druid_set_colors (GNOME_DRUID (d->druid));

    gtk_widget_show_all(d->window);

    g_list_free(orphans);

    return d;
}


/********************************************************************
 * make_commodity_druid_page : build a single commodity xlator
 * page.  This really should be done in glade, but I can't figure out
 * how to make it work.
 ********************************************************************/


static CommodityDruidPage *
make_commodity_druid_page(gnc_commodity * comm)
{
    CommodityDruidPage * retval = g_new0(CommodityDruidPage, 1);
    GtkWidget * alignment;
    GtkWidget * top_vbox;
    GtkWidget * info_label;
    GtkWidget * next_label;
    GtkWidget * temp;
    char      * title = NULL;
    GnomeDruidPageStandard * page;

    /* make the page widget */
    retval->page = gnome_druid_page_standard_new_with_vals("", NULL, NULL);
    g_object_set_data(G_OBJECT(retval->page), "page_struct", retval);

    page = GNOME_DRUID_PAGE_STANDARD(retval->page);

    /* save the old commodity name */
    retval->old_name = g_strdup(gnc_commodity_get_mnemonic(comm));
    title = g_strdup_printf("Enter information about \"%s\"",
                            retval->old_name ? retval->old_name : "");

    gnome_druid_page_standard_set_background(page, & std_bg_color);
    gnome_druid_page_standard_set_logo_background(page, & std_logo_bg_color);
    gnome_druid_page_standard_set_title_foreground(page, & std_title_color);
    gnome_druid_page_standard_set_title(page, title);
    g_free(title);

    alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
    gtk_box_pack_start(GTK_BOX(page->vbox), alignment, FALSE, FALSE, 0);

    top_vbox = gtk_vbox_new(FALSE, 3);
    gtk_container_add(GTK_CONTAINER(alignment), top_vbox);

    info_label =
        gtk_label_new(_("Pick the type of the currency or security. For "
                        "national currencies, use \"CURRENCY\". "
                        "Enter a new type in the box if the ones in the "
                        "pick list are inappropriate."));

    gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
    gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

    temp = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

    retval->new_type_combo = gtk_combo_new();
    gtk_box_pack_start(GTK_BOX(temp), retval->new_type_combo, TRUE, TRUE, 0);

    gnc_ui_update_namespace_picker(retval->new_type_combo,
                                   gnc_commodity_get_namespace(comm),
                                   DIAG_COMM_ALL);

    temp = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 5);

    info_label =
        gtk_label_new(_("Enter a descriptive name for the currency or stock, "
                        "such as \"US Dollar\" or \"Red Hat Stock\""));

    gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
    gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

    temp = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

    retval->new_name_entry = gtk_entry_new();
    gtk_box_pack_start(GTK_BOX(temp), retval->new_name_entry,
                       TRUE, TRUE, 0);
    gtk_entry_set_text(GTK_ENTRY(retval->new_name_entry),
                       gnc_commodity_get_fullname(comm));

    temp = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 5);

    info_label =
        gtk_label_new(_("Enter the ticker symbol (such as \"RHAT\"), "
                        "national currency symbol (such as \"USD\"), or "
                        "other unique abbreviation for the name."));

    gtk_label_set_justify (GTK_LABEL(info_label), GTK_JUSTIFY_LEFT);
    gtk_box_pack_start(GTK_BOX(top_vbox), info_label, TRUE, TRUE, 0);

    temp = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 0);

    retval->new_mnemonic_entry = gtk_entry_new();
    gtk_box_pack_start(GTK_BOX(temp), retval->new_mnemonic_entry,
                       TRUE, TRUE, 0);
    gtk_entry_set_text(GTK_ENTRY(retval->new_mnemonic_entry),
                       gnc_commodity_get_mnemonic(comm));

    temp = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(top_vbox), temp, FALSE, FALSE, 5);

    next_label = gtk_label_new(_("Click \"Next\" to accept the information "
                                 "and move to the next currency or stock."));
    gtk_label_set_justify (GTK_LABEL(next_label), GTK_JUSTIFY_LEFT);
    gtk_box_pack_start(GTK_BOX(top_vbox), next_label, TRUE, TRUE, 0);

    return retval;
}


/********************************************************************
 * gnc_ui_commodity_druid_destroy()
 ********************************************************************/

void
gnc_ui_commodity_druid_destroy(CommodityDruid * cd)
{
    GList * p;
    CommodityDruidPage * cdp;

    /* remove the old commodities no matter how we exit */
    gnc_commodity_table_delete_namespace(gnc_get_current_commodities(),
                                         GNC_COMMODITY_NS_LEGACY);

    for (p = cd->pages; p; p = p->next)
    {
        cdp = (CommodityDruidPage *)p->data;
        g_free(cdp->old_name);
        g_free(cdp);
    }

    g_list_free(cd->pages);
    g_hash_table_destroy(cd->new_map);
    g_hash_table_destroy(cd->old_map);

    gtk_widget_destroy(GTK_WIDGET(cd->window));

    if (cd->is_modal)
    {
        gtk_main_quit();
    }

    g_free (cd);
}


/********************************************************************
 * callbacks
 ********************************************************************/

static gboolean
gnc_ui_commodity_druid_cancel_cb(GnomeDruidPage * page, gpointer druid,
                                 gpointer user_data)
{
    CommodityDruid * cd = user_data;

    /* destroy the dialog */
    gnc_ui_commodity_druid_destroy(cd);

    /* unload the current file (can't have out-of-date commodities) */
    gnc_file_quit ();
    gnc_file_new ();

    return TRUE;
}


static gboolean
gnc_ui_commodity_druid_comm_check_cb(GnomeDruidPage * page, gpointer druid,
                                     gpointer user_data)
{
    CommodityDruid * cd = user_data;
    CommodityDruidPage * dpage = g_object_get_data(G_OBJECT(page), "page_struct");
    gchar * new_type;
    const gchar * new_name;
    const gchar * new_mnemonic;
    gnc_commodity * new_comm;

    new_type     = gnc_ui_namespace_picker_ns (dpage->new_type_combo);
    new_name     = gtk_entry_get_text(GTK_ENTRY(dpage->new_name_entry));
    new_mnemonic = gtk_entry_get_text(GTK_ENTRY(dpage->new_mnemonic_entry));
    if ((strlen(new_type) == 0) ||
            (strlen(new_name) == 0) ||
            (strlen(new_mnemonic) == 0))
    {
        gnc_warning_dialog(cd->window,
                           _("You must put values for the type, name, "
                             "and abbreviation of the currency/stock."));

        g_free(new_type);
        return TRUE;
    }

    if (gnc_commodity_namespace_is_iso (new_type) &&
            !gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                         new_type, new_mnemonic))
    {
        gnc_warning_dialog(cd->window,
                           _("You must enter an existing national "
                             "currency or enter a different type."));
        g_free(new_type);
        return TRUE;
    }
    new_comm = g_hash_table_lookup(cd->new_map, dpage->old_name);
    g_assert(new_comm);

    /* fill in the commodity structure info */
    gnc_commodity_set_fullname(new_comm, new_name);
    gnc_commodity_set_namespace(new_comm, new_type);
    gnc_commodity_set_mnemonic(new_comm, new_mnemonic);

    g_free(new_type);
    return FALSE;
}

static void
finish_helper(gpointer key, gpointer value, gpointer data)
{
    CommodityDruid * cd = data;
    gnc_commodity  * comm = value;
    gnc_commodity  * old_comm = g_hash_table_lookup(cd->old_map, key);
    GList          * accts;
    GList          * node;
    QofBook        * book = gnc_get_current_book ();

    if (!book)
    {
        PERR("finish_helper - no current book.");
        return;
    }

    /* key is the old mnemonic, value is a pointer to the gnc_commodity
     * structure. */
    comm = gnc_commodity_table_insert(gnc_get_current_commodities(), comm);

    /* s/old commodity/new commodity/g  in the pricedb */
    gnc_pricedb_substitute_commodity(gnc_book_get_pricedb(book),
                                     old_comm,
                                     comm);

    /* now replace all the accounts using old_comm with new_comm */
    accts = gnc_account_get_descendants(gnc_get_current_root_account ());

    for (node = accts; node; node = node->next)
    {
        Account *account = node->data;

        xaccAccountBeginEdit(account);

        if (gnc_commodity_equiv (DxaccAccountGetCurrency(account),
                                 old_comm))
            DxaccAccountSetCurrency (account, comm);

        if (gnc_commodity_equiv (xaccAccountGetCommodity(account), old_comm))
            xaccAccountSetCommodity(account, comm);

        xaccAccountCommitEdit(account);
    }

    g_list_free (accts);
}


static void
gnc_ui_commodity_druid_finish_cb(GnomeDruidPage * page, gpointer druid,
                                 gpointer user_data)
{
    CommodityDruid * cd = user_data;

    /* add the new commodities to the engine's namespace map and
     * replace the account commodity pointers */
    g_hash_table_foreach(cd->new_map, &finish_helper, (gpointer)cd);

    /* Fix account and transaction commodities */
    xaccAccountTreeScrubCommodities (gnc_get_current_root_account ());

    /* Fix split amount/value */
    xaccAccountTreeScrubSplits (gnc_get_current_root_account ());

    /* destroy the dialog */
    gnc_ui_commodity_druid_destroy(cd);
}


void gnc_import_commodities(QofSession *session, gpointer unused)
{
    const gchar *book_url;

    book_url = qof_session_get_url(session);
    gnc_import_legacy_commodities(book_url);
}
