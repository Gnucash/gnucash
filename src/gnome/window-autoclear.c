/********************************************************************\
 * window-autoclear.c -- the autoclear window                       *
 * Copyright (C) 2010 Cristian KLEIN                                *
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
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Scrub.h"
#include "dialog-account.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-event.h"
#include "gnc-gnome-utils.h"
#include "gnc-main-window.h"
#include "gnc-plugin-page-register.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "window-autoclear.h"

#define WINDOW_AUTOCLEAR_CM_CLASS "window-autoclear"

static QofLogModule log_module = GNC_MOD_GUI;

/** STRUCTS *********************************************************/
struct _AutoClearWindow
{
    Account *account;        /* The account that we are auto-clearing */

    gint component_id;       /* id of component                       */

    GtkWidget *window;       /* The auto-clear window                 */
    GNCAmountEdit *end_value;/* The ending value                      */
    GtkWidget *ok_button;
    GtkWidget *cancel_button;
    GtkLabel *status_label;
};

/** Callback prototypes************************************************/
void gnc_autoclear_window_ok_cb     (GtkWidget *widget,
                                     AutoClearWindow *data);
void gnc_autoclear_window_cancel_cb (GtkWidget *widget,
                                     AutoClearWindow *data);

/********************************************************************\
 * gnc_ui_autoclear_window_raise                                    *
 *   shows and raises an auto-clear window                          *
 *                                                                  *
 * Args:   autoClearData - the auto-clear window structure          *
\********************************************************************/
void
gnc_ui_autoclear_window_raise(AutoClearWindow * autoClearData)
{
    if (autoClearData == NULL)
        return;

    if (autoClearData->window == NULL)
        return;

    gtk_window_present(GTK_WINDOW(autoClearData->window));
}

static char *
gnc_autoclear_make_window_name(Account *account)
{
    char *fullname;
    char *title;

    fullname = gnc_account_get_full_name(account);
    title = g_strconcat(fullname, " - ", _("Auto-clear"), NULL);

    g_free(fullname);

    return title;
}

static gboolean
ght_gnc_numeric_equal(gconstpointer v1, gconstpointer v2)
{
    gnc_numeric n1 = *(gnc_numeric *)v1, n2 = *(gnc_numeric *)v2;
    return gnc_numeric_equal(n1, n2);
}

static guint
ght_gnc_numeric_hash(gconstpointer v1)
{
    gnc_numeric n1 = *(gnc_numeric *)v1;
    gdouble d1 = gnc_numeric_to_double(n1);
    return g_str_hash(&d1);
}

typedef struct _sack_foreach_data_t
{
    gnc_numeric split_value;
    GList *reachable_list;
} *sack_foreach_data_t;

static void sack_foreach_func(gpointer key, gpointer value, gpointer user_data)
{
    sack_foreach_data_t data = (sack_foreach_data_t)user_data;
    gnc_numeric thisvalue = *(gnc_numeric *)key;

    gnc_numeric reachable_value = gnc_numeric_add_fixed(thisvalue, data->split_value);
    data->reachable_list = g_list_append(data->reachable_list, g_memdup(&reachable_value, sizeof(gnc_numeric)));
    PINFO("    Sack: found %s, added %s\n", gnc_numeric_to_string(thisvalue), gnc_numeric_to_string(reachable_value));
}

void
gnc_autoclear_window_ok_cb (GtkWidget *widget,
                            AutoClearWindow *data)
{
    GList *node, *nc_list = 0, *toclear_list = 0;
    gnc_numeric toclear_value;
    GHashTable *sack;

    gtk_label_set_text(data->status_label, _("Searching for splits to clear ..."));

    /* Value we have to reach */
    toclear_value = gnc_amount_edit_get_amount(data->end_value);
    toclear_value = gnc_numeric_convert(toclear_value, xaccAccountGetCommoditySCU(data->account), GNC_HOW_RND_NEVER);

    /* Extract which splits are not cleared and compute the amount we have to clear */
    for (node = xaccAccountGetSplitList(data->account); node; node = node->next)
    {
        Split *split = (Split *)node->data;
        char recn;
        gnc_numeric value;

        recn = xaccSplitGetReconcile (split);
        value = xaccSplitGetAmount (split);

        if (recn == NREC)
            nc_list = g_list_append(nc_list, split);
        else
            toclear_value = gnc_numeric_sub_fixed(toclear_value, value);
    }

    /* Pretty print information */
    PINFO("Amount to clear: %s\n", gnc_numeric_to_string(toclear_value));
    PINFO("Available splits:\n");
    for (node = nc_list; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        gnc_numeric value = xaccSplitGetAmount (split);
        PINFO("  %s\n", gnc_numeric_to_string(value));
    }

    /* Run knapsack */
    /* Entries in the hash table are:
     *  - key   = amount to which we know how to clear (freed by GHashTable)
     *  - value = last split we used to clear this amount (not managed by GHashTable)
     */
    PINFO("Knapsacking ...\n");
    sack = g_hash_table_new_full (ght_gnc_numeric_hash, ght_gnc_numeric_equal, g_free, NULL);
    for (node = nc_list; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        gnc_numeric split_value = xaccSplitGetAmount(split);

        GList *node;
        struct _sack_foreach_data_t data[1];
        data->split_value = split_value;
        data->reachable_list = 0;

        PINFO("  Split value: %s\n", gnc_numeric_to_string(split_value));

        /* For each value in the sack, compute a new reachable value */
        g_hash_table_foreach (sack, sack_foreach_func, data);

        /* Add the value of the split itself to the reachable_list */
        data->reachable_list = g_list_append(data->reachable_list, g_memdup(&split_value, sizeof(gnc_numeric)));

        /* Add everything to the sack, looking out for duplicates */
        for (node = data->reachable_list; node; node = node->next)
        {
            gnc_numeric *reachable_value = node->data;
            Split *toinsert_split = split;

            PINFO("    Reachable value: %s ", gnc_numeric_to_string(*reachable_value));

            /* Check if it already exists */
            if (g_hash_table_lookup_extended(sack, reachable_value, NULL, NULL))
            {
                /* If yes, we are in trouble, we reached an amount using two solutions */
                toinsert_split = NULL;
                PINFO("dup");
            }
            g_hash_table_insert (sack, reachable_value, toinsert_split);
            PINFO("\n");
        }
        g_list_free(data->reachable_list);
    }

    /* Check solution */
    PINFO("Rebuilding solution ...\n");
    while (!gnc_numeric_zero_p(toclear_value))
    {
        gpointer psplit = NULL;

        PINFO("  Left to clear: %s\n", gnc_numeric_to_string(toclear_value));
        if (g_hash_table_lookup_extended(sack, &toclear_value, NULL, &psplit))
        {
            if (psplit != NULL)
            {
                /* Cast the gpointer to the kind of pointer we actually need */
                Split *split = (Split *)psplit;
                toclear_list = g_list_prepend(toclear_list, split);
                toclear_value = gnc_numeric_sub_fixed(toclear_value,
                                                      xaccSplitGetAmount(split));
                PINFO("    Cleared: %s -> %s\n",
                      gnc_numeric_to_string(xaccSplitGetAmount(split)),
                      gnc_numeric_to_string(toclear_value));
            }
            else
            {
                /* We couldn't reconstruct the solution */
                PINFO("    Solution not unique.\n");
                gtk_label_set_text(data->status_label, _("Cannot uniquely clear splits. Found multiple possibilities."));
                return;
            }
        }
        else
        {
            PINFO("    No solution found.\n");
            gtk_label_set_text(data->status_label, _("The selected amount cannot be cleared."));
            return;
        }
    }
    g_hash_table_destroy (sack);

    /* Show solution */
    PINFO("Clearing splits:\n");
    for (node = toclear_list; node; node = node->next)
    {
        Split *split = node->data;
        char recn;
        gnc_numeric value;

        recn = xaccSplitGetReconcile (split);
        value = xaccSplitGetAmount (split);

        PINFO("  %c %s\n", recn, gnc_numeric_to_string(value));

        xaccSplitSetReconcile (split, CREC);
    }
    if (toclear_list == 0)
        PINFO("  None\n");

    /* Free lists */
    g_list_free(nc_list);
    g_list_free(toclear_list);

    /* Close window */
    gtk_widget_destroy(data->window);
    g_free(data);
}

void
gnc_autoclear_window_cancel_cb (GtkWidget *widget,
                                AutoClearWindow *data)
{
    /* Close window */
    gtk_widget_destroy(data->window);
    g_free(data);
}

/********************************************************************\
 * autoClearWindow                                                  *
 *   opens up the window to auto-clear an account                   *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to auto-clear                      *
 * Return: autoClearData - the instance of this AutoClearWindow     *
\********************************************************************/
AutoClearWindow *
autoClearWindow (GtkWidget *parent, Account *account)
{
    GtkBox *box;
    GtkLabel *label;
    GtkBuilder *builder;
    AutoClearWindow *data;
    char *title;

    data = g_new0 (AutoClearWindow, 1);
    data->account = account;

    /* Create the dialog box */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "window-autoclear.glade", "Auto-clear Start Dialog");
    data->window = GTK_WIDGET(gtk_builder_get_object (builder, "Auto-clear Start Dialog"));
    title = gnc_autoclear_make_window_name (account);
    gtk_window_set_title(GTK_WINDOW(data->window), title);
    g_free (title);

    /* Add amount edit box */
    data->end_value = GNC_AMOUNT_EDIT(gnc_amount_edit_new());
    g_signal_connect(GTK_WIDGET(data->end_value), "activate",
                     G_CALLBACK(gnc_autoclear_window_ok_cb), data);

    box   = GTK_BOX(gtk_builder_get_object (builder, "end_value_box"));
    gtk_box_pack_start(box, GTK_WIDGET(data->end_value), TRUE, TRUE, 0);

    label = GTK_LABEL(gtk_builder_get_object (builder, "end_label"));
    gtk_label_set_mnemonic_widget(label, GTK_WIDGET(data->end_value));
    gtk_widget_grab_focus(GTK_WIDGET(data->end_value));

    data->status_label = GTK_LABEL(gtk_builder_get_object (builder, "status_label"));

    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (data->window), GTK_WINDOW (parent));

    gtk_builder_connect_signals(builder, data);
    g_object_unref(G_OBJECT(builder));

    return data;
}

