/********************************************************************\
 * dialog-account.c -- window for creating and editing accounts for *
 *                     GnuCash                                      *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
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

#include <gnome.h>
#include <glib/gi18n.h>
#include <math.h>
#ifdef G_OS_WIN32
#include <pow.h>
#endif
#include <string.h>

#include "Transaction.h"
#include "dialog-account.h"
#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-general-select.h"
#include "gnc-commodity.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"


#define DIALOG_NEW_ACCOUNT_CM_CLASS "dialog-new-account"
#define DIALOG_EDIT_ACCOUNT_CM_CLASS "dialog-edit-account"
#define GCONF_SECTION "dialogs/account"
#define DEFAULT_COLOR "#ededececebeb"

enum account_cols
{
    ACCOUNT_COL_FULLNAME = 0,
    ACCOUNT_COL_FIELDNAME,
    ACCOUNT_COL_OLD_VALUE,
    ACCOUNT_COL_NEW_VALUE,
    NUM_ACCOUNT_COLS
};

typedef enum
{
    NEW_ACCOUNT,
    EDIT_ACCOUNT
} AccountDialogType;

typedef struct _AccountWindow
{
    QofBook *book;
    gboolean modal;
    GtkWidget *dialog;

    AccountDialogType dialog_type;

    GUID    account;
    Account *created_account;

    gchar **subaccount_names;
    gchar **next_name;

    GNCAccountType type;

    GtkWidget * notebook;

    GtkWidget * name_entry;
    GtkWidget * description_entry;
    GtkWidget * color_entry_button;
    GtkWidget * color_default_button;
    GtkWidget * code_entry;
    GtkTextBuffer * notes_text_buffer;

    GtkWidget * commodity_edit;
    dialog_commodity_mode commodity_mode;
    GtkWidget * account_scu;

    guint32 valid_types;
    GNCAccountType preferred_account_type;
    GtkWidget * type_view;
    GtkTreeView * parent_tree;

    GtkWidget * opening_balance_edit;
    GtkWidget * opening_balance_date_edit;
    GtkWidget * opening_balance_page;

    GtkWidget * opening_equity_radio;
    GtkWidget * transfer_account_scroll;
    GtkWidget * transfer_tree;

    GtkWidget * tax_related_button;
    GtkWidget * placeholder_button;
    GtkWidget * hidden_button;

    gint component_id;
} AccountWindow;

typedef struct _RenumberDialog
{
    GtkWidget *dialog;
    GtkWidget *prefix;
    GtkWidget *interval;
    GtkWidget *example1;
    GtkWidget *example2;

    Account *parent;
    gint num_children;
} RenumberDialog;

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

static GNCAccountType last_used_account_type = ACCT_TYPE_BANK;

static GList *ac_destroy_cb_list = NULL;

/** Declarations *********************************************************/
static void gnc_account_window_set_name (AccountWindow *aw);

void gnc_account_renumber_prefix_changed_cb (GtkEditable *editable, RenumberDialog *data);
void gnc_account_renumber_interval_changed_cb (GtkSpinButton *spinbutton, RenumberDialog *data);
void gnc_account_renumber_response_cb (GtkDialog *dialog, gint response, RenumberDialog *data);

/** Implementation *******************************************************/

static void
aw_call_destroy_callbacks (Account* acc)
{
    GList *node;
    void (*cb)(Account*);

    for (node = ac_destroy_cb_list; node; node = node->next)
    {
        cb = node->data;
        (cb)(acc);
    }
}

static Account *
aw_get_account (AccountWindow *aw)
{
    if (!aw)
        return NULL;

    return xaccAccountLookup (&aw->account, aw->book);
}

static void
gnc_account_commodity_from_type (AccountWindow * aw, gboolean update)
{
    dialog_commodity_mode new_mode;

    if (aw->type == ACCT_TYPE_TRADING)
        new_mode = DIAG_COMM_ALL;
    else if ((aw->type == ACCT_TYPE_STOCK) || (aw->type == ACCT_TYPE_MUTUAL))
        new_mode = DIAG_COMM_NON_CURRENCY;
    else
        new_mode = DIAG_COMM_CURRENCY;

    if (update && (new_mode != aw->commodity_mode))
    {
        gnc_general_select_set_selected(GNC_GENERAL_SELECT (aw->commodity_edit),
                                        NULL);
    }

    aw->commodity_mode = new_mode;
}

/* Copy the account values to the GUI widgets */
static void
gnc_account_to_ui(AccountWindow *aw)
{
    Account *account;
    gnc_commodity * commodity;
    const char *string;
    GdkColor color;
    gboolean flag, nonstd_scu;
    gint index;

    ENTER("%p", aw);
    account = aw_get_account (aw);
    if (!account)
    {
        LEAVE("no account");
        return;
    }

    string = xaccAccountGetName (account);
    if (string == NULL) string = "";
    gtk_entry_set_text(GTK_ENTRY(aw->name_entry), string);

    string = xaccAccountGetDescription (account);
    if (string == NULL) string = "";
    gtk_entry_set_text(GTK_ENTRY(aw->description_entry), string);

    string = xaccAccountGetColor (account);
    if (string == NULL) string = "";
    if (gdk_color_parse(string, &color))
    {
        gtk_color_button_set_color(GTK_COLOR_BUTTON(aw->color_entry_button), &color);
    }

    commodity = xaccAccountGetCommodity (account);
    gnc_general_select_set_selected (GNC_GENERAL_SELECT (aw->commodity_edit),
                                     commodity);
    gnc_account_commodity_from_type (aw, FALSE);

    nonstd_scu = xaccAccountGetNonStdSCU (account);
    if (nonstd_scu)
    {
        index = xaccAccountGetCommoditySCUi(account);
        index = log10(index) + 1;
    }
    else
    {
        index = 0;
    }
    gtk_combo_box_set_active(GTK_COMBO_BOX(aw->account_scu), index);

    string = xaccAccountGetCode (account);
    if (string == NULL) string = "";
    gtk_entry_set_text(GTK_ENTRY(aw->code_entry), string);

    string = xaccAccountGetNotes (account);
    if (string == NULL) string = "";

    gtk_text_buffer_set_text (aw->notes_text_buffer, string, strlen(string));

    flag = xaccAccountGetTaxRelated (account);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (aw->tax_related_button),
                                  flag);

    flag = xaccAccountGetPlaceholder (account);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (aw->placeholder_button),
                                  flag);

    flag = xaccAccountGetHidden (account);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (aw->hidden_button),
                                  flag);
    LEAVE(" ");
}


static gboolean
gnc_account_create_transfer_balance (QofBook *book,
                                     Account *account,
                                     Account *transfer,
                                     gnc_numeric balance,
                                     time_t date)
{
    Transaction *trans;
    Split *split;

    if (gnc_numeric_zero_p (balance))
        return TRUE;

    g_return_val_if_fail (account != NULL, FALSE);
    g_return_val_if_fail (transfer != NULL, FALSE);

    xaccAccountBeginEdit (account);
    xaccAccountBeginEdit (transfer);

    trans = xaccMallocTransaction (book);

    xaccTransBeginEdit (trans);

    xaccTransSetCurrency (trans, xaccAccountGetCommodity (account));
    xaccTransSetDateSecs (trans, date);
    xaccTransSetDescription (trans, _("Opening Balance"));

    split = xaccMallocSplit (book);

    xaccTransAppendSplit (trans, split);
    xaccAccountInsertSplit (account, split);

    xaccSplitSetAmount (split, balance);
    xaccSplitSetValue (split, balance);

    balance = gnc_numeric_neg (balance);

    split = xaccMallocSplit (book);

    xaccTransAppendSplit (trans, split);
    xaccAccountInsertSplit (transfer, split);

    xaccSplitSetAmount (split, balance);
    xaccSplitSetValue (split, balance);

    xaccTransCommitEdit (trans);
    xaccAccountCommitEdit (transfer);
    xaccAccountCommitEdit (account);

    return TRUE;
}

/* Record the GUI values into the Account structure */
static void
gnc_ui_to_account(AccountWindow *aw)
{
    Account *account;
    gnc_commodity *commodity;
    Account *parent_account;
    const char *old_string;
    const char *string;
    GdkColor color;
    gboolean flag;
    gnc_numeric balance;
    gboolean use_equity, nonstd;
    time_t date;
    gint index, old_scu, new_scu;
    GtkTextIter start, end;

    account = aw_get_account (aw);
    if (!account)
    {
        LEAVE("no account");
        return;
    }

    if (aw->dialog_type == EDIT_ACCOUNT
            && aw->type != xaccAccountGetType (account))
    {
        /* Just refreshing won't work. */
        aw_call_destroy_callbacks (account);
    }

    xaccAccountBeginEdit (account);

    if (aw->type != xaccAccountGetType (account))
        xaccAccountSetType (account, aw->type);

    last_used_account_type = aw->type;

    string = gtk_entry_get_text (GTK_ENTRY(aw->name_entry));
    old_string = xaccAccountGetName (account);
    if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetName (account, string);

    string = gtk_entry_get_text (GTK_ENTRY(aw->description_entry));
    old_string = xaccAccountGetDescription (account);
    if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetDescription (account, string);

    gtk_color_button_get_color(GTK_COLOR_BUTTON(aw->color_entry_button), &color );
    string = gdk_color_to_string(&color);
    old_string = xaccAccountGetColor (account);
    if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetColor (account, string);

    commodity = (gnc_commodity *)
                gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));
    if (commodity &&
            !gnc_commodity_equiv(commodity, xaccAccountGetCommodity (account)))
    {
        xaccAccountSetCommodity (account, commodity);
        old_scu = 0;
    }
    else
    {
        old_scu = xaccAccountGetCommoditySCU(account);
    }

    index = gtk_combo_box_get_active(GTK_COMBO_BOX(aw->account_scu));
    nonstd = (index != 0);
    if (nonstd != xaccAccountGetNonStdSCU(account))
        xaccAccountSetNonStdSCU(account, nonstd);
    new_scu = (nonstd ? pow(10, index - 1) : gnc_commodity_get_fraction(commodity));
    if (old_scu != new_scu)
        xaccAccountSetCommoditySCU(account, new_scu);

    string = gtk_entry_get_text (GTK_ENTRY(aw->code_entry));
    old_string = xaccAccountGetCode (account);
    if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetCode (account, string);

    gtk_text_buffer_get_start_iter (aw->notes_text_buffer, &start);
    gtk_text_buffer_get_end_iter (aw->notes_text_buffer, &end);
    string = gtk_text_buffer_get_text (aw->notes_text_buffer, &start, &end, FALSE);
    old_string = xaccAccountGetNotes (account);
    if (safe_strcmp (string, old_string) != 0)
        xaccAccountSetNotes (account, string);

    flag =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (aw->tax_related_button));
    xaccAccountSetTaxRelated (account, flag);

    flag =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (aw->placeholder_button));
    xaccAccountSetPlaceholder (account, flag);

    flag =
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (aw->hidden_button));
    xaccAccountSetHidden (account, flag);

    parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));

    if (parent_account == NULL)
        parent_account = gnc_book_get_root_account(aw->book);
    if (parent_account != gnc_account_get_parent (account))
        gnc_account_append_child (parent_account, account);

    xaccAccountCommitEdit (account);

    balance = gnc_amount_edit_get_amount
              (GNC_AMOUNT_EDIT (aw->opening_balance_edit));

    if (gnc_numeric_zero_p (balance))
    {
        LEAVE("zero balance");
        return;
    }

    if (gnc_reverse_balance (account))
        balance = gnc_numeric_neg (balance);

    date = gnome_date_edit_get_time (
               GNOME_DATE_EDIT (aw->opening_balance_date_edit));

    use_equity = gtk_toggle_button_get_active
                 (GTK_TOGGLE_BUTTON (aw->opening_equity_radio));

    if (use_equity)
    {
        if (!gnc_account_create_opening_balance (account, balance, date, aw->book))
        {
            const char *message = _("Could not create opening balance.");
            gnc_error_dialog(aw->dialog, "%s", message);
        }
    }
    else
    {
        Account *transfer = NULL;

        transfer = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree));
        if (!transfer)
        {
            LEAVE("no transfer account");
            return;
        }

        gnc_account_create_transfer_balance (aw->book, account, transfer, balance, date);
    }
    LEAVE(" ");
}


static void
set_children_types (Account *account, GNCAccountType type)
{
    GList *children, *iter;

    children = gnc_account_get_children(account);
    if (children == NULL)
        return;

    for (iter = children; iter; iter = iter->next)
    {
        account = iter->data;
        if (type == xaccAccountGetType(account))
            continue;

        /* Just refreshing won't work. */
        aw_call_destroy_callbacks (account);

        xaccAccountBeginEdit (account);
        xaccAccountSetType (account, type);
        xaccAccountCommitEdit (account);

        set_children_types (account, type);
    }
    g_list_free(children);
}

static void
make_children_compatible (AccountWindow *aw)
{
    Account *account;

    g_return_if_fail (aw);

    if (aw->dialog_type == NEW_ACCOUNT)
        return;

    account = aw_get_account (aw);
    g_return_if_fail (account);

    if (xaccAccountTypesCompatible (xaccAccountGetType (account), aw->type))
        return;

    set_children_types (account, aw->type);
}


static void
gnc_finish_ok (AccountWindow *aw)
{
    ENTER("aw %p", aw);
    gnc_suspend_gui_refresh ();

    /* make the account changes */
    make_children_compatible (aw);
    gnc_ui_to_account (aw);

    gnc_resume_gui_refresh ();

    /* do it all again, if needed */
    if ((aw->dialog_type == NEW_ACCOUNT) && aw->next_name && *aw->next_name)
    {
        gnc_commodity *commodity;
        Account *parent;
        Account *account;

        gnc_suspend_gui_refresh ();

        parent = aw_get_account (aw);
        account = xaccMallocAccount (aw->book);
        aw->account = *xaccAccountGetGUID (account);
        aw->type = xaccAccountGetType (parent);

        xaccAccountSetName (account, *aw->next_name);
        aw->next_name++;

        gnc_account_to_ui (aw);

        gnc_account_window_set_name (aw);

        commodity = xaccAccountGetCommodity (parent);
        gnc_general_select_set_selected (GNC_GENERAL_SELECT (aw->commodity_edit),
                                         commodity);
        gnc_account_commodity_from_type (aw, FALSE);

        gnc_tree_view_account_set_selected_account (
            GNC_TREE_VIEW_ACCOUNT (aw->parent_tree), parent);

        gnc_resume_gui_refresh ();
        LEAVE("1");
        return;
    }

    /* save for posterity */
    aw->created_account = aw_get_account (aw);

    /* so it doesn't get freed on close */
    aw->account = *xaccGUIDNULL ();

    gnc_close_gui_component (aw->component_id);
    LEAVE("2");
}


static void
add_children_to_expander (GObject *object, GParamSpec *param_spec, gpointer data)
{
    GtkExpander *expander = GTK_EXPANDER (object);
    Account *account = data;
    GtkWidget *scrolled_window;
    GtkTreeView *view;

    if (gtk_expander_get_expanded (expander) &&
            !gtk_bin_get_child (GTK_BIN (expander)))
    {

        view = gnc_tree_view_account_new_with_root (account, FALSE);

        scrolled_window = gtk_scrolled_window_new (NULL, NULL);
        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
        gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled_window),
                                             GTK_SHADOW_IN);
        gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET (view));

        gtk_container_add (GTK_CONTAINER (expander), scrolled_window);
        gtk_widget_show_all (scrolled_window);
    }
}

/* Check whether there are children needing a type adjustment because of a
   a change to an incompatible type (like after some reparenting) and let the
   user decide whether he wants that */
static gboolean
verify_children_compatible (AccountWindow *aw)
{
    Account *account;
    GtkWidget *dialog, *vbox, *hbox, *label, *expander;
    gchar *str;
    gboolean result;

    if (aw == NULL)
        return FALSE;

    account = aw_get_account (aw);
    if (!account)
        return FALSE;

    if (xaccAccountTypesCompatible (xaccAccountGetType (account), aw->type))
        return TRUE;

    if (gnc_account_n_children(account) == 0)
        return TRUE;

    dialog = gtk_dialog_new_with_buttons ("",
                                          GTK_WINDOW(aw->dialog),
                                          GTK_DIALOG_DESTROY_WITH_PARENT |
                                          GTK_DIALOG_MODAL,
                                          GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OK, GTK_RESPONSE_OK,
                                          NULL);

    gtk_window_set_skip_taskbar_hint (GTK_WINDOW (dialog), TRUE);

    hbox = gtk_hbox_new (FALSE, 12);
    vbox = gtk_vbox_new (FALSE, 12);

    gtk_box_pack_start (
        GTK_BOX (hbox),
        gtk_image_new_from_stock (GTK_STOCK_DIALOG_INFO, GTK_ICON_SIZE_DIALOG),
        FALSE, FALSE, 0);

    /* primary label */
    label = gtk_label_new (_("Give the children the same type?"));
    gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
    gtk_label_set_selectable (GTK_LABEL (label), TRUE);
    gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.0);
    {
        gint size;
        PangoFontDescription *font_desc;

        size = pango_font_description_get_size (label->style->font_desc);
        font_desc = pango_font_description_new ();
        pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);
        pango_font_description_set_size (font_desc, size * PANGO_SCALE_LARGE);
        gtk_widget_modify_font (label, font_desc);
        pango_font_description_free (font_desc);
    }
    gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

    /* secondary label */
    str = g_strdup_printf (_("The children of the edited account have to be "
                             "changed to type \"%s\" to make them compatible."),
                           xaccAccountGetTypeStr (aw->type));
    label = gtk_label_new (str);
    g_free (str);
    gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
    gtk_label_set_selectable (GTK_LABEL (label), TRUE);
    gtk_misc_set_alignment (GTK_MISC (label), 0.0, 0.0);
    gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);

    /* children */
    expander = gtk_expander_new_with_mnemonic (_("_Show children accounts"));
    gtk_expander_set_spacing (GTK_EXPANDER (expander), 6);
    g_signal_connect (G_OBJECT (expander), "notify::expanded",
                      G_CALLBACK (add_children_to_expander), account);
    gtk_box_pack_start (GTK_BOX (vbox), expander, TRUE, TRUE, 0);

    gtk_box_pack_start (GTK_BOX (hbox), vbox, TRUE, TRUE, 0);

    gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox,
                        TRUE, TRUE, 0);

    /* spacings */
    gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
    gtk_container_set_border_width (GTK_CONTAINER (hbox), 5);
    gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (dialog)->vbox), 14);
    gtk_container_set_border_width (
        GTK_CONTAINER (GTK_DIALOG (dialog)->action_area), 5);
    gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (dialog)->action_area), 6);

    gtk_widget_show_all (hbox);

    gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    result = (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK);

    gtk_widget_destroy(dialog);

    return result;
}


static gboolean
gnc_filter_parent_accounts (Account *account, gpointer data)
{
    AccountWindow *aw = data;
    Account *aw_account = aw_get_account (aw);

    if (account == NULL)
        return FALSE;

    if (aw_account == NULL)
        return FALSE;

    if (gnc_account_is_root(account))
        return TRUE;

    if (account == aw_account)
        return FALSE;

    if (xaccAccountHasAncestor(account, aw_account))
        return FALSE;

    return TRUE;
}


static gboolean
gnc_common_ok (AccountWindow *aw)
{
    Account *root, *account, *parent;
    gnc_commodity * commodity;
    gchar *fullname, *fullname_parent;
    const gchar *name, *separator;

    ENTER("aw %p", aw);
    root = gnc_book_get_root_account (aw->book);

    separator = gnc_get_account_separator_string();

    /* check for valid name */
    name = gtk_entry_get_text(GTK_ENTRY(aw->name_entry));
    if (safe_strcmp(name, "") == 0)
    {
        const char *message = _("The account must be given a name.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE("bad name");
        return FALSE;
    }

    /* check for a duplicate name */
    parent = gnc_tree_view_account_get_selected_account
             (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
    if (parent == NULL)
    {
        account = gnc_account_lookup_by_full_name(root, name);
    }
    else
    {
        fullname_parent = gnc_account_get_full_name(parent);
        fullname = g_strconcat(fullname_parent, separator, name, NULL);

        account = gnc_account_lookup_by_full_name(root, fullname);

        g_free(fullname_parent);
        g_free(fullname);
    }
    if ((account != NULL) &&
            !guid_equal(&aw->account, xaccAccountGetGUID (account)))
    {
        const char *message = _("There is already an account with that name.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE("duplicate name");
        return FALSE;
    }

    /* Parent check, probably not needed, but be safe */
    if (!gnc_filter_parent_accounts(parent, aw))
    {
        const char *message = _("You must choose a valid parent account.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE("invalid parent");
        return FALSE;
    }

    /* check for valid type */
    if (aw->type == ACCT_TYPE_INVALID)
    {
        const char *message = _("You must select an account type.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE("invalid type");
        return FALSE;
    }

    /* check whether the types of child and parent are compatible */
    if (!xaccAccountTypesCompatible (aw->type, xaccAccountGetType (parent)))
    {
        const char *message = _("The selected account type is incompatible with "
                                "the one of the selected parent.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE("incompatible types");
        return FALSE;
    }

    /* check for commodity */
    commodity = (gnc_commodity *)
                gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));
    if (!commodity)
    {
        const char *message = _("You must choose a commodity.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE("invalid commodity");
        return FALSE;
    }

    LEAVE("passed");
    return TRUE;
}

static void
gnc_edit_account_ok(AccountWindow *aw)
{
    Account *account;

    ENTER("aw %p", aw);

    account = aw_get_account (aw);
    if (!account)
    {
        LEAVE(" ");
        return;
    }

    if (!gnc_common_ok(aw))
    {
        LEAVE(" ");
        return;
    }

    if (!verify_children_compatible (aw))
    {
        LEAVE(" ");
        return;
    }

    gnc_finish_ok (aw);
    LEAVE(" ");
}


static void
gnc_new_account_ok (AccountWindow *aw)
{
    gnc_numeric balance;

    ENTER("aw %p", aw);

    if (!gnc_common_ok(aw))
    {
        LEAVE(" ");
        return;
    }

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (aw->opening_balance_edit)))
    {
        const char *message = _("You must enter a valid opening balance "
                                "or leave it blank.");
        gnc_error_dialog(aw->dialog, "%s", message);
        LEAVE(" ");
        return;
    }

    balance = gnc_amount_edit_get_amount
              (GNC_AMOUNT_EDIT (aw->opening_balance_edit));

    if (!gnc_numeric_zero_p (balance))
    {
        gboolean use_equity;

        use_equity = gtk_toggle_button_get_active
                     (GTK_TOGGLE_BUTTON (aw->opening_equity_radio));

        if (!use_equity)
        {
            Account *transfer = NULL;

            transfer = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree));
            if (!transfer)
            {
                const char *message = _("You must select a transfer account or choose"
                                        " the opening balances equity account.");
                gnc_error_dialog(aw->dialog, "%s", message);
                LEAVE(" ");
                return;
            }
        }
    }

    gnc_finish_ok (aw);
    LEAVE(" ");
}

static void
gnc_account_window_response_cb (GtkDialog *dialog,
                                gint response,
                                gpointer data)
{
    AccountWindow *aw = data;

    ENTER("dialog %p, response %d, aw %p", dialog, response, aw);
    switch (response)
    {
    case GTK_RESPONSE_OK:
        switch (aw->dialog_type)
        {
        case NEW_ACCOUNT:
            DEBUG("new acct dialog, OK");
            gnc_new_account_ok (aw);
            break;
        case EDIT_ACCOUNT:
            DEBUG("edit acct dialog, OK");
            gnc_edit_account_ok (aw);
            break;
        default:
            g_assert_not_reached ();
            return;
        }
        break;
    case GTK_RESPONSE_HELP:
        switch (aw->dialog_type)
        {
        case NEW_ACCOUNT:
            DEBUG("new acct dialog, HELP");
            gnc_gnome_help(HF_HELP, HL_ACC);
            break;
        case EDIT_ACCOUNT:
            DEBUG("edit acct dialog, HELP");
            gnc_gnome_help(HF_HELP, HL_ACCEDIT);
            break;
        default:
            g_assert_not_reached ();
            return;
        }
        break;
    case GTK_RESPONSE_CANCEL:
    default:
        DEBUG("CANCEL");
        gnc_close_gui_component (aw->component_id);
        break;
    }
    LEAVE(" ");
}

static void
gnc_account_window_destroy_cb (GtkObject *object, gpointer data)
{
    AccountWindow *aw = data;
    Account *account;

    ENTER("object %p, aw %p", object, aw);
    account = aw_get_account (aw);

    gnc_suspend_gui_refresh ();

    switch (aw->dialog_type)
    {
    case NEW_ACCOUNT:
        if (account != NULL)
        {
            xaccAccountBeginEdit (account);
            xaccAccountDestroy (account);
            aw->account = *xaccGUIDNULL ();
        }

        DEBUG ("account add window destroyed\n");
        break;

    case EDIT_ACCOUNT:
        break;

    default:
        PERR ("unexpected dialog type\n");
        gnc_resume_gui_refresh ();
        LEAVE(" ");
        return;
    }

    gnc_unregister_gui_component (aw->component_id);

    gnc_resume_gui_refresh ();

    if (aw->subaccount_names)
    {
        g_strfreev(aw->subaccount_names);
        aw->subaccount_names = NULL;
        aw->next_name = NULL;
    }

    g_free (aw);
    LEAVE(" ");
}

static void
gnc_account_parent_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    AccountWindow *aw = data;
    Account *parent_account;
    guint32 types, old_types;
    GtkTreeModel *type_model;
    GtkTreeSelection *type_selection;
    gboolean scroll_to = FALSE;

    g_return_if_fail (aw);

    parent_account = gnc_tree_view_account_get_selected_account (
                         GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));
    if (!parent_account)
        return;

    if (gnc_account_is_root(parent_account))
    {
        types = aw->valid_types;
    }
    else
    {
        types = aw->valid_types &
                xaccParentAccountTypesCompatibleWith (xaccAccountGetType (parent_account));
    }

    type_model = gtk_tree_view_get_model (GTK_TREE_VIEW (aw->type_view));
    if (!type_model)
        return;

    if (aw->type != aw->preferred_account_type &&
            (types & (1 << aw->preferred_account_type)) != 0)
    {
        /* we can change back to the preferred account type */
        aw->type = aw->preferred_account_type;
        scroll_to = TRUE;
    }
    else if ((types & (1 << aw->type)) == 0)
    {
        /* our type is invalid now */
        aw->type = ACCT_TYPE_INVALID;
    }
    else
    {
        /* no type change, but maybe list of valid types changed */
        old_types = gnc_tree_model_account_types_get_mask (type_model);
        if (old_types != types)
            scroll_to = TRUE;
    }

    gnc_tree_model_account_types_set_mask (type_model, types);

    if (scroll_to)
    {
        type_selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (aw->type_view));
        gnc_tree_model_account_types_set_selection(type_selection, 1 << aw->type);
    }

    gnc_account_window_set_name(aw);
}

static void
gnc_account_type_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    AccountWindow *aw = data;
    gboolean sensitive;
    GNCAccountType type_id;

    g_return_if_fail (aw != NULL);

    sensitive = FALSE;

    type_id = gnc_tree_model_account_types_get_selection_single(selection);
    if (type_id == ACCT_TYPE_NONE)
    {
        aw->type = ACCT_TYPE_INVALID;
    }
    else
    {
        aw->type = type_id;
        aw->preferred_account_type = type_id;

        gnc_account_commodity_from_type (aw, TRUE);

        sensitive = (aw->type != ACCT_TYPE_EQUITY &&
                     aw->type != ACCT_TYPE_CURRENCY &&
                     aw->type != ACCT_TYPE_STOCK &&
                     aw->type != ACCT_TYPE_MUTUAL &&
                     aw->type != ACCT_TYPE_TRADING);
    }

    gtk_widget_set_sensitive (aw->opening_balance_page, sensitive);

    if (!sensitive)
    {
        gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                    gnc_numeric_zero ());
    }
}

static void
gnc_account_type_view_create (AccountWindow *aw)
{
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    GtkCellRenderer *renderer;
    GtkTreeView *view;

    if (aw->valid_types == 0)
    {
        /* no type restrictions, choose aw->type */
        aw->valid_types = xaccAccountTypesValid () | (1 << aw->type);
        aw->preferred_account_type = aw->type;
    }
    else if ((aw->valid_types & (1 << aw->type)) != 0)
    {
        /* aw->type is valid */
        aw->preferred_account_type = aw->type;
    }
    else if ((aw->valid_types & (1 << last_used_account_type)) != 0)
    {
        /* last used account type is valid */
        aw->type = last_used_account_type;
        aw->preferred_account_type = last_used_account_type;
    }
    else
    {
        /* choose first valid account type */
        int i;
        aw->preferred_account_type = aw->type;
        aw->type = ACCT_TYPE_INVALID;
        for (i = 0; i < 32; i++)
            if ((aw->valid_types & (1 << i)) != 0)
            {
                aw->type = i;
                break;
            }
    }

    model = gnc_tree_model_account_types_filter_using_mask (aw->valid_types);

    view = GTK_TREE_VIEW (aw->type_view);
    gtk_tree_view_set_model (view, model);
    g_object_unref (G_OBJECT (model));

    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_insert_column_with_attributes (
        view, -1, NULL, renderer,
        "text", GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME,
        NULL);
    gtk_tree_view_set_search_column (view, GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME);

    selection = gtk_tree_view_get_selection (view);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_account_type_changed_cb), aw);

    gnc_tree_model_account_types_set_selection(selection, 1 << aw->type);
}

static void
gnc_account_name_changed_cb(GtkWidget *widget, gpointer data)
{
    AccountWindow *aw = data;

    gnc_account_window_set_name (aw);
}

static void
gnc_account_color_default_cb(GtkWidget *widget, gpointer data)
{
    GdkColor color;
    AccountWindow *aw = data;

    gdk_color_parse( DEFAULT_COLOR, &color);
    gtk_color_button_set_color(GTK_COLOR_BUTTON(aw->color_entry_button), &color);

}

static void
commodity_changed_cb (GNCGeneralSelect *gsl, gpointer data)
{
    AccountWindow *aw = data;
    gnc_commodity *currency;
    GtkTreeSelection *selection;

    currency = (gnc_commodity *) gnc_general_select_get_selected (gsl);
    if (!currency)
        return;

    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                  gnc_commodity_get_fraction (currency));
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (aw->opening_balance_edit),
                                    gnc_commodity_print_info (currency, FALSE));

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (aw->transfer_tree));
    gtk_tree_selection_unselect_all (selection);
}

static gboolean
account_commodity_filter (GtkTreeSelection *selection,
                          GtkTreeModel *unused_model,
                          GtkTreePath *s_path,
                          gboolean path_currently_selected,
                          gpointer user_data)
{
    gnc_commodity *commodity;
    AccountWindow *aw;
    Account *account;

    g_return_val_if_fail (GTK_IS_TREE_SELECTION (selection), FALSE);

    aw = user_data;

    if (path_currently_selected)
    {
        /* already selected, don't waste time. */
        return TRUE;
    }

    account = gnc_tree_view_account_get_account_from_path (GNC_TREE_VIEW_ACCOUNT (aw->transfer_tree), s_path);
    if (!account)
    {
        return FALSE;
    }

    commodity = (gnc_commodity *)
                gnc_general_select_get_selected (GNC_GENERAL_SELECT (aw->commodity_edit));

    return gnc_commodity_equiv (xaccAccountGetCommodity (account), commodity);
}

static void
opening_equity_cb (GtkWidget *w, gpointer data)
{
    AccountWindow *aw = data;
    gboolean use_equity;

    use_equity = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));

    gtk_widget_set_sensitive (aw->transfer_account_scroll, !use_equity);
}

/********************************************************************\
 * gnc_account_window_create                                        *
 *   creates a window to create a new account.                      *
 *                                                                  *
 * Args:   aw - the information structure for this window           *
 * Return: the created window                                       *
 \*******************************************************************/
static void
gnc_account_window_create(AccountWindow *aw)
{
    GtkWidget *amount;
    GObject *awo;
    GtkWidget *box;
    GtkWidget *label;
    GladeXML  *xml;
    GtkTreeSelection *selection;

    ENTER("aw %p, modal %d", aw, aw->modal);
    xml = gnc_glade_xml_new ("account.glade", "Account Dialog");

    aw->dialog = glade_xml_get_widget (xml, "Account Dialog");
    awo = G_OBJECT (aw->dialog);

    g_object_set_data (awo, "dialog_info", aw);

    g_signal_connect (awo, "destroy",
                      G_CALLBACK (gnc_account_window_destroy_cb), aw);

    if (!aw->modal)
        g_signal_connect (awo, "response",
                          G_CALLBACK (gnc_account_window_response_cb), aw);
    else
        gtk_window_set_modal (GTK_WINDOW (aw->dialog), TRUE);

    aw->notebook = glade_xml_get_widget (xml, "account_notebook");

    aw->name_entry = glade_xml_get_widget (xml, "name_entry");
    g_signal_connect (G_OBJECT (aw->name_entry), "changed",
                      G_CALLBACK (gnc_account_name_changed_cb), aw);

    aw->description_entry = glade_xml_get_widget (xml, "description_entry");

    aw->color_entry_button = glade_xml_get_widget (xml, "color_entry_button");

    aw->color_default_button = glade_xml_get_widget (xml, "color_default_button");
    g_signal_connect (G_OBJECT (aw->color_default_button), "clicked",
                      G_CALLBACK (gnc_account_color_default_cb), aw);

    aw->code_entry =        glade_xml_get_widget (xml, "code_entry");
    aw->notes_text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (glade_xml_get_widget (xml, "notes_text")));

    box = glade_xml_get_widget (xml, "commodity_hbox");
    aw->commodity_edit = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
                         gnc_commodity_edit_get_string,
                         gnc_commodity_edit_new_select,
                         &aw->commodity_mode);
    gtk_box_pack_start(GTK_BOX(box), aw->commodity_edit, TRUE, TRUE, 0);
    gtk_widget_show (aw->commodity_edit);

    label = glade_xml_get_widget (xml, "security_label");
    gnc_general_select_make_mnemonic_target (GNC_GENERAL_SELECT(aw->commodity_edit), label);

    g_signal_connect (G_OBJECT (aw->commodity_edit), "changed",
                      G_CALLBACK (commodity_changed_cb), aw);

    aw->account_scu = glade_xml_get_widget (xml, "account_scu");

    box = glade_xml_get_widget (xml, "parent_scroll");

    aw->parent_tree = gnc_tree_view_account_new(TRUE);
    gtk_container_add(GTK_CONTAINER(box), GTK_WIDGET(aw->parent_tree));
    gtk_widget_show(GTK_WIDGET(aw->parent_tree));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (aw->parent_tree));
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_account_parent_changed_cb), aw);

    aw->tax_related_button = glade_xml_get_widget (xml, "tax_related_button");
    aw->placeholder_button = glade_xml_get_widget (xml, "placeholder_button");
    aw->hidden_button = glade_xml_get_widget (xml, "hidden_button");

    box = glade_xml_get_widget (xml, "opening_balance_box");
    amount = gnc_amount_edit_new ();
    aw->opening_balance_edit = amount;
    gtk_box_pack_start(GTK_BOX(box), amount, TRUE, TRUE, 0);
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (amount), TRUE);
    gtk_widget_show (amount);

    label = glade_xml_get_widget (xml, "balance_label");
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), amount);

    box = glade_xml_get_widget (xml, "opening_balance_date_box");
    aw->opening_balance_date_edit = glade_xml_get_widget (xml, "opening_balance_date_edit");

    aw->opening_balance_page =
        gtk_notebook_get_nth_page (GTK_NOTEBOOK (aw->notebook), 1);

    aw->opening_equity_radio = glade_xml_get_widget (xml,
                               "opening_equity_radio");
    g_signal_connect (G_OBJECT (aw->opening_equity_radio), "toggled",
                      G_CALLBACK (opening_equity_cb), aw);

    box = glade_xml_get_widget (xml, "transfer_account_scroll");
    aw->transfer_account_scroll = box;

    aw->transfer_tree = GTK_WIDGET(gnc_tree_view_account_new(FALSE));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(aw->transfer_tree));
    gtk_tree_selection_set_select_function(selection, account_commodity_filter, aw, NULL);

    gtk_container_add(GTK_CONTAINER(box), GTK_WIDGET(aw->transfer_tree));
    gtk_widget_show (GTK_WIDGET(aw->transfer_tree));

    label = glade_xml_get_widget (xml, "parent_label");
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), GTK_WIDGET(aw->parent_tree));

    /* This goes at the end so the select callback has good data. */
    aw->type_view = glade_xml_get_widget (xml, "type_view");
    gnc_account_type_view_create (aw);

    gnc_restore_window_size (GCONF_SECTION, GTK_WINDOW(aw->dialog));

    gtk_widget_grab_focus(GTK_WIDGET(aw->name_entry));
    LEAVE(" ");
}


static char *
get_ui_fullname (AccountWindow *aw)
{
    Account *parent_account;
    char *fullname;
    const gchar *name;

    name = gtk_entry_get_text (GTK_ENTRY(aw->name_entry));
    if (!name || *name == '\0')
        name = _("<No name>");

    parent_account = gnc_tree_view_account_get_selected_account (GNC_TREE_VIEW_ACCOUNT (aw->parent_tree));

    if (parent_account && !gnc_account_is_root(parent_account))
    {
        char *parent_name;
        const gchar *separator;

        parent_name = gnc_account_get_full_name (parent_account);

        separator = gnc_get_account_separator_string ();
        fullname = g_strconcat (parent_name, separator, name, NULL);

        g_free (parent_name);
    }
    else
        fullname = g_strdup (name);

    return fullname;
}

static void
gnc_account_window_set_name (AccountWindow *aw)
{
    char *fullname;
    char *title;

    if (!aw || !aw->parent_tree)
        return;

    fullname = get_ui_fullname (aw);

    if (aw->dialog_type == EDIT_ACCOUNT)
        title = g_strconcat(_("Edit Account"), " - ", fullname, NULL);
    else if (aw->next_name && (g_strv_length(aw->next_name) > 0))
    {
        const char *format = _("(%d) New Accounts");
        char *prefix;

        prefix = g_strdup_printf (format, g_strv_length(aw->next_name) + 1);

        title = g_strconcat (prefix, " - ", fullname, " ...", NULL);

        g_free (prefix);
    }
    else
        title = g_strconcat (_("New Account"), " - ", fullname, NULL);

    gtk_window_set_title (GTK_WINDOW(aw->dialog), title);

    g_free (fullname);
    g_free (title);
}


static void
close_handler (gpointer user_data)
{
    AccountWindow *aw = user_data;

    ENTER("aw %p, modal %d", aw, aw->modal);
    gnc_save_window_size (GCONF_SECTION, GTK_WINDOW(aw->dialog));

    gtk_widget_destroy (GTK_WIDGET (aw->dialog));
    LEAVE(" ");
}


/********************************************************************\
 * gnc_ui_refresh_account_window                                    *
 *   refreshes the edit window                                      *
 *                                                                  *
 * Args:   aw - the account window to refresh                       *
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_refresh_account_window (AccountWindow *aw)
{
    if (aw == NULL)
        return;

    /*  gnc_account_tree_refresh (GNC_ACCOUNT_TREE(aw->parent_tree));*/

    gnc_account_window_set_name (aw);
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    AccountWindow *aw = user_data;
    const EventInfo *info;
    Account *account;

    account = aw_get_account (aw);
    if (!account)
    {
        gnc_close_gui_component (aw->component_id);
        return;
    }

    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &aw->account);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (aw->component_id);
            return;
        }
    }

    gnc_ui_refresh_account_window (aw);
}


static AccountWindow *
gnc_ui_new_account_window_internal (QofBook *book,
                                    Account *base_account,
                                    gchar **subaccount_names,
                                    GList *valid_types,
                                    gnc_commodity * default_commodity,
                                    gboolean modal)
{
    gnc_commodity *commodity, *parent_commodity;
    AccountWindow *aw;
    Account *account;
    GList *list;

    g_return_val_if_fail(book, NULL);

    aw = g_new0 (AccountWindow, 1);

    aw->book = book;
    aw->modal = modal;
    aw->dialog_type = NEW_ACCOUNT;

    aw->valid_types = 0;
    for (list = valid_types; list; list = list->next)
        aw->valid_types |= (1 << GPOINTER_TO_INT (list->data));

    account = xaccMallocAccount (book);
    aw->account = *xaccAccountGetGUID (account);

    if (base_account)
    {
        aw->type = xaccAccountGetType (base_account);
        parent_commodity = xaccAccountGetCommodity (base_account);
    }
    else
    {
        aw->type = last_used_account_type;
        parent_commodity = gnc_default_currency ();
    }

    gnc_suspend_gui_refresh ();

    if (subaccount_names && *subaccount_names)
    {
        xaccAccountSetName (account, subaccount_names[0]);
        aw->subaccount_names = subaccount_names;
        aw->next_name = subaccount_names + 1;
    }

    gnc_account_window_create (aw);
    gnc_account_to_ui (aw);

    gnc_resume_gui_refresh ();

    if (default_commodity != NULL)
    {
        commodity = default_commodity;
        if ((aw->type == ACCT_TYPE_STOCK) || (aw->type == ACCT_TYPE_MUTUAL))
        {
            gtk_entry_set_text(GTK_ENTRY(aw->name_entry),
                               (gpointer) gnc_commodity_get_mnemonic(commodity));
            gtk_entry_set_text(GTK_ENTRY(aw->description_entry),
                               (gpointer) gnc_commodity_get_fullname(commodity));
        }
    }
    else if ((aw->type != ACCT_TYPE_STOCK) && (aw->type != ACCT_TYPE_MUTUAL))
    {
        commodity = parent_commodity;
    }
    else
    {
        commodity = NULL;
    }
    gnc_general_select_set_selected (GNC_GENERAL_SELECT (aw->commodity_edit),
                                     commodity);
    gnc_account_commodity_from_type (aw, FALSE);

    if (base_account == NULL)
    {
        base_account = gnc_book_get_root_account(book);
    }

    gtk_tree_view_collapse_all (aw->parent_tree);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT (aw->parent_tree), base_account);

    gtk_widget_show (aw->dialog);

    gnc_window_adjust_for_screen (GTK_WINDOW(aw->dialog));

    gnc_account_window_set_name (aw);

    aw->component_id = gnc_register_gui_component (DIALOG_NEW_ACCOUNT_CM_CLASS,
                       refresh_handler,
                       modal ? NULL : close_handler,
                       aw);

    gnc_gui_component_set_session (aw->component_id, gnc_get_current_session());
    gnc_gui_component_watch_entity_type (aw->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    return aw;
}


static gchar **
gnc_split_account_name (QofBook *book, const char *in_name, Account **base_account)
{
    Account *root, *account;
    gchar **names, **ptr, **out_names;
    GList *list, *node;

    root = gnc_book_get_root_account (book);
    list = gnc_account_get_children(root);
    names = g_strsplit(in_name, gnc_get_account_separator_string(), -1);

    for (ptr = names; *ptr; ptr++)
    {
        /* Stop if there are no children at the current level. */
        if (list == NULL)
            break;

        /* Look for the first name in the children. */
        for (node = list; node; node = g_list_next(node))
        {
            account = node->data;

            if (safe_strcmp(xaccAccountGetName (account), *ptr) == 0)
            {
                /* We found an account. */
                *base_account = account;
                break;
            }
        }

        /* Was there a match?  If no, stop the traversal. */
        if (node == NULL)
            break;

        g_list_free(list);
        list = gnc_account_get_children (account);
    }

    out_names = g_strdupv(ptr);
    g_strfreev(names);
    if (list)
        g_list_free(list);
    return out_names;
}


/************************************************************
 *              Entry points for a Modal Dialog             *
 ************************************************************/

Account *
gnc_ui_new_accounts_from_name_window (const char *name)
{
    return  gnc_ui_new_accounts_from_name_with_defaults (name, NULL, NULL, NULL);
}

Account *
gnc_ui_new_accounts_from_name_window_with_types (const char *name,
        GList *valid_types)
{
    return gnc_ui_new_accounts_from_name_with_defaults(name, valid_types, NULL, NULL);
}

Account *
gnc_ui_new_accounts_from_name_with_defaults (const char *name,
        GList *valid_types,
        gnc_commodity * default_commodity,
        Account * parent)
{
    QofBook *book;
    AccountWindow *aw;
    Account *base_account = NULL;
    Account *created_account = NULL;
    gchar ** subaccount_names;
    gint response;
    gboolean done = FALSE;

    ENTER("name %s, valid %p, commodity %p, account %p",
          name, valid_types, default_commodity, parent);
    book = gnc_get_current_book();
    if (!name || *name == '\0')
    {
        subaccount_names = NULL;
        base_account = NULL;
    }
    else
        subaccount_names = gnc_split_account_name (book, name, &base_account);

    if (parent != NULL)
    {
        base_account = parent;
    }
    aw = gnc_ui_new_account_window_internal (book, base_account, subaccount_names,
            valid_types, default_commodity,
            TRUE);

    while (!done)
    {
        response = gtk_dialog_run (GTK_DIALOG(aw->dialog));

        /* This can destroy the dialog */
        gnc_account_window_response_cb (GTK_DIALOG(aw->dialog), response, (gpointer)aw);

        switch (response)
        {
        case GTK_RESPONSE_OK:
            created_account = aw->created_account;
            done = (created_account != NULL);
            break;

        case GTK_RESPONSE_HELP:
            done = FALSE;
            break;

        default:
            done = TRUE;
            break;
        }
    }

    close_handler(aw);
    LEAVE("created %s (%p)", xaccAccountGetName(created_account), created_account);
    return created_account;
}

/************************************************************
 *            Entry points for a non-Modal Dialog           *
 ************************************************************/

static gboolean
find_by_account (gpointer find_data, gpointer user_data)
{
    Account *account = find_data;
    AccountWindow *aw = user_data;

    if (!aw)
        return FALSE;

    return guid_equal (&aw->account, xaccAccountGetGUID (account));
}

/*
 * opens up a window to edit an account
 *
 * Args:   account - the account to edit
 * Return: EditAccountWindow object
 */
void
gnc_ui_edit_account_window(Account *account)
{
    AccountWindow * aw;
    Account *parent;

    if (account == NULL)
        return;

    aw = gnc_find_first_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                                       find_by_account, account);
    if (aw)
    {
        gtk_window_present(GTK_WINDOW(aw->dialog));
        return;
    }

    aw = g_new0 (AccountWindow, 1);

    aw->book = gnc_account_get_book(account);
    aw->modal = FALSE;
    aw->dialog_type = EDIT_ACCOUNT;
    aw->account = *xaccAccountGetGUID (account);
    aw->subaccount_names = NULL;
    aw->type = xaccAccountGetType (account);

    gnc_suspend_gui_refresh ();

    gnc_account_window_create (aw);
    gnc_account_to_ui (aw);

    gnc_resume_gui_refresh ();

    gtk_widget_show_all (aw->dialog);
    gtk_widget_hide (aw->opening_balance_page);

    parent = gnc_account_get_parent (account);
    if (parent == NULL)
        parent = account;		/* must be at the root */

    gtk_tree_view_collapse_all (aw->parent_tree);
    gnc_tree_view_account_set_selected_account (
        GNC_TREE_VIEW_ACCOUNT(aw->parent_tree), parent);

    gnc_account_window_set_name (aw);

    gnc_window_adjust_for_screen(GTK_WINDOW(aw->dialog));

    aw->component_id = gnc_register_gui_component (DIALOG_EDIT_ACCOUNT_CM_CLASS,
                       refresh_handler,
                       close_handler, aw);

    gnc_gui_component_set_session (aw->component_id, gnc_get_current_session());
    gnc_gui_component_watch_entity_type (aw->component_id,
                                         GNC_ID_ACCOUNT,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_window_present(GTK_WINDOW(aw->dialog));
}


/*
 * opens up a window to create a new account
 *
 * Args:    book - containing book for the new account
 *        parent - The initial parent for the new account (optional)
 */
void
gnc_ui_new_account_window(QofBook *book, Account *parent)
{
    g_return_if_fail(book != NULL);
    if (parent && book)
        g_return_if_fail(gnc_account_get_book(parent) == book);

    gnc_ui_new_account_window_internal (book, parent, NULL, NULL, NULL, FALSE);
}

void
gnc_ui_new_account_with_types( QofBook *book,
                               GList *valid_types )
{
    gnc_ui_new_account_window_internal( book, NULL, NULL, valid_types, NULL, FALSE );
}

/************************************************************
 *             Callbacks for a non-Modal Dialog             *
 ************************************************************/

/*
 * register a callback that gets called when the account has changed
 * so significantly that you need to destroy yourself.  In particular
 * this is used by the ledger display to destroy ledgers when the
 * account type has changed.
 */
void
gnc_ui_register_account_destroy_callback (void (*cb)(Account *))
{
    if (!cb)
        return;

    if (g_list_index (ac_destroy_cb_list, cb) == -1)
        ac_destroy_cb_list = g_list_append (ac_destroy_cb_list, cb);

    return;
}

/**************************************************/

static void
gnc_account_renumber_update_examples (RenumberDialog *data)
{
    gchar *str;
    gchar *prefix;
    gint interval, num_digits;

    prefix = gtk_editable_get_chars(GTK_EDITABLE(data->prefix), 0, -1);
    interval = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(data->interval));
    num_digits = log10(data->num_children * interval) + 1;

    str = g_strdup_printf("%s-%0*d", prefix, num_digits, interval);
    gtk_label_set_text(GTK_LABEL(data->example1), str);
    g_free(str);

    str = g_strdup_printf("%s-%0*d", prefix, num_digits,
                          interval * data->num_children);
    gtk_label_set_text(GTK_LABEL(data->example2), str);
    g_free(str);

    g_free(prefix);
}

void
gnc_account_renumber_prefix_changed_cb (GtkEditable *editable,
                                        RenumberDialog *data)
{
    gnc_account_renumber_update_examples(data);
}

void
gnc_account_renumber_interval_changed_cb (GtkSpinButton *spinbutton,
        RenumberDialog *data)
{
    gnc_account_renumber_update_examples(data);
}

void
gnc_account_renumber_response_cb (GtkDialog *dialog,
                                  gint response,
                                  RenumberDialog *data)
{
    GList *children, *tmp;
    gchar *str;
    gchar *prefix;
    gint interval, num_digits, i;

    if (response == GTK_RESPONSE_OK)
    {
        gtk_widget_hide(data->dialog);
        children = gnc_account_get_children(data->parent);
        prefix = gtk_editable_get_chars(GTK_EDITABLE(data->prefix), 0, -1);
        interval =
            gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(data->interval));
        num_digits = log10(data->num_children * interval) + 1;

        gnc_set_busy_cursor (NULL, TRUE);
        for (tmp = children, i = 1; tmp; tmp = g_list_next(tmp), i += 1)
        {
            str = g_strdup_printf("%s-%0*d", prefix, num_digits, interval * i);
            xaccAccountSetCode(tmp->data, str);
            g_free(str);
        }
        gnc_unset_busy_cursor (NULL);
        g_list_free(children);
    }

    gtk_widget_destroy(data->dialog);
    g_free(data);
}

void
gnc_account_renumber_create_dialog (GtkWidget *window, Account *account)
{
    RenumberDialog *data;
    GladeXML *xml;
    GtkWidget *widget;
    gchar *string;

    data = g_new(RenumberDialog, 1);
    data->parent = account;
    data->num_children = gnc_account_n_children(account);

    xml = gnc_glade_xml_new ("account.glade", "Renumber Accounts");
    data->dialog = glade_xml_get_widget (xml, "Renumber Accounts");
    gtk_window_set_transient_for(GTK_WINDOW(data->dialog), GTK_WINDOW(window));
    g_object_set_data_full(G_OBJECT(data->dialog), "xml", xml, g_object_unref);

    widget = glade_xml_get_widget (xml, "header_label");
    string = g_strdup_printf(_( "Renumber the immediate sub-accounts of %s?  "
                                "This will replace the account code field of "
                                "each child account with a newly generated code."),
                             gnc_account_get_full_name(account));
    gtk_label_set_text(GTK_LABEL(widget), string);
    g_free(string);

    data->prefix = glade_xml_get_widget (xml, "prefix_entry");
    data->interval = glade_xml_get_widget (xml, "interval_spin");
    data->example1 = glade_xml_get_widget (xml, "example1_label");
    data->example2 = glade_xml_get_widget (xml, "example2_label");

    gtk_entry_set_text(GTK_ENTRY(data->prefix), xaccAccountGetCode(account));
    gnc_account_renumber_update_examples(data);

    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func,
                                      data);

    gtk_widget_show_all(data->dialog);
}
