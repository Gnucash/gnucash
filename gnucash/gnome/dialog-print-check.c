/********************************************************************\
 * dialog-print-check.c : dialog to control check printing.         *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (C) 2006,2007 David Hampton <hampton@employees.org>    *
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

/**
 * @file dialog-print-check.c
 * @brief Print Checks Dialog
 * @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
 * @author Copyright (C) 2006,2007 David Hampton <hampton@employees.org>
 */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <locale.h>
#include <math.h>

#include "qof.h"
#include "gnc-date.h"
#include "gnc-prefs.h"
#include "gnc-numeric.h"
#include "gnc-plugin-page-register.h"
#include "dialog-print-check.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "print-session.h"
#include "gnc-ui.h"
#include "gnc-date-format.h"
#include "gnc-ui-util.h"
#include "gnc-path.h"
#include "gnc-filepath-utils.h"
#include "gnc-gkeyfile-utils.h"

#include "gnc-engine.h"
#include "engine-helpers.h"
#include "Split.h"
#include "Transaction.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.printing.checks"

/* This static indicates the debugging module that this .o belongs to.
 */
G_GNUC_UNUSED static QofLogModule log_module = "gnc.printing.checks";

#define GNC_PREFS_GROUP             "dialogs.checkprinting"
#define GNC_PREF_CHECK_FORMAT_GUID  "check-format-guid"
#define GNC_PREF_CHECK_POSITION     "check-position"
#define GNC_PREF_FIRST_PAGE_COUNT   "first-page-count"
#define GNC_PREF_DATE_FORMAT_USER   "date-format-user"
#define GNC_PREF_CUSTOM_PAYEE       "custom-payee"
#define GNC_PREF_CUSTOM_DATE        "custom-date"
#define GNC_PREF_CUSTOM_WORDS       "custom-amount-words"
#define GNC_PREF_CUSTOM_NUMBER      "custom-amount-number"
#define GNC_PREF_CUSTOM_ADDRESS     "custom-address"
#define GNC_PREF_CUSTOM_NOTES       "custom-notes"
#define GNC_PREF_CUSTOM_MEMO        "custom-memo"
#define GNC_PREF_CUSTOM_TRANSLATION "custom-translation"
#define GNC_PREF_CUSTOM_ROTATION    "custom-rotation"
#define GNC_PREF_CUSTOM_UNITS       "custom-units"
#define GNC_PREF_PRINT_DATE_FMT     "print-date-format"
#define GNC_PREF_DEFAULT_FONT       "default-font"
#define GNC_PREF_BLOCKING_CHARS     "blocking-chars"
#define GNC_PREF_SPLITS_AMOUNT      "splits-amount"
#define GNC_PREF_SPLITS_MEMO        "splits-memo"
#define GNC_PREF_SPLITS_ACCOUNT     "splits-account"


#define DEFAULT_FONT            "sans 12"
#define CHECK_FMT_DIR           "checks"
#define CHECK_NAME_EXTENSION    ".chk"
#define DEGREES_TO_RADIANS      (G_PI / 180.0)

#define BLOCKING_CHAR_OFF	0
#define BLOCKING_CHAR_ON	1

#define KF_GROUP_TOP       "Top"
#define KF_GROUP_POS       "Check Positions"
#define KF_GROUP_ITEMS     "Check Items"
#define KF_KEY_GUID        "Guid"
#define KF_KEY_TITLE       "Title"
#define KF_KEY_ROTATION    "Rotation"
#define KF_KEY_TRANSLATION "Translation"
#define KF_KEY_FONT        "Font"
#define KF_KEY_ALIGN       "Align"
#define KF_KEY_BLOCKING    "Blocking_Chars"
#define KF_KEY_SHOW_GRID   "Show_Grid"
#define KF_KEY_SHOW_BOXES  "Show_Boxes"
#define KF_KEY_NAMES       "Names"
#define KF_KEY_HEIGHT      "Height"
#define KF_KEY_TYPE        "Type"
#define KF_KEY_COORDS      "Coords"
#define KF_KEY_TEXT        "Text"
#define KF_KEY_FILENAME    "Filename"
#define KF_KEY_DATE_FORMAT "DateFormat"
#define KF_KEY_SPLITS_AMOUNT  "SplitsAmount"
#define KF_KEY_SPLITS_MEMO    "SplitsMemo"
#define KF_KEY_SPLITS_ACCOUNT "SplitsAccount"

/* This enum specifies the columns used in the check format combobox.
 */
typedef enum format_combo_col_t
{
    COL_NAME = 0,               /**< This column holds a copy of the check
                                 *   format name and is what is displayed to
                                 *   the user in the combobox. It is NULL for
                                 *   separator lines. */
    COL_DATA,                   /**< This column holds a pointer to the check
                                 *   format data read in from a file.  It is
                                 *   NULL for the custom check format and for
                                 *   separator lines. */
    COL_SEP,                    /**< This column contains the value TRUE if
                                 *   this entry specifies a separator line. */
} format_combo_col;

static void gnc_print_check_format_changed (GObject *object, GParamSpec *pspec, PrintCheckDialog *pcd);
static void gnc_print_check_position_changed (GObject *object, GParamSpec *pspec, PrintCheckDialog *pcd);
static void gnc_print_check_save_button_clicked (GtkButton *button, PrintCheckDialog *pcd);
static void gnc_check_format_title_changed (GtkEditable *editable, GtkWidget *ok_button);
static gboolean print_check_close_request (GtkWindow *window, PrintCheckDialog *pcd);
static void print_check_help_clicked (GtkButton *button, PrintCheckDialog *pcd);
static void print_check_cancel_clicked (GtkButton *button, PrintCheckDialog *pcd);
static void print_check_clicked (GtkButton *button, PrintCheckDialog *pcd);
static void initialize_format_dropdown (PrintCheckDialog *pcd);
gchar* get_check_address (PrintCheckDialog *pcd);
static gboolean check_format_has_address (PrintCheckDialog *pcd);
gchar* get_check_splits_amount (PrintCheckDialog *pcd);
gchar* get_check_splits_memo (PrintCheckDialog *pcd);
gchar* get_check_splits_account (PrintCheckDialog *pcd);

/* This enum defines the types of items that gnucash knows how to
 * print on checks.  Most refer to specific fields from a gnucash
 * transaction and split, but some are generic items unrelated to
 * gnucash.
 */
#define ENUM_CHECK_ITEM_TYPE(_) \
        _(NONE,) \
        _(PAYEE,) \
        _(DATE,) \
        _(NOTES,) \
        _(CHECK_NUMBER,) \
                  \
        _(MEMO,) \
        _(ACTION,) \
        _(AMOUNT_NUMBER,) \
        _(AMOUNT_WORDS,) \
                         \
        _(TEXT,) \
        _(ADDRESS,) \
        _(DATE_FORMAT,) \
        _(SPLITS_AMOUNT,) \
        _(SPLITS_MEMO,) \
        _(SPLITS_ACCOUNT,) \
        _(PICTURE,)

DEFINE_ENUM(CheckItemType, ENUM_CHECK_ITEM_TYPE)
FROM_STRING_DEC(CheckItemType, ENUM_CHECK_ITEM_TYPE)
FROM_STRING_FUNC(CheckItemType, ENUM_CHECK_ITEM_TYPE)
AS_STRING_DEC(CheckItemType, ENUM_CHECK_ITEM_TYPE)
AS_STRING_FUNC(CheckItemType, ENUM_CHECK_ITEM_TYPE)

/* This data structure describes a single item printed on a check.
 * It is built from a description in a text file.
 */
typedef struct _check_item
{

    CheckItemType type;         /**< What type of item is this?  */

    gdouble x, y;               /**< The x/y coordinates where this item should
                                 *   be printed.  The origin for these
                                 *   coordinates is determined by the print
                                 *   system used.  */

    gdouble w, h;               /**< Optional. The width and height of this
                                 *   item.  Text will be clipped to this
                                 *   size. Pictures will be shrunk to fit if
                                 *   necessary.  */

    gchar *filename;            /**< The filename for picture items. Otherwise
                                 *   unused. */

    gchar *text;                /**< The text to be displayed (for text based
                                 *   items.) Otherwise unused.  */

    gchar *font;                /**< The font to use for text based items. This
                                 *   overrides any font in the check format.
                                 *   Unused for non-text items. */

    gboolean blocking;          /**< Optional. Overrides blocking in the check
                                 *   format. Default is no blocking characters
                                 *   are written.  Unused for non-text
                                 *   items. */

    gboolean print_date_format; /**< Optional.  Print date format.  Only
                                 *   applies to DATE items.  Default is no
                                 *   format is written. */

    PangoAlignment align;       /**< The alignment of a text based item. Only
                                 *   used for text based items when a width is
                                 *   specified.  */
} check_item_t;

/* This data structure describes an entire page of checks.  Depending
 * upon the check format, the page may contain multiple checks or
 * only a single check.  The data structure is built from a
 * description in a text file.
 */
typedef struct _check_format
{

    gchar *guid;                /**< Unique identifier for this format. */

    const gchar *group;         /**< The group where this format was found. */

    gchar *filename;            /**< The name of the file from which this data
                                 *   was read.  */

    gchar *title;               /**< Title of this check format. Displayed in
                                 *   the dialog box. */

    gboolean blocking;          /**< Default for printing blocking characters for
                                 *   this page of checks.  */

    gboolean print_date_format; /**< Default for printing date format characters for
                                 *   this page of checks.  */

    gboolean show_grid;         /**< Print a grid pattern on the page */

    gboolean show_boxes;        /**< Print boxes when width and height are
                                 *   known. */

    gdouble rotation;           /**< Rotate the entire page by this amount. */

    gdouble trans_x;            /**< Move entire page left by this amount. */

    gdouble trans_y;            /**< Move entire page down by this amount. */

    gchar *font;                /**< Default font for this page of checks. */

    gdouble height;             /**< Height of one check on a page. */

    GSList *positions;          /**< Names of the checks on the page. */

    GSList *items;              /**< List of items printed on each check. */
} check_format_t;


typedef struct _print_check_request PrintCheckRequest;

/* This data structure is used to manage the print check dialog, and
 * the overall check printing process.  It contains pointers to many
 * of the widgets in the dialog, pointers to the check descriptions
 * that have been read, and also contains the data from the gnucash
 * transaction/split that is to be printed.
 */
struct _print_check_dialog
{
    GtkWindow *window;
    GtkWindow *caller_window;
    Split *split;
    GList *splits;
    Account* account;
    GtkDropDown *format_dropdown;
    guint format_max;
    guint last_format;
    GtkDropDown *position_dropdown;
    guint position_max;
    GtkSpinButton *first_page_count;
    GtkWidget *custom_table;
    GtkSpinButton *payee_x, *payee_y;
    GtkSpinButton *date_x, *date_y;
    GtkSpinButton *words_x, *words_y;
    GtkSpinButton *number_x, *number_y;
    GtkSpinButton *address_x, *address_y;
    GtkSpinButton *notes_x, *notes_y;
    GtkSpinButton *memo_x, *memo_y;
    GtkSpinButton *splits_amount_x, *splits_amount_y;
    GtkSpinButton *splits_memo_x, *splits_memo_y;
    GtkSpinButton *splits_account_x, *splits_account_y;
    GtkSpinButton *translation_x, *translation_y;
    GtkSpinButton *check_rotation;
    GtkWidget *translation_label;
    GtkDropDown *units_dropdown;
    GtkWidget *date_format;
    GtkWidget *check_address_name;
    GtkWidget *check_address_1;
    GtkWidget *check_address_2;
    GtkWidget *check_address_3;
    GtkWidget *check_address_4;
    gchar *default_font;
    check_format_t *selected_format;
    GListStore *format_model;
    GtkWindow *format_title_window;
    PrintCheckRequest *print_request;
    gboolean close_after_print;
    gboolean closing;
};

struct _print_check_request
{
    GWeakRef window;
    GtkPrintDialog *dialog;
    GCancellable *cancellable;
    GFile *file;
};

static GQuark format_data_quark (void)
{ return g_quark_from_static_string ("gnc-print-check-format"); }
static GQuark format_separator_quark (void)
{ return g_quark_from_static_string ("gnc-print-check-separator"); }
static void free_check_format (check_format_t *data);

static GObject *
format_row_new (const gchar *name, check_format_t *format, gboolean separator)
{
    GtkStringObject *row = gtk_string_object_new (name ? name : "");
    if (format)
        g_object_set_qdata_full (G_OBJECT (row), format_data_quark (), format,
                                 (GDestroyNotify)free_check_format);
    if (separator)
        g_object_set_qdata (G_OBJECT (row), format_separator_quark (),
                            GINT_TO_POINTER (TRUE));
    return G_OBJECT (row);
}

static check_format_t *
format_row_get_format (GObject *row)
{ return row ? g_object_get_qdata (row, format_data_quark ()) : NULL; }

static gboolean
format_row_is_separator (GObject *row)
{ return row && GPOINTER_TO_INT (g_object_get_qdata (row, format_separator_quark ())); }

static check_format_t *
find_existing_format (GListModel *model, const gchar *guid, guint *position_out)
{
    guint n_items = g_list_model_get_n_items (model);
    for (guint position = 0; position < n_items; position++)
    {
        GObject *row = g_list_model_get_item (model, position);
        check_format_t *format = format_row_get_format (row);
        gboolean found = format && g_strcmp0 (format->guid, guid) == 0;
        g_object_unref (row);
        if (found)
        {
            if (position_out)
                *position_out = position;
            return format;
        }
    }
    return NULL;
}

static check_format_t *
selected_format (PrintCheckDialog *pcd)
{
    GListModel *model = gtk_drop_down_get_model (pcd->format_dropdown);
    guint position = gtk_drop_down_get_selected (pcd->format_dropdown);
    GObject *row;
    check_format_t *format;
    if (!model || position == GTK_INVALID_LIST_POSITION)
        return NULL;
    row = g_list_model_get_item (model, position);
    format = format_row_get_format (row);
    g_object_unref (row);
    return format;
}

static gint
selected_position (GtkDropDown *dropdown)
{
    guint position = gtk_drop_down_get_selected (dropdown);
    return position == GTK_INVALID_LIST_POSITION ? 0 : (gint)position;
}
/* This function returns a string containing the check address in a five-line
 * format.
 *
 * Note that the string needs to be freed with g_free.
 */
gchar *
get_check_address( PrintCheckDialog *pcd)
{
    gchar *address;
    address = g_strconcat(gnc_entry_get_text(GTK_ENTRY(pcd->check_address_name)), "\n",
                          gnc_entry_get_text(GTK_ENTRY(pcd->check_address_1)), "\n",
                          gnc_entry_get_text(GTK_ENTRY(pcd->check_address_2)), "\n",
                          gnc_entry_get_text(GTK_ENTRY(pcd->check_address_3)), "\n",
                          gnc_entry_get_text(GTK_ENTRY(pcd->check_address_4)),
                          NULL);
    return address;
}

struct _trans_amount
{
    const Transaction* trans;
    gnc_numeric amount;
};

static void
subtotal_subaccount(const Account *account, struct _trans_amount* trans_amount)
{
    /* Get the amount of this account in the transaction.*/
    gnc_numeric amount = xaccTransGetAccountAmount(trans_amount->trans, account);
    /* Accumulate. */
    trans_amount->amount = gnc_numeric_add_fixed(trans_amount->amount, amount);
}

/* This function returns the amount of the check.
 */
static gnc_numeric
get_check_amount(PrintCheckDialog *pcd)
{
    gnc_numeric amount;
    if (pcd->account)
    {
        /* A parent account, e.g. from a subaccount register plugin page.
         * Subtotal the amount of all splits from descendant accounts. */
        struct _trans_amount trans_amount;
        trans_amount.trans = xaccSplitGetParent(pcd->split);
        trans_amount.amount = gnc_numeric_zero();
        gnc_account_foreach_descendant(pcd->account, (AccountCb)subtotal_subaccount, &trans_amount);
        amount = trans_amount.amount;
    }
    else
    {
        /* Print just the amount of the split. */
        amount = xaccSplitGetAmount(pcd->split);
    }
    return gnc_numeric_abs(amount);
}

//@{
/** @name Split printing functions */

/* This function formats the splits amounts for printing.
 */
gchar *
get_check_splits_amount(PrintCheckDialog *pcd)
{
    gchar* amount = NULL;
    Transaction *trans;
    GList *node;
    SplitList* s_list;

    trans = xaccSplitGetParent(pcd->split);
    s_list = xaccTransGetSplitList(trans);
    if ( !s_list ) return NULL;

    amount = g_strconcat("", NULL);
    node = s_list;
    while ( node )
    {
        Split *split = node->data;
        /* Include all splits except the main split for the check */
        if (split != pcd->split)
        {
            const gchar* split_amount;
            gchar* amt_temp;
            split_amount = xaccPrintAmount(xaccSplitGetAmount(split), gnc_split_amount_print_info(split, TRUE));
            amt_temp = amount;
            if (amount && *amount)
                amount = g_strconcat(amt_temp, "\n", split_amount, NULL);
            else
                amount = g_strconcat(amt_temp, split_amount, NULL);
            g_free(amt_temp);
        }
        node = node->next;
    }
    return amount;
}


/* This function formats the splits memo fields for printing.
 */
gchar *
get_check_splits_memo(PrintCheckDialog *pcd)
{
    gchar* memo = NULL;
    const gchar* split_memo;
    Transaction *trans;
    GList *node;
    SplitList* s_list;

    trans = xaccSplitGetParent(pcd->split);
    s_list = xaccTransGetSplitList(trans);
    if ( !s_list ) return NULL;

    memo = g_strconcat("", NULL);
    node = s_list;
    while ( node )
    {
        Split *split = node->data;
        /* Include all splits except the main split for the check */
        if (split != pcd->split)
        {
            gchar* memo_temp;
            split_memo = xaccSplitGetMemo(split);
            memo_temp = memo;
            if (memo && *memo)
                memo = g_strconcat(memo_temp, "\n", split_memo, NULL);
            else
                memo = g_strconcat(memo_temp, split_memo, NULL);
            g_free(memo_temp);
        }
        node = node->next;
    }
    return memo;
}


/* This function formats the splits accounts for printing.
 */
gchar *
get_check_splits_account(PrintCheckDialog *pcd)
{
    gchar* account = NULL;
    Transaction *trans;
    GList *node;
    SplitList* s_list;

    trans = xaccSplitGetParent(pcd->split);
    s_list = xaccTransGetSplitList(trans);
    if ( !s_list ) return NULL;

    account = g_strconcat("", NULL);
    node = s_list;
    while ( node )
    {
        Split *split = node->data;
        /* Include all splits except the main split for the check */
        if (split != pcd->split)
        {
            gchar* account_temp;
            const gchar* aName = NULL;
            Account *pAccount;
            pAccount = xaccSplitGetAccount(split);
            aName = gnc_get_account_name_for_register(pAccount);
            account_temp = account;
            if (account && *account)
                account = g_strconcat(account_temp, "\n", aName, NULL);
            else
                account = g_strconcat(account_temp, aName, NULL);
            g_free(account_temp);
        }
        node = node->next;
    }
    return account;
}
//@}


/* This function determines if an address item is present in the check format.
 */
static gboolean
check_format_has_address ( PrintCheckDialog *pcd )
{
    /* check format for an ADDRESS item */
    check_item_t *item = NULL;
    GSList *elem;
    check_format_t *format = NULL;

    if ( !pcd ) return FALSE;

    /* If we're printing more than one check no addresses are allowed */
    if (g_list_length(pcd->splits) != 1)
        return FALSE;

    /* if format is NULL, then the custom format is being used
     * which has an ADDRESS item by definition */
    format = pcd->selected_format;
    if ( !format ) return TRUE;

    for (elem = pcd->selected_format->items; elem; elem = g_slist_next(elem))
    {
        item = elem->data;
        if ( item->type == ADDRESS ) return TRUE;
    }
    return FALSE;
}


static void
gnc_ui_print_save_dialog(PrintCheckDialog *pcd)
{
    const gchar *format;
    check_format_t *check = selected_format (pcd);
    gint active;

    gnc_prefs_set_string (GNC_PREFS_GROUP, GNC_PREF_CHECK_FORMAT_GUID,
                          check ? check->guid : "custom");
    gnc_prefs_set_int(GNC_PREFS_GROUP, GNC_PREF_CHECK_POSITION,
                      selected_position (pcd->position_dropdown));
    gnc_prefs_set_int(GNC_PREFS_GROUP, GNC_PREF_FIRST_PAGE_COUNT,
                      gtk_spin_button_get_value_as_int(pcd->first_page_count));
    active = gnc_date_format_get_format (GNC_DATE_FORMAT(pcd->date_format));
    gnc_prefs_set_int(GNC_PREFS_GROUP, GNC_PREF_DATE_FORMAT, active);
    if (active == QOF_DATE_FORMAT_CUSTOM)
    {
        format = gnc_date_format_get_custom (GNC_DATE_FORMAT(pcd->date_format));
        gnc_prefs_set_string (GNC_PREFS_GROUP, GNC_PREF_DATE_FORMAT_USER, format);
    }
    else
        gnc_prefs_reset (GNC_PREFS_GROUP, GNC_PREF_DATE_FORMAT_USER);

    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_PAYEE, gtk_spin_button_get_value(pcd->payee_x), gtk_spin_button_get_value(pcd->payee_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_DATE, gtk_spin_button_get_value(pcd->date_x), gtk_spin_button_get_value(pcd->date_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_WORDS, gtk_spin_button_get_value(pcd->words_x), gtk_spin_button_get_value(pcd->words_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_NUMBER, gtk_spin_button_get_value(pcd->number_x), gtk_spin_button_get_value(pcd->number_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_NOTES, gtk_spin_button_get_value(pcd->notes_x), gtk_spin_button_get_value(pcd->notes_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_MEMO, gtk_spin_button_get_value(pcd->memo_x), gtk_spin_button_get_value(pcd->memo_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_ADDRESS, gtk_spin_button_get_value(pcd->address_x), gtk_spin_button_get_value(pcd->address_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_SPLITS_AMOUNT, gtk_spin_button_get_value(pcd->splits_amount_x), gtk_spin_button_get_value(pcd->splits_amount_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_SPLITS_MEMO, gtk_spin_button_get_value(pcd->splits_memo_x), gtk_spin_button_get_value(pcd->splits_memo_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_SPLITS_ACCOUNT, gtk_spin_button_get_value(pcd->splits_account_x), gtk_spin_button_get_value(pcd->splits_account_y));
    gnc_prefs_set_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_TRANSLATION, gtk_spin_button_get_value(pcd->translation_x), gtk_spin_button_get_value(pcd->translation_y));
    gnc_prefs_set_float(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_ROTATION, gtk_spin_button_get_value(pcd->check_rotation));
    gnc_prefs_set_int(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_UNITS, selected_position (pcd->units_dropdown));
}

static void
gnc_ui_print_restore_dialog(PrintCheckDialog *pcd)
{
    gchar *format, *guid;
    gdouble x, y;
    gint active;
    guint position;

    guid = gnc_prefs_get_string (GNC_PREFS_GROUP, GNC_PREF_CHECK_FORMAT_GUID);
    if (guid && *guid && g_strcmp0 (guid, "custom") != 0 &&
        find_existing_format (G_LIST_MODEL (pcd->format_model), guid, &position))
        gtk_drop_down_set_selected (pcd->format_dropdown, position);
    else if (guid && g_strcmp0 (guid, "custom") == 0)
        gtk_drop_down_set_selected (pcd->format_dropdown, pcd->format_max - 1);
    else
        gtk_drop_down_set_selected (pcd->format_dropdown, 0);
    g_free (guid);

    active = gnc_prefs_get_int(GNC_PREFS_GROUP, GNC_PREF_CHECK_POSITION);
    if (active < 0 || active > (gint)pcd->position_max)
        active = 0;
    gtk_drop_down_set_selected (pcd->position_dropdown, active);
    gtk_spin_button_set_value(pcd->first_page_count, (gdouble)gnc_prefs_get_int(GNC_PREFS_GROUP, GNC_PREF_FIRST_PAGE_COUNT));
    active = gnc_prefs_get_int(GNC_PREFS_GROUP, GNC_PREF_DATE_FORMAT);
    gnc_date_format_set_format(GNC_DATE_FORMAT(pcd->date_format), active);
    if (active == QOF_DATE_FORMAT_CUSTOM)
    {
        format = gnc_prefs_get_string (GNC_PREFS_GROUP, GNC_PREF_DATE_FORMAT_USER);
        if (format && *format)
            gnc_date_format_set_custom(GNC_DATE_FORMAT(pcd->date_format), format);
        g_free(format);
    }
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_PAYEE, &x, &y); gtk_spin_button_set_value(pcd->payee_x, x); gtk_spin_button_set_value(pcd->payee_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_DATE, &x, &y); gtk_spin_button_set_value(pcd->date_x, x); gtk_spin_button_set_value(pcd->date_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_WORDS, &x, &y); gtk_spin_button_set_value(pcd->words_x, x); gtk_spin_button_set_value(pcd->words_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_NUMBER, &x, &y); gtk_spin_button_set_value(pcd->number_x, x); gtk_spin_button_set_value(pcd->number_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_ADDRESS, &x, &y); gtk_spin_button_set_value(pcd->address_x, x); gtk_spin_button_set_value(pcd->address_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_NOTES, &x, &y); gtk_spin_button_set_value(pcd->notes_x, x); gtk_spin_button_set_value(pcd->notes_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_MEMO, &x, &y); gtk_spin_button_set_value(pcd->memo_x, x); gtk_spin_button_set_value(pcd->memo_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_SPLITS_AMOUNT, &x, &y); gtk_spin_button_set_value(pcd->splits_amount_x, x); gtk_spin_button_set_value(pcd->splits_amount_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_SPLITS_MEMO, &x, &y); gtk_spin_button_set_value(pcd->splits_memo_x, x); gtk_spin_button_set_value(pcd->splits_memo_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_SPLITS_ACCOUNT, &x, &y); gtk_spin_button_set_value(pcd->splits_account_x, x); gtk_spin_button_set_value(pcd->splits_account_y, y);
    gnc_prefs_get_coords(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_TRANSLATION, &x, &y); gtk_spin_button_set_value(pcd->translation_x, x); gtk_spin_button_set_value(pcd->translation_y, y);
    gtk_spin_button_set_value(pcd->check_rotation, gnc_prefs_get_float(GNC_PREFS_GROUP, GNC_PREF_CUSTOM_ROTATION));
    gtk_drop_down_set_selected (pcd->units_dropdown, MAX (0, gnc_prefs_get_int (GNC_PREFS_GROUP, GNC_PREF_CUSTOM_UNITS)));
}

static gdouble
pcd_get_custom_multip(PrintCheckDialog *pcd)
{
    switch (selected_position (pcd->units_dropdown))
    {
    default: return 72.0;
    case 1: return 28.346;
    case 2: return 2.8346;
    case 3: return 1.0;
    }
}
static void
pcd_key_file_save_xy (GKeyFile *key_file, const gchar *group_name,
                      const gchar *key_name, gdouble multip,
                      GtkSpinButton *spin0, GtkSpinButton *spin1)
{
    gdouble dd[2];

    dd[0] = multip * gtk_spin_button_get_value(spin0);
    dd[1] = multip * gtk_spin_button_get_value(spin1);

    /* Clip the numbers to three decimal places. */
    dd[0] = round(dd[0] * 1000) / 1000;
    dd[1] = round(dd[1] * 1000) / 1000;
    g_key_file_set_double_list(key_file, group_name, key_name, dd, 2);
}


/* This function saves the information about a single printed item into a
 * check description file.  It uses a helper function to extracts and save the
 * item coordinates.
 */
static void
pcd_key_file_save_item_xy (GKeyFile *key_file, int index,
                           CheckItemType type, gdouble multip,
                           GtkSpinButton *spin0, GtkSpinButton *spin1)
{
    gchar *key;
    key = g_strdup_printf("Type_%d", index);
    g_key_file_set_string(key_file, KF_GROUP_ITEMS, key,
                          CheckItemTypeasString(type));
    g_free(key);
    key = g_strdup_printf("Coords_%d", index);
    pcd_key_file_save_xy(key_file, KF_GROUP_ITEMS, key, multip, spin0, spin1);
    g_free(key);
}


/* This function saves all of the information from the custom check dialog
 *  into a check description file.
 */
static void
pcd_save_custom_data(PrintCheckDialog *pcd, const gchar *title)
{
    GKeyFile *key_file;
    GError *error = NULL;
    gdouble multip;
    gint i = 1;
    GncGUID guid;
    char buf[GUID_ENCODING_LENGTH+1];
    gchar *filename, *pathname;

    multip = pcd_get_custom_multip(pcd);

    key_file = g_key_file_new();
    guid_replace(&guid);
    guid_to_string_buff(&guid, buf);
    g_key_file_set_string(key_file, KF_GROUP_TOP, KF_KEY_GUID, buf);
    g_key_file_set_string(key_file, KF_GROUP_TOP, KF_KEY_TITLE, title);
    g_key_file_set_boolean(key_file, KF_GROUP_TOP, KF_KEY_SHOW_GRID, FALSE);
    g_key_file_set_boolean(key_file, KF_GROUP_TOP, KF_KEY_SHOW_BOXES, FALSE);
    g_key_file_set_double(key_file, KF_GROUP_TOP, KF_KEY_ROTATION,
                          gtk_spin_button_get_value(pcd->check_rotation));
    pcd_key_file_save_xy(key_file, KF_GROUP_TOP, KF_KEY_TRANSLATION, multip,
                         pcd->translation_x, pcd->translation_y);

    pcd_key_file_save_item_xy(key_file, i++, PAYEE, multip,
                              pcd->payee_x, pcd->payee_y);
    pcd_key_file_save_item_xy(key_file, i++, DATE, multip,
                              pcd->date_x, pcd->date_y);
    pcd_key_file_save_item_xy(key_file, i++, AMOUNT_WORDS, multip,
                              pcd->words_x, pcd->words_y);
    pcd_key_file_save_item_xy(key_file, i++, AMOUNT_NUMBER, multip,
                              pcd->number_x, pcd->number_y);
    pcd_key_file_save_item_xy(key_file, i++, ADDRESS, multip,
                              pcd->address_x, pcd->address_y);
    pcd_key_file_save_item_xy(key_file, i++, NOTES, multip,
                              pcd->notes_x, pcd->notes_y);
    pcd_key_file_save_item_xy(key_file, i++, MEMO, multip,
                              pcd->memo_x, pcd->memo_y);
    pcd_key_file_save_item_xy(key_file, i++, SPLITS_AMOUNT, multip,
                              pcd->splits_amount_x, pcd->splits_amount_y);
    pcd_key_file_save_item_xy(key_file, i++, SPLITS_MEMO, multip,
                              pcd->splits_memo_x, pcd->splits_memo_y);
    pcd_key_file_save_item_xy(key_file, i++, SPLITS_ACCOUNT, multip,
                              pcd->splits_account_x, pcd->splits_account_y);

    filename = g_strconcat(title, CHECK_NAME_EXTENSION, NULL);
    pathname = g_build_filename(gnc_userdata_dir(), CHECK_FMT_DIR,
                                filename, NULL);

    if (gnc_key_file_save_to_file(pathname, key_file, &error))
    {
        if (!gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_PRINT_DATE_FMT))
            /* Reload the format combo box and reselect the "custom" entry */
            initialize_format_dropdown(pcd);

        gtk_drop_down_set_selected (pcd->format_dropdown, pcd->format_max - 1);
    }
    else
    {
        gchar *detail = g_strdup_printf (_("Cannot open file %s"),
                                         error->message);

        gnc_error_dialog (GTK_WINDOW (pcd->window), "%s\n\n%s",
                          _("Cannot save check format file."), detail);
        g_free (detail);
        g_error_free (error);
    }
    g_free(pathname);
    g_free(filename);
}


typedef struct
{
    PrintCheckDialog *pcd;
    GtkWindow *window;
    GtkEntry *entry;
} FormatTitleRequest;

static void
format_title_request_free (FormatTitleRequest *request)
{
    if (request->pcd && request->pcd->format_title_window == request->window)
        request->pcd->format_title_window = NULL;
    g_free (request);
}

static void
gnc_check_format_title_changed (GtkEditable *editable, GtkWidget *ok_button)
{
    const gchar *text = gtk_editable_get_text (editable);
    gtk_widget_set_sensitive (ok_button, text && *text);
}

static void
format_title_cancelled (GtkButton *button, FormatTitleRequest *request)
{
    (void)button;
    if (request->pcd)
        request->pcd->format_title_window = NULL;
    gtk_window_destroy (request->window);
}

static void
format_title_accepted (GtkButton *button, FormatTitleRequest *request)
{
    gchar *title;
    (void)button;
    title = g_strdup (gtk_editable_get_text (GTK_EDITABLE (request->entry)));
    if (title && *title)
        pcd_save_custom_data (request->pcd, title);
    g_free (title);
    if (request->pcd)
        request->pcd->format_title_window = NULL;
    gtk_window_destroy (request->window);
}

static gboolean
format_title_close_request (GtkWindow *window, FormatTitleRequest *request)
{
    (void)window;
    if (request->pcd)
        request->pcd->format_title_window = NULL;
    return FALSE;
}

static void
gnc_print_check_save_button_clicked(GtkButton *button, PrintCheckDialog *pcd)
{
    GtkWidget *content, *description, *actions, *cancel, *ok;
    FormatTitleRequest *request;
    (void)button;
    if (pcd->format_title_window)
    {
        gtk_window_present (pcd->format_title_window);
        return;
    }
    request = g_new0 (FormatTitleRequest, 1);
    request->pcd = pcd;
    request->window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (request->window);
    request->entry = GTK_ENTRY (gtk_entry_new ());
    pcd->format_title_window = request->window;
    gtk_window_set_title (request->window, _("Save Custom Check Format"));
    gtk_window_set_default_size (request->window, 500, -1);
    gtk_window_set_transient_for (request->window, pcd->window);
    gtk_window_set_modal (request->window, TRUE);
    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_top (content, 12); gtk_widget_set_margin_bottom (content, 12);
    gtk_widget_set_margin_start (content, 12); gtk_widget_set_margin_end (content, 12);
    description = gtk_label_new (_("Enter a title for this custom format. This title will appear in the \"Check format\" selector of the Print Check dialog. Using the title of an existing custom format will cause that format to be overwritten."));
    gtk_label_set_wrap (GTK_LABEL (description), TRUE);
    gtk_label_set_xalign (GTK_LABEL (description), 0.0);
    gtk_box_append (GTK_BOX (content), description);
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (request->entry));
    actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (actions, GTK_ALIGN_END);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    ok = gtk_button_new_with_mnemonic (_("_OK"));
    gtk_widget_set_sensitive (ok, FALSE);
    gtk_box_append (GTK_BOX (actions), cancel);
    gtk_box_append (GTK_BOX (actions), ok);
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_child (request->window, content);
    gtk_window_set_default_widget (request->window, ok);
    g_signal_connect (request->entry, "changed", G_CALLBACK (gnc_check_format_title_changed), ok);
    g_signal_connect (cancel, "clicked", G_CALLBACK (format_title_cancelled), request);
    g_signal_connect (ok, "clicked", G_CALLBACK (format_title_accepted), request);
    g_signal_connect (request->window, "close-request", G_CALLBACK (format_title_close_request), request);
    g_object_set_data_full (G_OBJECT (request->window), "gnc-print-check-format-title-request", request, (GDestroyNotify)format_title_request_free);
    gtk_window_present (request->window);
}
/* This function is an auxiliary debugging function for converting an array of
 * doubles into a printable string.
 */
static gchar *
doubles_to_string(gdouble *dd, gint len)
{
    GString *str;
    gint i;

    str = g_string_new_len(NULL, 50);
    for (i = 0; i < len; i++)
        g_string_append_printf(str, "%f ", dd[i]);
    return g_string_free(str, FALSE);
}


/* This function reads in the information describing the placement for each
 * item to be printed on a check.  This information is all relative to the
 * upper left hand corner of a "check".  See the format_read_multicheck_info()
 * function for determining if there are multiple checks on a single page of
 * paper. This data is build into a linked list and saved as part of the check
 * format information.  These items will be printed in the same order they are
 * read, meaning that items listed later in the date file can be printed over
 * top of items that appear earlier in the file.
 */
static GSList *
format_read_item_placement(const gchar *file,
                           GKeyFile *key_file, check_format_t *format)
{
    check_item_t *data = NULL;
    GError *error = NULL;
    GSList *list = NULL;
    gchar *key, *value, *name;
    int item_num;
    gboolean bval;
    gdouble *dd;
    gsize dd_len;

    /* Read until failure. */
    for (item_num = 1;; item_num++)
    {

        /* Create the per-item data structure */
        data = g_new0(check_item_t, 1);
        if (NULL == data)
            return list;

        /* Get the item type */
        key = g_strdup_printf("%s_%d", KF_KEY_TYPE, item_num);
        value = g_key_file_get_string(key_file, KF_GROUP_ITEMS, key, &error);
        if (error)
        {
            if ((error->domain == G_KEY_FILE_ERROR)
                    && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND))
            {
                /* This is the expected exit from this function. */
                goto cleanup;
            }
            goto failed;
        }
        DEBUG("Check file %s, group %s, key %s, value: %s",
                file, KF_GROUP_ITEMS, key, value);
        g_free(key);

        /* Convert the type from a string to an enum, ignoring case. */
        name = g_utf8_strup(value, -1);
        data->type = CheckItemTypefromString(name);
        g_free(name);
        g_free(value);


        /* Get the item location */
        key = g_strdup_printf("%s_%d", KF_KEY_COORDS, item_num);
        dd = g_key_file_get_double_list(key_file, KF_GROUP_ITEMS,
                                        key, &dd_len, &error);
        if (error)
            goto failed;
        value = doubles_to_string(dd, dd_len);
        DEBUG("Check file %s, group %s, key %s, length %"G_GSIZE_FORMAT"; values: %s",
                file, KF_GROUP_ITEMS, key, dd_len, value);
        g_free(value);

        /* Must have "x;y" or "x;y;w;h". */
        switch (dd_len)
        {
        case 4:
            data->w = dd[2];
            data->h = dd[3];
            /* fall through */
        case 2:
            data->x = dd[0];
            data->y = dd[1];
            break;
        default:
            g_warning
            ("Check file %s, group %s, key %s, error: 2 or 4 values only",
             file, KF_GROUP_ITEMS, key);
            goto cleanup;
        }
        g_free(dd);
        g_free(key);

        /* Any text item can specify:
         *   a font  FONT_n
         *   an alignment if a width was provided for the item  ALIGN_n
         *   blocking chars flag  BLOCKING_CHARS_n
         * These values are optional and do not cause a failure if they are missing. */

        if (data->type != PICTURE)
        {
            key = g_strdup_printf("%s_%d", KF_KEY_FONT, item_num);
            data->font =
                g_key_file_get_string(key_file, KF_GROUP_ITEMS, key, &error);
            if (!error)
            {
                DEBUG("Check file %s, group %s, key %s, value: %s",
                        file, KF_GROUP_ITEMS, key, data->font);
            }
            else
            {
                if (!((error->domain == G_KEY_FILE_ERROR)
                        && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
                    g_warning("Check file %s, group %s, key %s, error: %s",
                              file, KF_GROUP_ITEMS, key, error->message);
                g_clear_error(&error);
            }
            g_free(key);

            key = g_strdup_printf("%s_%d", KF_KEY_ALIGN, item_num);
            value =
                g_key_file_get_string(key_file, KF_GROUP_ITEMS, key, &error);
            if (!error)
            {
                DEBUG("Check file %s, group %s, key %s, value: %s",
                        file, KF_GROUP_ITEMS, key, value);
                name = g_utf8_strdown(value, -1);
                if (strcmp(name, "right") == 0)
                    data->align = PANGO_ALIGN_RIGHT;
                else if (strcmp(name, "center") == 0)
                    data->align = PANGO_ALIGN_CENTER;
                else
                    data->align = PANGO_ALIGN_LEFT;
                g_free(name);
                g_free(value);
            }
            else
            {
                if (!((error->domain == G_KEY_FILE_ERROR)
                        && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
                    g_warning("Check file %s, group %s, key %s, error: %s",
                              file, KF_GROUP_ITEMS, key, error->message);
                data->align = PANGO_ALIGN_LEFT;
                g_clear_error(&error);
            }
            g_free(key);

            key = g_strdup_printf("%s_%d", KF_KEY_BLOCKING, item_num);
            bval =
                g_key_file_get_boolean(key_file, KF_GROUP_ITEMS, key, &error);
            if (!error)
            {
                DEBUG("Check file %s, group %s, key %s, value: %d",
                        file, KF_GROUP_ITEMS, key, bval);
                data->blocking = bval;
            }
            else
            {
                if (!((error->domain == G_KEY_FILE_ERROR)
                        && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
                    g_warning("Check file %s, group %s, key %s, error: %s",
                              file, KF_GROUP_ITEMS, key, error->message);
                data->blocking = format->blocking;
                g_clear_error(&error);
            }
            g_free(key);
        }
        /* Get any extra data for specific items. */
        switch (data->type)
        {
        case PICTURE:
            key = g_strdup_printf("%s_%d", KF_KEY_FILENAME, item_num);
            data->filename =
                g_key_file_get_string(key_file, KF_GROUP_ITEMS, key,
                                      &error);
            if (error)
                goto failed;
            DEBUG("Check file %s, group %s, key %s, value: %s",
                    file, KF_GROUP_ITEMS, key, data->filename);
            g_free(key);
            break;
        case TEXT:
            key = g_strdup_printf("%s_%d", KF_KEY_TEXT, item_num);
            data->text =
                g_key_file_get_string(key_file, KF_GROUP_ITEMS, key,
                                      &error);
            if (error)
                goto failed;
            DEBUG("Check file %s, group %s, key %s, value: %s",
                    file, KF_GROUP_ITEMS, key, data->text);
            g_free(key);
            break;
        case DATE:
            /* no error if the date_format is not present */
            key = g_strdup_printf("%s_%d", KF_KEY_DATE_FORMAT, item_num);
            bval = g_key_file_get_boolean(key_file, KF_GROUP_ITEMS, key, &error);
            if (!error)
            {
                DEBUG("Check file %s, group %s, key %s, value: %d",
                        file, KF_GROUP_ITEMS, key, bval);
                data->print_date_format = bval;
            }
            else
            {
                if (!((error->domain == G_KEY_FILE_ERROR)
                        && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
                    g_warning("Check file %s, group %s, key %s, error: %s",
                              file, KF_GROUP_ITEMS, key, error->message);
                data->print_date_format = format->print_date_format;
                g_clear_error(&error);
            }
            g_free(key);
            break;
        default:
            break;
        }

        list = g_slist_append(list, data);
        data = NULL;
    }

    /* Should never be reached. */
    return list;

failed:
    g_warning("Check file %s, group %s, key %s, error: %s",
              file, KF_GROUP_ITEMS, key, error->message);
cleanup:
    if (error)
        g_error_free(error);
    if (data)
        g_free(data);
    if (key)
        g_free(key);
    return list;
}


/* Free the data describing the placement of a single item on a check.
 */
static void
format_free_item_placement(check_item_t *data)
{
    if (data->font)
        g_free(data->font);
    if (data->text)
        g_free(data->text);
    if (data->filename)
        g_free(data->filename);
    g_free(data);
}


/* Read the information describing whether a page contains multiple checks or
 * a single check.  If there are multiple checks on a page, this functions
 * builds a linked list of the position names and their offsets (from the
 * upper left corner of the page).
 */
static GSList *
format_read_multicheck_info(const gchar *file,
                            GKeyFile *key_file, check_format_t *format)
{
    GError *error = NULL;
    GSList *list = NULL;
    gchar *key, **names;
    gsize length;
    gint i;

    key = g_strdup_printf("%s", KF_KEY_HEIGHT);
    format->height = g_key_file_get_double(key_file, KF_GROUP_POS, key, &error);
    if (error)
    {
        if ((error->domain == G_KEY_FILE_ERROR)
                && ((error->code == G_KEY_FILE_ERROR_GROUP_NOT_FOUND)
                    || (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
        {
            g_clear_error(&error);
            format->height = 0.0;
        }
        else
        {
            g_warning("Check file %s, error reading group %s, key %s: %s",
                      file, KF_GROUP_POS, key, error->message);
            g_free(key);
            return NULL;
        }
    }

    names = g_key_file_get_string_list(key_file, KF_GROUP_POS, KF_KEY_NAMES,
                                       &length, &error);
    if (error)
    {
        if ((error->domain == G_KEY_FILE_ERROR)
                && ((error->code == G_KEY_FILE_ERROR_GROUP_NOT_FOUND)
                    || (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
        {
            /* This is the expected exit from this function. */
            g_free(key);
            return NULL;
        }
        g_warning("Check file %s, error reading group %s, key %s: %s",
                  file, KF_GROUP_POS, key, error->message);
        g_free(key);
        return list;
    }

    for (i = 0; i < length; i++)
        list = g_slist_append(list, g_strdup(names[i]));

    g_strfreev(names);
    return list;
}


/* Free the data describing the placement of multiple checks on a page.
 */
static void
free_check_position(gchar *name)
{
    g_free(name);
}


/* Read the information describing the general layout of a page of checks.
 * All items in this section are optional except or the name of the check
 * style.
 */
static gboolean
format_read_general_info(const gchar *file,
                         GKeyFile *key_file, check_format_t *format)
{
    GError *error = NULL;
    gchar **parts;
    gchar *value;
    double *dd;
    gsize dd_len;

    value = g_key_file_get_string(key_file, KF_GROUP_TOP, KF_KEY_GUID, &error);
    if (error)
    {
        g_warning("Check file %s, group %s, key %s, error: %s",
                  file, KF_GROUP_TOP, KF_KEY_GUID, error->message);
        g_error_free(error);
        return FALSE;
    }
    parts = g_strsplit(value, "-", -1);
    format->guid = g_strjoinv("", parts);
    g_strfreev(parts);
    DEBUG("Check file %s, group %s, key %s, value: %s",
            file, KF_GROUP_TOP, KF_KEY_GUID, format->guid);

    format->title =
        g_key_file_get_string(key_file, KF_GROUP_TOP, KF_KEY_TITLE, &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %s",
                file, KF_GROUP_TOP, KF_KEY_TITLE, format->title);
    }
    else
    {
        g_warning("Check file %s, group %s, key %s, error: %s",
                  file, KF_GROUP_TOP, KF_KEY_TITLE, error->message);
        return FALSE;
    }

    format->blocking =
        g_key_file_get_boolean(key_file, KF_GROUP_TOP, KF_KEY_BLOCKING,
                               &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %d",
                file, KF_GROUP_TOP, KF_KEY_BLOCKING, format->blocking);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group %s, key %s, error: %s",
                      file, KF_GROUP_TOP, KF_KEY_BLOCKING, error->message);
        if ( gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_BLOCKING_CHARS) )
        {
            format->blocking = TRUE;
        }
        else
        {
            format->blocking = FALSE;
        }
        g_clear_error(&error);
    }

    format->print_date_format =
        g_key_file_get_boolean(key_file, KF_GROUP_TOP, KF_KEY_DATE_FORMAT,
                               &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %d",
                file, KF_GROUP_TOP, KF_KEY_DATE_FORMAT, format->print_date_format);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group %s, key %s, error: %s",
                      file, KF_GROUP_TOP, KF_KEY_DATE_FORMAT, error->message);
        if ( gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_PRINT_DATE_FMT) )
        {
            format->print_date_format = TRUE;
        }
        else
        {
            format->print_date_format = FALSE;
        }
        g_clear_error(&error);
    }

    format->show_grid =
        g_key_file_get_boolean(key_file, KF_GROUP_TOP, KF_KEY_SHOW_GRID,
                               &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %d",
                file, KF_GROUP_TOP, KF_KEY_SHOW_GRID, format->show_grid);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group %s, key %s, error: %s",
                      file, KF_GROUP_TOP, KF_KEY_SHOW_GRID, error->message);
        format->show_grid = FALSE;
        g_clear_error(&error);
    }

    format->show_boxes =
        g_key_file_get_boolean(key_file, KF_GROUP_TOP, KF_KEY_SHOW_BOXES,
                               &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %d",
                file, KF_GROUP_TOP, KF_KEY_SHOW_BOXES, format->show_boxes);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group %s, key %s, error: %s",
                      file, KF_GROUP_TOP, KF_KEY_SHOW_BOXES, error->message);
        format->show_boxes = FALSE;
        g_clear_error(&error);
    }

    format->font =
        g_key_file_get_string(key_file, KF_GROUP_TOP, KF_KEY_FONT, &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %s",
                file, KF_GROUP_TOP, KF_KEY_FONT, format->font);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group %s, key %s, error: %s",
                      file, KF_GROUP_TOP, KF_KEY_FONT, error->message);
        g_clear_error(&error);
    }

    format->rotation =
        g_key_file_get_double(key_file, KF_GROUP_TOP, KF_KEY_ROTATION, &error);
    if (!error)
    {
        DEBUG("Check file %s, group %s, key %s, value: %f",
                file, KF_GROUP_TOP, KF_KEY_ROTATION, format->rotation);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group %s, key %s, error: %s",
                      file, KF_GROUP_TOP, KF_KEY_ROTATION, error->message);
        format->rotation = 0.0;
        g_clear_error(&error);
    }

    dd = g_key_file_get_double_list(key_file, KF_GROUP_TOP, KF_KEY_TRANSLATION,
                                    &dd_len, &error);
    if (!error)
    {
        value = doubles_to_string(dd, dd_len);
        DEBUG("Check file %s, group %s, key %s, length %"G_GSIZE_FORMAT"; values: %s",
                file, KF_GROUP_TOP, KF_KEY_TRANSLATION, dd_len, value);
        g_free(value);

        if (dd_len == 2)
        {
            format->trans_x = dd[0];
            format->trans_y = dd[1];
        }
        else
        {
            g_warning("Check file %s, error top %s, key %s: 2 values only",
                      file, KF_GROUP_TOP, KF_KEY_TRANSLATION);
        }
        g_free(dd);
    }
    else
    {
        if (!((error->domain == G_KEY_FILE_ERROR)
                && (error->code == G_KEY_FILE_ERROR_KEY_NOT_FOUND)))
            g_warning("Check file %s, group top %s, key %s: %s",
                      file, KF_GROUP_ITEMS, KF_KEY_TRANSLATION, error->message);
        g_clear_error(&error);
    }

    return TRUE;
}


/* Free all of the information describing a page of checks.
 */
static void
free_check_format(check_format_t *data)
{
    g_return_if_fail(data);
    g_free(data->guid);
    g_free(data->filename);
    g_free(data->title);
    g_free(data->font);
    g_slist_foreach(data->positions, (GFunc) free_check_position, NULL);
    g_slist_free(data->positions);
    g_slist_foreach(data->items, (GFunc) format_free_item_placement, NULL);
    g_slist_free(data->items);
    g_free(data);
}


/* Read a single check format file and append the resulting format to the
 * list of all known formats.  This function calls other functions to read
 * each section of the data file.
 */
static check_format_t *
read_one_check_format(PrintCheckDialog *pcd, const gchar *groupname,
                      const gchar *dirname, const gchar *file)
{
    gchar *pathname;
    GKeyFile *key_file;
    check_format_t *format;

    pathname = g_build_filename(dirname, file, (char *)NULL);
    key_file = gnc_key_file_load_from_file(pathname, FALSE, FALSE, NULL);
    g_free(pathname);
    if (!key_file)
    {
        g_warning("Check file %s, cannot load file", file);
        return NULL;
    }

    format = g_new0(check_format_t, 1);
    format->group = groupname;
    format->filename = g_strdup(file);
    if (format_read_general_info(file, key_file, format))
    {
        format->positions = format_read_multicheck_info(file, key_file, format);
        format->items = format_read_item_placement(file, key_file, format);
    }

    g_key_file_free(key_file);
    if ((NULL == format->title) || (NULL == format->items))
    {
        g_warning("Check file %s, no items read. Dropping file.", file);
        free_check_format(format);
        return NULL;
    }

    return format;
}


/* Iterate over a single check directory, throwing out any backup files and
 * then calling a helper function to read and parse the check format within
 * the file.
 */
static void
read_one_check_directory(PrintCheckDialog *pcd, GListStore *store,
                         const gchar *groupname, const gchar *dirname)
{
    GDir *dir;
    const gchar *filename;
    gboolean found = FALSE;

    dir = g_dir_open(dirname, 0, NULL);
    if (!dir)
        return;
    while ((filename = g_dir_read_name(dir)) != NULL)
    {
        check_format_t *format, *existing;
        GObject *row;
        if (g_str_has_prefix(filename, "#") || !g_str_has_suffix(filename, ".chk"))
            continue;
        format = read_one_check_format(pcd, groupname, dirname, filename);
        if (!format)
            continue;
        existing = find_existing_format (G_LIST_MODEL (store), format->guid, NULL);
        if (existing)
        {
            gchar *detail = g_strdup_printf (_("The GUIDs in the %s check format file '%s' and the %s check format file '%s' match."),
                                             existing->group, existing->filename,
                                             format->group, format->filename);
            gnc_error_dialog (pcd->window, "%s\n\n%s", _("There is a duplicate check format file."), detail);
            g_free (detail);
            free_check_format (format);
            continue;
        }
        row = format_row_new (format->title, format, FALSE);
        g_list_store_append (store, row);
        g_object_unref (row);
        found = TRUE;
    }
    g_dir_close(dir);
    if (found)
    {
        GObject *separator = format_row_new ("────────", NULL, TRUE);
        g_list_store_append (store, separator);
        g_object_unref (separator);
    }
}

static void
read_formats(PrintCheckDialog *pcd, GListStore *store)
{
    gchar *dirname, *pkgdatadir;
    pkgdatadir = gnc_path_get_pkgdatadir();
    dirname = g_build_filename(pkgdatadir, CHECK_FMT_DIR, NULL);
    read_one_check_directory(pcd, store, _("application"), dirname);
    g_free(dirname);
    g_free(pkgdatadir);
    dirname = gnc_build_userdata_path(CHECK_FMT_DIR);
    read_one_check_directory(pcd, store, _("user"), dirname);
    g_free(dirname);
}

static void
initialize_format_dropdown (PrintCheckDialog *pcd)
{
    GListStore *store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    GObject *custom;

    read_formats (pcd, store);
    custom = format_row_new (_("Custom"), NULL, FALSE);
    g_list_store_append (store, custom);
    g_object_unref (custom);
    pcd->selected_format = NULL;
    pcd->last_format = 0;
    pcd->format_max = g_list_model_get_n_items (G_LIST_MODEL (store));
    gtk_drop_down_set_model (pcd->format_dropdown, G_LIST_MODEL (store));
    g_clear_object (&pcd->format_model);
    pcd->format_model = store;
}
/*****************************************************
 * gnc_ui_print_check_dialog_create                  *
 * make a new print check dialog and wait for it.    *
 * If account is given, this is a parent account to  *
 * subtotal the amount of all splits under it.       *
 *****************************************************/
static void
print_check_dialog_free (PrintCheckDialog *pcd)
{
    if (pcd->print_request)
        g_cancellable_cancel (pcd->print_request->cancellable);
    if (pcd->format_title_window)
    {
        FormatTitleRequest *request = g_object_get_data (G_OBJECT (pcd->format_title_window), "gnc-print-check-format-title-request");
        if (request)
            request->pcd = NULL;
        gtk_window_destroy (pcd->format_title_window);
    }
    g_clear_object (&pcd->format_model);
    g_free (pcd->default_font);
    g_list_free (pcd->splits);
    g_free (pcd);
}

void
gnc_ui_print_check_dialog_create(GtkWidget *parent, GList *splits, Account* account)
{
    PrintCheckDialog *pcd = g_new0 (PrintCheckDialog, 1);
    GtkBuilder *builder = gtk_builder_new ();
    GtkWidget *table, *lower_left;
    GtkExpression *expression;
    GtkStringList *units;
    gchar *font;
    Transaction *trans = NULL;

    pcd->caller_window = parent ? GTK_WINDOW (parent) : NULL;
    pcd->splits = g_list_copy (splits);
    pcd->account = account;
    for (guint i = 1; i <= 24; i++)
    {
        gchar *id = g_strdup_printf ("adjustment%u", i);
        gnc_builder_add_from_file (builder, "dialog-print-check.glade", id);
        g_free (id);
    }
    gnc_builder_add_from_file (builder, "dialog-print-check.glade", "print_check_dialog");
    pcd->window = GTK_WINDOW (gtk_builder_get_object (builder, "print_check_dialog"));
    gtk_widget_set_name (GTK_WIDGET (pcd->window), "gnc-id-print-check");
    gtk_window_set_transient_for (pcd->window, pcd->caller_window);

    pcd->format_dropdown = GTK_DROP_DOWN (gtk_builder_get_object (builder, "check_format_combobox"));
    pcd->position_dropdown = GTK_DROP_DOWN (gtk_builder_get_object (builder, "check_position_combobox"));
    pcd->units_dropdown = GTK_DROP_DOWN (gtk_builder_get_object (builder, "units_combobox"));
    expression = gtk_property_expression_new (GTK_TYPE_STRING_OBJECT, NULL, "string");
    gtk_drop_down_set_expression (pcd->format_dropdown, expression);
    gtk_expression_unref (expression);
    pcd->first_page_count = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "first_page_count_entry"));
    pcd->custom_table = GTK_WIDGET(gtk_builder_get_object (builder, "custom_table"));
    pcd->payee_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "payee_x_entry")); pcd->payee_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "payee_y_entry"));
    pcd->date_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "date_x_entry")); pcd->date_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "date_y_entry"));
    pcd->words_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "amount_words_x_entry")); pcd->words_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "amount_words_y_entry"));
    pcd->number_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "amount_numbers_x_entry")); pcd->number_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "amount_numbers_y_entry"));
    pcd->notes_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "notes_x_entry")); pcd->notes_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "notes_y_entry"));
    pcd->memo_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "memo_x_entry")); pcd->memo_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "memo_y_entry"));
    pcd->address_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "address_x_entry")); pcd->address_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "address_y_entry"));
    pcd->splits_amount_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "splits_amount_x_entry")); pcd->splits_amount_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "splits_amount_y_entry"));
    pcd->splits_memo_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "splits_memo_x_entry")); pcd->splits_memo_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "splits_memo_y_entry"));
    pcd->splits_account_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "splits_account_x_entry")); pcd->splits_account_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "splits_account_y_entry"));
    pcd->translation_x = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "translation_x_entry")); pcd->translation_y = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "translation_y_entry"));
    pcd->translation_label = GTK_WIDGET(gtk_builder_get_object (builder, "translation_label"));
    pcd->check_rotation = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "check_rotation_entry"));
    pcd->check_address_name = GTK_WIDGET(gtk_builder_get_object (builder, "check_address_name"));
    pcd->check_address_1 = GTK_WIDGET(gtk_builder_get_object (builder, "check_address_1"));
    pcd->check_address_2 = GTK_WIDGET(gtk_builder_get_object (builder, "check_address_2"));
    pcd->check_address_3 = GTK_WIDGET(gtk_builder_get_object (builder, "check_address_3"));
    pcd->check_address_4 = GTK_WIDGET(gtk_builder_get_object (builder, "check_address_4"));
    table = GTK_WIDGET(gtk_builder_get_object (builder, "options_table"));
    pcd->date_format = gnc_date_format_new_without_label();
    gtk_grid_attach (GTK_GRID(table), pcd->date_format, 1, 4, 1, 1);
    lower_left = GTK_WIDGET(gtk_builder_get_object (builder, "lower_left"));
    gtk_widget_set_visible (lower_left, FALSE);

    font = gnc_prefs_get_string(GNC_PREFS_GROUP, GNC_PREF_DEFAULT_FONT);
    pcd->default_font = font && *font ? font : g_strdup(DEFAULT_FONT);
    units = gtk_string_list_new ((const char * const[]){ _("Inches"), _("Centimeters"), _("Millimeters"), _("Points"), NULL });
    gtk_drop_down_set_model (pcd->units_dropdown, G_LIST_MODEL (units));
    g_object_unref (units);
    initialize_format_dropdown (pcd);
    g_signal_connect (pcd->format_dropdown, "notify::selected", G_CALLBACK (gnc_print_check_format_changed), pcd);
    g_signal_connect (pcd->position_dropdown, "notify::selected", G_CALLBACK (gnc_print_check_position_changed), pcd);
    if (gtk_drop_down_get_selected (pcd->format_dropdown) == GTK_INVALID_LIST_POSITION)
        gtk_drop_down_set_selected (pcd->format_dropdown, 0);
    gnc_print_check_format_changed (NULL, NULL, pcd);

    if (g_list_length(pcd->splits) == 1)
    {
        GncOwner txn_owner;
        trans = xaccSplitGetParent((Split *)(pcd->splits->data));
        if (gncOwnerGetOwnerFromTxn (trans, &txn_owner))
        {
            GncOwner owner;
            gncOwnerCopy (gncOwnerGetEndOwner (&txn_owner), &owner);
            gnc_entry_set_text(GTK_ENTRY(pcd->check_address_name), gncOwnerGetName(&owner));
            gnc_entry_set_text(GTK_ENTRY(pcd->check_address_1), gncAddressGetAddr1 (gncOwnerGetAddr(&owner)));
            gnc_entry_set_text(GTK_ENTRY(pcd->check_address_2), gncAddressGetAddr2 (gncOwnerGetAddr(&owner)));
            gnc_entry_set_text(GTK_ENTRY(pcd->check_address_3), gncAddressGetAddr3 (gncOwnerGetAddr(&owner)));
            gnc_entry_set_text(GTK_ENTRY(pcd->check_address_4), gncAddressGetAddr4 (gncOwnerGetAddr(&owner)));
        }
    }
    if (trans && gtk_entry_get_text_length (GTK_ENTRY(pcd->check_address_name)) == 0)
        gnc_entry_set_text(GTK_ENTRY(pcd->check_address_name), xaccTransGetDescription(trans));

    g_object_set_data_full (G_OBJECT (pcd->window), "gnc-print-check-dialog", pcd, (GDestroyNotify)print_check_dialog_free);
    gnc_ui_print_restore_dialog(pcd);
    gnc_restore_window_size(GNC_PREFS_GROUP, pcd->window, pcd->caller_window);
    g_signal_connect (pcd->window, "close-request", G_CALLBACK (print_check_close_request), pcd);
    g_signal_connect (gtk_builder_get_object (builder, "save_button"), "clicked", G_CALLBACK (gnc_print_check_save_button_clicked), pcd);
    g_signal_connect (gtk_builder_get_object (builder, "helpbutton"), "clicked", G_CALLBACK (print_check_help_clicked), pcd);
    g_signal_connect (gtk_builder_get_object (builder, "cancelbutton"), "clicked", G_CALLBACK (print_check_cancel_clicked), pcd);
    g_signal_connect (gtk_builder_get_object (builder, "okbutton"), "clicked", G_CALLBACK (print_check_clicked), pcd);
    g_object_unref(builder);
    gtk_window_present (pcd->window);
}
/**************************************
 * Print check contents to the page.  *
 **************************************/

/* Draw a grid pattern on the page to be printed.  This grid is helpful when
 * figuring out the offsets for where to print various items on the page.
 */
static void
draw_grid(GtkPrintContext *context, gint width, gint height, const gchar *font)
{
    const double dash_pattern[2] = { 1.0, 5.0 };
    PangoFontDescription *desc;
    PangoLayout *layout;
    cairo_t *cr;
    gchar *text;
    gint i;

    /* Initialize for printing text */
    layout = gtk_print_context_create_pango_layout(context);
    desc = pango_font_description_from_string(font);
    pango_layout_set_font_description(layout, desc);
    pango_font_description_free(desc);
    pango_layout_set_alignment(layout, PANGO_ALIGN_LEFT);
    pango_layout_set_width(layout, -1);

    /* Set up the line to draw with. */
    cr = gtk_print_context_get_cairo_context(context);
    cairo_save(cr);
    cairo_set_line_width(cr, 1.0);
    cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
    cairo_set_dash(cr, dash_pattern, 2, 0);

    /* Draw horizontal lines */
    for (i = -200; i < (height + 200); i += 50)
    {
        text = g_strdup_printf("%d", (int)i);
        cairo_move_to(cr, -200, i);
        cairo_line_to(cr, width + 200, i);
        cairo_stroke(cr);
        pango_layout_set_text(layout, text, -1);
        cairo_move_to(cr, 0, i);
        pango_cairo_show_layout(cr, layout);
        g_free(text);
    }

    /* Draw vertical lines */
    for (i = -200; i < (width + 200); i += 50)
    {
        text = g_strdup_printf("%d", (int)i);
        cairo_move_to(cr, i, -200);
        cairo_line_to(cr, i, height + 200);
        cairo_stroke(cr);
        pango_layout_set_text(layout, text, -1);
        cairo_move_to(cr, i, 0);
        pango_cairo_show_layout(cr, layout);
        g_free(text);
    }

    /* Clean up after ourselves */
    cairo_restore(cr);
    g_object_unref(layout);
}


/* Print a single line of text to the printed page.  If a width and height
 * are specified, the line will be wrapped at the specified width, and the
 * resulting text will be clipped if it does not fit in the space
 * available.
 */
static gdouble
draw_text(GtkPrintContext *context, const gchar *text, check_item_t *data,
          PangoFontDescription *default_desc)
{
    PangoFontDescription *desc;
    PangoLayout *layout;
    cairo_t *cr;
    gint layout_height, layout_width;
    gdouble width, height;
    gchar *new_text;

    if ((NULL == text) || (strlen(text) == 0))
        return 0.0;

    /* Initialize for printing text */
    layout = gtk_print_context_create_pango_layout(context);
    if (data->font)
    {
        desc = pango_font_description_from_string(data->font);
        pango_layout_set_font_description(layout, desc);
        pango_font_description_free(desc);
    }
    else
    {
        pango_layout_set_font_description(layout, default_desc);
    }
    pango_layout_set_alignment(layout,
                               data->w ? data->align : PANGO_ALIGN_LEFT);
    pango_layout_set_width(layout, data->w ? data->w * PANGO_SCALE : -1);
    pango_layout_set_ellipsize(layout, PANGO_ELLIPSIZE_END);
    if ( data->blocking )
    {
        new_text = g_strdup_printf("***%s***", text);
        pango_layout_set_text(layout, new_text, -1);
        g_free(new_text);
    }
    else
    {
        pango_layout_set_text(layout, text, -1);
    }
    pango_layout_get_size(layout, &layout_width, &layout_height);
    width = (gdouble) layout_width / PANGO_SCALE;
    height = (gdouble) layout_height / PANGO_SCALE;

    cr = gtk_print_context_get_cairo_context(context);
    cairo_save(cr);

    /* Clip text to the enclosing rectangle */
    if (data->w && data->h)
    {
        DEBUG("Text clip rectangle, coords %f,%f, size %f,%f",
                data->x, data->y - data->h, data->w, data->h);
        cairo_rectangle(cr, data->x, data->y - data->h, data->w, data->h);
        cairo_clip_preserve(cr);
    }

    /* Draw the text */
    DEBUG("Text move to %f,%f, print '%s'", data->x, data->y,
            text ? text : "(null)");
    cairo_move_to(cr, data->x, data->y - height);
    pango_cairo_show_layout(cr, layout);

    /* Clean up after ourselves */
    cairo_restore(cr);
    g_object_unref(layout);
    return width;

}


/* Find and load the specified image from an absolute path, the application
 * check-format directory, or the user check-format directory. */
static GdkTexture *
read_image (const gchar *filename)
{
    gchar *pkgdatadir, *dirname, *pathname;
    GdkTexture *texture;
    GError *error = NULL;

    if (g_path_is_absolute (filename))
        pathname = g_strdup (filename);
    else
    {
        pkgdatadir = gnc_path_get_pkgdatadir ();
        pathname = g_build_filename (pkgdatadir, CHECK_FMT_DIR, filename, NULL);
        g_free (pkgdatadir);
        if (!g_file_test (pathname, G_FILE_TEST_EXISTS))
        {
            g_free (pathname);
            dirname = gnc_build_userdata_path (CHECK_FMT_DIR);
            pathname = g_build_filename (dirname, filename, NULL);
            g_free (dirname);
        }
    }
    texture = gdk_texture_new_from_filename (pathname, &error);
    if (!texture)
    {
        g_warning ("Filename '%s' cannot be read: %s", pathname,
                   error ? error->message : "unknown error");
        g_clear_error (&error);
    }
    g_free (pathname);
    return texture;
}

/* Print one image, preserving its aspect ratio and never scaling it up. */
static void
draw_picture(GtkPrintContext *context, check_item_t *data)
{
    cairo_t *cr = gtk_print_context_get_cairo_context (context);
    GdkTexture *texture;
    GdkTextureDownloader *downloader;
    GBytes *pixels;
    cairo_surface_t *surface;
    const guchar *pixel_data;
    gsize stride, length;
    gint pix_w, pix_h;
    gdouble scale_w = 1, scale_h = 1, scale;

    texture = read_image (data->filename);
    if (!texture)
        return;
    pix_w = gdk_texture_get_width (texture);
    pix_h = gdk_texture_get_height (texture);
    downloader = gdk_texture_downloader_new (texture);
    gdk_texture_downloader_set_format (downloader, GDK_MEMORY_DEFAULT);
    pixels = gdk_texture_downloader_download_bytes (downloader, &stride);
    gdk_texture_downloader_free (downloader);
    g_object_unref (texture);
    pixel_data = g_bytes_get_data (pixels, &length);
    surface = cairo_image_surface_create_for_data ((unsigned char *)pixel_data,
                                                   CAIRO_FORMAT_ARGB32,
                                                   pix_w, pix_h, (int)stride);
    if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS)
    {
        cairo_surface_destroy (surface);
        g_bytes_unref (pixels);
        return;
    }
    if (data->w && pix_w > data->w)
        scale_w = data->w / pix_w;
    if (data->h && pix_h > data->h)
        scale_h = data->h / pix_h;
    scale = MIN (scale_w, scale_h);

    cairo_save (cr);
    if (data->w && data->h)
        cairo_rectangle (cr, data->x, data->y - data->h, data->w, data->h);
    else
        cairo_rectangle (cr, data->x, data->y - pix_h * scale,
                         pix_w * scale, pix_h * scale);
    cairo_clip (cr);
    cairo_translate (cr, data->x, data->y - pix_h * scale);
    cairo_scale (cr, scale, scale);
    cairo_set_source_surface (cr, surface, 0, 0);
    cairo_paint (cr);
    cairo_restore (cr);
    cairo_surface_destroy (surface);
    g_bytes_unref (pixels);
}

#define DATE_FMT_HEIGHT 8
#define DATE_FMT_SLOP   2

/* There is a new Canadian requirement that all software that prints the date
 * on a check must also print the format of that date underneath using a 6-8
 * point font.  This function implements that requirement.  It requires the
 * font description used in printing the date so that it can print in the same
 * font using a smaller point size.  It also requires width of the printed
 * date as an argument, allowing it to center the format string under the
 * date.
 *
 * Note: This code only prints a date if the user has explicitly requested it
 * via a preference setting.  This is because gnucash has no way of
 * knowing if the user's checks already have a date format printed on them.
 */
static void
draw_date_format(GtkPrintContext *context, const gchar *date_format,
                 check_item_t *data, PangoFontDescription *default_desc,
                 gdouble width)
{
    PangoFontDescription *date_desc;
    check_item_t date_item;
    gchar *text = NULL, *expanded = NULL;
    const gchar *c;
    GString *cdn_fmt;

    if ( !data->print_date_format ) return;

    date_desc = pango_font_description_copy_static(default_desc);
    pango_font_description_set_size(date_desc, DATE_FMT_HEIGHT * PANGO_SCALE);
    date_item = *data;
    date_item.y += (DATE_FMT_HEIGHT + DATE_FMT_SLOP);
    date_item.w = width;
    date_item.h = DATE_FMT_HEIGHT + DATE_FMT_SLOP;
    date_item.align = PANGO_ALIGN_CENTER;

    /* This is a date format string. It should only contain ascii. */
    cdn_fmt = g_string_new_len(NULL, 50);
    for (c = date_format; c && *c; )
    {
        if ((c[0] != '%') || (c[1] == '\0'))
        {
            c += 1;
            continue;
        }
        switch (c[1])
        {
        case 'F':
            cdn_fmt = g_string_append(cdn_fmt, "YYYYMMDD");
            break;
        case 'Y':
            cdn_fmt = g_string_append(cdn_fmt, "YYYY");
            break;
        case 'y':
            cdn_fmt = g_string_append(cdn_fmt, "YY");
            break;
        case 'm':
            cdn_fmt = g_string_append(cdn_fmt, "MM");
            break;
        case 'd':
        case 'e':
            cdn_fmt = g_string_append(cdn_fmt, "DD");
            break;
        case 'x':
            expanded = g_strdup_printf("%s%s",
                                       qof_date_format_get_string(QOF_DATE_FORMAT_LOCALE),
                                       c + 2);
            c = expanded;
            continue;
        default:
            break;
        }
        c += 2;
    }

    text = g_string_free(cdn_fmt, FALSE);
    draw_text(context, text, &date_item, date_desc);
    g_free(text);
    if (expanded)
        g_free(expanded);
    pango_font_description_free(date_desc);
}


/* Print each of the items that in the description of a single check.  This
 * function uses helper functions to print text based and picture based items.
 */
static void
draw_page_items(GtkPrintContext *context,
                check_format_t *format, gpointer user_data)
{
    PrintCheckDialog *pcd = (PrintCheckDialog *) user_data;
    PangoFontDescription *default_desc;
    Transaction *trans;
    gnc_numeric amount;
    GNCPrintAmountInfo info;
    const gchar *date_format;
    gchar *text = NULL, buf[100];
    GSList *elem;
    check_item_t *item;
    gdouble width;
    gchar *address = NULL;

    trans = xaccSplitGetParent(pcd->split);
    /* This was valid when the check printing dialog was instantiated. */
    g_return_if_fail(trans);
    amount = get_check_amount(pcd);

    if (format->font)
        default_desc = pango_font_description_from_string(format->font);
    else
        default_desc = pango_font_description_from_string(pcd->default_font);

    /* Now put the actual data onto the page. */
    for (elem = format->items; elem; elem = g_slist_next(elem))
    {
        item = elem->data;

        switch (item->type)
        {
        case DATE:
        {
            GDate date;
            g_date_clear (&date, 1);
            gnc_gdate_set_time64 (&date, xaccTransGetDate(trans));
            date_format =
                gnc_date_format_get_custom(GNC_DATE_FORMAT
                                           (pcd->date_format));
            g_date_strftime(buf, 100, date_format, &date);
            width = draw_text(context, buf, item, default_desc);
            draw_date_format(context, date_format, item, default_desc, width);
            break;
        }

        case PAYEE:
            draw_text(context, xaccTransGetDescription(trans), item, default_desc);
            break;

        case NOTES:
            draw_text(context, xaccTransGetNotes(trans), item, default_desc);
            break;

        case MEMO:
            draw_text(context, xaccSplitGetMemo(pcd->split), item, default_desc);
            break;

        case ACTION:
            draw_text(context, gnc_get_action_num(trans, pcd->split), item,
                      default_desc);
            break;

        case CHECK_NUMBER:
            draw_text(context, gnc_get_num_action(trans, pcd->split), item,
                      default_desc);
            break;

        case AMOUNT_NUMBER:
            info = gnc_default_print_info(FALSE);
            draw_text(context, xaccPrintAmount(amount, info),
                      item, default_desc);
            break;

        case AMOUNT_WORDS:
            text = numeric_to_words(amount);
            draw_text(context, text, item, default_desc);
            g_free(text);
            break;

        case TEXT:
            draw_text(context, item->text, item, default_desc);
            break;

        case ADDRESS:
            address = get_check_address(pcd);
            draw_text(context, address, item, default_desc);
            g_free(address);
            break;

        case SPLITS_AMOUNT:
            text = get_check_splits_amount(pcd);
            draw_text(context, text, item, default_desc);
            g_free(text);
            break;

        case SPLITS_MEMO:
            text = get_check_splits_memo(pcd);
            draw_text(context, text, item, default_desc);
            g_free(text);
            break;

        case SPLITS_ACCOUNT:
            text = get_check_splits_account(pcd);
            draw_text(context, text, item, default_desc);
            g_free(text);
            break;

        case PICTURE:
            draw_picture(context, item);
            break;

        default:
            text = g_strdup_printf("(unknown check field, type %d)", item->type);
            draw_text(context, text, item, default_desc);
            g_free(text);
            break;
        }
    }

    pango_font_description_free(default_desc);
}


/* Print each of the items that in the description of a single check.  This
 * function uses helper functions to print text based and picture based items.
 */
static void
draw_page_boxes(GtkPrintContext *context,
                check_format_t *format, gpointer user_data)
{
    cairo_t *cr;
    GSList *elem;
    check_item_t *item;

    cr = gtk_print_context_get_cairo_context(context);

    /* Now put the actual data onto the page. */
    for (elem = format->items; elem; elem = g_slist_next(elem))
    {
        item = elem->data;
        if (!item->w || !item->h)
            continue;
        cairo_rectangle(cr, item->x, item->y - item->h, item->w, item->h);
        cairo_stroke(cr);
    }
}


/* Print an entire page based upon the layout in a check description file. This
 * function takes care of translating/rotating the page, calling the function to
 * print the grid pattern (if requested), and calls a helper function to print
 * all check items.
 */
static void
draw_check_format(GtkPrintContext *context, gint position,
                  check_format_t *format, gpointer user_data)
{
    PrintCheckDialog *pcd = (PrintCheckDialog *) user_data;
    gdouble x, y, r, multip;
    cairo_t *cr = gtk_print_context_get_cairo_context(context);

    /* Translate all subsequent check items if required. */
    if ((position > 0) && (position < pcd->position_max))
    {
        /* Standard positioning is used.
         * Note that the first check on the page (position 0) doesn't
         * need to be moved (hence the test for position > 0 above. */
        cairo_translate(cr, 0, format->height); /* Translation is relative to previous
                                                   check translation, not to page border ! */
        DEBUG("Position %d translated by %f relative to previous check (pre-defined)", position, format->height);
        DEBUG("                      by %f relative to page (pre-defined)", position * format->height);
    }
    else if (position == pcd->position_max)
    {
        /* Custom positioning is used. */
        multip = pcd_get_custom_multip(pcd);
        x = multip * gtk_spin_button_get_value(pcd->translation_x);
        y = multip * gtk_spin_button_get_value(pcd->translation_y);
        cairo_translate(cr, x, y);
        DEBUG("Position translated by %f,%f (custom)", x, y);
        r = gtk_spin_button_get_value(pcd->check_rotation);
        cairo_rotate(cr, r * DEGREES_TO_RADIANS);
        DEBUG("Position rotated by %f degrees (custom)", r);
    }

    /* Draw layout boxes if requested. Also useful when determining check
     * layouts. */
    if (format->show_boxes)
        draw_page_boxes(context, format, user_data);

    /* Draw the actual check data. */
    draw_page_items(context, format, user_data);
}


static void
draw_check_custom(GtkPrintContext *context, gpointer user_data)
{
    PrintCheckDialog *pcd = (PrintCheckDialog *) user_data;
    GNCPrintAmountInfo info;
    PangoFontDescription *desc;
    Transaction *trans;
    gnc_numeric amount;
    cairo_t *cr;
    const gchar *date_format;
    gchar *text = NULL, buf[100];
    check_item_t item = { 0 };
    gdouble x, y, multip, degrees;
    GDate date;
    gchar *address;

    trans = xaccSplitGetParent(pcd->split);
    /* This was valid when the check printing dialog was instantiated. */
    g_return_if_fail(trans);

    desc = pango_font_description_from_string(pcd->default_font);

    multip = pcd_get_custom_multip(pcd);
    degrees = gtk_spin_button_get_value(pcd->check_rotation);
    cr = gtk_print_context_get_cairo_context(context);
    cairo_rotate(cr, degrees * DEGREES_TO_RADIANS);
    DEBUG("Page rotated by %f degrees", degrees);

    x = multip * gtk_spin_button_get_value(pcd->translation_x);
    y = multip * gtk_spin_button_get_value(pcd->translation_y);
    cairo_translate(cr, x, y);
    DEBUG("Page translated by %f,%f", x, y);

    item.x = multip * gtk_spin_button_get_value(pcd->payee_x);
    item.y = multip * gtk_spin_button_get_value(pcd->payee_y);
    draw_text(context, xaccTransGetDescription(trans), &item, desc);

    item.x = multip * gtk_spin_button_get_value(pcd->date_x);
    item.y = multip * gtk_spin_button_get_value(pcd->date_y);
    g_date_clear (&date, 1);
    gnc_gdate_set_time64 (&date, xaccTransGetDate(trans));
    date_format = gnc_date_format_get_custom(GNC_DATE_FORMAT(pcd->date_format));
    g_date_strftime(buf, 100, date_format, &date);
    draw_text(context, buf, &item, desc);

    item.x = multip * gtk_spin_button_get_value(pcd->number_x);
    item.y = multip * gtk_spin_button_get_value(pcd->number_y);
    info = gnc_default_print_info(FALSE);
    amount = gnc_numeric_abs(xaccSplitGetAmount(pcd->split));
    draw_text(context, xaccPrintAmount(amount, info), &item, desc);

    item.x = multip * gtk_spin_button_get_value(pcd->words_x);
    item.y = multip * gtk_spin_button_get_value(pcd->words_y);
    text = numeric_to_words(amount);
    draw_text(context, text, &item, desc);
    g_free(text);

    item.x = multip * gtk_spin_button_get_value(pcd->address_x);
    item.y = multip * gtk_spin_button_get_value(pcd->address_y);
    address = get_check_address(pcd);
    draw_text(context, address, &item, desc);
    g_free(address);

    item.x = multip * gtk_spin_button_get_value(pcd->splits_amount_x);
    item.y = multip * gtk_spin_button_get_value(pcd->splits_amount_y);
    text = get_check_splits_amount(pcd);
    draw_text(context, text, &item, desc);
    g_free(text);

    item.x = multip * gtk_spin_button_get_value(pcd->splits_memo_x);
    item.y = multip * gtk_spin_button_get_value(pcd->splits_memo_y);
    text = get_check_splits_memo(pcd);
    draw_text(context, text, &item, desc);
    g_free(text);

    item.x = multip * gtk_spin_button_get_value(pcd->splits_account_x);
    item.y = multip * gtk_spin_button_get_value(pcd->splits_account_y);
    text = get_check_splits_account(pcd);
    draw_text(context, text, &item, desc);
    g_free(text);

    item.x = multip * gtk_spin_button_get_value(pcd->notes_x);
    item.y = multip * gtk_spin_button_get_value(pcd->notes_y);
    draw_text(context, xaccTransGetNotes(trans), &item, desc);

    item.x = multip * gtk_spin_button_get_value(pcd->memo_x);
    item.y = multip * gtk_spin_button_get_value(pcd->memo_y);
    draw_text(context, xaccSplitGetMemo(pcd->split), &item, desc);

    pango_font_description_free(desc);
}


/* Print a page of checks. This takes into account the number of checks to print,
 * the number of checks on a page, and the starting check position on the page.
 * This function is called once by the GtkPrint code once for each page to be printed.
 */
static void
draw_page(GtkPrintOperation *operation,
          GtkPrintContext *context, gint page_nr, gpointer user_data)
{
    PrintCheckDialog *pcd = (PrintCheckDialog *) user_data;
    check_format_t *format;
    cairo_t *cr = gtk_print_context_get_cairo_context(context);

    format = pcd->selected_format;
    if (format)
    {
        gint    first_check, last_check;
        gint    first_page_count;
        guint   check_count = g_list_length(pcd->splits);
        gint    check_number;
        gint    position = selected_position (pcd->position_dropdown);
        gint    last_blank_check_pos;
        gint    checks_per_page;
        GList   *next_split;

        if (position == pcd->position_max)
        {
            /* Custom position, one check per page */
            checks_per_page = 1;
            first_page_count = 1;
        }
        else
        {
            checks_per_page = pcd->position_max;
            first_page_count = gtk_spin_button_get_value_as_int(pcd->first_page_count);
        }

        if (page_nr == 0)
        {
            first_check = 0;
            last_check = first_page_count - 1;
            next_split = pcd->splits;
        }
        else
        {
            first_check = first_page_count + (page_nr - 1) * checks_per_page;
            last_check = MIN(check_count - 1, first_check + checks_per_page - 1);
            next_split = g_list_nth(pcd->splits, first_check);
            /* If position is not "custom" reset it to top */
            if (position < pcd->position_max)
                position = 0;
        }

        /* Do page level translations/rotations */
        cairo_translate(cr, format->trans_x, format->trans_y);
        DEBUG("Page translated by %f,%f", format->trans_x, format->trans_y);
        cairo_rotate(cr, format->rotation * DEGREES_TO_RADIANS);
        DEBUG("Page rotated by %f degrees", format->rotation);

        /* The grid is useful when determining check layouts */
        if (format->show_grid)
        {
            draw_grid(context,
                      gtk_print_context_get_width(context),
                      gtk_print_context_get_height(context),
                      pcd->default_font);
        }

        last_blank_check_pos = position - 1;
        /* Optionally skip blank check positions if */
        if ((page_nr == 0)                         /* on first page AND */
                && (last_blank_check_pos > 0)      /* there's more than one blank check */
                && (position < pcd->position_max)) /* but don't skip for custom positioning */
            cairo_translate(cr, 0, format->height * last_blank_check_pos);

        for (check_number = first_check; check_number <= last_check;
                check_number++, position++)
        {
            pcd->split = (Split *) next_split->data;
            next_split = g_list_next(next_split);
            draw_check_format(context, position, format, user_data);
        }
    }
    else
    {
        /* Custom check format */
        pcd->split = (Split *) g_list_nth_data(pcd->splits, page_nr);
        g_return_if_fail(pcd->split);
        draw_check_custom(context, user_data);
    }
}


/* Compute the number of pages required to complete this print operation.
 * This function is called once by the GtkPrint code to determine the number
 * of pages required to complete the print operation.
 */
static void
begin_print(GtkPrintOperation *operation,
            GtkPrintContext *context, gpointer user_data)
{
    PrintCheckDialog *pcd = (PrintCheckDialog *) user_data;
    guint check_count = g_list_length(pcd->splits);
    gint pages;
    gint position = selected_position (pcd->position_dropdown);

    if (pcd->selected_format /* User selected a format other than custom */
            && pcd->position_max > 1 /* The format has more than one check per page
                                        (position_max is equivalent to custom
                                        positioning, and there need to be at least two
                                        other check defined positions (0 and 1), so
                                        custom positioning should be at least
                                        at position 2, i.e. >1) */
            && position < pcd->position_max) /* User chose a check position other
                                                then custom (which is always at
                                                position_max in the list) */
    {
        gint first_page_count, remaining_count;

        first_page_count = gtk_spin_button_get_value_as_int(pcd->first_page_count);
        remaining_count = check_count - first_page_count;
        pages = 1                  /* First page, will have first_page_count checks */
                + remaining_count / pcd->position_max;
                                   /* Subsequent pages with all positions filled */
        if ((remaining_count % pcd->position_max) > 0)
            pages++;  /* Last page, not all positions are filled. Needs to be added
                         separately because integer division rounds towards 0 and
                         would omit the last checks if they didn't fill a full page */
    }
    else
        pages = check_count;
    gtk_print_operation_set_n_pages(operation, pages);
}


static void
print_check_request_free (PrintCheckRequest *request)
{
    if (!request)
        return;
    g_weak_ref_clear (&request->window);
    g_clear_object (&request->dialog);
    g_clear_object (&request->cancellable);
    g_clear_object (&request->file);
    g_free (request);
}

static void
print_check_request_remove_file (PrintCheckRequest *request)
{
    GError *error = NULL;

    if (!request->file)
        return;
    if (!g_file_delete (request->file, NULL, &error) &&
        !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_NOT_FOUND))
        g_warning ("Could not remove temporary check PDF: %s",
                   error ? error->message : "unknown error");
    g_clear_error (&error);
    g_clear_object (&request->file);
}

static PrintCheckDialog *
print_check_request_get_dialog (PrintCheckRequest *request,
                                GtkWindow **window_out)
{
    GtkWindow *window = GTK_WINDOW (g_weak_ref_get (&request->window));
    PrintCheckDialog *pcd = NULL;

    if (window && !gtk_widget_in_destruction (GTK_WIDGET (window)))
        pcd = g_object_get_data (G_OBJECT (window), "gnc-print-check-dialog");
    if (!pcd || pcd->print_request != request)
        pcd = NULL;
    if (window_out)
        *window_out = window;
    else
        g_clear_object (&window);
    return pcd;
}

static void
print_check_dialog_destroy (PrintCheckDialog *pcd)
{
    if (pcd->closing)
        return;
    pcd->closing = TRUE;
    gnc_save_window_size (GNC_PREFS_GROUP, pcd->window);
    gtk_window_destroy (pcd->window);
}

static void
print_check_request_complete (PrintCheckRequest *request, gboolean printed,
                              const GError *error)
{
    GtkWindow *window = NULL;
    PrintCheckDialog *pcd = print_check_request_get_dialog (request, &window);

    if (pcd)
    {
        pcd->print_request = NULL;
        if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED) &&
            !pcd->close_after_print)
            gnc_error_dialog (window, "%s", error->message);
        if (printed)
        {
            gnc_ui_print_save_dialog (pcd);
            print_check_dialog_destroy (pcd);
        }
        else if (pcd->close_after_print)
            print_check_dialog_destroy (pcd);
        else
            gtk_widget_set_sensitive (GTK_WIDGET (window), TRUE);
    }
    g_clear_object (&window);
    print_check_request_remove_file (request);
    print_check_request_free (request);
}

static gboolean
print_check_export_pdf (PrintCheckRequest *request, PrintCheckDialog *pcd,
                        GtkPrintSetup *setup, GError **error)
{
    GtkPrintOperation *operation;
    GtkPrintOperationResult result;
    GFileIOStream *stream = NULL;
    GtkPrintSettings *settings;
    GtkPageSetup *page_setup;
    gchar *filename;
    gboolean exported;

    request->file = g_file_new_tmp ("gnucash-checks-XXXXXX.pdf", &stream, error);
    if (!request->file)
        return FALSE;
    if (!g_io_stream_close (G_IO_STREAM (stream), NULL, error))
    {
        g_object_unref (stream);
        print_check_request_remove_file (request);
        return FALSE;
    }
    g_object_unref (stream);

    filename = g_file_get_path (request->file);
    if (!filename)
    {
        g_set_error (error, G_IO_ERROR, G_IO_ERROR_NOT_SUPPORTED,
                     "%s", _("The temporary check file has no local path."));
        print_check_request_remove_file (request);
        return FALSE;
    }

    operation = gtk_print_operation_new ();
    gnc_print_operation_init (operation, "GnuCash-Checks");
    settings = gtk_print_setup_get_print_settings (setup);
    page_setup = gtk_print_setup_get_page_setup (setup);
    if (settings)
        gtk_print_operation_set_print_settings (operation, settings);
    if (page_setup)
        gtk_print_operation_set_default_page_setup (operation, page_setup);
    gtk_print_operation_set_unit (operation, GTK_UNIT_POINTS);
    gtk_print_operation_set_use_full_page (operation, TRUE);
    gtk_print_operation_set_allow_async (operation, FALSE);
    gtk_print_operation_set_export_filename (operation, filename);
    g_signal_connect (operation, "begin_print", G_CALLBACK (begin_print), pcd);
    g_signal_connect (operation, "draw_page", G_CALLBACK (draw_page), pcd);
    result = gtk_print_operation_run (operation, GTK_PRINT_OPERATION_ACTION_EXPORT,
                                      pcd->window, error);
    exported = result == GTK_PRINT_OPERATION_RESULT_APPLY;
    if (!exported && error && !*error)
        g_set_error (error, G_IO_ERROR, G_IO_ERROR_FAILED,
                     "%s", _("Could not create the temporary check PDF."));
    g_object_unref (operation);
    g_free (filename);
    if (!exported)
        print_check_request_remove_file (request);
    return exported;
}

static void
print_check_file_finished (GObject *source, GAsyncResult *result,
                           gpointer user_data)
{
    PrintCheckRequest *request = user_data;
    GError *error = NULL;
    gboolean printed = gtk_print_dialog_print_file_finish (
        GTK_PRINT_DIALOG (source), result, &error);

    print_check_request_complete (request, printed, error);
    g_clear_error (&error);
}

static void
print_check_setup_finished (GObject *source, GAsyncResult *result,
                            gpointer user_data)
{
    PrintCheckRequest *request = user_data;
    GError *error = NULL;
    GtkPrintSetup *setup = gtk_print_dialog_setup_finish (
        GTK_PRINT_DIALOG (source), result, &error);
    GtkWindow *window = NULL;
    PrintCheckDialog *pcd;

    if (!setup)
    {
        print_check_request_complete (request, FALSE, error);
        g_clear_error (&error);
        return;
    }

    pcd = print_check_request_get_dialog (request, &window);
    if (!pcd)
    {
        gtk_print_setup_unref (setup);
        g_clear_object (&window);
        print_check_request_complete (request, FALSE, NULL);
        return;
    }

    gnc_print_setup_save (setup);
    if (!print_check_export_pdf (request, pcd, setup, &error))
    {
        gtk_print_setup_unref (setup);
        g_clear_object (&window);
        print_check_request_complete (request, FALSE, error);
        g_clear_error (&error);
        return;
    }

    gtk_print_dialog_print_file (request->dialog, window, setup, request->file,
                                 request->cancellable, print_check_file_finished,
                                 request);
    gtk_print_setup_unref (setup);
    g_clear_object (&window);
}

static void
print_check_request_start (PrintCheckDialog *pcd)
{
    PrintCheckRequest *request;
    GtkPrintOperation *operation;
    GtkPrintSettings *settings;
    GtkPageSetup *page_setup;

    if (pcd->print_request || pcd->closing)
        return;

    request = g_new0 (PrintCheckRequest, 1);
    g_weak_ref_init (&request->window, pcd->window);
    request->dialog = gtk_print_dialog_new ();
    request->cancellable = g_cancellable_new ();
    pcd->print_request = request;

    operation = gtk_print_operation_new ();
    gnc_print_operation_init (operation, "GnuCash-Checks");
    settings = gtk_print_operation_get_print_settings (operation);
    page_setup = gtk_print_operation_get_default_page_setup (operation);
    gtk_print_dialog_set_title (request->dialog, _("Print Checks"));
    gtk_print_dialog_set_accept_label (request->dialog, _("_Print"));
    gtk_print_dialog_set_modal (request->dialog, TRUE);
    if (settings)
        gtk_print_dialog_set_print_settings (request->dialog, settings);
    if (page_setup)
        gtk_print_dialog_set_page_setup (request->dialog, page_setup);
    gtk_print_dialog_setup (request->dialog, pcd->window, request->cancellable,
                            print_check_setup_finished, request);
    g_object_unref (operation);
}


static void
print_check_dialog_finish (PrintCheckDialog *pcd, gboolean print)
{
    if (pcd->closing)
        return;
    if (!print)
    {
        if (pcd->print_request)
        {
            pcd->close_after_print = TRUE;
            g_cancellable_cancel (pcd->print_request->cancellable);
            return;
        }
        print_check_dialog_destroy (pcd);
        return;
    }
    if (pcd->print_request)
        return;
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->window), FALSE);
    print_check_request_start (pcd);
}

static void
print_check_help_clicked (GtkButton *button, PrintCheckDialog *pcd)
{
    (void)button;
    gnc_gnome_help (pcd->window, DF_MANUAL, DL_PRINTCHECK);
}

static void
print_check_cancel_clicked (GtkButton *button, PrintCheckDialog *pcd)
{
    (void)button;
    print_check_dialog_finish (pcd, FALSE);
}

static void
print_check_clicked (GtkButton *button, PrintCheckDialog *pcd)
{
    (void)button;
    print_check_dialog_finish (pcd, TRUE);
}

static gboolean
print_check_close_request (GtkWindow *window, PrintCheckDialog *pcd)
{
    (void)window;
    if (pcd->print_request)
    {
        pcd->close_after_print = TRUE;
        g_cancellable_cancel (pcd->print_request->cancellable);
        return TRUE;
    }
    if (!pcd->closing)
    {
        pcd->closing = TRUE;
        gnc_save_window_size (GNC_PREFS_GROUP, pcd->window);
    }
    return FALSE;
}

static void
gnc_print_check_format_changed (GObject *object, GParamSpec *pspec,
                                PrintCheckDialog *pcd)
{
    GListModel *model = gtk_drop_down_get_model (pcd->format_dropdown);
    guint selected = gtk_drop_down_get_selected (pcd->format_dropdown);
    GObject *row;
    check_format_t *format;
    GtkStringList *positions;
    gint position;
    gboolean sensitive;

    (void)object;
    (void)pspec;
    if (!model || selected == GTK_INVALID_LIST_POSITION)
        return;
    row = g_list_model_get_item (model, selected);
    if (format_row_is_separator (row))
    {
        g_object_unref (row);
        gtk_drop_down_set_selected (pcd->format_dropdown, pcd->last_format);
        return;
    }
    format = format_row_get_format (row);
    g_object_unref (row);
    pcd->last_format = selected;
    position = selected_position (pcd->position_dropdown);
    pcd->selected_format = format;
    positions = gtk_string_list_new (NULL);
    if (format && format->positions)
    {
        pcd->position_max = g_slist_length (format->positions);
        for (GSList *elem = format->positions; elem; elem = g_slist_next (elem))
            gtk_string_list_append (positions, elem->data);
    }
    else if (format)
    {
        pcd->position_max = 1;
        gtk_string_list_append (positions, _("Top"));
    }
    else
        pcd->position_max = 0;
    gtk_string_list_append (positions, _("Custom"));
    gtk_drop_down_set_model (pcd->position_dropdown, G_LIST_MODEL (positions));
    g_object_unref (positions);
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->position_dropdown), pcd->position_max > 0);
    gtk_widget_set_sensitive (pcd->custom_table, format == NULL);
    position = MAX (MIN (position, (gint)pcd->position_max), 0);
    gtk_drop_down_set_selected (pcd->position_dropdown, position);
    sensitive = check_format_has_address (pcd);
    gtk_widget_set_sensitive (pcd->check_address_name, sensitive);
    gtk_widget_set_sensitive (pcd->check_address_1, sensitive);
    gtk_widget_set_sensitive (pcd->check_address_2, sensitive);
    gtk_widget_set_sensitive (pcd->check_address_3, sensitive);
    gtk_widget_set_sensitive (pcd->check_address_4, sensitive);
}

static void
gnc_print_check_position_changed (GObject *object, GParamSpec *pspec,
                                  PrintCheckDialog *pcd)
{
    gint position = selected_position (pcd->position_dropdown);
    guint check_count = g_list_length (pcd->splits);
    gint first_page_max, first_page_value;
    gboolean sensitive;

    (void)object;
    (void)pspec;
    sensitive = position == (gint)pcd->position_max;
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->translation_x), sensitive);
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->translation_y), sensitive);
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->check_rotation), sensitive);
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->units_dropdown), sensitive);
    first_page_max = MAX (1, MIN ((gint)pcd->position_max - position, (gint)check_count));
    first_page_value = MAX (MIN (gtk_spin_button_get_value_as_int (pcd->first_page_count), first_page_max), 1);
    gtk_spin_button_set_range (pcd->first_page_count, 1, first_page_max);
    gtk_spin_button_set_value (pcd->first_page_count, first_page_value);
    gtk_widget_set_sensitive (GTK_WIDGET (pcd->first_page_count), first_page_max > 1);
}
