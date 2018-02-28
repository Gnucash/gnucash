/********************************************************************\
 * Account.c -- Account data structure implementation               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2007 David Hampton <hampton@employees.org>         *
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

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "AccountP.h"
#include "Split.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "gnc-event.h"
#include "gnc-glib-utils.h"
#include "gnc-lot.h"
#include "gnc-pricedb.h"
#include "qofinstance-p.h"
#include "gnc-features.h"
#include "guid.hpp"

#include <numeric>

static QofLogModule log_module = GNC_MOD_ACCOUNT;

/* The Canonical Account Separator.  Pre-Initialized. */
static gchar account_separator[8] = ".";
static gunichar account_uc_separator = ':';
/* Predefined KVP paths */
static const char *KEY_ASSOC_INCOME_ACCOUNT = "ofx/associated-income-account";
#define AB_KEY "hbci"
#define AB_ACCOUNT_ID "account-id"
#define AB_ACCOUNT_UID "account-uid"
#define AB_BANK_CODE "bank-code"
#define AB_TRANS_RETRIEVAL "trans-retrieval"

using FinalProbabilityVec=std::vector<std::pair<std::string, int32_t>>;
using ProbabilityVec=std::vector<std::pair<std::string, struct AccountProbability>>;
using FlatKvpEntry=std::pair<std::string, KvpValue*>;

enum
{
    LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_NAME,                          /* Table */
    PROP_FULL_NAME,                     /* Constructed */
    PROP_CODE,                          /* Table */
    PROP_DESCRIPTION,                   /* Table */
    PROP_COLOR,                         /* KVP */
    PROP_NOTES,                         /* KVP */
    PROP_TYPE,                          /* Table */

//    PROP_PARENT,                      /* Table, Not a property */
    PROP_COMMODITY,                     /* Table */
    PROP_COMMODITY_SCU,                 /* Table */
    PROP_NON_STD_SCU,                   /* Table */
    PROP_END_BALANCE,                   /* Constructed */
    PROP_END_CLEARED_BALANCE,           /* Constructed */
    PROP_END_RECONCILED_BALANCE,        /* Constructed */

    PROP_TAX_RELATED,                   /* KVP */
    PROP_TAX_CODE,                      /* KVP */
    PROP_TAX_SOURCE,                    /* KVP */
    PROP_TAX_COPY_NUMBER,               /* KVP */

    PROP_HIDDEN,                        /* Table slot exists, but in KVP in memory & xml */
    PROP_PLACEHOLDER,                   /* Table slot exists, but in KVP in memory & xml */
    PROP_FILTER,                        /* KVP */
    PROP_SORT_ORDER,                    /* KVP */
    PROP_SORT_REVERSED,

    PROP_LOT_NEXT_ID,                   /* KVP */
    PROP_ONLINE_ACCOUNT,                /* KVP */
    PROP_OFX_INCOME_ACCOUNT,            /* KVP */
    PROP_AB_ACCOUNT_ID,                 /* KVP */
    PROP_AB_ACCOUNT_UID,                /* KVP */
    PROP_AB_BANK_CODE,                  /* KVP */
    PROP_AB_TRANS_RETRIEVAL,            /* KVP */

    PROP_RUNTIME_0,
    PROP_POLICY,                        /* Cached Value */
    PROP_MARK,                          /* Runtime Value */
    PROP_SORT_DIRTY,                    /* Runtime Value */
    PROP_BALANCE_DIRTY,                 /* Runtime Value */
    PROP_START_BALANCE,                 /* Runtime Value */
    PROP_START_CLEARED_BALANCE,         /* Runtime Value */
    PROP_START_RECONCILED_BALANCE,      /* Runtime Value */
};

#define GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_ACCOUNT, AccountPrivate))

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend to!  These functions perform actions on the        *
 * account data structure, in order to encapsulate the knowledge    *
 * of the internals of the Account in one file.                     *
\********************************************************************/

static void xaccAccountBringUpToDate (Account *acc);


/********************************************************************\
 * gnc_get_account_separator                                        *
 *   returns the current account separator character                *
 *                                                                  *
 * Args: none                                                       *
 * Returns: account separator character                             *
 \*******************************************************************/
const gchar *
gnc_get_account_separator_string (void)
{
    return account_separator;
}

gunichar
gnc_get_account_separator (void)
{
    return account_uc_separator;
}

void
gnc_set_account_separator (const gchar *separator)
{
    gunichar uc;
    gint count;

    uc = g_utf8_get_char_validated(separator, -1);
    if ((uc == (gunichar) - 2) || (uc == (gunichar) - 1) || g_unichar_isalnum(uc))
    {
        account_uc_separator = ':';
        strcpy(account_separator, ":");
        return;
    }

    account_uc_separator = uc;
    count = g_unichar_to_utf8(uc, account_separator);
    account_separator[count] = '\0';
}

gchar *gnc_account_name_violations_errmsg (const gchar *separator, GList* invalid_account_names)
{
    GList *node;
    gchar *message = NULL;
    gchar *account_list = NULL;

    if ( !invalid_account_names )
        return NULL;

    for ( node = invalid_account_names;  node; node = g_list_next(node))
    {
        if ( !account_list )
            account_list = static_cast<gchar *>(node->data);
        else
        {
            gchar *tmp_list = NULL;

            tmp_list = g_strconcat (account_list, "\n", node->data, NULL );
            g_free ( account_list );
            account_list = tmp_list;
        }
    }

    /* Translators: The first %s will be the account separator character,
       the second %s is a list of account names.
       The resulting string will be displayed to the user if there are
       account names containing the separator character. */
    message = g_strdup_printf(
                  _("The separator character \"%s\" is used in one or more account names.\n\n"
                    "This will result in unexpected behaviour. "
                    "Either change the account names or choose another separator character.\n\n"
                    "Below you will find the list of invalid account names:\n"
                    "%s"), separator, account_list );
    g_free ( account_list );
    return message;
}

GList *gnc_account_list_name_violations (QofBook *book, const gchar *separator)
{
    Account *root_account = gnc_book_get_root_account(book);
    GList   *accounts, *node;
    GList   *invalid_list = NULL;

    g_return_val_if_fail (separator != NULL, NULL);

    if (root_account == NULL)
        return NULL;

    accounts = gnc_account_get_descendants (root_account);
    for (node = accounts; node; node = g_list_next(node))
    {
        Account *acct      = (Account*)node->data;
        gchar   *acct_name = g_strdup ( xaccAccountGetName ( acct ) );

        if ( g_strstr_len ( acct_name, -1, separator ) )
            invalid_list = g_list_prepend ( invalid_list, (gpointer) acct_name );
        else
            g_free ( acct_name );
    }
    if (accounts != NULL)
    {
        g_list_free(accounts);
    }

    return invalid_list;
}

/********************************************************************\
\********************************************************************/

G_INLINE_FUNC void mark_account (Account *acc);
void
mark_account (Account *acc)
{
    qof_instance_set_dirty(&acc->inst);
}

/********************************************************************\
\********************************************************************/

/* GObject Initialization */
G_DEFINE_TYPE(Account, gnc_account, QOF_TYPE_INSTANCE)

static void
gnc_account_init(Account* acc)
{
    AccountPrivate *priv;

    priv = GET_PRIVATE(acc);
    priv->parent   = NULL;
    priv->children = NULL;

    priv->accountName = static_cast<char*>(qof_string_cache_insert(""));
    priv->accountCode = static_cast<char*>(qof_string_cache_insert(""));
    priv->description = static_cast<char*>(qof_string_cache_insert(""));

    priv->type = ACCT_TYPE_NONE;

    priv->mark = 0;

    priv->policy = xaccGetFIFOPolicy();
    priv->lots = NULL;

    priv->commodity = NULL;
    priv->commodity_scu = 0;
    priv->non_standard_scu = FALSE;

    priv->balance = gnc_numeric_zero();
    priv->cleared_balance = gnc_numeric_zero();
    priv->reconciled_balance = gnc_numeric_zero();
    priv->starting_balance = gnc_numeric_zero();
    priv->starting_cleared_balance = gnc_numeric_zero();
    priv->starting_reconciled_balance = gnc_numeric_zero();
    priv->balance_dirty = FALSE;

    priv->splits = NULL;
    priv->sort_dirty = FALSE;
}

static void
gnc_account_dispose (GObject *acctp)
{
    G_OBJECT_CLASS(gnc_account_parent_class)->dispose(acctp);
}

static void
gnc_account_finalize(GObject* acctp)
{
    G_OBJECT_CLASS(gnc_account_parent_class)->finalize(acctp);
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_account_get_property (GObject         *object,
                          guint            prop_id,
                          GValue          *value,
                          GParamSpec      *pspec)
{
    Account *account;
    AccountPrivate *priv;
    const gchar *key;

    g_return_if_fail(GNC_IS_ACCOUNT(object));

    account = GNC_ACCOUNT(object);
    priv = GET_PRIVATE(account);
    switch (prop_id)
    {
    case PROP_NAME:
        g_value_set_string(value, priv->accountName);
        break;
    case PROP_FULL_NAME:
        g_value_take_string(value, gnc_account_get_full_name(account));
        break;
    case PROP_CODE:
        g_value_set_string(value, priv->accountCode);
        break;
    case PROP_DESCRIPTION:
        g_value_set_string(value, priv->description);
        break;
    case PROP_COLOR:
        g_value_set_string(value, xaccAccountGetColor(account));
        break;
    case PROP_NOTES:
        g_value_set_string(value, xaccAccountGetNotes(account));
        break;
    case PROP_TYPE:
        // NEED TO BE CONVERTED TO A G_TYPE_ENUM
        g_value_set_int(value, priv->type);
        break;
    case PROP_COMMODITY:
        g_value_take_object(value, priv->commodity);
        break;
    case PROP_COMMODITY_SCU:
        g_value_set_int(value, priv->commodity_scu);
        break;
    case PROP_NON_STD_SCU:
        g_value_set_boolean(value, priv->non_standard_scu);
        break;
    case PROP_SORT_DIRTY:
        g_value_set_boolean(value, priv->sort_dirty);
        break;
    case PROP_BALANCE_DIRTY:
        g_value_set_boolean(value, priv->balance_dirty);
        break;
    case PROP_START_BALANCE:
        g_value_set_boxed(value, &priv->starting_balance);
        break;
    case PROP_START_CLEARED_BALANCE:
        g_value_set_boxed(value, &priv->starting_cleared_balance);
        break;
    case PROP_START_RECONCILED_BALANCE:
        g_value_set_boxed(value, &priv->starting_reconciled_balance);
        break;
    case PROP_END_BALANCE:
        g_value_set_boxed(value, &priv->balance);
        break;
    case PROP_END_CLEARED_BALANCE:
        g_value_set_boxed(value, &priv->cleared_balance);
        break;
    case PROP_END_RECONCILED_BALANCE:
        g_value_set_boxed(value, &priv->reconciled_balance);
        break;
    case PROP_POLICY:
        /* MAKE THIS A BOXED VALUE */
        g_value_set_pointer(value, priv->policy);
        break;
    case PROP_MARK:
        g_value_set_int(value, priv->mark);
        break;
    case PROP_TAX_RELATED:
        g_value_set_boolean(value, xaccAccountGetTaxRelated(account));
        break;
    case PROP_TAX_CODE:
        g_value_set_string(value, xaccAccountGetTaxUSCode(account));
        break;
    case PROP_TAX_SOURCE:
        g_value_set_string(value,
                           xaccAccountGetTaxUSPayerNameSource(account));
        break;
    case PROP_TAX_COPY_NUMBER:
        g_value_set_int64(value,
                          xaccAccountGetTaxUSCopyNumber(account));
        break;
    case PROP_HIDDEN:
        g_value_set_boolean(value, xaccAccountGetHidden(account));
        break;
    case PROP_PLACEHOLDER:
        g_value_set_boolean(value, xaccAccountGetPlaceholder(account));
        break;
    case PROP_FILTER:
        g_value_set_string(value, xaccAccountGetFilter(account));
        break;
    case PROP_SORT_ORDER:
        g_value_set_string(value, xaccAccountGetSortOrder(account));
        break;
    case PROP_SORT_REVERSED:
        g_value_set_boolean(value, xaccAccountGetSortReversed(account));
        break;
    case PROP_LOT_NEXT_ID:
        /* Pre-set the value in case the frame is empty */
        g_value_set_int64 (value, 0);
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {"lot-mgmt", "next-id"});
        break;
    case PROP_ONLINE_ACCOUNT:
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {"online_id"});
        break;
    case PROP_OFX_INCOME_ACCOUNT:
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {KEY_ASSOC_INCOME_ACCOUNT});
        break;
    case PROP_AB_ACCOUNT_ID:
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_ACCOUNT_ID});
        break;
    case PROP_AB_ACCOUNT_UID:
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_ACCOUNT_UID});
        break;
    case PROP_AB_BANK_CODE:
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_BANK_CODE});
        break;
    case PROP_AB_TRANS_RETRIEVAL:
        qof_instance_get_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_TRANS_RETRIEVAL});
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_account_set_property (GObject         *object,
                          guint            prop_id,
                          const GValue    *value,
                          GParamSpec      *pspec)
{
    Account *account;
    gnc_numeric *number;
    g_return_if_fail(GNC_IS_ACCOUNT(object));
    account = GNC_ACCOUNT(object);
    if (prop_id < PROP_RUNTIME_0)
        g_assert (qof_instance_get_editlevel(account));

    switch (prop_id)
    {
    case PROP_NAME:
        xaccAccountSetName(account, g_value_get_string(value));
        break;
    case PROP_CODE:
        xaccAccountSetCode(account, g_value_get_string(value));
        break;
    case PROP_DESCRIPTION:
        xaccAccountSetDescription(account, g_value_get_string(value));
        break;
    case PROP_COLOR:
        xaccAccountSetColor(account, g_value_get_string(value));
        break;
    case PROP_NOTES:
        xaccAccountSetNotes(account, g_value_get_string(value));
        break;
    case PROP_TYPE:
        // NEED TO BE CONVERTED TO A G_TYPE_ENUM
        xaccAccountSetType(account, static_cast<GNCAccountType>(g_value_get_int(value)));
        break;
    case PROP_COMMODITY:
        xaccAccountSetCommodity(account, static_cast<gnc_commodity*>(g_value_get_object(value)));
        break;
    case PROP_COMMODITY_SCU:
        xaccAccountSetCommoditySCU(account, g_value_get_int(value));
        break;
    case PROP_NON_STD_SCU:
        xaccAccountSetNonStdSCU(account, g_value_get_boolean(value));
        break;
    case PROP_SORT_DIRTY:
        gnc_account_set_sort_dirty(account);
        break;
    case PROP_BALANCE_DIRTY:
        gnc_account_set_balance_dirty(account);
        break;
    case PROP_START_BALANCE:
        number = static_cast<gnc_numeric*>(g_value_get_boxed(value));
        gnc_account_set_start_balance(account, *number);
        break;
    case PROP_START_CLEARED_BALANCE:
        number = static_cast<gnc_numeric*>(g_value_get_boxed(value));
        gnc_account_set_start_cleared_balance(account, *number);
        break;
    case PROP_START_RECONCILED_BALANCE:
        number = static_cast<gnc_numeric*>(g_value_get_boxed(value));
        gnc_account_set_start_reconciled_balance(account, *number);
        break;
    case PROP_POLICY:
        gnc_account_set_policy(account, static_cast<GNCPolicy*>(g_value_get_pointer(value)));
        break;
    case PROP_MARK:
        xaccAccountSetMark(account, g_value_get_int(value));
        break;
    case PROP_TAX_RELATED:
        xaccAccountSetTaxRelated(account, g_value_get_boolean(value));
        break;
    case PROP_TAX_CODE:
        xaccAccountSetTaxUSCode(account, g_value_get_string(value));
        break;
    case PROP_TAX_SOURCE:
        xaccAccountSetTaxUSPayerNameSource(account,
                                           g_value_get_string(value));
        break;
    case PROP_TAX_COPY_NUMBER:
        xaccAccountSetTaxUSCopyNumber(account,
                                      g_value_get_int64(value));
        break;
    case PROP_HIDDEN:
        xaccAccountSetHidden(account, g_value_get_boolean(value));
        break;
    case PROP_PLACEHOLDER:
        xaccAccountSetPlaceholder(account, g_value_get_boolean(value));
        break;
    case PROP_FILTER:
        xaccAccountSetFilter(account, g_value_get_string(value));
        break;
    case PROP_SORT_ORDER:
        xaccAccountSetSortOrder(account, g_value_get_string(value));
        break;
    case PROP_SORT_REVERSED:
        xaccAccountSetSortReversed(account, g_value_get_boolean(value));
        break;
    case PROP_LOT_NEXT_ID:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {"lot-mgmt", "next-id"});
        break;
    case PROP_ONLINE_ACCOUNT:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {"online_id"});
        break;
    case PROP_OFX_INCOME_ACCOUNT:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {KEY_ASSOC_INCOME_ACCOUNT});
        break;
    case PROP_AB_ACCOUNT_ID:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_ACCOUNT_ID});
        break;
    case PROP_AB_ACCOUNT_UID:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_ACCOUNT_UID});
        break;
    case PROP_AB_BANK_CODE:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_BANK_CODE});
        break;
    case PROP_AB_TRANS_RETRIEVAL:
        qof_instance_set_path_kvp (QOF_INSTANCE (account), value, {AB_KEY, AB_TRANS_RETRIEVAL});
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_account_class_init (AccountClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    gobject_class->dispose = gnc_account_dispose;
    gobject_class->finalize = gnc_account_finalize;
    gobject_class->set_property = gnc_account_set_property;
    gobject_class->get_property = gnc_account_get_property;

    g_type_class_add_private(klass, sizeof(AccountPrivate));

    g_object_class_install_property
    (gobject_class,
     PROP_NAME,
     g_param_spec_string ("name",
                          "Account Name",
                          "The accountName is an arbitrary string "
                          "assigned by the user.  It is intended to "
                          "a short, 5 to 30 character long string "
                          "that is displayed by the GUI as the "
                          "account mnemonic.  Account names may be "
                          "repeated. but no two accounts that share "
                          "a parent may have the same name.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_FULL_NAME,
     g_param_spec_string ("fullname",
                          "Full Account Name",
                          "The name of the account concatenated with "
                          "all its parent account names to indicate "
                          "a unique account.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READABLE)));

    g_object_class_install_property
    (gobject_class,
     PROP_CODE,
     g_param_spec_string ("code",
                          "Account Code",
                          "The account code is an arbitrary string "
                          "assigned by the user. It is intended to "
                          "be reporting code that is a synonym for "
                          "the accountName.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_DESCRIPTION,
     g_param_spec_string ("description",
                          "Account Description",
                          "The account description is an arbitrary "
                          "string assigned by the user. It is intended "
                          "to be a longer, 1-5 sentence description of "
                          "what this account is all about.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_COLOR,
     g_param_spec_string ("color",
                          "Account Color",
                          "The account color is a color string assigned "
                          "by the user. It is intended to highlight the "
                          "account based on the users wishes.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_NOTES,
     g_param_spec_string ("notes",
                          "Account Notes",
                          "The account notes is an arbitrary provided "
                          "for the user to attach any other text that "
                          "they would like to associate with the account.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_TYPE,
     g_param_spec_int ("type",
                       "Account Type",
                       "The account type, picked from the enumerated list "
                       "that includes ACCT_TYPE_BANK, ACCT_TYPE_STOCK, "
                       "ACCT_TYPE_CREDIT, ACCT_TYPE_INCOME, etc.",
                       ACCT_TYPE_NONE,
                       NUM_ACCOUNT_TYPES - 1,
                       ACCT_TYPE_BANK,
                       static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_COMMODITY,
     g_param_spec_object ("commodity",
                          "Commodity",
                          "The commodity field denotes the kind of "
                          "'stuff' stored  in this account, whether "
                          "it is USD, gold, stock, etc.",
                          GNC_TYPE_COMMODITY,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_COMMODITY_SCU,
     g_param_spec_int ("commodity-scu",
                       "Commodity SCU",
                       "The smallest fraction of the commodity that is "
                       "tracked.  This number is used as the denominator "
                       "value in 1/x, so a value of 100 says that the "
                       "commodity can be divided into hundreths.  E.G."
                       "1 USD can be divided into 100 cents.",
                       0,
                       G_MAXINT32,
                       1000000,
                       static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_NON_STD_SCU,
     g_param_spec_boolean ("non-std-scu",
                           "Non-std SCU",
                           "TRUE if the account SCU doesn't match "
                           "the commodity SCU.  This indicates a case "
                           "where the two were accidentally set to "
                           "mismatched values in older versions of "
                           "GnuCash.",
                           FALSE,
                           static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_SORT_DIRTY,
     g_param_spec_boolean("sort-dirty",
                          "Sort Dirty",
                          "TRUE if the splits in the account needs to be "
                          "resorted.  This flag is set by the accounts "
                          "code for certain internal modifications, or "
                          "when external code calls the engine to say a "
                          "split has been modified in a way that may "
                          "affect the sort order of the account. Note: "
                          "This value can only be set to TRUE.",
                          FALSE,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_BALANCE_DIRTY,
     g_param_spec_boolean("balance-dirty",
                          "Balance Dirty",
                          "TRUE if the running balances in the account "
                          "needs to be recalculated.  This flag is set "
                          "by the accounts code for certain internal "
                          "modifications, or when external code calls "
                          "the engine to say a split has been modified. "
                          "Note: This value can only be set to TRUE.",
                          FALSE,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_START_BALANCE,
     g_param_spec_boxed("start-balance",
                        "Starting Balance",
                        "The starting balance for the account.  This "
                        "parameter is intended for use with backends that "
                        "do not return the complete list of splits for an "
                        "account, but rather return a partial list.  In "
                        "such a case, the backend will typically return "
                        "all of the splits after some certain date, and "
                        "the 'starting balance' will represent the "
                        "summation of the splits up to that date.",
                        GNC_TYPE_NUMERIC,
                        static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_START_CLEARED_BALANCE,
     g_param_spec_boxed("start-cleared-balance",
                        "Starting Cleared Balance",
                        "The starting cleared balance for the account.  "
                        "This parameter is intended for use with backends "
                        "that do not return the complete list of splits "
                        "for an account, but rather return a partial "
                        "list.  In such a case, the backend will "
                        "typically return all of the splits after "
                        "some certain date, and the 'starting cleared "
                        "balance' will represent the summation of the "
                        "splits up to that date.",
                        GNC_TYPE_NUMERIC,
                        static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_START_RECONCILED_BALANCE,
     g_param_spec_boxed("start-reconciled-balance",
                        "Starting Reconciled Balance",
                        "The starting reconciled balance for the "
                        "account.  This parameter is intended for use "
                        "with backends that do not return the complete "
                        "list of splits for an account, but rather return "
                        "a partial list.  In such a case, the backend "
                        "will typically return all of the splits after "
                        "some certain date, and the 'starting reconciled "
                        "balance' will represent the summation of the "
                        "splits up to that date.",
                        GNC_TYPE_NUMERIC,
                        static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_END_BALANCE,
     g_param_spec_boxed("end-balance",
                        "Ending Account Balance",
                        "This is the current ending balance for the "
                        "account.  It is computed from the sum of the "
                        "starting balance and all splits in the account.",
                        GNC_TYPE_NUMERIC,
                        G_PARAM_READABLE));

    g_object_class_install_property
    (gobject_class,
     PROP_END_CLEARED_BALANCE,
     g_param_spec_boxed("end-cleared-balance",
                        "Ending Account Cleared Balance",
                        "This is the current ending cleared balance for "
                        "the account.  It is computed from the sum of the "
                        "starting balance and all cleared splits in the "
                        "account.",
                        GNC_TYPE_NUMERIC,
                        G_PARAM_READABLE));

    g_object_class_install_property
    (gobject_class,
     PROP_END_RECONCILED_BALANCE,
     g_param_spec_boxed("end-reconciled-balance",
                        "Ending Account Reconciled Balance",
                        "This is the current ending reconciled balance "
                        "for the account.  It is computed from the sum of "
                        "the starting balance and all reconciled splits "
                        "in the account.",
                        GNC_TYPE_NUMERIC,
                        static_cast<GParamFlags>(G_PARAM_READABLE)));

    g_object_class_install_property
    (gobject_class,
     PROP_POLICY,
     g_param_spec_pointer ("policy",
                           "Policy",
                           "The account lots policy.",
                           static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_MARK,
     g_param_spec_int ("acct-mark",
                       "Account Mark",
                       "Ipsum Lorem",
                       0,
                       G_MAXINT16,
                       0,
                       static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_TAX_RELATED,
     g_param_spec_boolean ("tax-related",
                           "Tax Related",
                           "Whether the account maps to an entry on an "
                           "income tax document.",
                           FALSE,
                           static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_TAX_CODE,
     g_param_spec_string ("tax-code",
                          "Tax Code",
                          "This is the code for mapping an account to a "
                          "specific entry on a taxable document.  In the "
                          "United States it is used to transfer totals "
                          "into tax preparation software.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_TAX_SOURCE,
     g_param_spec_string ("tax-source",
                          "Tax Source",
                          "This specifies where exported name comes from.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_TAX_COPY_NUMBER,
     g_param_spec_int64 ("tax-copy-number",
                         "Tax Copy Number",
                         "This specifies the copy number of the tax "
                         "form/schedule.",
                         (gint64)1,
                         G_MAXINT64,
                         (gint64)1,
                         static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_HIDDEN,
     g_param_spec_boolean ("hidden",
                           "Hidden",
                           "Whether the account should be hidden in the  "
                           "account tree.",
                           FALSE,
                           static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_PLACEHOLDER,
     g_param_spec_boolean ("placeholder",
                           "Placeholder",
                           "Whether the account is a placeholder account which does not "
                           "allow transactions to be created, edited or deleted.",
                           FALSE,
                           static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_FILTER,
     g_param_spec_string ("filter",
                          "Account Filter",
                          "The account filter is a value saved to allow "
                          "filters to be recalled.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_SORT_ORDER,
     g_param_spec_string ("sort-order",
                          "Account Sort Order",
                          "The account sort order is a value saved to allow "
                          "the sort order to be recalled.",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_SORT_REVERSED,
     g_param_spec_boolean ("sort-reversed",
                          "Account Sort Reversed",
                          "Parameter to store whether the sort order is reversed or not.",
                          FALSE,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_LOT_NEXT_ID,
     g_param_spec_int64 ("lot-next-id",
                         "Lot Next ID",
                         "Tracks the next id to use in gnc_lot_make_default.",
                         (gint64)1,
                         G_MAXINT64,
                         (gint64)1,
                         static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_ONLINE_ACCOUNT,
     g_param_spec_string ("online-id",
                          "Online Account ID",
                          "The online account which corresponds to this "
                          "account for OFX import",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

     g_object_class_install_property(
       gobject_class,
       PROP_OFX_INCOME_ACCOUNT,
        g_param_spec_boxed("ofx-income-account",
                           "Associated income account",
                           "Used by the OFX importer.",
                           GNC_TYPE_GUID,
                           static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_AB_ACCOUNT_ID,
     g_param_spec_string ("ab-account-id",
                          "AQBanking Account ID",
                          "The AqBanking account which corresponds to this "
                          "account for AQBanking import",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));
    g_object_class_install_property
    (gobject_class,
     PROP_AB_BANK_CODE,
     g_param_spec_string ("ab-bank-code",
                          "AQBanking Bank Code",
                          "The online account which corresponds to this "
                          "account for AQBanking import",
                          NULL,
                          static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_AB_ACCOUNT_UID,
     g_param_spec_int64 ("ab-account-uid",
                         "AQBanking Account UID",
                         "Tracks the next id to use in gnc_lot_make_default.",
                         (gint64)1,
                         G_MAXINT64,
                         (gint64)1,
                         static_cast<GParamFlags>(G_PARAM_READWRITE)));

    g_object_class_install_property
    (gobject_class,
     PROP_AB_TRANS_RETRIEVAL,
     g_param_spec_boxed("ab-trans-retrieval",
                        "AQBanking Last Transaction Retrieval",
                        "The time of the last transaction retrieval for this "
                        "account.",
                        GNC_TYPE_TIMESPEC,
                        static_cast<GParamFlags>(G_PARAM_READWRITE)));

}

static void
xaccInitAccount (Account * acc, QofBook *book)
{
    ENTER ("book=%p\n", book);
    qof_instance_init_data (&acc->inst, GNC_ID_ACCOUNT, book);

    LEAVE ("account=%p\n", acc);
}

/********************************************************************\
\********************************************************************/

QofBook *
gnc_account_get_book(const Account *account)
{
    return qof_instance_get_book(QOF_INSTANCE(account));
}

/********************************************************************\
\********************************************************************/

static Account *
gnc_coll_get_root_account (QofCollection *col)
{
    if (!col) return NULL;
    return static_cast<Account*>(qof_collection_get_data (col));
}

static void
gnc_coll_set_root_account (QofCollection *col, Account *root)
{
    AccountPrivate *rpriv;
    Account *old_root;
    if (!col) return;

    old_root = gnc_coll_get_root_account (col);
    if (old_root == root) return;

    /* If the new root is already linked into the tree somewhere, then
     * remove it from its current position before adding it at the
     * top. */
    rpriv = GET_PRIVATE(root);
    if (rpriv->parent)
    {
        xaccAccountBeginEdit(root);
        gnc_account_remove_child(rpriv->parent, root);
        xaccAccountCommitEdit(root);
    }

    qof_collection_set_data (col, root);

    if (old_root)
    {
        xaccAccountBeginEdit (old_root);
        xaccAccountDestroy (old_root);
    }
}

Account *
gnc_book_get_root_account (QofBook *book)
{
    QofCollection *col;
    Account *root;

    if (!book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_ROOT_ACCOUNT);
    root = gnc_coll_get_root_account (col);
    if (root == NULL)
        root = gnc_account_create_root(book);
    return root;
}

void
gnc_book_set_root_account (QofBook *book, Account *root)
{
    QofCollection *col;
    if (!book) return;

    if (root && gnc_account_get_book(root) != book)
    {
        PERR ("cannot mix and match books freely!");
        return;
    }

    col = qof_book_get_collection (book, GNC_ID_ROOT_ACCOUNT);
    gnc_coll_set_root_account (col, root);
}

/********************************************************************\
\********************************************************************/

Account *
xaccMallocAccount (QofBook *book)
{
    Account *acc;

    g_return_val_if_fail (book, NULL);

    acc = static_cast<Account*>(g_object_new (GNC_TYPE_ACCOUNT, NULL));
    xaccInitAccount (acc, book);
    qof_event_gen (&acc->inst, QOF_EVENT_CREATE, NULL);

    return acc;
}

Account *
gnc_account_create_root (QofBook *book)
{
    Account *root;
    AccountPrivate *rpriv;

    root = xaccMallocAccount(book);
    rpriv = GET_PRIVATE(root);
    xaccAccountBeginEdit(root);
    rpriv->type = ACCT_TYPE_ROOT;
    rpriv->accountName = qof_string_cache_replace(rpriv->accountName, "Root Account");
    mark_account (root);
    xaccAccountCommitEdit(root);
    gnc_book_set_root_account(book, root);
    return root;
}

Account *
xaccCloneAccount(const Account *from, QofBook *book)
{
    Account *ret;
    AccountPrivate *from_priv, *priv;

    g_return_val_if_fail(GNC_IS_ACCOUNT(from), NULL);
    g_return_val_if_fail(QOF_IS_BOOK(book), NULL);

    ENTER (" ");
    ret = static_cast<Account*>(g_object_new (GNC_TYPE_ACCOUNT, NULL));
    g_return_val_if_fail (ret, NULL);

    from_priv = GET_PRIVATE(from);
    priv = GET_PRIVATE(ret);
    xaccInitAccount (ret, book);

    /* Do not Begin/CommitEdit() here; give the caller
     * a chance to fix things up, and let them do it.
     * Also let caller issue the generate_event (EVENT_CREATE) */
    priv->type = from_priv->type;

    priv->accountName = static_cast<char*>(qof_string_cache_insert(from_priv->accountName));
    priv->accountCode = static_cast<char*>(qof_string_cache_insert(from_priv->accountCode));
    priv->description = static_cast<char*>(qof_string_cache_insert(from_priv->description));

    qof_instance_copy_kvp (QOF_INSTANCE (ret), QOF_INSTANCE (from));

    /* The new book should contain a commodity that matches
     * the one in the old book. Find it, use it. */
    priv->commodity = gnc_commodity_obtain_twin(from_priv->commodity, book);
    gnc_commodity_increment_usage_count(priv->commodity);

    priv->commodity_scu = from_priv->commodity_scu;
    priv->non_standard_scu = from_priv->non_standard_scu;

    qof_instance_set_dirty(&ret->inst);
    LEAVE (" ");
    return ret;
}

/********************************************************************\
\********************************************************************/

static void
xaccFreeOneChildAccount (Account *acc, gpointer dummy)
{
    /* FIXME: this code is kind of hacky.  actually, all this code
     * seems to assume that the account edit levels are all 1. */
    if (qof_instance_get_editlevel(acc) == 0)
        xaccAccountBeginEdit(acc);
    xaccAccountDestroy(acc);
}

static void
xaccFreeAccountChildren (Account *acc)
{
    AccountPrivate *priv;
    GList *children;

    /* Copy the list since it will be modified */
    priv = GET_PRIVATE(acc);
    children = g_list_copy(priv->children);
    g_list_foreach(children, (GFunc)xaccFreeOneChildAccount, NULL);
    g_list_free(children);

    /* The foreach should have removed all the children already. */
    if (priv->children)
        g_list_free(priv->children);
    priv->children = NULL;
}

/* The xaccFreeAccount() routine releases memory associated with the
 * account.  It should never be called directly from user code;
 * instead, the xaccAccountDestroy() routine should be used (because
 * xaccAccountDestroy() has the correct commit semantics). */
static void
xaccFreeAccount (Account *acc)
{
    AccountPrivate *priv;
    GList *lp;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    qof_event_gen (&acc->inst, QOF_EVENT_DESTROY, NULL);

    if (priv->children)
    {
        PERR (" instead of calling xaccFreeAccount(), please call \n"
              " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");

        /* First, recursively free children */
        xaccFreeAccountChildren(acc);
    }

    /* remove lots -- although these should be gone by now. */
    if (priv->lots)
    {
        PERR (" instead of calling xaccFreeAccount(), please call \n"
              " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");

        for (lp = priv->lots; lp; lp = lp->next)
        {
            GNCLot *lot = static_cast<GNCLot*>(lp->data);
            gnc_lot_destroy (lot);
        }
        g_list_free (priv->lots);
        priv->lots = NULL;
    }

    /* Next, clean up the splits */
    /* NB there shouldn't be any splits by now ... they should
     * have been all been freed by CommitEdit().  We can remove this
     * check once we know the warning isn't occurring any more. */
    if (priv->splits)
    {
        GList *slist;
        PERR (" instead of calling xaccFreeAccount(), please call \n"
              " xaccAccountBeginEdit(); xaccAccountDestroy(); \n");

        qof_instance_reset_editlevel(acc);

        slist = g_list_copy(priv->splits);
        for (lp = slist; lp; lp = lp->next)
        {
            Split *s = (Split *) lp->data;
            g_assert(xaccSplitGetAccount(s) == acc);
            xaccSplitDestroy (s);
        }
        g_list_free(slist);
/* Nothing here (or in xaccAccountCommitEdit) NULLs priv->splits, so this asserts every time.
        g_assert(priv->splits == NULL);
*/
    }

    qof_string_cache_remove(priv->accountName);
    qof_string_cache_remove(priv->accountCode);
    qof_string_cache_remove(priv->description);
    priv->accountName = priv->accountCode = priv->description = nullptr;

    /* zero out values, just in case stray
     * pointers are pointing here. */

    priv->parent = nullptr;
    priv->children = nullptr;

    priv->balance  = gnc_numeric_zero();
    priv->cleared_balance = gnc_numeric_zero();
    priv->reconciled_balance = gnc_numeric_zero();

    priv->type = ACCT_TYPE_NONE;
    gnc_commodity_decrement_usage_count(priv->commodity);
    priv->commodity = NULL;

    priv->balance_dirty = FALSE;
    priv->sort_dirty = FALSE;

    /* qof_instance_release (&acc->inst); */
    g_object_unref(acc);
}

/********************************************************************\
 * transactional routines
\********************************************************************/

void
xaccAccountBeginEdit (Account *acc)
{
    g_return_if_fail(acc);
    qof_begin_edit(&acc->inst);
}

static void on_done(QofInstance *inst)
{
    /* old event style */
    qof_event_gen (inst, QOF_EVENT_MODIFY, NULL);
}

static void on_err (QofInstance *inst, QofBackendError errcode)
{
    PERR("commit error: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void acc_free (QofInstance *inst)
{
    AccountPrivate *priv;
    Account *acc = (Account *) inst;

    priv = GET_PRIVATE(acc);
    if (priv->parent)
        gnc_account_remove_child(priv->parent, acc);
    xaccFreeAccount(acc);
}

static void
destroy_pending_splits_for_account(QofInstance *ent, gpointer acc)
{
    Transaction *trans = (Transaction *) ent;
    Split *split;

    if (xaccTransIsOpen(trans))
        while ((split = xaccTransFindSplitByAccount(trans, static_cast<Account*>(acc))))
            xaccSplitDestroy(split);
}

void
xaccAccountCommitEdit (Account *acc)
{
    AccountPrivate *priv;
    QofBook *book;

    g_return_if_fail(acc);
    if (!qof_commit_edit(&acc->inst)) return;

    /* If marked for deletion, get rid of subaccounts first,
     * and then the splits ... */
    priv = GET_PRIVATE(acc);
    if (qof_instance_get_destroying(acc))
    {
        GList *lp, *slist;
        QofCollection *col;

        qof_instance_increase_editlevel(acc);

        /* First, recursively free children */
        xaccFreeAccountChildren(acc);

        PINFO ("freeing splits for account %p (%s)",
               acc, priv->accountName ? priv->accountName : "(null)");

        book = qof_instance_get_book(acc);

        /* If book is shutting down, just clear the split list.  The splits
           themselves will be destroyed by the transaction code */
        if (!qof_book_shutting_down(book))
        {
            slist = g_list_copy(priv->splits);
            for (lp = slist; lp; lp = lp->next)
            {
                Split *s = static_cast<Split *>(lp->data);
                xaccSplitDestroy (s);
            }
            g_list_free(slist);
        }
        else
        {
            g_list_free(priv->splits);
            priv->splits = NULL;
        }

        /* It turns out there's a case where this assertion does not hold:
           When the user tries to delete an Imbalance account, while also
           deleting all the splits in it.  The splits will just get
           recreated and put right back into the same account!

           g_assert(priv->splits == NULL || qof_book_shutting_down(acc->inst.book));
        */

        if (!qof_book_shutting_down(book))
        {
            col = qof_book_get_collection(book, GNC_ID_TRANS);
            qof_collection_foreach(col, destroy_pending_splits_for_account, acc);

            /* the lots should be empty by now */
            for (lp = priv->lots; lp; lp = lp->next)
            {
                GNCLot *lot = static_cast<GNCLot*>(lp->data);
                gnc_lot_destroy (lot);
            }
        }
        g_list_free(priv->lots);
        priv->lots = NULL;

        qof_instance_set_dirty(&acc->inst);
        qof_instance_decrease_editlevel(acc);
    }
    else
    {
        xaccAccountBringUpToDate(acc);
    }

    qof_commit_edit_part2(&acc->inst, on_err, on_done, acc_free);
}

void
xaccAccountDestroy (Account *acc)
{
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    qof_instance_set_destroying(acc, TRUE);

    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/
static gint
compare_account_by_name (gconstpointer a, gconstpointer b)
{
    AccountPrivate *priv_a, *priv_b;
    if (a && !b) return 1;
    if (b && !a) return -1;
    if (!a && !b) return 0;
    priv_a = GET_PRIVATE((Account*)a);
    priv_b = GET_PRIVATE((Account*)b);
    if ((priv_a->accountCode && strlen (priv_a->accountCode)) ||
        (priv_b->accountCode && strlen (priv_b->accountCode)))
        return g_strcmp0 (priv_a->accountCode, priv_b->accountCode);
    return g_strcmp0 (priv_a->accountName, priv_b->accountName);
}

static gboolean
xaccAcctChildrenEqual(const GList *na,
                      const GList *nb,
                      gboolean check_guids)
{
    if ((!na && nb) || (na && !nb))
    {
        PINFO ("only one has accounts");
        return(FALSE);
    }
    if (g_list_length ((GList*)na) != g_list_length ((GList*)nb))
    {
        PINFO ("Accounts have different numbers of children");
        return (FALSE);
    }

    while (na)
    {
        Account *aa = static_cast<Account*>(na->data);
        Account *ab;
        GList *node = g_list_find_custom ((GList*)nb, aa,
                                          (GCompareFunc)compare_account_by_name);

        if (!node)
        {
            PINFO ("Unable to find matching child account.");
            return FALSE;
        }
        ab = static_cast<Account*>(node->data);
        if (!xaccAccountEqual(aa, ab, check_guids))
        {
            char sa[GUID_ENCODING_LENGTH + 1];
            char sb[GUID_ENCODING_LENGTH + 1];

            guid_to_string_buff (xaccAccountGetGUID (aa), sa);
            guid_to_string_buff (xaccAccountGetGUID (ab), sb);

            PWARN ("accounts %s and %s differ", sa, sb);

            return(FALSE);
        }

        na = na->next;
    }

    return(TRUE);
}

gboolean
xaccAccountEqual(const Account *aa, const Account *ab, gboolean check_guids)
{
    AccountPrivate *priv_aa, *priv_ab;

    if (!aa && !ab) return TRUE;

    g_return_val_if_fail(GNC_IS_ACCOUNT(aa), FALSE);
    g_return_val_if_fail(GNC_IS_ACCOUNT(ab), FALSE);

    priv_aa = GET_PRIVATE(aa);
    priv_ab = GET_PRIVATE(ab);
    if (priv_aa->type != priv_ab->type)
    {
        PWARN ("types differ: %d vs %d", priv_aa->type, priv_ab->type);
        return FALSE;
    }

    if (g_strcmp0(priv_aa->accountName, priv_ab->accountName) != 0)
    {
        PWARN ("names differ: %s vs %s", priv_aa->accountName, priv_ab->accountName);
        return FALSE;
    }

    if (g_strcmp0(priv_aa->accountCode, priv_ab->accountCode) != 0)
    {
        PWARN ("codes differ: %s vs %s", priv_aa->accountCode, priv_ab->accountCode);
        return FALSE;
    }

    if (g_strcmp0(priv_aa->description, priv_ab->description) != 0)
    {
        PWARN ("descriptions differ: %s vs %s", priv_aa->description, priv_ab->description);
        return FALSE;
    }

    if (!gnc_commodity_equal(priv_aa->commodity, priv_ab->commodity))
    {
        PWARN ("commodities differ");
        return FALSE;
    }

    if (check_guids)
    {
        if (qof_instance_guid_compare(aa, ab) != 0)
        {
            PWARN ("GUIDs differ");
            return FALSE;
        }
    }

    if (qof_instance_compare_kvp (QOF_INSTANCE (aa), QOF_INSTANCE (ab)) != 0)
    {
        char *frame_a;
        char *frame_b;

        frame_a = qof_instance_kvp_as_string (QOF_INSTANCE (aa));
        frame_b = qof_instance_kvp_as_string (QOF_INSTANCE (ab));

        PWARN ("kvp frames differ:\n%s\n\nvs\n\n%s", frame_a, frame_b);

        g_free (frame_a);
        g_free (frame_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(priv_aa->starting_balance, priv_ab->starting_balance))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(priv_aa->starting_balance);
        str_b = gnc_numeric_to_string(priv_ab->starting_balance);

        PWARN ("starting balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(priv_aa->starting_cleared_balance,
                           priv_ab->starting_cleared_balance))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(priv_aa->starting_cleared_balance);
        str_b = gnc_numeric_to_string(priv_ab->starting_cleared_balance);

        PWARN ("starting cleared balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(priv_aa->starting_reconciled_balance,
                           priv_ab->starting_reconciled_balance))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(priv_aa->starting_reconciled_balance);
        str_b = gnc_numeric_to_string(priv_ab->starting_reconciled_balance);

        PWARN ("starting reconciled balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(priv_aa->balance, priv_ab->balance))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(priv_aa->balance);
        str_b = gnc_numeric_to_string(priv_ab->balance);

        PWARN ("balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(priv_aa->cleared_balance, priv_ab->cleared_balance))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(priv_aa->cleared_balance);
        str_b = gnc_numeric_to_string(priv_ab->cleared_balance);

        PWARN ("cleared balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    if (!gnc_numeric_equal(priv_aa->reconciled_balance, priv_ab->reconciled_balance))
    {
        char *str_a;
        char *str_b;

        str_a = gnc_numeric_to_string(priv_aa->reconciled_balance);
        str_b = gnc_numeric_to_string(priv_ab->reconciled_balance);

        PWARN ("reconciled balances differ: %s vs %s", str_a, str_b);

        g_free (str_a);
        g_free (str_b);

        return FALSE;
    }

    /* no parent; always compare downwards. */

    {
        GList *la = priv_aa->splits;
        GList *lb = priv_ab->splits;

        if ((la && !lb) || (!la && lb))
        {
            PWARN ("only one has splits");
            return FALSE;
        }

        if (la && lb)
        {
            /* presume that the splits are in the same order */
            while (la && lb)
            {
                Split *sa = (Split *) la->data;
                Split *sb = (Split *) lb->data;

                if (!xaccSplitEqual(sa, sb, check_guids, TRUE, FALSE))
                {
                    PWARN ("splits differ");
                    return(FALSE);
                }

                la = la->next;
                lb = lb->next;
            }

            if ((la != NULL) || (lb != NULL))
            {
                PWARN ("number of splits differs");
                return(FALSE);
            }
        }
    }

    if (!xaccAcctChildrenEqual(priv_aa->children, priv_ab->children, check_guids))
    {
        PWARN ("children differ");
        return FALSE;
    }

    return(TRUE);
}

/********************************************************************\
\********************************************************************/
void
gnc_account_set_sort_dirty (Account *acc)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    if (qof_instance_get_destroying(acc))
        return;

    priv = GET_PRIVATE(acc);
    priv->sort_dirty = TRUE;
}

void
gnc_account_set_balance_dirty (Account *acc)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    if (qof_instance_get_destroying(acc))
        return;

    priv = GET_PRIVATE(acc);
    priv->balance_dirty = TRUE;
}

/********************************************************************\
\********************************************************************/

gboolean
gnc_account_insert_split (Account *acc, Split *s)
{
    AccountPrivate *priv;
    GList *node;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    g_return_val_if_fail(GNC_IS_SPLIT(s), FALSE);

    priv = GET_PRIVATE(acc);
    node = g_list_find(priv->splits, s);
    if (node)
        return FALSE;

    if (qof_instance_get_editlevel(acc) == 0)
    {
        priv->splits = g_list_insert_sorted(priv->splits, s,
                                            (GCompareFunc)xaccSplitOrder);
    }
    else
    {
        priv->splits = g_list_prepend(priv->splits, s);
        priv->sort_dirty = TRUE;
    }

    //FIXME: find better event
    qof_event_gen (&acc->inst, QOF_EVENT_MODIFY, NULL);
    /* Also send an event based on the account */
    qof_event_gen(&acc->inst, GNC_EVENT_ITEM_ADDED, s);

    priv->balance_dirty = TRUE;
//  DRH: Should the below be added? It is present in the delete path.
//  xaccAccountRecomputeBalance(acc);
    return TRUE;
}

gboolean
gnc_account_remove_split (Account *acc, Split *s)
{
    AccountPrivate *priv;
    GList *node;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    g_return_val_if_fail(GNC_IS_SPLIT(s), FALSE);

    priv = GET_PRIVATE(acc);
    node = g_list_find(priv->splits, s);
    if (NULL == node)
        return FALSE;

    priv->splits = g_list_delete_link(priv->splits, node);
    //FIXME: find better event type
    qof_event_gen(&acc->inst, QOF_EVENT_MODIFY, NULL);
    // And send the account-based event, too
    qof_event_gen(&acc->inst, GNC_EVENT_ITEM_REMOVED, s);

    priv->balance_dirty = TRUE;
    xaccAccountRecomputeBalance(acc);
    return TRUE;
}

void
xaccAccountSortSplits (Account *acc, gboolean force)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    if (!priv->sort_dirty || (!force && qof_instance_get_editlevel(acc) > 0))
        return;
    priv->splits = g_list_sort(priv->splits, (GCompareFunc)xaccSplitOrder);
    priv->sort_dirty = FALSE;
    priv->balance_dirty = TRUE;
}

static void
xaccAccountBringUpToDate(Account *acc)
{
    if (!acc) return;

    /* if a re-sort happens here, then everything will update, so the
       cost basis and balance calls are no-ops */
    xaccAccountSortSplits(acc, FALSE);
    xaccAccountRecomputeBalance(acc);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetGUID (Account *acc, const GncGUID *guid)
{
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(guid);

    /* XXX this looks fishy and weird to me ... */
    PINFO("acct=%p", acc);
    xaccAccountBeginEdit (acc);
    qof_instance_set_guid (&acc->inst, guid);
    qof_instance_set_dirty(&acc->inst);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

Account *
xaccAccountLookup (const GncGUID *guid, QofBook *book)
{
    QofCollection *col;
    if (!guid || !book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_ACCOUNT);
    return (Account *) qof_collection_lookup_entity (col, guid);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetMark (Account *acc, short m)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    priv->mark = m;
}

void
xaccClearMark (Account *acc, short val)
{
    Account *root;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    root = gnc_account_get_root(acc);
    xaccClearMarkDown(root ? root : acc, val);
}

void
xaccClearMarkDown (Account *acc, short val)
{
    AccountPrivate *priv;
    GList *node;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    priv->mark = val;
    for (node = priv->children; node; node = node->next)
    {
        xaccClearMarkDown(static_cast<Account*>(node->data), val);
    }
}

/********************************************************************\
\********************************************************************/

GNCPolicy *
gnc_account_get_policy (Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);

    return GET_PRIVATE(acc)->policy;
}

void
gnc_account_set_policy (Account *acc, GNCPolicy *policy)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    priv->policy = policy ? policy : xaccGetFIFOPolicy();
}

/********************************************************************\
\********************************************************************/

void
xaccAccountRemoveLot (Account *acc, GNCLot *lot)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(GNC_IS_LOT(lot));

    priv = GET_PRIVATE(acc);
    g_return_if_fail(priv->lots);

    ENTER ("(acc=%p, lot=%p)", acc, lot);
    priv->lots = g_list_remove(priv->lots, lot);
    qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_REMOVE, NULL);
    qof_event_gen (&acc->inst, QOF_EVENT_MODIFY, NULL);
    LEAVE ("(acc=%p, lot=%p)", acc, lot);
}

void
xaccAccountInsertLot (Account *acc, GNCLot *lot)
{
    AccountPrivate *priv, *opriv;
    Account * old_acc = NULL;
    Account* lot_account;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(GNC_IS_LOT(lot));

    /* optimizations */
    lot_account = gnc_lot_get_account(lot);
    if (lot_account == acc)
        return;

    ENTER ("(acc=%p, lot=%p)", acc, lot);

    /* pull it out of the old account */
    if (lot_account)
    {
        old_acc = lot_account;
        opriv = GET_PRIVATE(old_acc);
        opriv->lots = g_list_remove(opriv->lots, lot);
    }

    priv = GET_PRIVATE(acc);
    priv->lots = g_list_prepend(priv->lots, lot);
    gnc_lot_set_account(lot, acc);

    /* Don't move the splits to the new account.  The caller will do this
     * if appropriate, and doing it here will not work if we are being
     * called from gnc_book_close_period since xaccAccountInsertSplit
     * will try to balance capital gains and things aren't ready for that. */

    qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_ADD, NULL);
    qof_event_gen (&acc->inst, QOF_EVENT_MODIFY, NULL);

    LEAVE ("(acc=%p, lot=%p)", acc, lot);
}

/********************************************************************\
\********************************************************************/
static void
xaccPreSplitMove (Split *split, gpointer dummy)
{
    xaccTransBeginEdit (xaccSplitGetParent (split));
}

static void
xaccPostSplitMove (Split *split, Account *accto)
{
    Transaction *trans;

    xaccSplitSetAccount(split, accto);
    xaccSplitSetAmount(split, split->amount);
    trans = xaccSplitGetParent (split);
    xaccTransCommitEdit (trans);
}

void
xaccAccountMoveAllSplits (Account *accfrom, Account *accto)
{
    AccountPrivate *from_priv;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(accfrom));
    g_return_if_fail(GNC_IS_ACCOUNT(accto));

    /* optimizations */
    from_priv = GET_PRIVATE(accfrom);
    if (!from_priv->splits || accfrom == accto)
        return;

    /* check for book mix-up */
    g_return_if_fail (qof_instance_books_equal(accfrom, accto));
    ENTER ("(accfrom=%p, accto=%p)", accfrom, accto);

    xaccAccountBeginEdit(accfrom);
    xaccAccountBeginEdit(accto);
    /* Begin editing both accounts and all transactions in accfrom. */
    g_list_foreach(from_priv->splits, (GFunc)xaccPreSplitMove, NULL);

    /* Concatenate accfrom's lists of splits and lots to accto's lists. */
    //to_priv->splits = g_list_concat(to_priv->splits, from_priv->splits);
    //to_priv->lots = g_list_concat(to_priv->lots, from_priv->lots);

    /* Set appropriate flags. */
    //from_priv->balance_dirty = TRUE;
    //from_priv->sort_dirty = FALSE;
    //to_priv->balance_dirty = TRUE;
    //to_priv->sort_dirty = TRUE;

    /*
     * Change each split's account back pointer to accto.
     * Convert each split's amount to accto's commodity.
     * Commit to editing each transaction.
     */
    g_list_foreach(from_priv->splits, (GFunc)xaccPostSplitMove, (gpointer)accto);

    /* Finally empty accfrom. */
    g_assert(from_priv->splits == NULL);
    g_assert(from_priv->lots == NULL);
    xaccAccountCommitEdit(accfrom);
    xaccAccountCommitEdit(accto);

    LEAVE ("(accfrom=%p, accto=%p)", accfrom, accto);
}


/********************************************************************\
 * xaccAccountRecomputeBalance                                      *
 *   recomputes the partial balances and the current balance for    *
 *   this account.                                                  *
 *                                                                  *
 * The way the computation is done depends on whether the partial   *
 * balances are for a monetary account (bank, cash, etc.) or a      *
 * certificate account (stock portfolio, mutual fund).  For bank    *
 * accounts, the invariant amount is the dollar amount. For share   *
 * accounts, the invariant amount is the number of shares. For      *
 * share accounts, the share price fluctuates, and the current      *
 * value of such an account is the number of shares times the       *
 * current share price.                                             *
 *                                                                  *
 * Part of the complexity of this computation stems from the fact   *
 * xacc uses a double-entry system, meaning that one transaction    *
 * appears in two accounts: one account is debited, and the other   *
 * is credited.  When the transaction represents a sale of shares,  *
 * or a purchase of shares, some care must be taken to compute      *
 * balances correctly.  For a sale of shares, the stock account must*
 * be debited in shares, but the bank account must be credited      *
 * in dollars.  Thus, two different mechanisms must be used to      *
 * compute balances, depending on account type.                     *
 *                                                                  *
 * Args:   account -- the account for which to recompute balances   *
 * Return: void                                                     *
\********************************************************************/

void
xaccAccountRecomputeBalance (Account * acc)
{
    AccountPrivate *priv;
    gnc_numeric  balance;
    gnc_numeric  cleared_balance;
    gnc_numeric  reconciled_balance;
    GList *lp;

    if (NULL == acc) return;

    priv = GET_PRIVATE(acc);
    if (qof_instance_get_editlevel(acc) > 0) return;
    if (!priv->balance_dirty) return;
    if (qof_instance_get_destroying(acc)) return;
    if (qof_book_shutting_down(qof_instance_get_book(acc))) return;

    balance            = priv->starting_balance;
    cleared_balance    = priv->starting_cleared_balance;
    reconciled_balance = priv->starting_reconciled_balance;

    PINFO ("acct=%s starting baln=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT,
           priv->accountName, balance.num, balance.denom);
    for (lp = priv->splits; lp; lp = lp->next)
    {
        Split *split = (Split *) lp->data;
        gnc_numeric amt = xaccSplitGetAmount (split);

        balance = gnc_numeric_add_fixed(balance, amt);

        if (NREC != split->reconciled)
        {
            cleared_balance = gnc_numeric_add_fixed(cleared_balance, amt);
        }

        if (YREC == split->reconciled ||
                FREC == split->reconciled)
        {
            reconciled_balance =
                gnc_numeric_add_fixed(reconciled_balance, amt);
        }

        split->balance = balance;
        split->cleared_balance = cleared_balance;
        split->reconciled_balance = reconciled_balance;

    }

    priv->balance = balance;
    priv->cleared_balance = cleared_balance;
    priv->reconciled_balance = reconciled_balance;
    priv->balance_dirty = FALSE;
}

/********************************************************************\
\********************************************************************/

/* The sort order is used to implicitly define an
 * order for report generation */

static int typeorder[NUM_ACCOUNT_TYPES] =
{
    ACCT_TYPE_BANK, ACCT_TYPE_STOCK, ACCT_TYPE_MUTUAL, ACCT_TYPE_CURRENCY,
    ACCT_TYPE_CASH, ACCT_TYPE_ASSET, ACCT_TYPE_RECEIVABLE,
    ACCT_TYPE_CREDIT, ACCT_TYPE_LIABILITY, ACCT_TYPE_PAYABLE,
    ACCT_TYPE_INCOME, ACCT_TYPE_EXPENSE, ACCT_TYPE_EQUITY, ACCT_TYPE_TRADING
};

static int revorder[NUM_ACCOUNT_TYPES] =
{
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
};


int
xaccAccountOrder (const Account *aa, const Account *ab)
{
    AccountPrivate *priv_aa, *priv_ab;
    char *da, *db;
    char *endptr = NULL;
    int ta, tb, result;
    long la, lb;

    if ( aa && !ab ) return -1;
    if ( !aa && ab ) return +1;
    if ( !aa && !ab ) return 0;

    priv_aa = GET_PRIVATE(aa);
    priv_ab = GET_PRIVATE(ab);

    /* sort on accountCode strings */
    da = priv_aa->accountCode;
    db = priv_ab->accountCode;

    /* If accountCodes are both base 36 integers do an integer sort */
    la = strtoul (da, &endptr, 36);
    if ((*da != '\0') && (*endptr == '\0'))
    {
        lb = strtoul (db, &endptr, 36);
        if ((*db != '\0') && (*endptr == '\0'))
        {
            if (la < lb) return -1;
            if (la > lb) return +1;
        }
    }

    /* Otherwise do a string sort */
    result = g_strcmp0 (da, db);
    if (result)
        return result;

    /* if account-type-order array not initialized, initialize it */
    /* this will happen at most once during program invocation */
    if (-1 == revorder[0])
    {
        int i;
        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
        {
            revorder [typeorder[i]] = i;
        }
    }

    /* otherwise, sort on account type */
    ta = priv_aa->type;
    tb = priv_ab->type;
    ta = revorder[ta];
    tb = revorder[tb];
    if (ta < tb) return -1;
    if (ta > tb) return +1;

    /* otherwise, sort on accountName strings */
    da = priv_aa->accountName;
    db = priv_ab->accountName;
    result = safe_utf8_collate(da, db);
    if (result)
        return result;

    /* guarantee a stable sort */
    return qof_instance_guid_compare(aa, ab);
}

static int
qof_xaccAccountOrder (const Account **aa, const Account **ab)
{
    return xaccAccountOrder(*aa, *ab);
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetType (Account *acc, GNCAccountType tip)
{
    AccountPrivate *priv;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(tip < NUM_ACCOUNT_TYPES);

    /* optimizations */
    priv = GET_PRIVATE(acc);
    if (priv->type == tip)
        return;

    xaccAccountBeginEdit(acc);
    priv->type = tip;
    priv->balance_dirty = TRUE; /* new type may affect balance computation */
    mark_account(acc);
    xaccAccountCommitEdit(acc);
}

void
xaccAccountSetName (Account *acc, const char *str)
{
    AccountPrivate *priv;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(str);

    /* optimizations */
    priv = GET_PRIVATE(acc);
    if (g_strcmp0(str, priv->accountName) == 0)
        return;

    xaccAccountBeginEdit(acc);
    priv->accountName = qof_string_cache_replace(priv->accountName, str);
    mark_account (acc);
    xaccAccountCommitEdit(acc);
}

void
xaccAccountSetCode (Account *acc, const char *str)
{
    AccountPrivate *priv;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    /* optimizations */
    priv = GET_PRIVATE(acc);
    if (g_strcmp0(str, priv->accountCode) == 0)
        return;

    xaccAccountBeginEdit(acc);
    priv->accountCode = qof_string_cache_replace(priv->accountCode, str ? str : "");
    mark_account (acc);
    xaccAccountCommitEdit(acc);
}

void
xaccAccountSetDescription (Account *acc, const char *str)
{
    AccountPrivate *priv;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    /* optimizations */
    priv = GET_PRIVATE(acc);
    if (g_strcmp0(str, priv->description) == 0)
        return;

    xaccAccountBeginEdit(acc);
    priv->description = qof_string_cache_replace(priv->description, str ? str : "");
    mark_account (acc);
    xaccAccountCommitEdit(acc);
}

static void
set_kvp_string_tag (Account *acc, const char *tag, const char *value)
{
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    xaccAccountBeginEdit(acc);
    if (value)
    {
        gchar *tmp = g_strstrip(g_strdup(value));
        if (strlen (tmp))
        {
            GValue v = G_VALUE_INIT;
            g_value_init (&v, G_TYPE_STRING);
            g_value_set_string (&v, tmp);
            qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {tag});
        }
        else
            qof_instance_set_path_kvp (QOF_INSTANCE (acc), NULL, {tag});
        g_free(tmp);
    }
    else
    {
         qof_instance_set_path_kvp (QOF_INSTANCE (acc), NULL, {tag});
    }
    mark_account (acc);
    xaccAccountCommitEdit(acc);
}

static const char*
get_kvp_string_tag (const Account *acc, const char *tag)
{
    GValue v = G_VALUE_INIT;
    if (acc == NULL || tag == NULL) return NULL;
    qof_instance_get_path_kvp (QOF_INSTANCE (acc), &v, {tag});
    return G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : NULL;
}

void
xaccAccountSetColor (Account *acc, const char *str)
{
    set_kvp_string_tag (acc, "color", str);
}

void
xaccAccountSetFilter (Account *acc, const char *str)
{
    set_kvp_string_tag (acc, "filter", str);
}

void
xaccAccountSetSortOrder (Account *acc, const char *str)
{
    set_kvp_string_tag (acc, "sort-order", str);
}

void
xaccAccountSetSortReversed (Account *acc, gboolean sortreversed)
{
    set_kvp_string_tag (acc, "sort-reversed", sortreversed ? "true" : NULL);
}

static void
qofAccountSetParent (Account *acc, QofInstance *parent)
{
    Account *parent_acc;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(GNC_IS_ACCOUNT(parent));

    parent_acc = GNC_ACCOUNT(parent);
    xaccAccountBeginEdit(acc);
    xaccAccountBeginEdit(parent_acc);
    gnc_account_append_child(parent_acc, acc);
    mark_account (parent_acc);
    mark_account (acc);
    xaccAccountCommitEdit(acc);
    xaccAccountCommitEdit(parent_acc);
}

void
xaccAccountSetNotes (Account *acc, const char *str)
{
    set_kvp_string_tag (acc, "notes", str);
}

void
xaccAccountSetCommodity (Account * acc, gnc_commodity * com)
{
    AccountPrivate *priv;
    GList *lp;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(GNC_IS_COMMODITY(com));

    /* optimizations */
    priv = GET_PRIVATE(acc);
    if (com == priv->commodity)
        return;

    xaccAccountBeginEdit(acc);
    gnc_commodity_decrement_usage_count(priv->commodity);
    priv->commodity = com;
    gnc_commodity_increment_usage_count(com);
    priv->commodity_scu = gnc_commodity_get_fraction(com);
    priv->non_standard_scu = FALSE;

    /* iterate over splits */
    for (lp = priv->splits; lp; lp = lp->next)
    {
        Split *s = (Split *) lp->data;
        Transaction *trans = xaccSplitGetParent (s);

        xaccTransBeginEdit (trans);
        xaccSplitSetAmount (s, xaccSplitGetAmount(s));
        xaccTransCommitEdit (trans);
    }

    priv->sort_dirty = TRUE;  /* Not needed. */
    priv->balance_dirty = TRUE;
    mark_account (acc);

    xaccAccountCommitEdit(acc);
}

/*
 * Set the account scu and then check to see if it is the same as the
 * commodity scu.  This function is called when parsing the data file
 * and is designed to catch cases where the two were accidentally set
 * to mismatched values in the past.
 */
void
xaccAccountSetCommoditySCU (Account *acc, int scu)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    xaccAccountBeginEdit(acc);
    priv->commodity_scu = scu;
    if (scu != gnc_commodity_get_fraction(priv->commodity))
        priv->non_standard_scu = TRUE;
    mark_account(acc);
    xaccAccountCommitEdit(acc);
}

int
xaccAccountGetCommoditySCUi (const Account * acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), 0);
    return GET_PRIVATE(acc)->commodity_scu;
}

int
xaccAccountGetCommoditySCU (const Account * acc)
{
    AccountPrivate *priv;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), 0);

    priv = GET_PRIVATE(acc);
    if (priv->non_standard_scu || !priv->commodity)
        return priv->commodity_scu;
    return gnc_commodity_get_fraction(priv->commodity);
}

void
xaccAccountSetNonStdSCU (Account *acc, gboolean flag)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    if (priv->non_standard_scu == flag)
        return;
    xaccAccountBeginEdit(acc);
    priv->non_standard_scu = flag;
    mark_account (acc);
    xaccAccountCommitEdit(acc);
}

gboolean
xaccAccountGetNonStdSCU (const Account * acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), 0);
    return GET_PRIVATE(acc)->non_standard_scu;
}

/********************************************************************\
\********************************************************************/
/* below follow the old, deprecated currency/security routines. */

void
DxaccAccountSetCurrency (Account * acc, gnc_commodity * currency)
{
    QofBook *book;
    GValue v = G_VALUE_INIT;
    const char *s = gnc_commodity_get_unique_name (currency);
    gnc_commodity *commodity;
    gnc_commodity_table *table;

    if ((!acc) || (!currency)) return;
    g_value_init (&v, G_TYPE_STRING);
    g_value_set_string (&v, s);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"old-currency"});
    mark_account (acc);
    xaccAccountCommitEdit(acc);

    table = gnc_commodity_table_get_table (qof_instance_get_book(acc));
    commodity = gnc_commodity_table_lookup_unique (table, s);

    if (!commodity)
    {
        book = qof_instance_get_book(acc);
        gnc_commodity_table_insert (gnc_commodity_table_get_table (book),
				    currency);
    }
}

/********************************************************************\
\********************************************************************/

void
gnc_account_append_child (Account *new_parent, Account *child)
{
    AccountPrivate *ppriv, *cpriv;
    Account *old_parent;
    QofCollection *col;

    /* errors */
    g_assert(GNC_IS_ACCOUNT(new_parent));
    g_assert(GNC_IS_ACCOUNT(child));

    /* optimizations */
    ppriv = GET_PRIVATE(new_parent);
    cpriv = GET_PRIVATE(child);
    old_parent = cpriv->parent;
    if (old_parent == new_parent)
        return;

    //  xaccAccountBeginEdit(new_parent);
    xaccAccountBeginEdit(child);
    if (old_parent)
    {
        gnc_account_remove_child(old_parent, child);

        if (!qof_instance_books_equal(old_parent, new_parent))
        {
            /* hack alert -- this implementation is not exactly correct.
             * If the entity tables are not identical, then the 'from' book
             * may have a different backend than the 'to' book.  This means
             * that we should get the 'from' backend to destroy this account,
             * and the 'to' backend to save it.  Right now, this is broken.
             *
             * A 'correct' implementation similar to this is in Period.c
             * except its for transactions ...
             *
             * Note also, we need to reparent the children to the new book as well.
             */
            PWARN ("reparenting accounts across books is not correctly supported\n");

            qof_event_gen (&child->inst, QOF_EVENT_DESTROY, NULL);
            col = qof_book_get_collection (qof_instance_get_book(new_parent),
                                           GNC_ID_ACCOUNT);
            qof_collection_insert_entity (col, &child->inst);
            qof_event_gen (&child->inst, QOF_EVENT_CREATE, NULL);
        }
    }
    cpriv->parent = new_parent;
    ppriv->children = g_list_append(ppriv->children, child);
    qof_instance_set_dirty(&new_parent->inst);
    qof_instance_set_dirty(&child->inst);

    /* Send events data. Warning: The call to commit_edit is also going
     * to send a MODIFY event. If the gtktreemodelfilter code gets the
     * MODIFY before it gets the ADD, it gets very confused and thinks
     * that two nodes have been added. */
    qof_event_gen (&child->inst, QOF_EVENT_ADD, NULL);
    // qof_event_gen (&new_parent->inst, QOF_EVENT_MODIFY, NULL);

    xaccAccountCommitEdit (child);
    //  xaccAccountCommitEdit(new_parent);
}

void
gnc_account_remove_child (Account *parent, Account *child)
{
    AccountPrivate *ppriv, *cpriv;
    GncEventData ed;

    if (!child) return;

    /* Note this routine might be called on accounts which
     * are not yet parented. */
    if (!parent) return;

    ppriv = GET_PRIVATE(parent);
    cpriv = GET_PRIVATE(child);

    if (cpriv->parent != parent)
    {
        PERR ("account not a child of parent");
        return;
    }

    /* Gather event data */
    ed.node = parent;
    ed.idx = g_list_index(ppriv->children, child);

    ppriv->children = g_list_remove(ppriv->children, child);

    /* Now send the event. */
    qof_event_gen(&child->inst, QOF_EVENT_REMOVE, &ed);

    /* clear the account's parent pointer after REMOVE event generation. */
    cpriv->parent = NULL;

    qof_event_gen (&parent->inst, QOF_EVENT_MODIFY, NULL);
}

Account *
gnc_account_get_parent (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return GET_PRIVATE(acc)->parent;
}

Account *
gnc_account_get_root (Account *acc)
{
    AccountPrivate *priv;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);

    priv = GET_PRIVATE(acc);
    while (priv->parent)
    {
        acc = priv->parent;
        priv = GET_PRIVATE(acc);
    }

    return acc;
}

gboolean
gnc_account_is_root (const Account *account)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(account), FALSE);
    return (GET_PRIVATE(account)->parent == NULL);
}

GList *
gnc_account_get_children (const Account *account)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(account), NULL);
    return g_list_copy(GET_PRIVATE(account)->children);
}

GList *
gnc_account_get_children_sorted (const Account *account)
{
    AccountPrivate *priv;

    /* errors */
    g_return_val_if_fail(GNC_IS_ACCOUNT(account), NULL);

    /* optimizations */
    priv = GET_PRIVATE(account);
    if (!priv->children)
        return NULL;
    return g_list_sort(g_list_copy(priv->children), (GCompareFunc)xaccAccountOrder);
}

gint
gnc_account_n_children (const Account *account)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(account), 0);
    return g_list_length(GET_PRIVATE(account)->children);
}

gint
gnc_account_child_index (const Account *parent, const Account *child)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(parent), -1);
    g_return_val_if_fail(GNC_IS_ACCOUNT(child), -1);
    return g_list_index(GET_PRIVATE(parent)->children, child);
}

Account *
gnc_account_nth_child (const Account *parent, gint num)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(parent), NULL);
    return static_cast<Account*>(g_list_nth_data(GET_PRIVATE(parent)->children, num));
}

gint
gnc_account_n_descendants (const Account *account)
{
    AccountPrivate *priv;
    GList *node;
    gint count = 0;

    g_return_val_if_fail(GNC_IS_ACCOUNT(account), 0);

    priv = GET_PRIVATE(account);
    for (node = priv->children; node; node = g_list_next(node))
    {
        count += gnc_account_n_descendants(static_cast<Account*>(node->data)) + 1;
    }
    return count;
}

gint
gnc_account_get_current_depth (const Account *account)
{
    AccountPrivate *priv;
    int depth = 0;

    g_return_val_if_fail(GNC_IS_ACCOUNT(account), 0);

    priv = GET_PRIVATE(account);
    while (priv->parent && (priv->type != ACCT_TYPE_ROOT))
    {
        account = priv->parent;
        priv = GET_PRIVATE(account);
        depth++;
    }

    return depth;
}

gint
gnc_account_get_tree_depth (const Account *account)
{
    AccountPrivate *priv;
    GList *node;
    gint depth = 0, child_depth;

    g_return_val_if_fail(GNC_IS_ACCOUNT(account), 0);

    priv = GET_PRIVATE(account);
    if (!priv->children)
        return 1;

    for (node = priv->children; node; node = g_list_next(node))
    {
        child_depth = gnc_account_get_tree_depth(static_cast<Account const *>(node->data));
        depth = MAX(depth, child_depth);
    }
    return depth + 1;
}

GList *
gnc_account_get_descendants (const Account *account)
{
    AccountPrivate *priv;
    GList *child, *descendants;

    g_return_val_if_fail(GNC_IS_ACCOUNT(account), NULL);

    priv = GET_PRIVATE(account);
    if (!priv->children)
        return NULL;

    descendants = NULL;
    for (child = priv->children; child; child = g_list_next(child))
    {
        descendants = g_list_append(descendants, child->data);
        descendants = g_list_concat(descendants,
                gnc_account_get_descendants(static_cast<Account const *>(child->data)));
    }
    return descendants;
}

GList *
gnc_account_get_descendants_sorted (const Account *account)
{
    AccountPrivate *priv;
    GList *child, *children, *descendants;

    /* errors */
    g_return_val_if_fail(GNC_IS_ACCOUNT(account), NULL);

    /* optimizations */
    priv = GET_PRIVATE(account);
    if (!priv->children)
        return NULL;

    descendants = NULL;
    children = g_list_sort(g_list_copy(priv->children), (GCompareFunc)xaccAccountOrder);
    for (child = children; child; child = g_list_next(child))
    {
        descendants = g_list_append(descendants, child->data);
        descendants = g_list_concat(descendants,
                gnc_account_get_descendants_sorted(static_cast<Account const *>(child->data)));
    }
    g_list_free(children);
    return descendants;
}

Account *
gnc_account_lookup_by_name (const Account *parent, const char * name)
{
    AccountPrivate *cpriv, *ppriv;
    Account *child, *result;
    GList *node;

    g_return_val_if_fail(GNC_IS_ACCOUNT(parent), NULL);
    g_return_val_if_fail(name, NULL);

    /* first, look for accounts hanging off the current node */
    ppriv = GET_PRIVATE(parent);
    for (node = ppriv->children; node; node = node->next)
    {
        child = static_cast<Account*>(node->data);
        cpriv = GET_PRIVATE(child);
        if (g_strcmp0(cpriv->accountName, name) == 0)
            return child;
    }

    /* if we are still here, then we haven't found the account yet.
     * Recursively search each of the child accounts next */
    for (node = ppriv->children; node; node = node->next)
    {
        child = static_cast<Account*>(node->data);
        result = gnc_account_lookup_by_name (child, name);
        if (result)
            return result;
    }

    return NULL;
}

Account *
gnc_account_lookup_by_code (const Account *parent, const char * code)
{
    AccountPrivate *cpriv, *ppriv;
    Account *child, *result;
    GList *node;

    g_return_val_if_fail(GNC_IS_ACCOUNT(parent), NULL);
    g_return_val_if_fail(code, NULL);

    /* first, look for accounts hanging off the current node */
    ppriv = GET_PRIVATE(parent);
    for (node = ppriv->children; node; node = node->next)
    {
        child = static_cast<Account*>(node->data);
        cpriv = GET_PRIVATE(child);
        if (g_strcmp0(cpriv->accountCode, code) == 0)
            return child;
    }

    /* if we are still here, then we haven't found the account yet.
     * Recursively search each of the child accounts next */
    for (node = ppriv->children; node; node = node->next)
    {
        child = static_cast<Account*>(node->data);
        result = gnc_account_lookup_by_code (child, code);
        if (result)
            return result;
    }

    return NULL;
}

/********************************************************************\
 * Fetch an account, given its full name                            *
\********************************************************************/

static Account *
gnc_account_lookup_by_full_name_helper (const Account *parent,
                                        gchar **names)
{
    const AccountPrivate *priv, *ppriv;
    Account *found;
    GList *node;

    g_return_val_if_fail(GNC_IS_ACCOUNT(parent), NULL);
    g_return_val_if_fail(names, NULL);

    /* Look for the first name in the children. */
    ppriv = GET_PRIVATE(parent);
    for (node = ppriv->children; node; node = node->next)
    {
        Account *account = static_cast<Account*>(node->data);

        priv = GET_PRIVATE(account);
        if (g_strcmp0(priv->accountName, names[0]) == 0)
        {
            /* We found an account.  If the next entry is NULL, there is
             * nothing left in the name, so just return the account. */
            if (names[1] == NULL)
                return account;

            /* No children?  We're done. */
            if (!priv->children)
                return NULL;

            /* There's stuff left to search for.  Search recursively. */
            found = gnc_account_lookup_by_full_name_helper(account, &names[1]);
            if (found != NULL)
            {
                return found;
            }
        }
    }

    return NULL;
}


Account *
gnc_account_lookup_by_full_name (const Account *any_acc,
                                 const gchar *name)
{
    const AccountPrivate *rpriv;
    const Account *root;
    Account *found;
    gchar **names;

    g_return_val_if_fail(GNC_IS_ACCOUNT(any_acc), NULL);
    g_return_val_if_fail(name, NULL);

    root = any_acc;
    rpriv = GET_PRIVATE(root);
    while (rpriv->parent)
    {
        root = rpriv->parent;
        rpriv = GET_PRIVATE(root);
    }
    names = g_strsplit(name, gnc_get_account_separator_string(), -1);
    found = gnc_account_lookup_by_full_name_helper(root, names);
    g_strfreev(names);
    return found;
}

void
gnc_account_foreach_child (const Account *acc,
                           AccountCb thunk,
                           gpointer user_data)
{
    const AccountPrivate *priv;
    GList *node;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(thunk);

    priv = GET_PRIVATE(acc);
    for (node = priv->children; node; node = node->next)
    {
        thunk (static_cast<Account*>(node->data), user_data);
    }
}

void
gnc_account_foreach_descendant (const Account *acc,
                                AccountCb thunk,
                                gpointer user_data)
{
    const AccountPrivate *priv;
    GList *node;
    Account *child;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(thunk);

    priv = GET_PRIVATE(acc);
    for (node = priv->children; node; node = node->next)
    {
        child = static_cast<Account*>(node->data);
        thunk(child, user_data);
        gnc_account_foreach_descendant(child, thunk, user_data);
    }
}

gpointer
gnc_account_foreach_descendant_until (const Account *acc,
                                      AccountCb2 thunk,
                                      gpointer user_data)
{
    const AccountPrivate *priv;
    GList *node;
    Account *child;
    gpointer result;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    g_return_val_if_fail(thunk, NULL);

    priv = GET_PRIVATE(acc);
    for (node = priv->children; node; node = node->next)
    {
        child = static_cast<Account*>(node->data);
        result = thunk(child, user_data);
        if (result)
            return(result);

        result = gnc_account_foreach_descendant_until(child, thunk, user_data);
        if (result)
            return(result);
    }

    return NULL;
}


GNCAccountType
xaccAccountGetType (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), ACCT_TYPE_NONE);
    return GET_PRIVATE(acc)->type;
}

static const char*
qofAccountGetTypeString (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return xaccAccountTypeEnumAsString(GET_PRIVATE(acc)->type);
}

static void
qofAccountSetType (Account *acc, const char *type_string)
{
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_return_if_fail(type_string);
    xaccAccountSetType(acc, xaccAccountStringToEnum(type_string));
}

const char *
xaccAccountGetName (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return GET_PRIVATE(acc)->accountName;
}

gchar *
gnc_account_get_full_name(const Account *account)
{
    AccountPrivate *priv;
    const Account *a;
    char *fullname;
    gchar **names;
    int level;

    /* So much for hardening the API. Too many callers to this function don't
     * bother to check if they have a non-NULL pointer before calling. */
    if (NULL == account)
        return g_strdup("");

    /* errors */
    g_return_val_if_fail(GNC_IS_ACCOUNT(account), g_strdup(""));

    /* optimizations */
    priv = GET_PRIVATE(account);
    if (!priv->parent)
        return g_strdup("");

    /* Figure out how much space is needed by counting the nodes up to
     * the root. */
    level = 0;
    for (a = account; a; a = priv->parent)
    {
        priv = GET_PRIVATE(a);
        level++;
    }

    /* Get all the pointers in the right order. The root node "entry"
     * becomes the terminating NULL pointer for the array of strings. */
    names = (gchar **)g_malloc(level * sizeof(gchar *));
    names[--level] = NULL;
    for (a = account; level > 0; a = priv->parent)
    {
        priv = GET_PRIVATE(a);
        names[--level] = priv->accountName;
    }

    /* Build the full name */
    fullname =  g_strjoinv(account_separator, names);
    g_free(names);

    return fullname;
}

const char *
xaccAccountGetCode (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return GET_PRIVATE(acc)->accountCode;
}

const char *
xaccAccountGetDescription (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return GET_PRIVATE(acc)->description;
}

const char *
xaccAccountGetColor (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return get_kvp_string_tag (acc, "color");
}

const char *
xaccAccountGetFilter (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), 0);
    return get_kvp_string_tag (acc, "filter");
}

const char *
xaccAccountGetSortOrder (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), 0);
    return get_kvp_string_tag (acc, "sort-order");
}

gboolean
xaccAccountGetSortReversed (const Account *acc)
{

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    return g_strcmp0 (get_kvp_string_tag (acc, "sort-reversed"), "true") == 0;
}

const char *
xaccAccountGetNotes (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return get_kvp_string_tag (acc, "notes");
}

gnc_commodity *
DxaccAccountGetCurrency (const Account *acc)
{
    GValue v = G_VALUE_INIT;
    const char *s = NULL;
    gnc_commodity_table *table;

    if (!acc) return NULL;
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"old-currency"});
    if (G_VALUE_HOLDS_STRING (&v))
        s = g_value_get_string (&v);
    if (!s) return NULL;

    table = gnc_commodity_table_get_table (qof_instance_get_book(acc));

    return gnc_commodity_table_lookup_unique (table, s);
}

gnc_commodity *
xaccAccountGetCommodity (const Account *acc)
{
    if (!GNC_IS_ACCOUNT(acc))
        return NULL;
    return GET_PRIVATE(acc)->commodity;
}

gnc_commodity * gnc_account_get_currency_or_parent(const Account* account)
{
    gnc_commodity * commodity;
    g_assert(account);

    commodity = xaccAccountGetCommodity (account);
    if (gnc_commodity_is_currency(commodity))
        return commodity;
    else
    {
        const Account *parent_account = account;
        /* Account commodity is not a currency, walk up the tree until
         * we find a parent account that is a currency account and use
         * it's currency.
         */
        do
        {
            parent_account = gnc_account_get_parent (parent_account);
            if (parent_account)
            {
                commodity = xaccAccountGetCommodity (parent_account);
                if (gnc_commodity_is_currency(commodity))
                {
                    return commodity;
                    //break;
                }
            }
        }
        while (parent_account);
    }
    return NULL; // no suitable commodity found.
}

/********************************************************************\
\********************************************************************/
void
gnc_account_set_start_balance (Account *acc, const gnc_numeric start_baln)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    priv->starting_balance = start_baln;
    priv->balance_dirty = TRUE;
}

void
gnc_account_set_start_cleared_balance (Account *acc,
                                       const gnc_numeric start_baln)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    priv->starting_cleared_balance = start_baln;
    priv->balance_dirty = TRUE;
}

void
gnc_account_set_start_reconciled_balance (Account *acc,
        const gnc_numeric start_baln)
{
    AccountPrivate *priv;

    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    priv = GET_PRIVATE(acc);
    priv->starting_reconciled_balance = start_baln;
    priv->balance_dirty = TRUE;
}

gnc_numeric
xaccAccountGetBalance (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());
    return GET_PRIVATE(acc)->balance;
}

gnc_numeric
xaccAccountGetClearedBalance (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());
    return GET_PRIVATE(acc)->cleared_balance;
}

gnc_numeric
xaccAccountGetReconciledBalance (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());
    return GET_PRIVATE(acc)->reconciled_balance;
}

gnc_numeric
xaccAccountGetProjectedMinimumBalance (const Account *acc)
{
    AccountPrivate *priv;
    GList *node;
    time64 today;
    gnc_numeric lowest = gnc_numeric_zero ();
    int seen_a_transaction = 0;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());

    priv = GET_PRIVATE(acc);
    today = gnc_time64_get_today_end();
    for (node = g_list_last(priv->splits); node; node = node->prev)
    {
        Split *split = static_cast<Split*>(node->data);

        if (!seen_a_transaction)
        {
            lowest = xaccSplitGetBalance (split);
            seen_a_transaction = 1;
        }
        else if (gnc_numeric_compare(xaccSplitGetBalance (split), lowest) < 0)
        {
            lowest = xaccSplitGetBalance (split);
        }

        if (xaccTransGetDate (xaccSplitGetParent (split)) <= today)
            return lowest;
    }

    return lowest;
}


/********************************************************************\
\********************************************************************/

gnc_numeric
xaccAccountGetBalanceAsOfDate (Account *acc, time64 date)
{
    /* Ideally this could use xaccAccountForEachSplit, but
     * it doesn't exist yet and I'm uncertain of exactly how
     * it would work at this time, since it differs from
     * xaccAccountForEachTransaction by using gpointer return
     * values rather than gints.
     */
    AccountPrivate *priv;
    GList   *lp;
    gboolean found = FALSE;
    gnc_numeric balance;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());

    xaccAccountSortSplits (acc, TRUE); /* just in case, normally a noop */
    xaccAccountRecomputeBalance (acc); /* just in case, normally a noop */

    priv = GET_PRIVATE(acc);
    balance = priv->balance;

    /* Since transaction post times are stored as a Timespec,
     * convert date into a Timespec as well rather than converting
     * each transaction's Timespec into a time64.
     *
     * FIXME: CAS: I think this comment is a bogus justification for
     * using xaccTransGetDatePostedTS.  There's no benefit to using
     * Timespec when the input argument is time64, and it's hard to
     * imagine that casting long long to long and comparing two longs is
     * worse than comparing two long longs every time.  IMO,
     * xaccAccountGetPresentBalance gets this right, and its algorithm
     * should be used here.
     */

    lp = priv->splits;
    while ( lp && !found )
    {
        time64 trans_time = xaccTransRetDatePosted( xaccSplitGetParent( (Split *)lp->data ));
        if ( trans_time >= date )
            found = TRUE;
        else
            lp = lp->next;
    }

    if ( lp )
    {
        if ( lp->prev )
        {
            /* Since lp is now pointing to a split which was past the reconcile
             * date, get the running balance of the previous split.
             */
            balance = xaccSplitGetBalance( (Split *)lp->prev->data );
        }
        else
        {
            /* AsOf date must be before any entries, return zero. */
            balance = gnc_numeric_zero();
        }
    }

    /* Otherwise there were no splits posted after the given date,
     * so the latest account balance should be good enough.
     */

    return( balance );
}

/*
 * Originally gsr_account_present_balance in gnc-split-reg.c
 *
 * How does this routine compare to xaccAccountGetBalanceAsOfDate just
 * above?  These two routines should eventually be collapsed into one.
 * Perhaps the startup logic from that one, and the logic from this
 * one that walks from the tail of the split list.
 */
gnc_numeric
xaccAccountGetPresentBalance (const Account *acc)
{
    AccountPrivate *priv;
    GList *node;
    time64 today;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());

    priv = GET_PRIVATE(acc);
    today = gnc_time64_get_today_end();
    for (node = g_list_last(priv->splits); node; node = node->prev)
    {
        Split *split = static_cast<Split*>(node->data);

        if (xaccTransGetDate (xaccSplitGetParent (split)) <= today)
            return xaccSplitGetBalance (split);
    }

    return gnc_numeric_zero ();
}


/********************************************************************\
\********************************************************************/
/* XXX TODO: These 'GetBal' routines should be moved to some
 * utility area outside of the core account engine area.
 */

/*
 * Convert a balance from one currency to another.
 */
gnc_numeric
xaccAccountConvertBalanceToCurrency(const Account *acc, /* for book */
                                    gnc_numeric balance,
                                    const gnc_commodity *balance_currency,
                                    const gnc_commodity *new_currency)
{
    QofBook *book;
    GNCPriceDB *pdb;

    if (gnc_numeric_zero_p (balance) ||
            gnc_commodity_equiv (balance_currency, new_currency))
        return balance;

    book = gnc_account_get_book (acc);
    pdb = gnc_pricedb_get_db (book);

    balance = gnc_pricedb_convert_balance_latest_price(
                  pdb, balance, balance_currency, new_currency);

    return balance;
}

/*
 * Convert a balance from one currency to another with price of
 * a given date.
 */
gnc_numeric
xaccAccountConvertBalanceToCurrencyAsOfDate(const Account *acc, /* for book */
        gnc_numeric balance,
        gnc_commodity *balance_currency,
        gnc_commodity *new_currency,
        time64 date)
{
    QofBook *book;
    GNCPriceDB *pdb;
    Timespec ts;

    if (gnc_numeric_zero_p (balance) ||
            gnc_commodity_equiv (balance_currency, new_currency))
        return balance;

    book = gnc_account_get_book (acc);
    pdb = gnc_pricedb_get_db (book);

    balance = gnc_pricedb_convert_balance_nearest_price(
                  pdb, balance, balance_currency, new_currency, date);

    return balance;
}

/*
 * Given an account and a GetBalanceFn pointer, extract the requested
 * balance from the account and then convert it to the desired
 * currency.
 */
static gnc_numeric
xaccAccountGetXxxBalanceInCurrency (const Account *acc,
                                    xaccGetBalanceFn fn,
                                    const gnc_commodity *report_currency)
{
    AccountPrivate *priv;
    gnc_numeric balance;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());
    g_return_val_if_fail(fn, gnc_numeric_zero());
    g_return_val_if_fail(GNC_IS_COMMODITY(report_currency), gnc_numeric_zero());

    priv = GET_PRIVATE(acc);
    balance = fn(acc);
    balance = xaccAccountConvertBalanceToCurrency(acc, balance,
              priv->commodity,
              report_currency);
    return balance;
}

static gnc_numeric
xaccAccountGetXxxBalanceAsOfDateInCurrency(Account *acc, time64 date,
        xaccGetBalanceAsOfDateFn fn,
        const gnc_commodity *report_commodity)
{
    AccountPrivate *priv;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), gnc_numeric_zero());
    g_return_val_if_fail(fn, gnc_numeric_zero());
    g_return_val_if_fail(GNC_IS_COMMODITY(report_commodity), gnc_numeric_zero());

    priv = GET_PRIVATE(acc);
    return xaccAccountConvertBalanceToCurrency(
               acc, fn(acc, date), priv->commodity, report_commodity);
}

/*
 * Data structure used to pass various arguments into the following fn.
 */
typedef struct
{
    const gnc_commodity *currency;
    gnc_numeric balance;
    xaccGetBalanceFn fn;
    xaccGetBalanceAsOfDateFn asOfDateFn;
    time64 date;
} CurrencyBalance;


/*
 * A helper function for iterating over all the accounts in a list or
 * tree.  This function is called once per account, and sums up the
 * values of all these accounts.
 */
static void
xaccAccountBalanceHelper (Account *acc, gpointer data)
{
    CurrencyBalance *cb = static_cast<CurrencyBalance*>(data);
    gnc_numeric balance;

    if (!cb->fn || !cb->currency)
        return;
    balance = xaccAccountGetXxxBalanceInCurrency (acc, cb->fn, cb->currency);
    cb->balance = gnc_numeric_add (cb->balance, balance,
                                   gnc_commodity_get_fraction (cb->currency),
                                   GNC_HOW_RND_ROUND_HALF_UP);
}

static void
xaccAccountBalanceAsOfDateHelper (Account *acc, gpointer data)
{
    CurrencyBalance *cb = static_cast<CurrencyBalance*>(data);
    gnc_numeric balance;

    g_return_if_fail (cb->asOfDateFn && cb->currency);

    balance = xaccAccountGetXxxBalanceAsOfDateInCurrency (
                  acc, cb->date, cb->asOfDateFn, cb->currency);
    cb->balance = gnc_numeric_add (cb->balance, balance,
                                   gnc_commodity_get_fraction (cb->currency),
                                   GNC_HOW_RND_ROUND_HALF_UP);
}



/*
 * Common function that iterates recursively over all accounts below
 * the specified account.  It uses xaccAccountBalanceHelper to sum up
 * the balances of all its children, and uses the specified function
 * 'fn' for extracting the balance.  This function may extract the
 * current value, the reconciled value, etc.
 *
 * If 'report_commodity' is NULL, just use the account's commodity.
 * If 'include_children' is FALSE, this function doesn't recurse at all.
 */
static gnc_numeric
xaccAccountGetXxxBalanceInCurrencyRecursive (const Account *acc,
        xaccGetBalanceFn fn,
        const gnc_commodity *report_commodity,
        gboolean include_children)
{
    gnc_numeric balance;

    if (!acc) return gnc_numeric_zero ();
    if (!report_commodity)
        report_commodity = xaccAccountGetCommodity (acc);
    if (!report_commodity)
        return gnc_numeric_zero();

    balance = xaccAccountGetXxxBalanceInCurrency (acc, fn, report_commodity);

    /* If needed, sum up the children converting to the *requested*
       commodity. */
    if (include_children)
    {
#ifdef _MSC_VER
        /* MSVC compiler: Somehow, the struct initialization containing a
           gnc_numeric doesn't work. As an exception, we hand-initialize
           that member afterwards. */
        CurrencyBalance cb = { report_commodity, { 0 }, fn, NULL, 0 };
        cb.balance = balance;
#else
        CurrencyBalance cb = { report_commodity, balance, fn, NULL, 0 };
#endif

        gnc_account_foreach_descendant (acc, xaccAccountBalanceHelper, &cb);
        balance = cb.balance;
    }

    return balance;
}

static gnc_numeric
xaccAccountGetXxxBalanceAsOfDateInCurrencyRecursive (
    Account *acc, time64 date, xaccGetBalanceAsOfDateFn fn,
    gnc_commodity *report_commodity, gboolean include_children)
{
    gnc_numeric balance;

    g_return_val_if_fail(acc, gnc_numeric_zero());
    if (!report_commodity)
        report_commodity = xaccAccountGetCommodity (acc);
    if (!report_commodity)
        return gnc_numeric_zero();

    balance = xaccAccountGetXxxBalanceAsOfDateInCurrency(
                  acc, date, fn, report_commodity);

    /* If needed, sum up the children converting to the *requested*
       commodity. */
    if (include_children)
    {
#ifdef _MSC_VER
        /* MSVC compiler: Somehow, the struct initialization containing a
           gnc_numeric doesn't work. As an exception, we hand-initialize
           that member afterwards. */
        CurrencyBalance cb = { report_commodity, 0, NULL, fn, date };
        cb.balance = balance;
#else
        CurrencyBalance cb = { report_commodity, balance, NULL, fn, date };
#endif

        gnc_account_foreach_descendant (acc, xaccAccountBalanceAsOfDateHelper, &cb);
        balance = cb.balance;
    }

    return balance;
}

gnc_numeric
xaccAccountGetBalanceInCurrency (const Account *acc,
                                 const gnc_commodity *report_commodity,
                                 gboolean include_children)
{
    gnc_numeric rc;
    rc = xaccAccountGetXxxBalanceInCurrencyRecursive (
             acc, xaccAccountGetBalance, report_commodity, include_children);
    PINFO(" baln=%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, rc.num, rc.denom);
    return rc;
}


gnc_numeric
xaccAccountGetClearedBalanceInCurrency (const Account *acc,
                                        const gnc_commodity *report_commodity,
                                        gboolean include_children)
{
    return xaccAccountGetXxxBalanceInCurrencyRecursive (
               acc, xaccAccountGetClearedBalance, report_commodity,
               include_children);
}

gnc_numeric
xaccAccountGetReconciledBalanceInCurrency (const Account *acc,
        const gnc_commodity *report_commodity,
        gboolean include_children)
{
    return xaccAccountGetXxxBalanceInCurrencyRecursive (
               acc, xaccAccountGetReconciledBalance, report_commodity,
               include_children);
}

gnc_numeric
xaccAccountGetPresentBalanceInCurrency (const Account *acc,
                                        const gnc_commodity *report_commodity,
                                        gboolean include_children)
{
    return xaccAccountGetXxxBalanceInCurrencyRecursive (
               acc, xaccAccountGetPresentBalance, report_commodity,
               include_children);
}

gnc_numeric
xaccAccountGetProjectedMinimumBalanceInCurrency (
    const Account *acc,
    const gnc_commodity *report_commodity,
    gboolean include_children)
{
    return xaccAccountGetXxxBalanceInCurrencyRecursive (
               acc, xaccAccountGetProjectedMinimumBalance, report_commodity,
               include_children);
}

gnc_numeric
xaccAccountGetBalanceAsOfDateInCurrency(
    Account *acc, time64 date, gnc_commodity *report_commodity,
    gboolean include_children)
{
    return xaccAccountGetXxxBalanceAsOfDateInCurrencyRecursive (
               acc, date, xaccAccountGetBalanceAsOfDate, report_commodity,
               include_children);
}

gnc_numeric
xaccAccountGetBalanceChangeForPeriod (Account *acc, time64 t1, time64 t2,
                                      gboolean recurse)
{
    gnc_numeric b1, b2;

    b1 = xaccAccountGetBalanceAsOfDateInCurrency(acc, t1, NULL, recurse);
    b2 = xaccAccountGetBalanceAsOfDateInCurrency(acc, t2, NULL, recurse);
    return gnc_numeric_sub(b2, b1, GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
}


/********************************************************************\
\********************************************************************/

/* THIS API NEEDS TO CHANGE.
 *
 * This code exposes the internal structure of the account object to
 * external callers by returning the actual list used by the object.
 * It should instead return a copy of the split list that the caller
 * is required to free.  That change would provide the freedom of
 * allowing the internal organization to change data structures if
 * necessary for whatever reason, while leaving the external API
 * unchanged. */
/* XXX: violates the const'ness by forcing a sort before returning
 * the splitlist */
SplitList *
xaccAccountGetSplitList (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    xaccAccountSortSplits((Account*)acc, FALSE);  // normally a noop
    return GET_PRIVATE(acc)->splits;
}

gint64
xaccAccountCountSplits (const Account *acc, gboolean include_children)
{
    gint64 nr, i;

    nr = 0;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), 0);

    nr = g_list_length(xaccAccountGetSplitList(acc));
    if (include_children && (gnc_account_n_children(acc) != 0))
    {
        for (i=0; i < gnc_account_n_children(acc); i++)
        {
            nr += xaccAccountCountSplits(gnc_account_nth_child(acc, i), TRUE);
        }
    }
    return nr;
}

LotList *
xaccAccountGetLotList (const Account *acc)
{
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    return g_list_copy(GET_PRIVATE(acc)->lots);
}

LotList *
xaccAccountFindOpenLots (const Account *acc,
                         gboolean (*match_func)(GNCLot *lot,
                                 gpointer user_data),
                         gpointer user_data, GCompareFunc sort_func)
{
    AccountPrivate *priv;
    GList *lot_list;
    GList *retval = NULL;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);

    priv = GET_PRIVATE(acc);
    for (lot_list = priv->lots; lot_list; lot_list = lot_list->next)
    {
        GNCLot *lot = static_cast<GNCLot*>(lot_list->data);

        /* If this lot is closed, then ignore it */
        if (gnc_lot_is_closed (lot))
            continue;

        if (match_func && !(match_func)(lot, user_data))
            continue;

        /* Ok, this is a valid lot.  Add it to our list of lots */
        if (sort_func)
            retval = g_list_insert_sorted (retval, lot, sort_func);
        else
            retval = g_list_prepend (retval, lot);
    }

    return retval;
}

gpointer
xaccAccountForEachLot(const Account *acc,
                      gpointer (*proc)(GNCLot *lot, void *data), void *data)
{
    AccountPrivate *priv;
    LotList *node;
    gpointer result = NULL;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), NULL);
    g_return_val_if_fail(proc, NULL);

    priv = GET_PRIVATE(acc);
    for (node = priv->lots; node; node = node->next)
        if ((result = proc((GNCLot *)node->data, data)))
            break;

    return result;
}

static void
set_boolean_key (Account *acc, std::vector<std::string> const & path, gboolean option)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v, G_TYPE_BOOLEAN);
    g_value_set_boolean (&v, option);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, path);
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

static gboolean
boolean_from_key (const Account *acc, std::vector<std::string> const & path)
{
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, path);
    if (G_VALUE_HOLDS_INT64 (&v))
        return g_value_get_int64 (&v) != 0;
    if (G_VALUE_HOLDS_BOOLEAN (&v))
         return g_value_get_boolean (&v);
    if (G_VALUE_HOLDS_STRING (&v))
         return strcmp (g_value_get_string (&v), "true") == 0;
    return FALSE;
}

/********************************************************************\
\********************************************************************/

/* These functions use interchange gint64 and gboolean.  Is that right? */
gboolean
xaccAccountGetTaxRelated (const Account *acc)
{
    return boolean_from_key(acc, {"tax-related"});
}

void
xaccAccountSetTaxRelated (Account *acc, gboolean tax_related)
{
    set_boolean_key(acc, {"tax-related"}, tax_related);
}

const char *
xaccAccountGetTaxUSCode (const Account *acc)
{
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"tax-US", "code"});
    return G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : NULL;
}

void
xaccAccountSetTaxUSCode (Account *acc, const char *code)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v, G_TYPE_STRING);
    g_value_set_string (&v, code);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"tax-US", "code"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

const char *
xaccAccountGetTaxUSPayerNameSource (const Account *acc)
{
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"tax-US", "payer-name-source"});
    return G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : NULL;
 }

void
xaccAccountSetTaxUSPayerNameSource (Account *acc, const char *source)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v, G_TYPE_STRING);
    g_value_set_string (&v, source);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"tax-US", "payer-name-source"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

gint64
xaccAccountGetTaxUSCopyNumber (const Account *acc)
{
    gint64 copy_number = 0;
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"tax-US", "copy-number"});
    if (G_VALUE_HOLDS_INT64 (&v))
        copy_number = g_value_get_int64 (&v);

    return (copy_number == 0) ? 1 : copy_number;
}

void
xaccAccountSetTaxUSCopyNumber (Account *acc, gint64 copy_number)
{
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    xaccAccountBeginEdit (acc);
    if (copy_number != 0)
    {
        GValue v = G_VALUE_INIT;
        g_value_init (&v, G_TYPE_INT64);
        g_value_set_int64 (&v, copy_number);
        qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"tax-US", "copy-number"});
    }
    else
    {
        qof_instance_set_path_kvp (QOF_INSTANCE (acc), nullptr, {"tax-US", "copy-number"});
    }
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetPlaceholder (const Account *acc)
{
    return boolean_from_key(acc, {"placeholder"});
}

void
xaccAccountSetPlaceholder (Account *acc, gboolean val)
{
    set_boolean_key(acc, {"placeholder"}, val);
}

GNCPlaceholderType
xaccAccountGetDescendantPlaceholder (const Account *acc)
{
    GList *descendants, *node;
    GNCPlaceholderType ret = PLACEHOLDER_NONE;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), PLACEHOLDER_NONE);
    if (xaccAccountGetPlaceholder(acc)) return PLACEHOLDER_THIS;

    descendants = gnc_account_get_descendants(acc);
    for (node = descendants; node; node = node->next)
        if (xaccAccountGetPlaceholder((Account *) node->data))
        {
            ret = PLACEHOLDER_CHILD;
            break;
        }

    g_list_free(descendants);
    return ret;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetHidden (const Account *acc)
{
    return boolean_from_key (acc, {"hidden"});
}

void
xaccAccountSetHidden (Account *acc, gboolean val)
{
    set_boolean_key (acc, {"hidden"}, val);
}

gboolean
xaccAccountIsHidden (const Account *acc)
{
    AccountPrivate *priv;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);

    if (xaccAccountGetHidden(acc))
        return TRUE;
    priv = GET_PRIVATE(acc);
    while ((acc = priv->parent) != NULL)
    {
        priv = GET_PRIVATE(acc);
        if (xaccAccountGetHidden(acc))
            return TRUE;
    }
    return FALSE;
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountHasAncestor (const Account *acc, const Account * ancestor)
{
    const Account *parent;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    g_return_val_if_fail(GNC_IS_ACCOUNT(ancestor), FALSE);

    parent = acc;
    while (parent && parent != ancestor)
        parent = GET_PRIVATE(parent)->parent;

    return (parent == ancestor);
}

/********************************************************************\
\********************************************************************/

/* You must edit the functions in this block in tandem.  KEEP THEM IN
   SYNC! */

#define GNC_RETURN_ENUM_AS_STRING(x) case (ACCT_TYPE_ ## x): return #x;

const char *
xaccAccountTypeEnumAsString(GNCAccountType type)
{
    switch (type)
    {
        GNC_RETURN_ENUM_AS_STRING(NONE);
        GNC_RETURN_ENUM_AS_STRING(BANK);
        GNC_RETURN_ENUM_AS_STRING(CASH);
        GNC_RETURN_ENUM_AS_STRING(CREDIT);
        GNC_RETURN_ENUM_AS_STRING(ASSET);
        GNC_RETURN_ENUM_AS_STRING(LIABILITY);
        GNC_RETURN_ENUM_AS_STRING(STOCK);
        GNC_RETURN_ENUM_AS_STRING(MUTUAL);
        GNC_RETURN_ENUM_AS_STRING(CURRENCY);
        GNC_RETURN_ENUM_AS_STRING(INCOME);
        GNC_RETURN_ENUM_AS_STRING(EXPENSE);
        GNC_RETURN_ENUM_AS_STRING(EQUITY);
        GNC_RETURN_ENUM_AS_STRING(RECEIVABLE);
        GNC_RETURN_ENUM_AS_STRING(PAYABLE);
        GNC_RETURN_ENUM_AS_STRING(ROOT);
        GNC_RETURN_ENUM_AS_STRING(TRADING);
        GNC_RETURN_ENUM_AS_STRING(CHECKING);
        GNC_RETURN_ENUM_AS_STRING(SAVINGS);
        GNC_RETURN_ENUM_AS_STRING(MONEYMRKT);
        GNC_RETURN_ENUM_AS_STRING(CREDITLINE);
    default:
        PERR ("asked to translate unknown account type %d.\n", type);
        break;
    }
    return(NULL);
}

#undef GNC_RETURN_ENUM_AS_STRING

#define GNC_RETURN_ON_MATCH(x) \
  if(g_strcmp0(#x, (str)) == 0) { *type = ACCT_TYPE_ ## x; return(TRUE); }

gboolean
xaccAccountStringToType(const char* str, GNCAccountType *type)
{

    GNC_RETURN_ON_MATCH(NONE);
    GNC_RETURN_ON_MATCH(BANK);
    GNC_RETURN_ON_MATCH(CASH);
    GNC_RETURN_ON_MATCH(CREDIT);
    GNC_RETURN_ON_MATCH(ASSET);
    GNC_RETURN_ON_MATCH(LIABILITY);
    GNC_RETURN_ON_MATCH(STOCK);
    GNC_RETURN_ON_MATCH(MUTUAL);
    GNC_RETURN_ON_MATCH(CURRENCY);
    GNC_RETURN_ON_MATCH(INCOME);
    GNC_RETURN_ON_MATCH(EXPENSE);
    GNC_RETURN_ON_MATCH(EQUITY);
    GNC_RETURN_ON_MATCH(RECEIVABLE);
    GNC_RETURN_ON_MATCH(PAYABLE);
    GNC_RETURN_ON_MATCH(ROOT);
    GNC_RETURN_ON_MATCH(TRADING);
    GNC_RETURN_ON_MATCH(CHECKING);
    GNC_RETURN_ON_MATCH(SAVINGS);
    GNC_RETURN_ON_MATCH(MONEYMRKT);
    GNC_RETURN_ON_MATCH(CREDITLINE);

    PERR("asked to translate unknown account type string %s.\n",
         str ? str : "(null)");

    return(FALSE);
}

#undef GNC_RETURN_ON_MATCH

/* impedance mismatch is a source of loss */
GNCAccountType
xaccAccountStringToEnum(const char* str)
{
    GNCAccountType type;
    gboolean rc;
    rc = xaccAccountStringToType(str, &type);
    if (FALSE == rc) return ACCT_TYPE_INVALID;
    return type;
}

/********************************************************************\
\********************************************************************/

static char const *
account_type_name[NUM_ACCOUNT_TYPES] =
{
    N_("Bank"),
    N_("Cash"),
    N_("Asset"),
    N_("Credit Card"),
    N_("Liability"),
    N_("Stock"),
    N_("Mutual Fund"),
    N_("Currency"),
    N_("Income"),
    N_("Expense"),
    N_("Equity"),
    N_("A/Receivable"),
    N_("A/Payable"),
    N_("Root"),
    N_("Trading")
    /*
      N_("Checking"),
      N_("Savings"),
      N_("Money Market"),
      N_("Credit Line")
    */
};

const char *
xaccAccountGetTypeStr(GNCAccountType type)
{
    if (type < 0 || NUM_ACCOUNT_TYPES <= type ) return "";
    return _(account_type_name [type]);
}

/********************************************************************\
\********************************************************************/

guint32
xaccAccountTypesCompatibleWith (GNCAccountType type)
{
    switch (type)
    {
        case ACCT_TYPE_BANK:
        case ACCT_TYPE_CASH:
        case ACCT_TYPE_ASSET:
        case ACCT_TYPE_CREDIT:
        case ACCT_TYPE_LIABILITY:
        case ACCT_TYPE_INCOME:
        case ACCT_TYPE_EXPENSE:
        case ACCT_TYPE_EQUITY:
            return
            (1 << ACCT_TYPE_BANK)       |
            (1 << ACCT_TYPE_CASH)       |
            (1 << ACCT_TYPE_ASSET)      |
            (1 << ACCT_TYPE_CREDIT)     |
            (1 << ACCT_TYPE_LIABILITY)  |
            (1 << ACCT_TYPE_INCOME)     |
            (1 << ACCT_TYPE_EXPENSE)    |
            (1 << ACCT_TYPE_EQUITY);
        case ACCT_TYPE_STOCK:
        case ACCT_TYPE_MUTUAL:
        case ACCT_TYPE_CURRENCY:
            return
            (1 << ACCT_TYPE_STOCK)      |
            (1 << ACCT_TYPE_MUTUAL)     |
            (1 << ACCT_TYPE_CURRENCY);
        case ACCT_TYPE_RECEIVABLE:
            return (1 << ACCT_TYPE_RECEIVABLE);
        case ACCT_TYPE_PAYABLE:
            return (1 << ACCT_TYPE_PAYABLE);
        case ACCT_TYPE_TRADING:
            return (1 << ACCT_TYPE_TRADING);
        default:
            PERR("bad account type: %d", type);
            return 0;
    }
}
guint32
xaccParentAccountTypesCompatibleWith (GNCAccountType type)
{
    switch (type)
    {
    case ACCT_TYPE_BANK:
    case ACCT_TYPE_CASH:
    case ACCT_TYPE_ASSET:
    case ACCT_TYPE_STOCK:
    case ACCT_TYPE_MUTUAL:
    case ACCT_TYPE_CURRENCY:
    case ACCT_TYPE_CREDIT:
    case ACCT_TYPE_LIABILITY:
    case ACCT_TYPE_RECEIVABLE:
    case ACCT_TYPE_PAYABLE:
        return
            (1 << ACCT_TYPE_BANK)       |
            (1 << ACCT_TYPE_CASH)       |
            (1 << ACCT_TYPE_ASSET)      |
            (1 << ACCT_TYPE_STOCK)      |
            (1 << ACCT_TYPE_MUTUAL)     |
            (1 << ACCT_TYPE_CURRENCY)   |
            (1 << ACCT_TYPE_CREDIT)     |
            (1 << ACCT_TYPE_LIABILITY)  |
            (1 << ACCT_TYPE_RECEIVABLE) |
            (1 << ACCT_TYPE_PAYABLE)    |
            (1 << ACCT_TYPE_ROOT);
    case ACCT_TYPE_INCOME:
    case ACCT_TYPE_EXPENSE:
        return
            (1 << ACCT_TYPE_INCOME)     |
            (1 << ACCT_TYPE_EXPENSE)    |
            (1 << ACCT_TYPE_ROOT);
    case ACCT_TYPE_EQUITY:
        return
            (1 << ACCT_TYPE_EQUITY)     |
            (1 << ACCT_TYPE_ROOT);
    case ACCT_TYPE_TRADING:
        return
            (1 << ACCT_TYPE_TRADING)    |
            (1 << ACCT_TYPE_ROOT);
    default:
        PERR("bad account type: %d", type);
        return 0;
    }
}

gboolean
xaccAccountTypesCompatible (GNCAccountType parent_type,
                            GNCAccountType child_type)
{
    return ((xaccParentAccountTypesCompatibleWith (parent_type) &
             (1 << child_type))
            != 0);
}

guint32
xaccAccountTypesValid(void)
{
    guint32 mask = (1 << NUM_ACCOUNT_TYPES) - 1;
    mask &= ~((1 << ACCT_TYPE_CURRENCY) |  /* DEPRECATED */
              (1 << ACCT_TYPE_ROOT));      /* ROOT */

    return mask;
}

gboolean xaccAccountIsAssetLiabType(GNCAccountType t)
{
    switch (t)
    {
    case ACCT_TYPE_RECEIVABLE:
    case ACCT_TYPE_PAYABLE:
        return FALSE;
    default:
        return (xaccAccountTypesCompatible(ACCT_TYPE_ASSET, t)
                || xaccAccountTypesCompatible(ACCT_TYPE_LIABILITY, t));
    }
}

gboolean xaccAccountIsAPARType(GNCAccountType t)
{
    switch (t)
    {
    case ACCT_TYPE_RECEIVABLE:
    case ACCT_TYPE_PAYABLE:
        return TRUE;
    default:
        return FALSE;
    }
}

gboolean xaccAccountIsEquityType(GNCAccountType t)
{
    switch (t)
    {
    case ACCT_TYPE_EQUITY:
        return TRUE;
    default:
        return FALSE;
    }
}

gboolean
xaccAccountIsPriced(const Account *acc)
{
    AccountPrivate *priv;

    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);

    priv = GET_PRIVATE(acc);
    return (priv->type == ACCT_TYPE_STOCK || priv->type == ACCT_TYPE_MUTUAL ||
            priv->type == ACCT_TYPE_CURRENCY);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcileLastDate (const Account *acc, time64 *last_date)
{
    gint64 date = 0;
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"reconcile-info", "last-date"});
    if (G_VALUE_HOLDS_INT64 (&v))
        date = g_value_get_int64 (&v);

    if (date)
    {
        if (last_date)
            *last_date = date;
        return TRUE;
    }
    return FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileLastDate (Account *acc, time64 last_date)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v, G_TYPE_INT64);
    g_value_set_int64 (&v, last_date);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"reconcile-info", "last-date"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcileLastInterval (const Account *acc,
                                     int *months, int *days)
{
    GValue v1 = G_VALUE_INIT, v2 = G_VALUE_INIT;
    int64_t m = 0, d = 0;

    if (!acc) return FALSE;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v1,
            {"reconcile-info", "last-interval", "months"});
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v2,
            {"reconcile-info", "last-interval", "days"});
    if (G_VALUE_HOLDS_INT64 (&v1))
        m = g_value_get_int64 (&v1);
    if (G_VALUE_HOLDS_INT64 (&v2))
        d = g_value_get_int64 (&v2);
    if (m && d)
    {
        if (months)
            *months = m;
        if (days)
            *days = d;
        return TRUE;
    }
    return FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileLastInterval (Account *acc, int months, int days)
{
    GValue v1 = G_VALUE_INIT, v2 = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v1, G_TYPE_INT64);
    g_value_set_int64 (&v1, months);
    g_value_init (&v2, G_TYPE_INT64);
    g_value_set_int64 (&v2, days);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v1,
            {"reconcile-info", "last-interval", "months"});
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v2,
            {"reconcile-info", "last-interval", "days"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcilePostponeDate (const Account *acc, time64 *postpone_date)
{
    gint64 date = 0;
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v,
            {"reconcile-info", "postpone", "date"});
    if (G_VALUE_HOLDS_INT64 (&v))
        date = g_value_get_int64 (&v);

    if (date)
    {
        if (postpone_date)
            *postpone_date = date;
        return TRUE;
    }
    return FALSE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcilePostponeDate (Account *acc, time64 postpone_date)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v, G_TYPE_INT64);
    g_value_set_int64 (&v, postpone_date);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v,
            {"reconcile-info", "postpone", "date"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcilePostponeBalance (const Account *acc,
                                        gnc_numeric *balance)
{
    gnc_numeric bal = gnc_numeric_zero ();
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v,
            {"reconcile-info", "postpone", "balance"});
    if (!G_VALUE_HOLDS_INT64 (&v))
        return FALSE;

    bal = *(gnc_numeric*)g_value_get_boxed (&v);
    if (!bal.denom)
        return FALSE;

    if (balance)
        *balance = bal;

    return TRUE;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcilePostponeBalance (Account *acc, gnc_numeric balance)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));

    g_value_init (&v, GNC_TYPE_NUMERIC);
    g_value_set_boxed (&v, &balance);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v,
            {"reconcile-info", "postpone", "balance"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\

\********************************************************************/

void
xaccAccountClearReconcilePostpone (Account *acc)
{
    if (!acc) return;

    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE(acc), nullptr, {"reconcile-info", "postpone"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

/* xaccAccountGetAutoInterestXfer: determine whether the auto interest
 * xfer option is enabled for this account, and return that value.
 * If it is not defined for the account, return the default value.
 */
gboolean
xaccAccountGetAutoInterestXfer (const Account *acc, gboolean default_value)
{
    return boolean_from_key (acc, {"reconcile-info", "auto-interest-transfer"});
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetAutoInterestXfer (Account *acc, gboolean option)
{
    set_boolean_key (acc, {"reconcile-info", "auto-interest-transfer"}, option);
}

/********************************************************************\
\********************************************************************/

const char *
xaccAccountGetLastNum (const Account *acc)
{
    GValue v = G_VALUE_INIT;
    g_return_val_if_fail(GNC_IS_ACCOUNT(acc), FALSE);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"last-num"});
    return G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetLastNum (Account *acc, const char *num)
{
    GValue v = G_VALUE_INIT;
    g_return_if_fail(GNC_IS_ACCOUNT(acc));
    g_value_init (&v, G_TYPE_STRING);

    g_value_set_string (&v, num);
    xaccAccountBeginEdit (acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"last-num"});
    mark_account (acc);
    xaccAccountCommitEdit (acc);
}

static Account *
GetOrMakeOrphanAccount (Account *root, gnc_commodity * currency)
{
    char * accname;
    Account * acc;

    g_return_val_if_fail (root, NULL);

    /* build the account name */
    if (!currency)
    {
        PERR ("No currency specified!");
        return NULL;
    }

    accname = g_strconcat (_("Orphaned Gains"), "-",
                           gnc_commodity_get_mnemonic (currency), NULL);

    /* See if we've got one of these going already ... */
    acc = gnc_account_lookup_by_name(root, accname);

    if (acc == NULL)
    {
        /* Guess not. We'll have to build one. */
        acc = xaccMallocAccount (gnc_account_get_book(root));
        xaccAccountBeginEdit (acc);
        xaccAccountSetName (acc, accname);
        xaccAccountSetCommodity (acc, currency);
        xaccAccountSetType (acc, ACCT_TYPE_INCOME);
        xaccAccountSetDescription (acc, _("Realized Gain/Loss"));
        xaccAccountSetNotes (acc,
                             _("Realized Gains or Losses from "
                               "Commodity or Trading Accounts "
                               "that haven't been recorded elsewhere."));

        /* Hang the account off the root. */
        gnc_account_append_child (root, acc);
        xaccAccountCommitEdit (acc);
    }

    g_free (accname);

    return acc;
}

Account *
xaccAccountGainsAccount (Account *acc, gnc_commodity *curr)
{
    GValue v = G_VALUE_INIT;
    std::vector<std::string> path {"lot-mgmt", "gains-acct",
        gnc_commodity_get_unique_name (curr)};
    GncGUID *guid = NULL;
    Account *gains_account;

    g_return_val_if_fail (acc != NULL, NULL);
    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, path);
    if (G_VALUE_HOLDS_BOXED (&v))
        guid = (GncGUID*)g_value_get_boxed (&v);
    if (guid == NULL) /* No gains account for this currency */
    {
        gains_account = GetOrMakeOrphanAccount (gnc_account_get_root (acc),
                                                curr);
        guid = (GncGUID*)qof_instance_get_guid (QOF_INSTANCE (gains_account));
        xaccAccountBeginEdit (acc);
        {
             GValue vr = G_VALUE_INIT;
             g_value_init (&vr, GNC_TYPE_GUID);
             g_value_set_boxed (&vr, guid);
             qof_instance_set_path_kvp (QOF_INSTANCE (acc), &vr, path);
             qof_instance_set_dirty (QOF_INSTANCE (acc));
        }
        xaccAccountCommitEdit (acc);
    }
    else
        gains_account = xaccAccountLookup (guid,
                                           qof_instance_get_book(acc));

    return gains_account;
}

/********************************************************************\
\********************************************************************/

void
dxaccAccountSetPriceSrc(Account *acc, const char *src)
{
    if (!acc) return;

    if (xaccAccountIsPriced(acc))
    {
        xaccAccountBeginEdit(acc);
        if (src)
        {
            GValue v = G_VALUE_INIT;
            g_value_init (&v, G_TYPE_STRING);
            g_value_set_string (&v, src);
            qof_instance_set_path_kvp (QOF_INSTANCE(acc), &v, {"old-price-source"});
        }
        else
            qof_instance_set_path_kvp (QOF_INSTANCE(acc), nullptr, {"old-price-source"});

        mark_account (acc);
        xaccAccountCommitEdit(acc);
    }
}

/********************************************************************\
\********************************************************************/

const char*
dxaccAccountGetPriceSrc(const Account *acc)
{
    GValue v = G_VALUE_INIT;
    if (!acc) return NULL;

    if (!xaccAccountIsPriced(acc)) return NULL;

    qof_instance_get_path_kvp (QOF_INSTANCE(acc), &v, {"old-price-source"});
    return G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : NULL;
}

/********************************************************************\
\********************************************************************/

void
dxaccAccountSetQuoteTZ(Account *acc, const char *tz)
{
    GValue v = G_VALUE_INIT;
    if (!acc) return;
    if (!xaccAccountIsPriced(acc)) return;
    xaccAccountBeginEdit(acc);
    g_value_init (&v, G_TYPE_STRING);
    g_value_set_string (&v, tz);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v, {"old-quote-tz"});
    mark_account (acc);
    xaccAccountCommitEdit(acc);
}

/********************************************************************\
\********************************************************************/

const char*
dxaccAccountGetQuoteTZ(const Account *acc)
{
    GValue v = G_VALUE_INIT;
    if (!acc) return NULL;
    if (!xaccAccountIsPriced(acc)) return NULL;
    qof_instance_get_path_kvp (QOF_INSTANCE (acc), &v, {"old-quote-tz"});
    return G_VALUE_HOLDS_STRING (&v) ? g_value_get_string (&v) : NULL;
}

/********************************************************************\
\********************************************************************/

void
xaccAccountSetReconcileChildrenStatus(Account *acc, gboolean status)
{
    GValue v = G_VALUE_INIT;
    if (!acc) return;

    xaccAccountBeginEdit (acc);
    /* Would have been nice to use G_TYPE_BOOLEAN, but the other
     * boolean kvps save the value as "true" or "false" and that would
     * be file-incompatible with this.
     */
    g_value_init (&v, G_TYPE_INT64);
    g_value_set_int64 (&v, status);
    qof_instance_set_path_kvp (QOF_INSTANCE (acc), &v,
            {"reconcile-info", "include-children"});
    mark_account(acc);
    xaccAccountCommitEdit (acc);
}

/********************************************************************\
\********************************************************************/

gboolean
xaccAccountGetReconcileChildrenStatus(const Account *acc)
{
    /* access the account's kvp-data for status and return that, if no value
     * is found then we can assume not to include the children, that being
     * the default behaviour
     */
    GValue v = G_VALUE_INIT;
    if (!acc) return FALSE;
    qof_instance_get_path_kvp (QOF_INSTANCE (acc), &v,
            {"reconcile-info", "include-children"});
    return G_VALUE_HOLDS_INT64 (&v) ? g_value_get_int64 (&v) : FALSE;
}

/********************************************************************\
\********************************************************************/

/* The caller of this function can get back one or both of the
 * matching split and transaction pointers, depending on whether
 * a valid pointer to the location to store those pointers is
 * passed.
 */
static void
finder_help_function(const Account *acc, const char *description,
                     Split **split, Transaction **trans )
{
    AccountPrivate *priv;
    GList *slp;

    /* First, make sure we set the data to NULL BEFORE we start */
    if (split) *split = NULL;
    if (trans) *trans = NULL;

    /* Then see if we have any work to do */
    if (acc == NULL) return;

    /* Why is this loop iterated backwards ?? Presumably because the split
     * list is in date order, and the most recent matches should be
     * returned!?  */
    priv = GET_PRIVATE(acc);
    for (slp = g_list_last(priv->splits); slp; slp = slp->prev)
    {
        Split *lsplit = static_cast<Split*>(slp->data);
        Transaction *ltrans = xaccSplitGetParent(lsplit);

        if (g_strcmp0 (description, xaccTransGetDescription (ltrans)) == 0)
        {
            if (split) *split = lsplit;
            if (trans) *trans = ltrans;
            return;
        }
    }
}

Split *
xaccAccountFindSplitByDesc(const Account *acc, const char *description)
{
    Split *split;

    /* Get the split which has a transaction matching the description. */
    finder_help_function(acc, description, &split, NULL);
    return split;
}

/* This routine is for finding a matching transaction in an account by
 * matching on the description field. [CAS: The rest of this comment
 * seems to belong somewhere else.] This routine is used for
 * auto-filling in registers with a default leading account. The
 * dest_trans is a transaction used for currency checking. */
Transaction *
xaccAccountFindTransByDesc(const Account *acc, const char *description)
{
    Transaction *trans;

    /* Get the translation matching the description. */
    finder_help_function(acc, description, NULL, &trans);
    return trans;
}

/* ================================================================ */
/* Concatenation, Merging functions                                */

void
gnc_account_join_children (Account *to_parent, Account *from_parent)
{
    AccountPrivate *from_priv;
    GList *children, *node;

    /* errors */
    g_return_if_fail(GNC_IS_ACCOUNT(to_parent));
    g_return_if_fail(GNC_IS_ACCOUNT(from_parent));

    /* optimizations */
    from_priv = GET_PRIVATE(from_parent);
    if (!from_priv->children)
        return;

    ENTER (" ");
    children = g_list_copy(from_priv->children);
    for (node = children; node; node = g_list_next(node))
        gnc_account_append_child(to_parent, static_cast <Account*> (node->data));
    g_list_free(children);
    LEAVE (" ");
}
/********************************************************************\
\********************************************************************/

void
gnc_account_merge_children (Account *parent)
{
    AccountPrivate *ppriv, *priv_a, *priv_b;
    GList *node_a, *node_b, *work, *worker;

    g_return_if_fail(GNC_IS_ACCOUNT(parent));

    ppriv = GET_PRIVATE(parent);
    for (node_a = ppriv->children; node_a; node_a = node_a->next)
    {
        Account *acc_a = static_cast <Account*> (node_a->data);

        priv_a = GET_PRIVATE(acc_a);
        for (node_b = node_a->next; node_b; node_b = g_list_next(node_b))
        {
            Account *acc_b = static_cast <Account*> (node_b->data);

            priv_b = GET_PRIVATE(acc_b);
            if (0 != null_strcmp(priv_a->accountName, priv_b->accountName))
                continue;
            if (0 != null_strcmp(priv_a->accountCode, priv_b->accountCode))
                continue;
            if (0 != null_strcmp(priv_a->description, priv_b->description))
                continue;
            if (0 != null_strcmp(xaccAccountGetColor(acc_a),
                                 xaccAccountGetColor(acc_b)))
                continue;
            if (!gnc_commodity_equiv(priv_a->commodity, priv_b->commodity))
                continue;
            if (0 != null_strcmp(xaccAccountGetNotes(acc_a),
                                 xaccAccountGetNotes(acc_b)))
                continue;
            if (priv_a->type != priv_b->type)
                continue;

            /* consolidate children */
            if (priv_b->children)
            {
                work = g_list_copy(priv_b->children);
                for (worker = work; worker; worker = g_list_next(worker))
                    gnc_account_append_child (acc_a, (Account *)worker->data);
                g_list_free(work);

                qof_event_gen (&acc_a->inst, QOF_EVENT_MODIFY, NULL);
                qof_event_gen (&acc_b->inst, QOF_EVENT_MODIFY, NULL);
            }

            /* recurse to do the children's children */
            gnc_account_merge_children (acc_a);

            /* consolidate transactions */
            while (priv_b->splits)
                xaccSplitSetAccount (static_cast <Split*> (priv_b->splits->data), acc_a);

            /* move back one before removal. next iteration around the loop
             * will get the node after node_b */
            node_b = g_list_previous(node_b);

            /* The destroy function will remove from list -- node_a is ok,
             * it's before node_b */
            xaccAccountBeginEdit (acc_b);
            xaccAccountDestroy (acc_b);
        }
    }
}

/* ================================================================ */
/* Transaction Traversal functions                                  */


void
xaccSplitsBeginStagedTransactionTraversals (GList *splits)
{
    GList *lp;

    for (lp = splits; lp; lp = lp->next)
    {
        Split *s = static_cast <Split*> (lp->data);
        Transaction *trans = s->parent;

        if (trans)
            trans->marker = 0;
    }
}

/* original function */
void
xaccAccountBeginStagedTransactionTraversals (const Account *account)
{
    AccountPrivate *priv;

    if (!account)
        return;
    priv = GET_PRIVATE(account);
    xaccSplitsBeginStagedTransactionTraversals(priv->splits);
}

gboolean
xaccTransactionTraverse (Transaction *trans, int stage)
{
    if (trans == NULL) return FALSE;

    if (trans->marker < stage)
    {
        trans->marker = stage;
        return TRUE;
    }

    return FALSE;
}

static void do_one_split (Split *s, gpointer data)
{
    Transaction *trans = s->parent;
    trans->marker = 0;
}

static void do_one_account (Account *account, gpointer data)
{
    AccountPrivate *priv = GET_PRIVATE(account);
    g_list_foreach(priv->splits, (GFunc)do_one_split, NULL);
}

/* Replacement for xaccGroupBeginStagedTransactionTraversals */
void
gnc_account_tree_begin_staged_transaction_traversals (Account *account)
{
    GList *descendants;

    descendants = gnc_account_get_descendants(account);
    g_list_foreach(descendants, (GFunc)do_one_account, NULL);
    g_list_free(descendants);
}

int
xaccAccountStagedTransactionTraversal (const Account *acc,
                                       unsigned int stage,
                                       TransactionCallback thunk,
                                       void *cb_data)
{
    AccountPrivate *priv;
    GList *split_p;
    GList *next;
    Transaction *trans;
    Split *s;
    int retval;

    if (!acc) return 0;

    priv = GET_PRIVATE(acc);
    for (split_p = priv->splits; split_p; split_p = next)
    {
        /* Get the next element in the split list now, just in case some
         * naughty thunk destroys the one we're using. This reduces, but
         * does not eliminate, the possibility of undefined results if
         * a thunk removes splits from this account. */
        next = g_list_next(split_p);

        s = static_cast <Split*> (split_p->data);
        trans = s->parent;
        if (trans && (trans->marker < stage))
        {
            trans->marker = stage;
            if (thunk)
            {
                retval = thunk(trans, cb_data);
                if (retval) return retval;
            }
        }
    }

    return 0;
}

int
gnc_account_tree_staged_transaction_traversal (const Account *acc,
        unsigned int stage,
        TransactionCallback thunk,
        void *cb_data)
{
    const AccountPrivate *priv;
    GList *acc_p, *split_p;
    Transaction *trans;
    Split *s;
    int retval;

    if (!acc) return 0;

    /* depth first traversal */
    priv = GET_PRIVATE(acc);
    for (acc_p = priv->children; acc_p; acc_p = g_list_next(acc_p))
    {
        retval = gnc_account_tree_staged_transaction_traversal(static_cast <Account*> (acc_p->data),
                stage, thunk, cb_data);
        if (retval) return retval;
    }

    /* Now this account */
    for (split_p = priv->splits; split_p; split_p = g_list_next(split_p))
    {
        s = static_cast <Split*> (split_p->data);
        trans = s->parent;
        if (trans && (trans->marker < stage))
        {
            trans->marker = stage;
            if (thunk)
            {
                retval = thunk(trans, cb_data);
                if (retval) return retval;
            }
        }
    }

    return 0;
}

/********************************************************************\
\********************************************************************/

int
xaccAccountTreeForEachTransaction (Account *acc,
                                   int (*proc)(Transaction *t, void *data),
                                   void *data)
{
    if (!acc || !proc) return 0;

    gnc_account_tree_begin_staged_transaction_traversals (acc);
    return gnc_account_tree_staged_transaction_traversal (acc, 42, proc, data);
}


gint
xaccAccountForEachTransaction(const Account *acc, TransactionCallback proc,
                              void *data)
{
    if (!acc || !proc) return 0;
    xaccAccountBeginStagedTransactionTraversals (acc);
    return xaccAccountStagedTransactionTraversal(acc, 42, proc, data);
}

/* ================================================================ */
/* The following functions are used by
 * src/import-export/import-backend.c to manipulate the contra-account
 * matching data. See src/import-export/import-backend.c for explanations.
 */

#define IMAP_FRAME              "import-map"
#define IMAP_FRAME_BAYES        "import-map-bayes"

/* Obtain an ImportMatchMap object from an Account or a Book */
GncImportMatchMap *
gnc_account_imap_create_imap (Account *acc)
{
    GncImportMatchMap *imap;

    if (!acc) return NULL;

    imap = g_new0(GncImportMatchMap, 1);

    /* Cache the book for easy lookups; store the account/book for
     * marking dirtiness
     */
    imap->acc = acc;
    imap->book = gnc_account_get_book (acc);

    return imap;
}

/* Look up an Account in the map */
Account*
gnc_account_imap_find_account (GncImportMatchMap *imap,
                               const char *category,
                               const char *key)
{
    GValue v = G_VALUE_INIT;
    GncGUID * guid = NULL;
    if (!imap || !key) return NULL;
    std::vector<std::string> path {IMAP_FRAME};
    if (category)
        path.push_back (category);
    path.push_back (key);
    qof_instance_get_path_kvp (QOF_INSTANCE (imap->acc), &v, path);
    if (G_VALUE_HOLDS_BOXED (&v))
        guid = (GncGUID*)g_value_get_boxed (&v);
    return xaccAccountLookup (guid, imap->book);
}

/* Store an Account in the map */
void
gnc_account_imap_add_account (GncImportMatchMap *imap,
                              const char *category,
                              const char *key,
                              Account *acc)
{
    GValue v = G_VALUE_INIT;
    if (!imap || !key || !acc || (strlen (key) == 0)) return;
    std::vector<std::string> path {IMAP_FRAME};
    if (category)
        path.emplace_back (category);
    path.emplace_back (key);
    g_value_init (&v, GNC_TYPE_GUID);
    g_value_set_boxed (&v, xaccAccountGetGUID (acc));
    xaccAccountBeginEdit (imap->acc);
    qof_instance_set_path_kvp (QOF_INSTANCE (imap->acc), &v, path);
    qof_instance_set_dirty (QOF_INSTANCE (imap->acc));
    xaccAccountCommitEdit (imap->acc);
}

/* Remove a reference to an Account in the map */
void
gnc_account_imap_delete_account (GncImportMatchMap *imap,
                                 const char *category,
                                 const char *key)
{
    if (!imap || !key) return;
    std::vector<std::string> path {IMAP_FRAME};
    if (category)
        path.emplace_back (category);
    path.emplace_back (key);
    xaccAccountBeginEdit (imap->acc);
    if (qof_instance_has_path_slot (QOF_INSTANCE (imap->acc), path))
    {
        qof_instance_slot_path_delete (QOF_INSTANCE (imap->acc), path);
        if (category)
            qof_instance_slot_path_delete_if_empty (QOF_INSTANCE (imap->acc), {IMAP_FRAME, category});
        qof_instance_slot_path_delete_if_empty (QOF_INSTANCE (imap->acc), {IMAP_FRAME});
    }
    qof_instance_set_dirty (QOF_INSTANCE (imap->acc));
    xaccAccountCommitEdit (imap->acc);
}

/*--------------------------------------------------------------------------
 Below here is the bayes transaction to account matching system
--------------------------------------------------------------------------*/


/** intermediate values used to calculate the bayes probability of a given account
  where p(AB) = (a*b)/[a*b + (1-a)(1-b)], product is (a*b),
  product_difference is (1-a) * (1-b)
 */
struct AccountProbability
{
    double product; /* product of probabilities */
    double product_difference; /* product of (1-probabilities) */
};

struct AccountTokenCount
{
    std::string account_guid;
    int64_t token_count; /** occurrences of a given token for this account_guid */
};

/** total_count and the token_count for a given account let us calculate the
 * probability of a given account with any single token
 */
struct TokenAccountsInfo
{
    std::vector<AccountTokenCount> accounts;
    int64_t total_count;
};

/** holds an account guid and its corresponding integer probability
  the integer probability is some factor of 10
 */
struct AccountInfo
{
    std::string account_guid;
    int32_t probability;
};

static void
build_token_info(char const * key, KvpValue * value, TokenAccountsInfo & tokenInfo)
{
    tokenInfo.total_count += value->get<int64_t>();
    AccountTokenCount this_account;
    std::string account_guid {key};
    /*By convention, the key ends with the account GUID.*/
    this_account.account_guid = account_guid.substr(account_guid.size() - GUID_ENCODING_LENGTH);
    this_account.token_count = value->get<int64_t>();
    tokenInfo.accounts.push_back(this_account);
}

/** We scale the probability values by probability_factor.
  ie. with probability_factor of 100000, 10% would be
  0.10 * 100000 = 10000 */
static constexpr int probability_factor = 100000;

static FinalProbabilityVec
build_probabilities(ProbabilityVec const & first_pass)
{
    FinalProbabilityVec ret;
    for (auto const & first_pass_prob : first_pass)
    {
        auto const & account_probability = first_pass_prob.second;
        /* P(AB) = A*B / [A*B + (1-A)*(1-B)]
         * NOTE: so we only keep track of a running product(A*B*C...)
         * and product difference ((1-A)(1-B)...)
         */
        int32_t probability = (account_probability.product /
                (account_probability.product + account_probability.product_difference)) * probability_factor;
        ret.push_back({first_pass_prob.first, probability});
    }
    return ret;
}

static AccountInfo
highest_probability(FinalProbabilityVec const & probabilities)
{
    AccountInfo ret {"", std::numeric_limits<int32_t>::min()};
    for (auto const & prob : probabilities)
        if (prob.second > ret.probability)
            ret = AccountInfo {prob.first, prob.second};
    return ret;
}

static ProbabilityVec
get_first_pass_probabilities(GncImportMatchMap * imap, GList * tokens)
{
    ProbabilityVec ret;
    /* find the probability for each account that contains any of the tokens
     * in the input tokens list. */
    for (auto current_token = tokens; current_token; current_token = current_token->next)
    {
        TokenAccountsInfo tokenInfo{};
        auto path = std::string{IMAP_FRAME_BAYES "/"} + static_cast <char const *> (current_token->data);
        qof_instance_foreach_slot_prefix (QOF_INSTANCE (imap->acc), path, &build_token_info, tokenInfo);
        for (auto const & current_account_token : tokenInfo.accounts)
        {
            auto item = std::find_if(ret.begin(), ret.end(), [&current_account_token]
                (std::pair<std::string, AccountProbability> const & a) {
                    return current_account_token.account_guid == a.first;
                });
            if (item != ret.end())
            {/* This account is already in the map */
                item->second.product = ((double)current_account_token.token_count /
                                      (double)tokenInfo.total_count) * item->second.product;
                item->second.product_difference = ((double)1 - ((double)current_account_token.token_count /
                                              (double)tokenInfo.total_count)) * item->second.product_difference;
            }
            else
            {
                /* add a new entry */
                AccountProbability new_probability;
                new_probability.product = ((double)current_account_token.token_count /
                                      (double)tokenInfo.total_count);
                new_probability.product_difference = 1 - (new_probability.product);
                ret.push_back({current_account_token.account_guid, std::move(new_probability)});
            }
        } /* for all accounts in tokenInfo */
    }
    return ret;
}

static std::string
look_for_old_separator_descendants (Account *root, std::string const & full_name, const gchar *separator)
{
    GList *top_accounts, *ptr;
    gint   found_len = 0;
    gchar  found_sep;
    top_accounts = gnc_account_get_descendants (root);
    PINFO("Incoming full_name is '%s', current separator is '%s'", full_name.c_str (), separator);
    /* Go through list of top level accounts */
    for (ptr = top_accounts; ptr; ptr = g_list_next (ptr))
    {
        const gchar *name = xaccAccountGetName (static_cast <Account const *> (ptr->data));
        // we are looking for the longest top level account that matches
        if (g_str_has_prefix (full_name.c_str (), name))
        {
            gint name_len = strlen (name);
            const gchar old_sep = full_name[name_len];
            if (!g_ascii_isalnum (old_sep)) // test for non alpha numeric
            {
                if (name_len > found_len)
                {
                    found_sep = full_name[name_len];
                    found_len = name_len;
                }
            }
        }
    }
    g_list_free (top_accounts); // Free the List
    std::string new_name {full_name};
    if (found_len > 1)
        std::replace (new_name.begin (), new_name.end (), found_sep, *separator);
    PINFO ("Return full_name is '%s'", new_name.c_str ());
    return new_name;
}

static std::string
get_guid_from_account_name (Account * root, std::string const & name)
{
    auto map_account = gnc_account_lookup_by_full_name (root, name.c_str ());
    if (!map_account)
    {
        auto temp_account_name = look_for_old_separator_descendants (root, name,
             gnc_get_account_separator_string ());
        map_account = gnc_account_lookup_by_full_name (root, temp_account_name.c_str ());
    }
    auto temp_guid = gnc::GUID {*xaccAccountGetGUID (map_account)};
    return temp_guid.to_string ();
}

static FlatKvpEntry
convert_entry (KvpEntry entry, Account* root)
{
    /*We need to make a copy here.*/
    auto account_name = entry.first.back();
    if (!gnc::GUID::is_valid_guid (account_name))
    {
        /* Earlier version stored the account name in the import map, and
         * there were early beta versions of 2.7 that stored a GUID.
         * If there is no GUID, we assume it's an account name. */
        /* Take off the account name and replace it with the GUID */
        entry.first.pop_back();
        auto guid_str = get_guid_from_account_name (root, account_name);
        entry.first.emplace_back (guid_str);
    }
    std::string new_key {std::accumulate (entry.first.begin(), entry.first.end(), std::string {})};
    new_key = IMAP_FRAME_BAYES + new_key;
    return {new_key, entry.second};
}

static std::vector<FlatKvpEntry>
get_new_flat_imap (Account * acc)
{
    auto frame = qof_instance_get_slots (QOF_INSTANCE (acc));
    auto slot = frame->get_slot ({IMAP_FRAME_BAYES});
    if (!slot)
        return {};
    auto imap_frame = slot->get<KvpFrame*> ();
    auto flat_kvp = imap_frame->flatten_kvp ();
    auto root = gnc_account_get_root (acc);
    std::vector <FlatKvpEntry> ret;
    for (auto const & flat_entry : flat_kvp)
    {
        auto converted_entry = convert_entry (flat_entry, root);
        /*If the entry was invalid, we don't perpetuate it.*/
        if (converted_entry.first.size())
            ret.emplace_back (converted_entry);
    }
    return ret;
}

static bool
convert_imap_account_bayes_to_flat (Account *acc)
{
    auto frame = qof_instance_get_slots (QOF_INSTANCE (acc));
    if (!frame->get_keys().size())
        return false;
    auto new_imap = get_new_flat_imap(acc);
    xaccAccountBeginEdit(acc);
    frame->set({IMAP_FRAME_BAYES}, nullptr);
    if (!new_imap.size ())
    {
        xaccAccountCommitEdit(acc);
        return false;
    }
    std::for_each(new_imap.begin(), new_imap.end(), [&frame] (FlatKvpEntry const & entry) {
        frame->set({entry.first.c_str()}, entry.second);
    });
    qof_instance_set_dirty (QOF_INSTANCE (acc));
    xaccAccountCommitEdit(acc);
    return true;
}

/*
 * Checks for import map data and converts them when found.
 */
static bool
imap_convert_bayes_to_flat (QofBook * book)
{
    auto root = gnc_book_get_root_account (book);
    auto accts = gnc_account_get_descendants_sorted (root);
    bool ret = false;
    for (auto ptr = accts; ptr; ptr = g_list_next (ptr))
    {
        Account *acc = static_cast <Account*> (ptr->data);
        if (convert_imap_account_bayes_to_flat (acc))
        {
            ret = true;
            gnc_features_set_used (book, GNC_FEATURE_GUID_FLAT_BAYESIAN);
        }
    }
    g_list_free (accts);
    return ret;
}

/*
 * Here we check to see the state of import map data.
 *
 * If the GUID_FLAT_BAYESIAN feature flag is set, everything
 * should be fine.
 *
 * If it is not set, there are two possibilities: import data
 * are present from a previous version or not. If they are,
 * they are converted, and the feature flag set. If there are
 * no previous data, nothing is done.
 */
static void
check_import_map_data (QofBook *book)
{
    if (gnc_features_check_used (book, GNC_FEATURE_GUID_FLAT_BAYESIAN))
        return;
    /* This function will set GNC_FEATURE_GUID_FLAT_BAYESIAN if necessary.*/
    imap_convert_bayes_to_flat (book);
}

static constexpr double threshold = .90 * probability_factor; /* 90% */

/** Look up an Account in the map */
Account*
gnc_account_imap_find_account_bayes (GncImportMatchMap *imap, GList *tokens)
{
    if (!imap)
        return nullptr;
    check_import_map_data (imap->book);
    auto first_pass = get_first_pass_probabilities(imap, tokens);
    if (!first_pass.size())
        return nullptr;
    auto final_probabilities = build_probabilities(first_pass);
    if (!final_probabilities.size())
        return nullptr;
    auto best = highest_probability(final_probabilities);
    if (best.account_guid == "")
        return nullptr;
    if (best.probability < threshold)
        return nullptr;
    gnc::GUID guid;
    try {
        guid = gnc::GUID::from_string(best.account_guid);
    } catch (gnc::guid_syntax_exception) {
        return nullptr;
    }
    auto account = xaccAccountLookup (reinterpret_cast<GncGUID*>(&guid), imap->book);
    return account;
}

static void
change_imap_entry (GncImportMatchMap *imap, std::string const & path, int64_t token_count)
{
    GValue value = G_VALUE_INIT;

    PINFO("Source Account is '%s', Count is '%" G_GINT64_FORMAT "'",
           xaccAccountGetName (imap->acc), token_count);

    // check for existing guid entry
    if (qof_instance_has_slot (QOF_INSTANCE(imap->acc), path.c_str ()))
    {
        int64_t  existing_token_count = 0;

        // get the existing_token_count value
        qof_instance_get_path_kvp (QOF_INSTANCE (imap->acc), &value, {path});

        if (G_VALUE_HOLDS_INT64 (&value))
            existing_token_count = g_value_get_int64 (&value);

        PINFO("found existing value of '%" G_GINT64_FORMAT "'", existing_token_count);

        token_count = token_count + existing_token_count;
    }

    if (!G_IS_VALUE (&value))
        g_value_init (&value, G_TYPE_INT64);

    g_value_set_int64 (&value, token_count);

    // Add or Update the entry based on guid
    qof_instance_set_path_kvp (QOF_INSTANCE (imap->acc), &value, {path});
    gnc_features_set_used (imap->book, GNC_FEATURE_GUID_FLAT_BAYESIAN);
}

/** Updates the imap for a given account using a list of tokens */
void
gnc_account_imap_add_account_bayes (GncImportMatchMap *imap,
                                    GList *tokens,
                                    Account *acc)
{
    GList *current_token;
    gint64 token_count;
    char *account_fullname;
    char *guid_string;

    ENTER(" ");
    if (!imap)
    {
        LEAVE(" ");
        return;
    }
    check_import_map_data (imap->book);

    g_return_if_fail (acc != NULL);
    account_fullname = gnc_account_get_full_name(acc);
    xaccAccountBeginEdit (imap->acc);

    PINFO("account name: '%s'", account_fullname);

    guid_string = guid_to_string (xaccAccountGetGUID (acc));

    /* process each token in the list */
    for (current_token = g_list_first(tokens); current_token;
            current_token = current_token->next)
    {
        /* Jump to next iteration if the pointer is not valid or if the
                 string is empty. In HBCI import we almost always get an empty
                 string, which doesn't work in the kvp loopkup later. So we
                 skip this case here. */
        if (!current_token->data || (*((char*)current_token->data) == '\0'))
            continue;
        /* start off with one token for this account */
        token_count = 1;
        PINFO("adding token '%s'", (char*)current_token->data);
        auto path = std::string {IMAP_FRAME_BAYES} + '/' + static_cast<char*>(current_token->data) + '/' + guid_string;
        /* change the imap entry for the account */
        change_imap_entry (imap, path, token_count);
    }
    /* free up the account fullname and guid string */
    qof_instance_set_dirty (QOF_INSTANCE (imap->acc));
    xaccAccountCommitEdit (imap->acc);
    g_free (account_fullname);
    g_free (guid_string);
    LEAVE(" ");
}

/*******************************************************************************/

static void
build_non_bayes (const char *key, const GValue *value, gpointer user_data)
{
    if (!G_VALUE_HOLDS_BOXED (value))
        return;
    QofBook     *book;
    GncGUID     *guid = NULL;
    gchar       *kvp_path;
    gchar       *guid_string = NULL;
    auto imapInfo = (GncImapInfo*)user_data;
    // Get the book
    book = qof_instance_get_book (imapInfo->source_account);

    guid = (GncGUID*)g_value_get_boxed (value);
    guid_string = guid_to_string (guid);

    PINFO("build_non_bayes: account '%s', match account guid: '%s'",
                            (char*)key, guid_string);

    kvp_path = g_strconcat (imapInfo->category_head, "/", key, NULL);

    PINFO("build_non_bayes: kvp_path is '%s'", kvp_path);

    auto imapInfo_node = static_cast <GncImapInfo*> (g_malloc(sizeof(GncImapInfo)));

    imapInfo_node->source_account = imapInfo->source_account;
    imapInfo_node->map_account    = xaccAccountLookup (guid, book);
    imapInfo_node->full_category  = g_strdup (kvp_path);
    imapInfo_node->match_string   = g_strdup (key);
    imapInfo_node->category_head  = g_strdup (imapInfo->category_head);
    imapInfo_node->count          = g_strdup (" ");

    imapInfo->list = g_list_append (imapInfo->list, imapInfo_node);

    g_free (kvp_path);
    g_free (guid_string);
}

static std::tuple<std::string, std::string, std::string>
parse_bayes_imap_info (std::string const & imap_bayes_entry)
{
    auto header_length = strlen (IMAP_FRAME_BAYES);
    std::string header {imap_bayes_entry.substr (0, header_length)};
    auto guid_start = imap_bayes_entry.size() - GUID_ENCODING_LENGTH;
    std::string keyword {imap_bayes_entry.substr (header_length + 1, guid_start - header_length - 2)};
    std::string account_guid {imap_bayes_entry.substr (guid_start)};
    return std::tuple <std::string, std::string, std::string> {header, keyword, account_guid};
}

static void
build_bayes (const char *key, KvpValue * value, GncImapInfo & imapInfo)
{
    auto slots = qof_instance_get_slots_prefix (QOF_INSTANCE (imapInfo.source_account), IMAP_FRAME_BAYES);
    if (!slots.size()) return;
    for (auto const & entry : slots)
    {
        auto parsed_key = parse_bayes_imap_info (entry.first);
        auto temp_guid = gnc::GUID::from_string (std::get <2> (parsed_key));
        GncGUID guid = temp_guid;
        auto map_account = xaccAccountLookup (&guid, gnc_account_get_book (imapInfo.source_account));
        std::string category_head {std::get <0> (parsed_key) + "/" + std::get <1> (parsed_key)};
        auto imap_node = static_cast <GncImapInfo*> (g_malloc (sizeof (GncImapInfo)));
        auto count = entry.second->get <int64_t> ();
        imap_node->source_account = imapInfo.source_account;
        imap_node->map_account = map_account;
        imap_node->full_category = g_strdup (key);
        imap_node->match_string = g_strdup (std::get <1> (parsed_key).c_str ());
        imap_node->category_head = g_strdup (category_head.c_str ());
        imap_node->count = g_strdup_printf ("%" G_GINT64_FORMAT, count);
        imapInfo.list = g_list_append (imapInfo.list, imap_node);
    };
}

GList *
gnc_account_imap_get_info_bayes (Account *acc)
{
    check_import_map_data (gnc_account_get_book (acc));
    /* A dummy object which is used to hold the specified account, and the list
     * of data about which we care. */
    GncImapInfo imapInfo {acc, nullptr};
    qof_instance_foreach_slot_prefix (QOF_INSTANCE (acc), IMAP_FRAME_BAYES, &build_bayes, imapInfo);
    return imapInfo.list;
}

GList *
gnc_account_imap_get_info (Account *acc, const char *category)
{
    GList *list = NULL;
    gchar *category_head = NULL;

    GncImapInfo imapInfo;

    imapInfo.source_account = acc;
    imapInfo.list = list;

    category_head = g_strdup_printf (IMAP_FRAME "/%s", category);
    imapInfo.category_head = category_head;

    if (qof_instance_has_slot (QOF_INSTANCE(acc), category_head))
        qof_instance_foreach_slot (QOF_INSTANCE(acc), category_head,
                                   build_non_bayes, &imapInfo);

    g_free (category_head);

    return imapInfo.list;
}

/*******************************************************************************/

gchar *
gnc_account_get_map_entry (Account *acc, const char *full_category)
{
    GValue v = G_VALUE_INIT;
    gchar *text = NULL;
    std::vector<std::string> path {full_category};
    if (qof_instance_has_path_slot (QOF_INSTANCE (acc), path))
    {
        qof_instance_get_path_kvp (QOF_INSTANCE (acc), &v, path);
        if (G_VALUE_HOLDS_STRING (&v))
        {
            gchar const *string;
            string = g_value_get_string (&v);
            text = g_strdup (string);
        }
    }
    return text;
}


void
gnc_account_delete_map_entry (Account *acc, char *full_category, gboolean empty)
{
    gchar *kvp_path = g_strdup (full_category);
    if ((acc != NULL) && qof_instance_has_slot (QOF_INSTANCE(acc), kvp_path))
    {
        xaccAccountBeginEdit (acc);
        if (empty)
            qof_instance_slot_path_delete_if_empty (QOF_INSTANCE(acc), {kvp_path});
        else
            qof_instance_slot_path_delete (QOF_INSTANCE(acc), {kvp_path});
        PINFO("Account is '%s', path is '%s'", xaccAccountGetName (acc), kvp_path);
        qof_instance_set_dirty (QOF_INSTANCE(acc));
        xaccAccountCommitEdit (acc);
    }
    g_free (kvp_path);
    g_free (full_category);
}

/* ================================================================ */
/* QofObject function implementation and registration */

static void
gnc_account_book_end(QofBook* book)
{
    Account *root_account = gnc_book_get_root_account(book);
    xaccAccountBeginEdit(root_account);
    xaccAccountDestroy(root_account);
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject account_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_ACCOUNT,
    DI(.type_label        = ) "Account",
    DI(.create            = ) (void*(*)(QofBook*)) xaccMallocAccount,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_account_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) (const char * (*)(gpointer)) xaccAccountGetName,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean xaccAccountRegister (void)
{
    static QofParam params[] =
    {
        {
            ACCOUNT_NAME_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetName,
            (QofSetterFunc) xaccAccountSetName
        },
        {
            ACCOUNT_CODE_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetCode,
            (QofSetterFunc) xaccAccountSetCode
        },
        {
            ACCOUNT_DESCRIPTION_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetDescription,
            (QofSetterFunc) xaccAccountSetDescription
        },
        {
            ACCOUNT_COLOR_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetColor,
            (QofSetterFunc) xaccAccountSetColor
        },
        {
            ACCOUNT_FILTER_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetFilter,
            (QofSetterFunc) xaccAccountSetFilter
        },
        {
            ACCOUNT_SORT_ORDER_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetSortOrder,
            (QofSetterFunc) xaccAccountSetSortOrder
        },
        {
            ACCOUNT_SORT_REVERSED_, QOF_TYPE_BOOLEAN,
            (QofAccessFunc) xaccAccountGetSortReversed,
            (QofSetterFunc) xaccAccountSetSortReversed
        },
        {
            ACCOUNT_NOTES_, QOF_TYPE_STRING,
            (QofAccessFunc) xaccAccountGetNotes,
            (QofSetterFunc) xaccAccountSetNotes
        },
        {
            ACCOUNT_PRESENT_, QOF_TYPE_NUMERIC,
            (QofAccessFunc) xaccAccountGetPresentBalance, NULL
        },
        {
            ACCOUNT_BALANCE_, QOF_TYPE_NUMERIC,
            (QofAccessFunc) xaccAccountGetBalance, NULL
        },
        {
            ACCOUNT_CLEARED_, QOF_TYPE_NUMERIC,
            (QofAccessFunc) xaccAccountGetClearedBalance, NULL
        },
        {
            ACCOUNT_RECONCILED_, QOF_TYPE_NUMERIC,
            (QofAccessFunc) xaccAccountGetReconciledBalance, NULL
        },
        {
            ACCOUNT_TYPE_, QOF_TYPE_STRING,
            (QofAccessFunc) qofAccountGetTypeString,
            (QofSetterFunc) qofAccountSetType
        },
        {
            ACCOUNT_FUTURE_MINIMUM_, QOF_TYPE_NUMERIC,
            (QofAccessFunc) xaccAccountGetProjectedMinimumBalance, NULL
        },
        {
            ACCOUNT_TAX_RELATED, QOF_TYPE_BOOLEAN,
            (QofAccessFunc) xaccAccountGetTaxRelated,
            (QofSetterFunc) xaccAccountSetTaxRelated
        },
        {
            ACCOUNT_SCU, QOF_TYPE_INT32,
            (QofAccessFunc) xaccAccountGetCommoditySCU,
            (QofSetterFunc) xaccAccountSetCommoditySCU
        },
        {
            ACCOUNT_NSCU, QOF_TYPE_BOOLEAN,
            (QofAccessFunc) xaccAccountGetNonStdSCU,
            (QofSetterFunc) xaccAccountSetNonStdSCU
        },
        {
            ACCOUNT_PARENT, GNC_ID_ACCOUNT,
            (QofAccessFunc) gnc_account_get_parent,
            (QofSetterFunc) qofAccountSetParent
        },
        {
            QOF_PARAM_BOOK, QOF_ID_BOOK,
            (QofAccessFunc) qof_instance_get_book, NULL
        },
        {
            QOF_PARAM_GUID, QOF_TYPE_GUID,
            (QofAccessFunc) qof_instance_get_guid, NULL
        },
        { NULL },
    };

    qof_class_register (GNC_ID_ACCOUNT, (QofSortFunc) qof_xaccAccountOrder, params);

    return qof_object_register (&account_object_def);
}

/* ======================= UNIT TESTING ACCESS =======================
 * The following functions are for unit testing use only.
 */
static AccountPrivate*
utest_account_get_private (Account *acc)
{
    return GET_PRIVATE (acc);
}

AccountTestFunctions*
_utest_account_fill_functions(void)
{
    AccountTestFunctions* func = g_new(AccountTestFunctions, 1);

    func->get_private = utest_account_get_private;
    func->coll_get_root_account = gnc_coll_get_root_account;
    func->xaccFreeAccountChildren = xaccFreeAccountChildren;
    func->xaccFreeAccount = xaccFreeAccount;
    func->qofAccountSetParent = qofAccountSetParent;
    func->gnc_account_lookup_by_full_name_helper =
        gnc_account_lookup_by_full_name_helper;

    return func;
}
/* ======================= END OF FILE =========================== */
