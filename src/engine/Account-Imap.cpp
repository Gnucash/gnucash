/********************************************************************\
 * Account-Imap.cpp -- Account Import Mapping                       *
 * Copyright (C) 2007 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2016 Robert Fewell                                 *
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

extern "C"
{
#include <config.h>
#include "AccountP.h"
#include <qof.h>
#include <qofinstance-p.h>
#include "gnc-features.h"
}

#include "Account-Imap.h"
#include <kvp_frame.hpp>

#include <iostream>
#include <string>
#include <algorithm>
#include <unordered_map>

static QofLogModule log_module = GNC_MOD_ACCOUNT;


/* ================================================================ */
/* The following functions are used by
 * src/import-export/import-backend.c to manipulate the contra-account
 * matching data. See src/import-export/import-backend.c for explanations.
 */

GncImportMatchMap::GncImportMatchMap (Account *acc) :
    m_account {acc},
    m_book {gnc_account_get_book(acc)}
{
    /* Cache the book for easy lookups; store the account/book for
     * marking dirtiness */
}

GncImportMatchMap::~GncImportMatchMap()
{
}

Account *
GncImportMatchMap::get_account()
{
    return m_account;
}

QofBook *
GncImportMatchMap::get_book()
{
    return m_book;
}

Account *
GncImportMatchMap::find_account (const char* category, const char *key)
{
    KvpValue *value;

    if (!key) return nullptr;

    ENTER(" ");

    auto slots = qof_instance_get_slots(QOF_INSTANCE(m_account));

    if (!category)
        value = slots->get_slot({IMAP_FRAME, key});
    else
        value = slots->get_slot({IMAP_FRAME, category, key});

    if (value == nullptr || value->get_type() != KvpValue::Type::GUID)
    {
       LEAVE("No Guid");
       return nullptr;
    }
    LEAVE("Guid found");

    return xaccAccountLookup(value->get<GncGUID*>(), m_book);
}

void
GncImportMatchMap::add_account (const char *category, const char *key, Account *acc)
{
    if (!key || !acc || (strlen (key) == 0)) return;

    ENTER(" ");

    auto acc_slots = qof_instance_get_slots(QOF_INSTANCE(m_account));

    auto acc_val = new KvpValue(const_cast<GncGUID*>(xaccAccountGetGUID(acc)));

    xaccAccountBeginEdit (m_account);

    if (!category)
        acc_slots->set_path({IMAP_FRAME, key}, acc_val);
    else
        acc_slots->set_path({IMAP_FRAME, category, key}, acc_val);

    qof_instance_set_dirty (QOF_INSTANCE (m_account));
    xaccAccountCommitEdit (m_account);

    LEAVE(" ");
}

void
GncImportMatchMap::delete_account (const char *category, const char *key)
{
    std::string kvp_path;
    std::string delim = "/";

    if (!key) return;

    ENTER(" ");

    std::string str_key (key);

    if (!category)
        kvp_path = IMAP_FRAME + delim + str_key;
    else
    {
        std::string str_category (category);
        kvp_path = IMAP_FRAME + delim + str_category + delim + str_key;
    }
    xaccAccountBeginEdit (m_account);

    if (qof_instance_has_slot (QOF_INSTANCE (m_account), kvp_path.c_str()))
    {
        qof_instance_slot_delete (QOF_INSTANCE (m_account), kvp_path.c_str());

        if (category)
        {
            std::string str_category (category);
            kvp_path = IMAP_FRAME + delim + str_category;

            qof_instance_slot_delete_if_empty (QOF_INSTANCE (m_account), kvp_path.c_str());
        }
        qof_instance_slot_delete_if_empty (QOF_INSTANCE (m_account), IMAP_FRAME);
    }
    qof_instance_set_dirty (QOF_INSTANCE (m_account));
    xaccAccountCommitEdit (m_account);

    LEAVE(" ");
}


using AccountTokenCount = std::pair<std::string, int64_t>; // guid, token_count
//using AccountTokenCountPtr = std::shared_ptr<AccountTokenCount>;
//using TokenAccountsInfo = std::pair<std::vector<AccountTokenCountPtr>, int64_t>;

/** total_token_count and the token_count for a given account let us calculate the
 * probability of a given account with any single token
 */
struct TokenAccountsInfo
{
    std::vector<AccountTokenCount> accounts; // vector of AccountTokenCount
    int64_t total_token_count; // the sum of token_count for a given token
};

/** gpointer is a pointer to a struct TokenAccountsInfo
 * \note Can always assume that keys are unique, reduces code in this function
 */
static void
buildTokenInfo(const char *key, const GValue *value, gpointer data)
{
    struct TokenAccountsInfo *tokenInfo = (struct TokenAccountsInfo*)data;

    //  PINFO("buildTokenInfo: account '%s', token_count: '%" G_GINT64_FORMAT "'", (char*)key,
    //                  g_value_get_int64(value));

    /* add the count to the total_token_count */
    tokenInfo->total_token_count += g_value_get_int64(value);

    /* fill in the account guid and number of tokens found for this account */
    AccountTokenCount account_token_count = std::make_pair (key, g_value_get_int64(value));

    /* append onto the vector the new AccountTokenCount pair */
    tokenInfo->accounts.push_back (account_token_count);
}

/** intermediate values used to calculate the bayes probability of a given
    account where p(AB) = (a*b)/[a*b + (1-a)(1-b)], product is (a*b),
    product_difference is (1-a) * (1-b)
 */
class AccountProbability
{
    public:
    double m_product;             /* product of probabilities */
    double m_product_difference;  /* product of (1-probabilities) */

    AccountProbability (double p, double pd)
    {
        m_product = p;
        m_product_difference = pd;
    }
};

/** conversion factors for the unordered map of 100000x the percentage match value,
    ie. 10% would be  0.10 * 100000 = 10000
 */
static const double PROBABILITY_FACTOR = 100000.0;
static const double CONFIDENCE {0.90}; // 90%
static constexpr double threshold {CONFIDENCE * PROBABILITY_FACTOR};

/** Look up an Account in the map */
Account *
GncImportMatchMap::find_account_bayes (GList* tokens)
{
    GList *current_token;               /* pointer to the current token from the input GList tokens */

    struct TokenAccountsInfo tokenInfo; /* holds the accounts and total_token_count for a single token */

    std::string selected_account_guid;
    int32_t max_probability = 0;
    std::unordered_map<std::string, AccountProbability>probability_hash;

    ENTER(" ");

    /* find the probability for each account that contains any of the tokens
     * in the input tokens list
     */
    for (current_token = tokens; current_token; current_token = current_token->next)
    {
        std::string delim = "/";
        std::string kvp_path = IMAP_FRAME_BAYES + delim + ((char*)current_token->data);

        /* Zero out the TokenAccountsInfo structure */
        tokenInfo.total_token_count = 0;
        tokenInfo.accounts.clear();

        PINFO("token: '%s'", (char*)current_token->data);

        /* process the accounts for this token, adding the account if it
         * doesn't already exist or adding to the existing accounts token
         * count if it does  */
        qof_instance_foreach_slot(QOF_INSTANCE (m_account), kvp_path.c_str(),
                                                buildTokenInfo, &tokenInfo);

        /* for each account we have just found, see if the account already
         * exists in the list of account probabilities, if not add it */
        for (auto it = begin (tokenInfo.accounts); it != end (tokenInfo.accounts); ++it)
        {
            PINFO("account_guid('%s'), token_count('%" G_GINT64_FORMAT
                  "')/total_count('%" G_GINT64_FORMAT "')",
                  it->first.c_str(), it->second,
                  tokenInfo.total_token_count);

            auto itr = probability_hash.find (it->first);
            if (itr != probability_hash.end()) // Existing entry
            {
                PINFO("Existing entry for this account");

                itr->second.m_product = (((double)it->second /
                                       (double)tokenInfo.total_token_count)
                                        * itr->second.m_product);

                itr->second.m_product_difference = ((double)1 - ((double)it->second /
                                                  (double)tokenInfo.total_token_count))
                                                   * itr->second.m_product_difference;

                PINFO("product == %f, product_difference == %f",
                      itr->second.m_product, itr->second.m_product_difference);
            }
            else // New entry
            {
                PINFO("Adding a new entry for this account");

                /* set the product and product difference values */
                double product = ((double)it->second /
                                  (double)tokenInfo.total_token_count);

                double product_difference = (double)1 - ((double)it->second /
                                            (double)tokenInfo.total_token_count);

                AccountProbability a_prob({product, product_difference});

                probability_hash.emplace (it->first, a_prob);

                PINFO("product == %f, product_difference == %f",
                      product, product_difference);
            }
        } // for all accounts in tokenInfo
    }

    // Now look for highest probability
    for (auto &itr : probability_hash)
    {
        /* P(AB) = A*B / [A*B + (1-A)*(1-B)]
         * NOTE: so we only keep track of a running product(A*B*C...)
         * and product difference ((1-A)(1-B)...) */

        int32_t probability =
            (itr.second.m_product /
            (itr.second.m_product + itr.second.m_product_difference))
             * PROBABILITY_FACTOR;

        PINFO("guid is '%s', probability is %f", itr.first.c_str(), (double)probability);

        if (probability > max_probability)
        {
            max_probability = probability;
            selected_account_guid = itr.first;
        }
    }

    PINFO("highest P('%s') = '%d'",
          selected_account_guid.c_str() ? selected_account_guid.c_str() : "(null)",
          max_probability);

    /* has this probability met our threshold? */
    if (max_probability >= threshold)
    {
        GncGUID  guid;
        Account *account = nullptr;

        PINFO("Probability has met threshold");

        if (string_to_guid (selected_account_guid.c_str(), &guid))
            account = xaccAccountLookup (&guid, m_book);

        if (account != nullptr)
            LEAVE("Return account is '%s'", xaccAccountGetName (account));
        else
            LEAVE("Return NULL, account for Guid '%s' can not be found", selected_account_guid.c_str());

        return account;
    }
    LEAVE("Probability has not met threshold, return NULL");

    return nullptr; /* we didn't meet our threshold, return NULL for an account */
}


static void
change_imap_entry (GncImportMatchMap *imap, std::string kvp_path, int64_t token_count)
{
    PINFO("Source Account is '%s', kvp_path is '%s', Count is '%" G_GINT64_FORMAT "'",
           xaccAccountGetName (imap->get_account()), kvp_path.c_str(), token_count);

    // check for existing guid entry
    if (qof_instance_has_slot (QOF_INSTANCE(imap->get_account()), kvp_path.c_str()))
    {
        int64_t existing_token_count = 0;

        auto account_frame = qof_instance_get_slots (QOF_INSTANCE(imap->get_account()));

        auto value = account_frame->get_slot(kvp_path.c_str());

        // get the existing_token_count value
        if (value != nullptr && value->get_type() == KvpValue::Type::INT64)
            existing_token_count = value->get<int64_t>();

        PINFO("found existing value of '%" G_GINT64_FORMAT "'", existing_token_count);

        token_count = token_count + existing_token_count;
    }
    // Add or Update the entry based on guid
    auto account_frame = qof_instance_get_slots (QOF_INSTANCE(imap->get_account()));

    auto new_value = new KvpValue(token_count);

    account_frame->set_path(kvp_path.c_str(), new_value);

    /* Set a feature flag in the book for the change to use guid.
     * This will prevent older GnuCash versions that don't support this feature
     * from opening this file. */
    gnc_features_set_used (imap->get_book(), GNC_FEATURE_GUID_BAYESIAN);
}


void
GncImportMatchMap::add_account_bayes (GList* tokens, Account *acc)
{
    GList *current_token;

    std::string account_fullname;
    std::string kvp_path;
    std::string guid_string;
    std::string delim = "/";

    ENTER(" ");

    g_return_if_fail (acc != NULL);
    account_fullname = gnc_account_get_full_name (acc);
    xaccAccountBeginEdit (m_account);

    PINFO("account name: '%s'", account_fullname.c_str());

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

        std::string token_data = ((char*)current_token->data);

        /* start off with one token for this account */
        int64_t token_count = 1;

        PINFO("adding token '%s'", token_data.c_str());

        kvp_path = IMAP_FRAME_BAYES + delim + token_data + delim + guid_string;

        /* change the imap entry for the account */
        change_imap_entry (this, kvp_path, token_count);

    }
    /* free up the account fullname and guid string */
    qof_instance_set_dirty (QOF_INSTANCE (m_account));
    xaccAccountCommitEdit (m_account);

    LEAVE(" ");
}

/*****************************************************************************/

GncImportMatchMap *
gnc_account_imap_create_imap (Account *acc)
{
    if (!acc) return nullptr;

    GncImportMatchMap *ImportMap = new GncImportMatchMap(acc);
    return ImportMap;
}

void
gnc_account_imap_delete_imap (GncImportMatchMap *imap)
{
    if (!imap) return;
    delete (imap);
}

Account *
gnc_account_imap_find_account (GncImportMatchMap *imap, const char* category, const char *key)
{
    return imap->find_account (category, key);
}

void
gnc_account_imap_add_account (GncImportMatchMap *imap, const char* category, const char *key, Account *acc)
{
    imap->add_account (category, key, acc);
}

void
gnc_account_imap_delete_account (GncImportMatchMap *imap, const char *category, const char *key)
{
    imap->delete_account (category, key);
}

Account *
gnc_account_imap_find_account_bayes (GncImportMatchMap *imap, GList* tokens)
{
    return imap->find_account_bayes (tokens);
}

void gnc_account_imap_add_account_bayes (GncImportMatchMap *imap, GList* tokens, Account *acc)
{
    imap->add_account_bayes (tokens, acc);
}

/*****************************************************************************/

static void
build_bayes_layer_two (const char *key, const GValue *value, gpointer user_data)
{
    QofBook     *book;
    Account     *map_account = NULL;
    GncGUID     *guid;
    gchar       *kvp_path;
    gchar       *count;

    struct imap_info *imapInfo_node;

    struct imap_info *imapInfo = (struct imap_info*)user_data;

    // Get the book
    book = qof_instance_get_book (imapInfo->source_account);

    if (G_VALUE_HOLDS_INT64 (value))
    {
        PINFO("build_bayes_layer_two: account '%s', token_count: '%" G_GINT64_FORMAT "'",
                                  (char*)key, g_value_get_int64(value));

        count = g_strdup_printf ("%" G_GINT64_FORMAT, g_value_get_int64 (value));
    }
    else
        count = g_strdup ("0");

    kvp_path = g_strconcat (imapInfo->category_head, "/", key, NULL);

    PINFO("build_bayes_layer_two: kvp_path is '%s'", kvp_path);

    guid = g_new (GncGUID, 1);

    if (string_to_guid (key, guid))
        map_account = xaccAccountLookup (guid, book);

    g_free (guid);

    imapInfo_node = (imap_info*)g_malloc(sizeof(*imapInfo_node));

    imapInfo_node->source_account = imapInfo->source_account;
    imapInfo_node->map_account    = map_account;
    imapInfo_node->full_category  = g_strdup (kvp_path);
    imapInfo_node->match_string   = g_strdup (imapInfo->match_string);
    imapInfo_node->category_head  = g_strdup (imapInfo->category_head);
    imapInfo_node->count          = g_strdup (count);

    imapInfo->list = g_list_append (imapInfo->list, imapInfo_node);

    g_free (kvp_path);
    g_free (count);
}

static void
build_bayes (const char *key, const GValue *value, gpointer user_data)
{
    gchar *kvp_path;
    struct imap_info *imapInfo = (struct imap_info*)user_data;
    struct imap_info  imapInfol2;

    PINFO("build_bayes: match string '%s'", (char*)key);

    if (G_VALUE_HOLDS (value, G_TYPE_STRING) && g_value_get_string (value) == NULL)
    {
        kvp_path = g_strdup_printf ("%s/%s", IMAP_FRAME_BAYES, key);

        if (qof_instance_has_slot (QOF_INSTANCE(imapInfo->source_account), kvp_path))
        {
            PINFO("build_bayes: kvp_path is '%s', key '%s'", kvp_path, key);

            imapInfol2.source_account = imapInfo->source_account;
            imapInfol2.match_string   = g_strdup (key);
            imapInfol2.category_head  = g_strdup (kvp_path);
            imapInfol2.list           = imapInfo->list;

            qof_instance_foreach_slot (QOF_INSTANCE(imapInfo->source_account), kvp_path,
                                       build_bayes_layer_two, &imapInfol2);

            imapInfo->list = imapInfol2.list;
            g_free (imapInfol2.match_string);
            g_free (imapInfol2.category_head);
        }
        g_free (kvp_path);
    }
}


static void
build_non_bayes (const char *key, const GValue *value, gpointer user_data)
{
    if (G_VALUE_HOLDS_BOXED (value))
    {
        QofBook     *book;
        GncGUID     *guid = NULL;
        gchar       *kvp_path;
        gchar       *guid_string = NULL;

        struct imap_info *imapInfo_node;

        struct imap_info *imapInfo = (struct imap_info*)user_data;

        // Get the book
        book = qof_instance_get_book (imapInfo->source_account);

        guid = (GncGUID*)g_value_get_boxed (value);
        guid_string = guid_to_string (guid);

        PINFO("build_non_bayes: account '%s', match account guid: '%s'",
                                (char*)key, guid_string);

        kvp_path = g_strconcat (imapInfo->category_head, "/", key, NULL);

        PINFO("build_non_bayes: kvp_path is '%s'", kvp_path);

        imapInfo_node = (imap_info*)g_malloc(sizeof(*imapInfo_node));

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
}


GList *
gnc_account_imap_get_info_bayes (Account *acc)
{
    GList *list = NULL;

    GncImapInfo imapInfo;

    imapInfo.source_account = acc;
    imapInfo.list = list;

    if (qof_instance_has_slot (QOF_INSTANCE(acc), IMAP_FRAME_BAYES))
        qof_instance_foreach_slot (QOF_INSTANCE(acc), IMAP_FRAME_BAYES,
                                   build_bayes, &imapInfo);

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

    category_head = g_strdup_printf ("%s/%s", IMAP_FRAME, category);
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
    gchar *kvp_path = g_strdup (full_category);

    if (qof_instance_has_slot (QOF_INSTANCE(acc), kvp_path))
    {
        qof_instance_get_kvp (QOF_INSTANCE(acc), kvp_path, &v);

        if (G_VALUE_HOLDS_STRING (&v))
        {
            gchar const *string;
            string = g_value_get_string (&v);
            text = g_strdup (string);
        }
    }
    g_free (kvp_path);
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
            qof_instance_slot_delete_if_empty (QOF_INSTANCE(acc), kvp_path);
        else
            qof_instance_slot_delete (QOF_INSTANCE(acc), kvp_path);

        PINFO("Account is '%s', path is '%s'", xaccAccountGetName (acc), kvp_path);

        qof_instance_set_dirty (QOF_INSTANCE(acc));
        xaccAccountCommitEdit (acc);
    }
    g_free (kvp_path);
    g_free (full_category);
}

/*******************************************************************************/

static gchar *
look_for_old_separator_descendants (Account *root, gchar *full_name, const gchar *separator)
{
    GList *top_accounts, *ptr;
    gint   found_len = 0;
    gchar  found_sep;

    top_accounts = gnc_account_get_descendants (root);

    PINFO("Incoming full_name is '%s', current separator is '%s'", full_name, separator);

    /* Go through list of top level accounts */
    for (ptr = top_accounts; ptr; ptr = g_list_next (ptr))
    {
        const gchar *name = xaccAccountGetName ((Account*)ptr->data);

        // we are looking for the longest top level account that matches
        if (g_str_has_prefix (full_name, name))
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

    if (found_len > 1)
        full_name = g_strdelimit (full_name, &found_sep, *separator);

    PINFO("Return full_name is '%s'", full_name);

    return full_name;
}


static void
convert_imap_entry (GncImapInfo *imapInfo, Account *map_account)
{
    GncImportMatchMap *imap;
    gchar   *guid_string;
    gchar   *kvp_path;
    int64_t  token_count = 1;

    GValue value = G_VALUE_INIT;

    // Create an ImportMatchMap object
    imap = gnc_account_imap_create_imap (imapInfo->source_account);

    xaccAccountBeginEdit (imapInfo->source_account);

    guid_string = guid_to_string (xaccAccountGetGUID (map_account));

    PINFO("Map Account is '%s', GUID is '%s', Count is %s", xaccAccountGetName (map_account),
               guid_string, imapInfo->count);

    // save converting string, get the count value
    qof_instance_get_kvp (QOF_INSTANCE (imapInfo->source_account), imapInfo->full_category, &value);

    if (G_VALUE_HOLDS_INT64 (&value))
        token_count = g_value_get_int64 (&value);

    // Delete the old entry based on full account name
    kvp_path = g_strdup (imapInfo->full_category);
    gnc_account_delete_map_entry (imapInfo->source_account, kvp_path, FALSE);

    // create path based on guid
    kvp_path = g_strdup_printf ("/%s/%s", imapInfo->category_head, guid_string);

    std::string k_path (kvp_path);  //FIXME needs changing

    // change the imap entry of source_account
    change_imap_entry (imap, k_path, token_count);

    qof_instance_set_dirty (QOF_INSTANCE (imapInfo->source_account));
    xaccAccountCommitEdit (imapInfo->source_account);

    g_free (kvp_path);
    g_free (guid_string);
}


static Account *
look_for_old_mapping (GncImapInfo *imapInfo)
{
    Account       *root, *map_account = NULL;
    const gchar   *sep = gnc_get_account_separator_string ();
    gchar         *full_name;

    PINFO("Category Head is '%s', Full Category is '%s'", imapInfo->category_head, imapInfo->full_category);

    // do we have a map_account all ready, implying a guid string
    if (imapInfo->map_account != NULL)
        return NULL;

    root = gnc_account_get_root (imapInfo->source_account);

    full_name = g_strdup (imapInfo->full_category + strlen (imapInfo->category_head) + 1);

    // may be top level or match with existing separator
    map_account = gnc_account_lookup_by_full_name (root, full_name);

    // do we have a valid account, if not, look for old separator
    if (map_account == NULL)
    {
        full_name = look_for_old_separator_descendants (root, full_name, sep);
        map_account = gnc_account_lookup_by_full_name (root, full_name); // lets try again
    }

    PINFO("Full account name is '%s'", full_name);

    g_free (full_name);

    return map_account;
}

static void
convert_imap_account (Account *acc)
{
    GList *imap_list, *node;
    gchar *acc_name = NULL;

    acc_name = gnc_account_get_full_name (acc);
    PINFO("Source Acc '%s'", acc_name);

    imap_list = gnc_account_imap_get_info_bayes (acc);

    if (g_list_length (imap_list) > 0) // we have mappings
    {
        PINFO("List length is %d", g_list_length (imap_list));

        for (node = imap_list;  node; node = g_list_next (node))
        {
            Account *map_account = NULL;
            GncImapInfo *imapInfo = (GncImapInfo*)node->data;

            // Lets start doing stuff
            map_account = look_for_old_mapping (imapInfo);

            if (map_account != NULL) // we have an account, try and update it
                convert_imap_entry (imapInfo, map_account);

            // Free the members and structure
            g_free (imapInfo->category_head);
            g_free (imapInfo->full_category);
            g_free (imapInfo->match_string);
            g_free (imapInfo->count);
            g_free (imapInfo);
        }
    }
    g_free (acc_name);
    g_list_free (imap_list); // Free the List
}

void
gnc_account_imap_convert_bayes (QofBook *book)
{
    Account      *root;
    GList        *accts, *ptr;
    gboolean      run_once = FALSE;
    GValue        value_s = G_VALUE_INIT;

    // get the run-once value
    qof_instance_get_kvp (QOF_INSTANCE (book), "changed-bayesian-to-guid", &value_s);

    if (G_VALUE_HOLDS_STRING (&value_s) && (strcmp(g_value_get_string (&value_s), "true") == 0))
        run_once = TRUE;

    if (run_once == FALSE)
    {
        GValue value_b = G_VALUE_INIT;

        /* Get list of Accounts */
        root = gnc_book_get_root_account (book);
        accts = gnc_account_get_descendants_sorted (root);

        /* Go through list of accounts */
        for (ptr = accts; ptr; ptr = g_list_next (ptr))
        {
            Account *acc = (Account*)ptr->data;

            convert_imap_account (acc);
        }
        g_list_free (accts);

        g_value_init (&value_b, G_TYPE_BOOLEAN);

        g_value_set_boolean (&value_b, TRUE);

        // set the run-once value
        qof_instance_set_kvp (QOF_INSTANCE (book), "changed-bayesian-to-guid", &value_b);
    }
}

