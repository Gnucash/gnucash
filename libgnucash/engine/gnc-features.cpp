/********************************************************************\
 * gnc-features.c -- manage GnuCash features table                  *
 * Copyright (C) 2011 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Geert Janssens <geert@kobaltwit.be>           *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include <unordered_map>
#include <string>
#include <numeric>
#include <algorithm>

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include "qofbook.h"
#include "qofbook.hpp"

#include "gnc-features.h"

static const FeaturesTable features_table
{
    { GNC_FEATURE_CREDIT_NOTES, "Customer and vendor credit notes (requires at least GnuCash 2.5.0)" },
    { GNC_FEATURE_NUM_FIELD_SOURCE, "User specifies source of 'num' field'; either transaction number or split action (requires at least GnuCash 2.5.0)" },
    { GNC_FEATURE_KVP_EXTRA_DATA, "Extra data for addresses, jobs or invoice entries (requires at least GnuCash 2.6.4)" },
    { GNC_FEATURE_GUID_BAYESIAN, "Use account GUID as key for Bayesian data (requires at least GnuCash 2.6.12)" },
    { GNC_FEATURE_GUID_FLAT_BAYESIAN, "Use account GUID as key for bayesian data and store KVP flat (requires at least Gnucash 2.6.19)" },
    { GNC_FEATURE_SQLITE3_ISO_DATES, "Use ISO formatted date-time strings in SQLite3 databases (requires at least GnuCash 2.6.20)"},
    { GNC_FEATURE_REG_SORT_FILTER, "Store the register sort and filter settings in .gcm metadata file (requires at least GnuCash 3.3)"},
    { GNC_FEATURE_BUDGET_UNREVERSED, "Store budget amounts unreversed (i.e. natural) signs (requires at least Gnucash 3.8)"},
    { GNC_FEATURE_BUDGET_SHOW_EXTRA_ACCOUNT_COLS, "Show extra account columns in the Budget View (requires at least Gnucash 3.8)"},
    { GNC_FEATURE_EQUITY_TYPE_OPENING_BALANCE, GNC_FEATURE_EQUITY_TYPE_OPENING_BALANCE " (requires at least Gnucash 4.3)" },
};

/* To obsolete a feature leave the #define in gnc-features.h and move the
 * feature from features_table to obsolete_features after removing all of the
 * code that depends on the feature. The feature will be removed from the book's
 * KVP, allowing the book to be opened with GnuCash versions that don't support
 * the feature.
 *
 * Do this only if the book's data is restored to compatibility with older
 * GnuCash versions lacking the feature. In general this can be used only in
 * cases where a feature was created but never implemented in a way that affects
 * the book's data.
 */
static const FeaturesTable obsolete_features{
    {GNC_FEATURE_BOOK_CURRENCY, "User-specified book currency stored in KVP. Never implemented but some user managed to get it set anyway. (requires at least GnuCash 2.7.0)"},
};

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/********************************************************************\
\********************************************************************/

static const char*
header = N_("This Dataset contains features not supported "
            "by this version of GnuCash. You must use a "
            "newer version of GnuCash in order to support "
            "the following features:");

/* Check if the session requires features unknown to this version of GnuCash.
 *
 * Returns a message to display if we found unknown features, NULL if
 * we're okay.
 */
gchar *gnc_features_test_unknown (QofBook *book)
{
    auto unknowns {qof_book_get_unknown_features (book, features_table)};
    if (unknowns.empty())
        return nullptr;

    auto obsolete = std::remove_if(unknowns.begin(), unknowns.end(),
                                   [](auto& unknown){
                                       return obsolete_features.find(unknown.first) != obsolete_features.end();
                                   });
    while (obsolete != unknowns.end())
    {
        qof_book_unset_feature(book, obsolete->first.data());
        obsolete = unknowns.erase(obsolete);
    }

    if (unknowns.empty())
        return nullptr;

    auto accum = [](const auto& a, const auto& b){ return a + "\n* " + b.second.data(); };
    auto msg {std::accumulate (unknowns.begin(), unknowns.end(),
                                   std::string (_(header)), accum)};
    return g_strdup (msg.c_str());
}

void gnc_features_set_used (QofBook *book, const gchar *feature)
{
    g_return_if_fail (book);
    g_return_if_fail (feature);

    /* Can't set an unknown feature */
    auto iter = features_table.find (feature);
    if (iter == features_table.end ())
    {
        PWARN("Tried to set unknown feature as used.");
        return;
    }

    qof_book_set_feature (book, feature, iter->second.data());
}


void gnc_features_set_unused (QofBook *book, const gchar *feature)
{
    g_return_if_fail (book);
    g_return_if_fail (feature);

    /* Can't set an unknown feature */
    auto iter = features_table.find (feature);
    if (iter == features_table.end ())
    {
        PWARN("Tried to set unknown feature as unused.");
        return;
    }

    qof_book_unset_feature (book, feature);
}

gboolean gnc_features_check_used (QofBook *book, const gchar * feature)
{
    return qof_book_test_feature (book, feature);
}

