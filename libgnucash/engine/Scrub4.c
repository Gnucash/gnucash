/********************************************************************\
 * Scrub4.c -- ensure closing entries have KVP flag                 *
 * Copyright (c) 2021 Christopher Lam                               *
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
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "gnc-engine.h"
#include "gnc-features.h"
#include "qofquery.h"
#include "Query.h"
#include "Scrub4.h"
#include "Transaction.h"

static QofLogModule log_module = "gnc.engine.scrub";

static GList * get_closing_without_KVP (QofBook *book)
{
    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, book);
    xaccQueryAddDescriptionMatch (q, _("Closing Entries"), FALSE, FALSE,
                                  QOF_COMPARE_CONTAINS, QOF_QUERY_AND);
    xaccQueryAddClosingTransMatch (q, FALSE, QOF_QUERY_AND);
    return xaccQueryGetSplitsUniqueTrans (q);
}

gboolean gnc_scrub_closing_kvp_check (QofBook *book)
{
    if (gnc_features_check_used (book, GNC_FEATURE_SCRUBBED_CLOSING))
        return FALSE;
    else
    {
        GList *lst = get_closing_without_KVP (book);
        gboolean retval = (lst != NULL);
        g_list_free (lst);
        return retval;
    }
}

void gnc_scrub_closing_kvp (QofBook *book)
{
    Account* root = gnc_book_get_root_account (book);
    GList *lst;

    if (gnc_features_check_used (book, GNC_FEATURE_SCRUBBED_CLOSING))
    {
        PWARN ("Don't run gnc_scrub_closing_entries with featured book!");
        return;
    }

    lst = get_closing_without_KVP (book);

    if (!lst)
    {
        PWARN ("No closing entries without KVP flag found in this book");
        goto end;
    }

    for (GList *n = lst; n; n = n->next)
    {
        Split *s = n->data;
        Transaction *t = xaccSplitGetParent (s);
        PWARN ("Setting Closing KVP: Date[%s] Description[%s] Amount[%s]",
               qof_print_date (xaccTransGetDate (t)),
               xaccTransGetDescription (t),
               gnc_numeric_to_string (xaccSplitGetAmount (s)));
        xaccTransSetIsClosingTxn (t, TRUE);
    }
    g_list_free (lst);    

 end:
    gnc_features_set_used (book, GNC_FEATURE_SCRUBBED_CLOSING);
    PWARN ("Scrubbed Closing Entries!");
}
/* ==================== END OF FILE ==================== */
