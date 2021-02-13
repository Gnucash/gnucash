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
#include "qofinstance-p.h"
#include "qofquery.h"
#include "Query.h"
#include "Scrub4.h"
#include "Transaction.h"

#define CLOSING_SCRUB_FLAG "closing-entries-scrubbed"

static QofLogModule log_module = "gnc.engine.scrub";

static GList * get_closing_splits_without_KVP (QofBook *book)
{
    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, book);
    xaccQueryAddDescriptionMatch (q, _("Closing Entries"), FALSE, FALSE,
                                  QOF_COMPARE_CONTAINS, QOF_QUERY_AND);
    xaccQueryAddClosingTransMatch (q, FALSE, QOF_QUERY_AND);
    return xaccQueryGetSplitsUniqueTrans (q);
}

static gboolean book_has_scrubbed_flag (QofBook *book)
{
    GValue value_s = G_VALUE_INIT;
    qof_instance_get_kvp (QOF_INSTANCE (book), &value_s, 1, CLOSING_SCRUB_FLAG);
    return (G_VALUE_HOLDS_STRING (&value_s) &&
            (g_strcmp0 (g_value_get_string (&value_s), "true") == 0));
}

static void set_book_scrubbed_flag (QofBook *book)
{
    GValue value_b = G_VALUE_INIT;
    g_value_init (&value_b, G_TYPE_BOOLEAN);
    g_value_set_boolean (&value_b, TRUE);
    qof_instance_set_kvp (QOF_INSTANCE (book), &value_b, 1, CLOSING_SCRUB_FLAG);
}

static void set_closing_kvp (Split *s)
{
    Transaction *t = xaccSplitGetParent (s);
    PWARN ("Setting Closing KVP: Date[%s] Description[%s] Amount[%s]",
           qof_print_date (xaccTransGetDate (t)),
           xaccTransGetDescription (t),
           gnc_numeric_to_string (xaccSplitGetAmount (s)));
    xaccTransSetIsClosingTxn (t, TRUE);
}

void gnc_scrub_closing_kvp (QofBook *book)
{
    Account* root = gnc_book_get_root_account (book);
    GList *lst;

    if (book_has_scrubbed_flag (book))
        return;

    lst = get_closing_splits_without_KVP (book);
    for (GList *n = lst; n; n = n->next)
        set_closing_kvp (n->data);
    g_list_free (lst);

    set_book_scrubbed_flag (book);
    PWARN ("Scrubbed Closing Entries!");
}
/* ==================== END OF FILE ==================== */
