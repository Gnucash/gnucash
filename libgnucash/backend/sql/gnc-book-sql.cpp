/********************************************************************
 * gnc-book-sql.c: load and save data to SQL                        *
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
/** @file gnc-book-sql.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db
 */
extern "C"
{
#include <config.h>

#include <glib.h>

#include "qof.h"

#include "gnc-engine.h"
#include "SX-book.h"
#include "SX-book-p.h"

#if defined( S_SPLINT_S )
#include "splint-defs.h"
#endif
}

#include "gnc-sql-connection.hpp"
#include "gnc-sql-backend.hpp"
#include "gnc-sql-object-backend.hpp"
#include "gnc-sql-column-table-entry.hpp"
#include "gnc-book-sql.h"
#include "gnc-slots-sql.h"


#define BOOK_TABLE "books"
#define TABLE_VERSION 1

G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

static  gpointer get_root_account_guid (gpointer pObject);
static void set_root_account_guid (gpointer pObject,  gpointer pValue);
static  gpointer get_root_template_guid (gpointer pObject);
static void set_root_template_guid (gpointer pObject,  gpointer pValue);

static const EntryVec col_table
{
    gnc_sql_make_table_entry<CT_GUID>(
        "guid", 0, COL_NNUL | COL_PKEY, "guid"),
    gnc_sql_make_table_entry<CT_GUID>("root_account_guid",  0, COL_NNUL,
                                      (QofAccessFunc)get_root_account_guid,
                                      set_root_account_guid),
    gnc_sql_make_table_entry<CT_GUID>("root_template_guid", 0, COL_NNUL,
                                      (QofAccessFunc)get_root_template_guid,
                                      set_root_template_guid)
};

GncSqlBookBackend::GncSqlBookBackend() :
    GncSqlObjectBackend(TABLE_VERSION, GNC_ID_BOOK,
                        BOOK_TABLE, col_table) {}

/* ================================================================= */
static  gpointer
get_root_account_guid (gpointer pObject)
{
    QofBook* book = QOF_BOOK (pObject);
    const Account* root;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (QOF_IS_BOOK (pObject), NULL);

    root = gnc_book_get_root_account (book);
    return (gpointer)qof_instance_get_guid (QOF_INSTANCE (root));
}

static void
set_root_account_guid (gpointer pObject,  gpointer pValue)
{
    QofBook* book = QOF_BOOK (pObject);
    const Account* root;
    GncGUID* guid = (GncGUID*)pValue;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (QOF_IS_BOOK (pObject));
    g_return_if_fail (pValue != NULL);

    root = gnc_book_get_root_account (book);
    qof_instance_set_guid (QOF_INSTANCE (root), guid);
}

static  gpointer
get_root_template_guid (gpointer pObject)
{
    const QofBook* book = QOF_BOOK (pObject);
    const Account* root;

    g_return_val_if_fail (pObject != NULL, NULL);
    g_return_val_if_fail (QOF_IS_BOOK (pObject), NULL);

    root = gnc_book_get_template_root (book);
    return (gpointer)qof_instance_get_guid (QOF_INSTANCE (root));
}

static void
set_root_template_guid (gpointer pObject,  gpointer pValue)
{
    QofBook* book = QOF_BOOK (pObject);
    GncGUID* guid = (GncGUID*)pValue;
    Account* root;

    g_return_if_fail (pObject != NULL);
    g_return_if_fail (QOF_IS_BOOK (pObject));
    g_return_if_fail (pValue != NULL);

    root = gnc_book_get_template_root (book);
    if (root == NULL)
    {
        root = xaccMallocAccount (book);
        xaccAccountBeginEdit (root);
        xaccAccountSetType (root, ACCT_TYPE_ROOT);
        xaccAccountCommitEdit (root);
        gnc_book_set_template_root (book, root);
    }
    qof_instance_set_guid (QOF_INSTANCE (root), guid);
}

/* ================================================================= */
static void
load_single_book (GncSqlBackend* sql_be, GncSqlRow& row)
{
    QofBook* pBook;

    g_return_if_fail (sql_be != NULL);

    gnc_sql_load_guid (sql_be, row);

    pBook = sql_be->book();
    if (pBook == NULL)
    {
        pBook = qof_book_new ();
    }

    qof_book_begin_edit (pBook);
    gnc_sql_load_object (sql_be, row, GNC_ID_BOOK, pBook, col_table);
    gnc_sql_slots_load (sql_be, QOF_INSTANCE (pBook));
    qof_book_commit_edit (pBook);

    qof_instance_mark_clean (QOF_INSTANCE (pBook));
}

void
GncSqlBookBackend::load_all (GncSqlBackend* sql_be)
{
    g_return_if_fail (sql_be != NULL);

    std::stringstream sql;
    sql << "SELECT * FROM " << BOOK_TABLE;
    auto stmt = sql_be->create_statement_from_sql(sql.str());
    if (stmt != nullptr)
    {
        auto result = sql_be->execute_select_statement(stmt);
        auto row = result->begin();

        /* If there are no rows, try committing the book; unset
         * loading so that it will actually get saved.
         */
        if (row == result->end())
        {
            sql_be->set_loading(false);
            commit (sql_be, QOF_INSTANCE (sql_be->book()));
            sql_be->set_loading(true);
        }
        else
        {
            // Otherwise, load the 1st book.
            load_single_book (sql_be, *row);
        }
    }
}

/* ========================== END OF FILE ===================== */
