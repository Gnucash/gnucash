/********************************************************************\
 * SX-book.c -- scheduled transaction dataset access                *
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

/*
 * FILE:
 * SX-book.c
 *
 * FUNCTION:
 * Anchor Scheduled Transaction Info into the book.
 * See src/doc/books.txt for design overview.
 *
 * HISTORY:
 * Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */

#include <config.h>

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-engine.h"
#include "Account.h"
#include "Split.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-book-p.h"
#include "gnc-event.h"
#include <qofinstance-p.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.engine.sx"

/* XXX this whole file is crufty, it doesn't really use entities
 * in the most efficient/best way */

/* ====================================================================== */

static Account *
gnc_collection_get_template_root( const QofCollection *col )
{
    return qof_collection_get_data (col);
}

Account *
gnc_book_get_template_root( const QofBook *book )
{
    QofCollection *col;
    if (!book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_SXTG);
    return gnc_collection_get_template_root (col);
}

static void
gnc_collection_set_template_root (QofCollection *col,
                                  Account *templateRoot)
{
    Account *old_root;
    if (!col) return;

    old_root = gnc_collection_get_template_root (col);
    if (old_root == templateRoot) return;

    qof_collection_set_data (col, templateRoot);

    if (old_root)
    {
        xaccAccountBeginEdit (old_root);
        xaccAccountDestroy (old_root);
    }
}


void
gnc_book_set_template_root (QofBook *book, Account *templateRoot)
{
    QofCollection *col;
    if (!book) return;

    if (templateRoot && gnc_account_get_book(templateRoot) != book)
    {
        g_critical("cannot mix and match books freely!");
        return;
    }

    col = qof_book_get_collection (book, GNC_ID_SXTG);
    gnc_collection_set_template_root (col, templateRoot);
}


/* ====================================================================== */
/* gncObject function implementation and registration */

static void
sxtg_book_begin (QofBook *book)
{
    Account *root;

    root = xaccMallocAccount(book);
    xaccAccountBeginEdit(root);
    xaccAccountSetType(root, ACCT_TYPE_ROOT);
    xaccAccountSetName(root, "Template Root");
    qof_instance_set_dirty (QOF_INSTANCE (root));
    xaccAccountCommitEdit(root);
    gnc_book_set_template_root (book, root);
}

static void
sxtg_book_end (QofBook *book)
{
//    gnc_book_set_template_root (book, NULL);
}

static gboolean
sxtg_is_dirty(const QofCollection *col)
{
    Account *root;
    GList *descendants, *node;
    gboolean dirty = FALSE;

    root = gnc_collection_get_template_root(col);
    descendants = gnc_account_get_descendants(root);
    for (node = descendants; node; node = g_list_next(node))
    {
        if (qof_instance_is_dirty(node->data))
        {
            dirty = TRUE;
            break;
        }
    }
    g_list_free(descendants);

    return dirty;
}

/* EFFECTIVE FRIEND FUNCTION declared in qofinstance-p.h */
extern void qof_instance_mark_clean (QofInstance *);

static void
sxtg_mark_clean(QofCollection *col)
{
    Account *root;
    GList *descendants;

    root = gnc_collection_get_template_root(col);
    qof_collection_mark_clean(col);

    descendants = gnc_account_get_descendants(root);
    g_list_foreach(descendants, (GFunc)qof_instance_mark_clean, NULL);
    g_list_free(descendants);
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject sxtg_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_SXTG,
    DI(.type_label        = ) "Scheduled Transaction Templates",
    DI(.create            = ) NULL,
    DI(.book_begin        = ) sxtg_book_begin,
    DI(.book_end          = ) sxtg_book_end,
    DI(.is_dirty          = ) sxtg_is_dirty,
    DI(.mark_clean        = ) sxtg_mark_clean,
    DI(.foreach           = ) NULL,
    DI(.printable         = ) NULL,
};

/* ====================================================================== */

SchedXactions*
gnc_collection_get_schedxactions(const QofCollection *col)
{
    SchedXactions *rtn = qof_collection_get_data(col);
    // @@assert(rtn != null);
    return rtn;
}

SchedXactions*
gnc_book_get_schedxactions(QofBook *book)
{
    QofCollection *col;
    col = qof_book_get_collection(book, GNC_ID_SCHEDXACTION);
    return gnc_collection_get_schedxactions(col);
}

void
gnc_sxes_add_sx(SchedXactions *sxes, SchedXaction *sx)
{
    if (g_list_find(sxes->sx_list, sx) != NULL)
        return;
    sxes->sx_list = g_list_append(sxes->sx_list, sx);
    qof_event_gen(&sxes->inst, GNC_EVENT_ITEM_ADDED, (gpointer)sx);
}

void
gnc_sxes_del_sx(SchedXactions *sxes, SchedXaction *sx)
{
    GList *to_remove;
    to_remove = g_list_find(sxes->sx_list, sx);
    if (to_remove == NULL)
        return;
    sxes->sx_list = g_list_delete_link(sxes->sx_list, to_remove);
    qof_event_gen(&sxes->inst, GNC_EVENT_ITEM_REMOVED, (gpointer)sx);
}

/* ====================================================================== */
/* SX-trans stuff */

/* GObject initialization */
QOF_GOBJECT_IMPL(gnc_schedxactions, SchedXactions, QOF_TYPE_INSTANCE);

static void
gnc_schedxactions_init(SchedXactions* sxs)
{
}

static void
gnc_schedxactions_dispose_real (GObject *sxsp)
{
}

static void
gnc_schedxactions_finalize_real(GObject* sxsp)
{
}

static void
mark_sx_clean(gpointer data, gpointer user_data)
{
    SchedXaction *sx = (SchedXaction *) data;
    qof_instance_mark_clean (QOF_INSTANCE(sx));
}

static void
book_sxes_setup(QofBook *book)
{
    QofCollection *col;
    SchedXactions *sxes;

    col = qof_book_get_collection(book, GNC_ID_SCHEDXACTION);
    sxes = g_object_new (GNC_TYPE_SCHEDXACTIONS, NULL);
    g_assert(sxes);
    qof_instance_init_data(&sxes->inst, GNC_ID_SXES, book);
    sxes->sx_list = NULL;
    sxes->sx_notsaved = TRUE;
    qof_collection_set_data(col, sxes);
}

static void
book_sxes_end(QofBook* book)
{
    QofCollection *col;
    SchedXactions *sxes;

    col = qof_book_get_collection(book, GNC_ID_SCHEDXACTION);
    sxes = qof_collection_get_data(col);
    if (sxes != NULL)
    {
        g_object_unref(sxes);
        qof_collection_set_data(col, NULL);
    }
}

static void
book_sxns_mark_saved(QofCollection *col)
{
    SchedXactions *sxl;
    sxl = gnc_collection_get_schedxactions(col);
    if (!sxl)
        return;
    sxl->sx_notsaved = FALSE;
    g_list_foreach(sxl->sx_list,
                   mark_sx_clean,
                   NULL);
}

static gboolean
book_sxlist_notsaved(const QofCollection *col)
{
    GList *sxlist;
    SchedXactions *sxl;

    sxl = gnc_collection_get_schedxactions(col);
    if (!sxl) return FALSE;
    if (sxl->sx_notsaved) return TRUE;

    for (sxlist = sxl->sx_list;
            sxlist != NULL;
            sxlist = g_list_next(sxlist))
    {
        SchedXaction *sx;
        sx = (SchedXaction *) (sxlist->data);
        if (xaccSchedXactionIsDirty( sx ))
            return TRUE;
    }

    return FALSE;
}

static QofObject sxes_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_SXES,
    DI(.type_label        = ) "Scheduled Transactions List",
    DI(.create            = ) NULL,
    DI(.book_begin        = ) book_sxes_setup,
    DI(.book_end          = ) book_sxes_end,
    DI(.is_dirty          = ) book_sxlist_notsaved,
    DI(.mark_clean        = ) book_sxns_mark_saved,
    DI(.foreach           = ) NULL,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) NULL
};

static QofObject sxtt_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_SXTT,
    DI(.type_label        = ) "Scheduled Transaction Templates",
    DI(.create            = ) NULL,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) NULL,
    DI(.is_dirty          = ) NULL,
    DI(.mark_clean        = ) NULL,
    DI(.foreach           = ) NULL,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) NULL,
};

gboolean
gnc_sxtt_register (void)
{
    if (!qof_object_register(&sxes_object_def))
        return FALSE;
    if (!qof_object_register(&sxtg_object_def))
        return FALSE;
    return qof_object_register(&sxtt_object_def);
}

GList*
gnc_sx_get_sxes_referencing_account(QofBook *book, Account *acct)
{
    GList *rtn = NULL;
    const GncGUID *acct_guid = qof_entity_get_guid(QOF_INSTANCE(acct));
    GList *sx_list;
    SchedXactions *sxactions = gnc_book_get_schedxactions(book);
    g_return_val_if_fail( sxactions != NULL, rtn);
    for (sx_list = sxactions->sx_list; sx_list != NULL; sx_list = sx_list->next)
    {
        SchedXaction *sx = (SchedXaction*)sx_list->data;
        GList *splits = xaccSchedXactionGetSplits(sx);
        for (; splits != NULL; splits = splits->next)
        {
            Split *s = (Split*)splits->data;
            GncGUID *guid = NULL;
            qof_instance_get (QOF_INSTANCE (s), "sx-account", &guid, NULL);
            if (guid_equal(acct_guid, guid))
            {
                rtn = g_list_append(rtn, sx);
            }
        }
    }
    return rtn;
}

/* ========================== END OF FILE =============================== */
