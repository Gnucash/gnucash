/********************************************************************\
 * gnc-lot.c -- AR/AP invoices; inventory lots; stock lots          *
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
 * gnc-lot.c
 *
 * FUNCTION:
 * Lots implement the fundamental conceptual idea behind invoices,
 * inventory lots, and stock market investment lots.  See the file
 * src/doc/lots.txt for implementation overview.
 *
 * XXX Lots are not currently treated in a correct transactional
 * manner.  There's now a per-Lot dirty flag in the QofInstance, but
 * this code still needs to emit the correct signals when a lot has
 * changed.  This is true both in the Scrub2.c and in
 * src/gnome/dialog-lot-viewer.c
 *
 * HISTORY:
 * Created by Linas Vepstas May 2002
 * Copyright (c) 2002,2003 Linas Vepstas <linas@linas.org>
 */

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <qofinstance-p.h>

#include "Account.h"
#include "AccountP.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"
#include "cap-gains.h"
#include "Transaction.h"
#include "TransactionP.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_LOT;

struct gnc_lot_s
{
    QofInstance inst;
};

enum
{
    PROP_0,
//  PROP_ACCOUNT, 	/* Table */
    PROP_IS_CLOSED,	/* Table */

    PROP_INVOICE,	/* KVP */
    PROP_OWNER_TYPE,	/* KVP */
    PROP_OWNER_GUID,	/* KVP */

    PROP_RUNTIME_0,
    PROP_MARKER,	/* Runtime */
};

typedef struct LotPrivate
{
    /* Account to which this lot applies.  All splits in the lot must
     * belong to this account.
     */
    Account * account;

    /* List of splits that belong to this lot. */
    SplitList *splits;

    /* Handy cached value to indicate if lot is closed. */
    /* If value is negative, then the cache is invalid. */
    signed char is_closed;
#define LOT_CLOSED_UNKNOWN (-1)

    /* traversal marker, handy for preventing recursion */
    unsigned char marker;
} LotPrivate;

#define GET_PRIVATE(o) \
    (G_TYPE_INSTANCE_GET_PRIVATE((o), GNC_TYPE_LOT, LotPrivate))

#define gnc_lot_set_guid(L,G)  qof_instance_set_guid(QOF_INSTANCE(L),&(G))

/* ============================================================= */

static void gnc_lot_set_invoice (GNCLot* lot, GncGUID *guid);
static GncGUID *gnc_lot_get_invoice (GNCLot* lot);

/* ============================================================= */

/* GObject Initialization */
G_DEFINE_TYPE(GNCLot, gnc_lot, QOF_TYPE_INSTANCE)

static void
gnc_lot_init(GNCLot* lot)
{
    LotPrivate* priv;

    priv = GET_PRIVATE(lot);
    priv->account = NULL;
    priv->splits = NULL;
    priv->is_closed = LOT_CLOSED_UNKNOWN;
    priv->marker = 0;
}

static void
gnc_lot_dispose(GObject *lotp)
{
    G_OBJECT_CLASS(gnc_lot_parent_class)->dispose(lotp);
}

static void
gnc_lot_finalize(GObject* lotp)
{
    G_OBJECT_CLASS(gnc_lot_parent_class)->finalize(lotp);
}

static void
gnc_lot_get_property(GObject* object, guint prop_id, GValue* value, GParamSpec* pspec)
{
    GNCLot* lot;
    LotPrivate* priv;
    KvpFrame *frame;
    gchar *key;
    GValue *temp;

    g_return_if_fail(GNC_IS_LOT(object));

    lot = GNC_LOT(object);
    priv = GET_PRIVATE(lot);
    switch (prop_id)
    {
    case PROP_IS_CLOSED:
        g_value_set_int(value, priv->is_closed);
        break;
    case PROP_MARKER:
        g_value_set_int(value, priv->marker);
        break;
    case PROP_INVOICE:
	key = GNC_INVOICE_ID "/" GNC_INVOICE_GUID;
	qof_instance_get_kvp (QOF_INSTANCE (lot), key, value);
	break;
    case PROP_OWNER_TYPE:
	key = GNC_OWNER_ID"/" GNC_OWNER_TYPE;
	qof_instance_get_kvp (QOF_INSTANCE (lot), key, value);
	break;
    case PROP_OWNER_GUID:
	key = GNC_OWNER_ID "/" GNC_OWNER_GUID;
	qof_instance_get_kvp (QOF_INSTANCE (lot), key, value);
	break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_lot_set_property (GObject* object,
		      guint prop_id,
		      const GValue* value,
		      GParamSpec* pspec)
{
    GNCLot* lot;
    LotPrivate* priv;
    KvpFrame *frame;
    gchar *key = NULL;

    g_return_if_fail(GNC_IS_LOT(object));

    lot = GNC_LOT(object);
    if (prop_id < PROP_RUNTIME_0)
	g_assert (qof_instance_get_editlevel(lot));

    priv = GET_PRIVATE(lot);
    switch (prop_id)
    {
    case PROP_IS_CLOSED:
        priv->is_closed = g_value_get_int(value);
        break;
    case PROP_MARKER:
        priv->marker = g_value_get_int(value);
        break;
    case PROP_INVOICE:
	key = GNC_INVOICE_ID"/" GNC_INVOICE_GUID;
	qof_instance_set_kvp (QOF_INSTANCE (lot), key, value);
	break;
    case PROP_OWNER_TYPE:
	key = GNC_OWNER_ID "/" GNC_OWNER_TYPE;
	qof_instance_set_kvp (QOF_INSTANCE (lot), key, value);
	break;
    case PROP_OWNER_GUID:
	key = GNC_OWNER_ID "/" GNC_OWNER_GUID;
	qof_instance_set_kvp (QOF_INSTANCE (lot), key, value);
	break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
     }
}

static void
gnc_lot_class_init(GNCLotClass* klass)
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_lot_dispose;
    gobject_class->finalize = gnc_lot_finalize;
    gobject_class->get_property = gnc_lot_get_property;
    gobject_class->set_property = gnc_lot_set_property;

    g_type_class_add_private(klass, sizeof(LotPrivate));

    g_object_class_install_property(
        gobject_class,
        PROP_IS_CLOSED,
        g_param_spec_int("is-closed",
                         "Is Lot Closed",
                         "Indication of whether this lot is open "
                         "or closed to further changes.",
                         -1, 1, 0,
                         G_PARAM_READWRITE));

    g_object_class_install_property(
        gobject_class,
        PROP_MARKER,
        g_param_spec_int("marker",
                         "Lot marker",
                         "Ipsum Lorem",
                         0, G_MAXINT8, 0,
                         G_PARAM_READWRITE));

     g_object_class_install_property(
       gobject_class,
        PROP_INVOICE,
        g_param_spec_boxed("invoice",
			   "Invoice attached to lot",
			   "Used by GncInvoice",
			   GNC_TYPE_GUID,
			   G_PARAM_READWRITE));

     g_object_class_install_property(
       gobject_class,
        PROP_OWNER_TYPE,
        g_param_spec_int64("owner-type",
			   "Owning Entity Type of  lot",
			   "Used by GncOwner",
			   0, G_MAXINT64, 0,
			   G_PARAM_READWRITE));

     g_object_class_install_property(
       gobject_class,
        PROP_OWNER_GUID,
        g_param_spec_boxed("owner-guid",
			   "Owner attached to lot",
			   "Used by GncOwner",
			   GNC_TYPE_GUID,
			   G_PARAM_READWRITE));
}

GNCLot *
gnc_lot_new (QofBook *book)
{
    GNCLot *lot;
    g_return_val_if_fail (book, NULL);

    lot = g_object_new (GNC_TYPE_LOT, NULL);
    qof_instance_init_data(QOF_INSTANCE(lot), GNC_ID_LOT, book);
    qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_CREATE, NULL);
    return lot;
}

static void
gnc_lot_free(GNCLot* lot)
{
    GList *node;
    LotPrivate* priv;
    if (!lot) return;

    ENTER ("(lot=%p)", lot);
    qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_DESTROY, NULL);

    priv = GET_PRIVATE(lot);
    for (node = priv->splits; node; node = node->next)
    {
        Split *s = node->data;
        s->lot = NULL;
    }
    g_list_free (priv->splits);

    priv->account = NULL;
    priv->is_closed = TRUE;
    /* qof_instance_release (&lot->inst); */
    g_object_unref (lot);

    LEAVE();
}

void
gnc_lot_destroy (GNCLot *lot)
{
    if (!lot) return;

    gnc_lot_begin_edit(lot);
    qof_instance_set_destroying(lot, TRUE);
    gnc_lot_commit_edit(lot);
}

/* ============================================================= */

void
gnc_lot_begin_edit (GNCLot *lot)
{
    qof_begin_edit(QOF_INSTANCE(lot));
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
    PERR ("Failed to commit: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void lot_free(QofInstance* inst)
{
    GNCLot* lot = GNC_LOT(inst);

    gnc_lot_free(lot);
}

static void noop (QofInstance *inst) {}

void
gnc_lot_commit_edit (GNCLot *lot)
{
    if (!qof_commit_edit (QOF_INSTANCE(lot))) return;
    qof_commit_edit_part2 (QOF_INSTANCE(lot), commit_err, noop, lot_free);
}

/* ============================================================= */

GNCLot *
gnc_lot_lookup (const GncGUID *guid, QofBook *book)
{
    QofCollection *col;
    if (!guid || !book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_LOT);
    return (GNCLot *) qof_collection_lookup_entity (col, guid);
}

QofBook *
gnc_lot_get_book (GNCLot *lot)
{
    return qof_instance_get_book(QOF_INSTANCE(lot));
}

/* ============================================================= */

gboolean
gnc_lot_is_closed (GNCLot *lot)
{
    LotPrivate* priv;
    if (!lot) return TRUE;
    priv = GET_PRIVATE(lot);
    if (0 > priv->is_closed) gnc_lot_get_balance (lot);
    return priv->is_closed;
}

Account *
gnc_lot_get_account (const GNCLot *lot)
{
    LotPrivate* priv;
    if (!lot) return NULL;
    priv = GET_PRIVATE(lot);
    return priv->account;
}

void
gnc_lot_set_account(GNCLot* lot, Account* account)
{
    if (lot != NULL)
    {
        LotPrivate* priv;
        priv = GET_PRIVATE(lot);
        priv->account = account;
    }
}

void
gnc_lot_set_closed_unknown(GNCLot* lot)
{
    LotPrivate* priv;
    if (lot != NULL)
    {
        priv = GET_PRIVATE(lot);
        priv->is_closed = LOT_CLOSED_UNKNOWN;
    }
}

SplitList *
gnc_lot_get_split_list (const GNCLot *lot)
{
    LotPrivate* priv;
    if (!lot) return NULL;
    priv = GET_PRIVATE(lot);
    return priv->splits;
}

gint gnc_lot_count_splits (const GNCLot *lot)
{
    LotPrivate* priv;
    if (!lot) return 0;
    priv = GET_PRIVATE(lot);
    return g_list_length (priv->splits);
}

/* ============================================================== */
/* Hmm, we should probably inline these. */

const char *
gnc_lot_get_title (const GNCLot *lot)
{
    if (!lot) return NULL;
    return kvp_frame_get_string (qof_instance_get_slots(QOF_INSTANCE (lot)),
				 "/title");
}

const char *
gnc_lot_get_notes (const GNCLot *lot)
{
    if (!lot) return NULL;
    return kvp_frame_get_string (qof_instance_get_slots(QOF_INSTANCE (lot)),
				 "/notes");
}

void
gnc_lot_set_title (GNCLot *lot, const char *str)
{
    if (!lot) return;
    qof_begin_edit(QOF_INSTANCE(lot));
    qof_instance_set_dirty(QOF_INSTANCE(lot));
    kvp_frame_set_string (qof_instance_get_slots(QOF_INSTANCE (lot)),
		       "/title", str);
    gnc_lot_commit_edit(lot);
}

void
gnc_lot_set_notes (GNCLot *lot, const char *str)
{
    if (!lot) return;
    gnc_lot_begin_edit(lot);
    qof_instance_set_dirty(QOF_INSTANCE(lot));
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (lot)),
		       "/notes", str);
    gnc_lot_commit_edit(lot);
}

/* ============================================================= */

gnc_numeric
gnc_lot_get_balance (GNCLot *lot)
{
    LotPrivate* priv;
    GList *node;
    gnc_numeric zero = gnc_numeric_zero();
    gnc_numeric baln = zero;
    if (!lot) return zero;

    priv = GET_PRIVATE(lot);
    if (!priv->splits)
    {
        priv->is_closed = FALSE;
        return zero;
    }

    /* Sum over splits; because they all belong to same account
     * they will have same denominator.
     */
    for (node = priv->splits; node; node = node->next)
    {
        Split *s = node->data;
        gnc_numeric amt = xaccSplitGetAmount (s);
        baln = gnc_numeric_add_fixed (baln, amt);
        g_assert (gnc_numeric_check (baln) == GNC_ERROR_OK);
    }

    /* cache a zero balance as a closed lot */
    if (gnc_numeric_equal (baln, zero))
    {
        priv->is_closed = TRUE;
    }
    else
    {
        priv->is_closed = FALSE;
    }

    return baln;
}

/* ============================================================= */

void
gnc_lot_get_balance_before (const GNCLot *lot, const Split *split,
                            gnc_numeric *amount, gnc_numeric *value)
{
    LotPrivate* priv;
    GList *node;
    gnc_numeric zero = gnc_numeric_zero();
    gnc_numeric amt = zero;
    gnc_numeric val = zero;

    *amount = amt;
    *value = val;
    if (lot == NULL) return;

    priv = GET_PRIVATE(lot);
    if (priv->splits)
    {
        Transaction *ta, *tb;
        const Split *target;
        /* If this is a gains split, find the source of the gains and use
           its transaction for the comparison.  Gains splits are in separate
           transactions that may sort after non-gains transactions.  */
        target = xaccSplitGetGainsSourceSplit (split);
        if (target == NULL)
            target = split;
        tb = xaccSplitGetParent (target);
        for (node = priv->splits; node; node = node->next)
        {
            Split *s = node->data;
            Split *source = xaccSplitGetGainsSourceSplit (s);
            if (source == NULL)
                source = s;
            ta = xaccSplitGetParent (source);
            if ((ta == tb && source != target) ||
                    xaccTransOrder (ta, tb) < 0)
            {
                gnc_numeric tmpval = xaccSplitGetAmount (s);
                amt = gnc_numeric_add_fixed (amt, tmpval);
                tmpval = xaccSplitGetValue (s);
                val = gnc_numeric_add_fixed (val, tmpval);
            }
        }
    }

    *amount = amt;
    *value = val;
}

/* ============================================================= */

void
gnc_lot_add_split (GNCLot *lot, Split *split)
{
    LotPrivate* priv;
    Account * acc;
    if (!lot || !split) return;
    priv = GET_PRIVATE(lot);

    ENTER ("(lot=%p, split=%p) %s amt=%s val=%s", lot, split,
           gnc_lot_get_title (lot),
           gnc_num_dbg_to_string (split->amount),
           gnc_num_dbg_to_string (split->value));
    gnc_lot_begin_edit(lot);
    acc = xaccSplitGetAccount (split);
    qof_instance_set_dirty(QOF_INSTANCE(lot));
    if (NULL == priv->account)
    {
        xaccAccountInsertLot (acc, lot);
    }
    else if (priv->account != acc)
    {
        PERR ("splits from different accounts cannot "
              "be added to this lot!\n"
              "\tlot account=\'%s\', split account=\'%s\'\n",
              xaccAccountGetName(priv->account), xaccAccountGetName (acc));
        gnc_lot_commit_edit(lot);
        LEAVE("different accounts");
        return;
    }

    if (lot == split->lot)
    {
        gnc_lot_commit_edit(lot);
        LEAVE("already in lot");
        return; /* handle not-uncommon no-op */
    }
    if (split->lot)
    {
        gnc_lot_remove_split (split->lot, split);
    }
    xaccSplitSetLot(split, lot);

    priv->splits = g_list_append (priv->splits, split);

    /* for recomputation of is-closed */
    priv->is_closed = LOT_CLOSED_UNKNOWN;
    gnc_lot_commit_edit(lot);

    qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_MODIFY, NULL);
    LEAVE("added to lot");
}

void
gnc_lot_remove_split (GNCLot *lot, Split *split)
{
    LotPrivate* priv;
    if (!lot || !split) return;
    priv = GET_PRIVATE(lot);

    ENTER ("(lot=%p, split=%p)", lot, split);
    gnc_lot_begin_edit(lot);
    qof_instance_set_dirty(QOF_INSTANCE(lot));
    priv->splits = g_list_remove (priv->splits, split);
    xaccSplitSetLot(split, NULL);
    priv->is_closed = LOT_CLOSED_UNKNOWN;   /* force an is-closed computation */

    if (NULL == priv->splits)
    {
        xaccAccountRemoveLot (priv->account, lot);
        priv->account = NULL;
    }
    gnc_lot_commit_edit(lot);
    qof_event_gen (QOF_INSTANCE(lot), QOF_EVENT_MODIFY, NULL);
    LEAVE("removed from lot");
}

/* ============================================================== */
/* Utility function, get earliest split in lot */

Split *
gnc_lot_get_earliest_split (GNCLot *lot)
{
    LotPrivate* priv;
    if (!lot) return NULL;
    priv = GET_PRIVATE(lot);
    if (! priv->splits) return NULL;
    priv->splits = g_list_sort (priv->splits, (GCompareFunc) xaccSplitOrderDateOnly);
    return priv->splits->data;
}

/* Utility function, get latest split in lot */
Split *
gnc_lot_get_latest_split (GNCLot *lot)
{
    LotPrivate* priv;
    SplitList *node;

    if (!lot) return NULL;
    priv = GET_PRIVATE(lot);
    if (! priv->splits) return NULL;
    priv->splits = g_list_sort (priv->splits, (GCompareFunc) xaccSplitOrderDateOnly);

    for (node = priv->splits; node->next; node = node->next)
        ;

    return node->data;
}

/* ============================================================= */

static void
destroy_lot_on_book_close(QofInstance *ent, gpointer data)
{
    GNCLot* lot = GNC_LOT(ent);

    gnc_lot_destroy(lot);
}

static void
gnc_lot_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_LOT);
    qof_collection_foreach(col, destroy_lot_on_book_close, NULL);
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject gncLotDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_LOT,
    DI(.type_label        = ) "Lot",
    DI(.create            = ) (gpointer)gnc_lot_new,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) gnc_lot_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer))qof_instance_version_cmp,
};


gboolean gnc_lot_register (void)
{
    static const QofParam params[] =
    {
        {
            LOT_TITLE, QOF_TYPE_STRING,
            (QofAccessFunc) gnc_lot_get_title,
            (QofSetterFunc) gnc_lot_set_title
        },
        {
            LOT_NOTES, QOF_TYPE_STRING,
            (QofAccessFunc) gnc_lot_get_notes,
            (QofSetterFunc) gnc_lot_set_notes
        },
        {
            QOF_PARAM_GUID, QOF_TYPE_GUID,
            (QofAccessFunc) qof_entity_get_guid, NULL
        },
        {
            QOF_PARAM_BOOK, QOF_ID_BOOK,
            (QofAccessFunc) gnc_lot_get_book, NULL
        },
        {
            LOT_IS_CLOSED, QOF_TYPE_BOOLEAN,
            (QofAccessFunc) gnc_lot_is_closed, NULL
        },
        {
            LOT_BALANCE, QOF_TYPE_NUMERIC,
            (QofAccessFunc) gnc_lot_get_balance, NULL
        },
        { NULL },
    };

    qof_class_register (GNC_ID_LOT, NULL, params);
    return qof_object_register(&gncLotDesc);
}

GNCLot * gnc_lot_make_default (Account *acc)
{
    GNCLot * lot;
    gint64 id = 0;
    gchar *buff;

    lot = gnc_lot_new (qof_instance_get_book(acc));

    /* Provide a reasonable title for the new lot */
    xaccAccountBeginEdit (acc);
    qof_instance_get (QOF_INSTANCE (acc), "lot-next-id", &id, NULL);
    buff = g_strdup_printf ("%s %" G_GINT64_FORMAT, _("Lot"), id);
    gnc_lot_set_title (lot, buff);
    id ++;
    qof_instance_set (QOF_INSTANCE (acc), "lot-next-id", id, NULL);
    xaccAccountCommitEdit (acc);
    g_free (buff);
    return lot;
}

/* ========================== END OF FILE ========================= */
