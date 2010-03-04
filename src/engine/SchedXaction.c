/********************************************************************\
 * SchedXaction.c -- Scheduled Transaction implementation.          *
 * Copyright (C) 2001,2007 Joshua Sled <jsled@asynchronous.org>     *
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

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <string.h>

#include "qof.h"

#include "Account.h"
#include "SX-book.h"
#include "SX-ttinfo.h"
#include "SchedXaction.h"
#include "Transaction.h"
#include "gnc-engine.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.engine.sx"

/* Local Prototypes *****/

void sxprivtransactionListMapDelete( gpointer data, gpointer user_data );

enum
{
    PROP_0,
    PROP_NAME,
    PROP_ENABLED,
    PROP_NUM_OCCURANCE,
    PROP_REM_OCCURANCE,
    PROP_AUTO_CREATE,
    PROP_AUTO_CREATE_NOTIFY,
    PROP_ADVANCE_CREATION_DAYS,
    PROP_ADVANCE_REMINDER_DAYS,
    PROP_START_DATE,
    PROP_END_DATE,
    PROP_LAST_OCCURANCE_DATE,
    PROP_INSTANCE_COUNT,
    PROP_TEMPLATE_ACCOUNT
};

/* GObject initialization */
G_DEFINE_TYPE(SchedXaction, gnc_schedxaction, QOF_TYPE_INSTANCE);

static void
gnc_schedxaction_init(SchedXaction* sx)
{
    sx->schedule = NULL;

    g_date_clear( &sx->last_date, 1 );
    g_date_clear( &sx->start_date, 1 );
    g_date_clear( &sx->end_date, 1 );

    sx->enabled = 1;
    sx->num_occurances_total = 0;
    sx->autoCreateOption = FALSE;
    sx->autoCreateNotify = FALSE;
    sx->advanceCreateDays = 0;
    sx->advanceRemindDays = 0;
    sx->instance_num = 0;
    sx->deferredList = NULL;
}

static void
gnc_schedxaction_dispose(GObject *sxp)
{
    G_OBJECT_CLASS(gnc_schedxaction_parent_class)->dispose(sxp);
}

static void
gnc_schedxaction_finalize(GObject* sxp)
{
    G_OBJECT_CLASS(gnc_schedxaction_parent_class)->finalize(sxp);
}

static void
gnc_schedxaction_get_property (GObject         *object,
                               guint            prop_id,
                               GValue          *value,
                               GParamSpec      *pspec)
{
    SchedXaction *sx;

    g_return_if_fail(GNC_IS_SCHEDXACTION(object));

    sx = GNC_SCHEDXACTION(object);
    switch (prop_id)
    {
    case PROP_NAME:
        g_value_set_string(value, sx->name);
        break;
    case PROP_ENABLED:
        g_value_set_boolean(value, sx->enabled);
        break;
    case PROP_NUM_OCCURANCE:
        g_value_set_int(value, sx->num_occurances_total);
        break;
    case PROP_REM_OCCURANCE:
        g_value_set_int(value, sx->num_occurances_remain);
        break;
    case PROP_AUTO_CREATE:
        g_value_set_boolean(value, sx->autoCreateOption);
        break;
    case PROP_AUTO_CREATE_NOTIFY:
        g_value_set_boolean(value, sx->autoCreateNotify);
        break;
    case PROP_ADVANCE_CREATION_DAYS:
        g_value_set_int(value, sx->advanceCreateDays);
        break;
    case PROP_ADVANCE_REMINDER_DAYS:
        g_value_set_int(value, sx->advanceRemindDays);
        break;
    case PROP_START_DATE:
        g_value_set_boxed(value, &sx->start_date);
        break;
    case PROP_END_DATE:
        g_value_set_boxed(value, &sx->end_date);
        break;
    case PROP_LAST_OCCURANCE_DATE:
        g_value_set_boxed(value, &sx->last_date);
        break;
    case PROP_INSTANCE_COUNT:
        g_value_set_int(value, sx->instance_num);
        break;
    case PROP_TEMPLATE_ACCOUNT:
        g_value_set_object(value, sx->template_acct);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_schedxaction_set_property (GObject         *object,
                               guint            prop_id,
                               const GValue     *value,
                               GParamSpec      *pspec)
{
    SchedXaction *sx;

    g_return_if_fail(GNC_IS_SCHEDXACTION(object));

    sx = GNC_SCHEDXACTION(object);
    switch (prop_id)
    {
    case PROP_NAME:
        xaccSchedXactionSetName(sx, g_value_get_string(value));
        break;
    case PROP_ENABLED:
        xaccSchedXactionSetEnabled(sx, g_value_get_boolean(value));
        break;
    case PROP_NUM_OCCURANCE:
        xaccSchedXactionSetNumOccur(sx, g_value_get_int(value));
        break;
    case PROP_REM_OCCURANCE:
        xaccSchedXactionSetRemOccur(sx, g_value_get_int(value));
        break;
    case PROP_AUTO_CREATE:
        xaccSchedXactionSetAutoCreate(sx, g_value_get_boolean(value), sx->autoCreateNotify);
        break;
    case PROP_AUTO_CREATE_NOTIFY:
        xaccSchedXactionSetAutoCreate(sx, sx->autoCreateOption, g_value_get_boolean(value));
        break;
    case PROP_ADVANCE_CREATION_DAYS:
        xaccSchedXactionSetAdvanceCreation(sx, g_value_get_int(value));
        break;
    case PROP_ADVANCE_REMINDER_DAYS:
        xaccSchedXactionSetAdvanceReminder(sx, g_value_get_int(value));
        break;
    case PROP_START_DATE:
        /* Note: when passed through a boxed gvalue, the julian value of the date is copied.
           The date may appear invalid until a function requiring for dmy calculation is
           called. */
        xaccSchedXactionSetStartDate(sx, g_value_get_boxed(value));
        break;
    case PROP_END_DATE:
        /* Note: when passed through a boxed gvalue, the julian value of the date is copied.
           The date may appear invalid until a function requiring for dmy calculation is
           called. */
        xaccSchedXactionSetEndDate(sx, g_value_get_boxed(value));
        break;
    case PROP_LAST_OCCURANCE_DATE:
        /* Note: when passed through a boxed gvalue, the julian value of the date is copied.
           The date may appear invalid until a function requiring for dmy calculation is
           called. */
        xaccSchedXactionSetLastOccurDate(sx, g_value_get_boxed(value));
        break;
    case PROP_INSTANCE_COUNT:
        gnc_sx_set_instance_count(sx, g_value_get_int(value));
        break;
    case PROP_TEMPLATE_ACCOUNT:
        sx_set_template_account(sx, g_value_get_object(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_schedxaction_class_init (SchedXactionClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    gobject_class->dispose = gnc_schedxaction_dispose;
    gobject_class->finalize = gnc_schedxaction_finalize;
    gobject_class->set_property = gnc_schedxaction_set_property;
    gobject_class->get_property = gnc_schedxaction_get_property;

    g_object_class_install_property
    (gobject_class,
     PROP_NAME,
     g_param_spec_string ("name",
                          "Scheduled Transaction Name",
                          "The name is an arbitrary string "
                          "assigned by the user.  It is intended to "
                          "a short, 5 to 30 character long string "
                          "that is displayed by the GUI.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ENABLED,
     g_param_spec_boolean ("enabled",
                           "Enabled",
                           "TRUE if the scheduled transaction is enabled.",
                           TRUE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_NUM_OCCURANCE,
     g_param_spec_int ("num-occurance",
                       "Number of occurances",
                       "Total number of occurances for this scheduled transaction.",
                       0,
                       G_MAXINT16,
                       1,
                       G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_REM_OCCURANCE,
     g_param_spec_int ("rem-occurance",
                       "Number of occurances remaining",
                       "Remaining number of occurances for this scheduled transaction.",
                       0,
                       G_MAXINT16,
                       1,
                       G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_AUTO_CREATE,
     g_param_spec_boolean ("auto-create",
                           "Auto-create",
                           "TRUE if the transaction will be automatically "
                           "created when its time comes.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_AUTO_CREATE_NOTIFY,
     g_param_spec_boolean ("auto-create-notify",
                           "Auto-create-notify",
                           "TRUE if the the user will be notified when the transaction "
                           "is automatically created.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ADVANCE_CREATION_DAYS,
     g_param_spec_int ("advance-creation-days",
                       "Days in advance to create",
                       "Number of days in advance to create this scheduled transaction.",
                       0,
                       G_MAXINT16,
                       0,
                       G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_ADVANCE_REMINDER_DAYS,
     g_param_spec_int ("advance-reminder-days",
                       "Days in advance to remind",
                       "Number of days in advance to remind about this scheduled transaction.",
                       0,
                       G_MAXINT16,
                       0,
                       G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_START_DATE,
     g_param_spec_boxed("start-date",
                        "Start Date",
                        "Date for the first occurence for the scheduled transaction.",
                        G_TYPE_DATE,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_END_DATE,
     g_param_spec_boxed("end-date",
                        "End Date",
                        "Date for the scheduled transaction to end.",
                        G_TYPE_DATE,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_LAST_OCCURANCE_DATE,
     g_param_spec_boxed("last-occurance-date",
                        "Last Occurance Date",
                        "Date for the last occurance of the scheduled transaction.",
                        G_TYPE_DATE,
                        G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_INSTANCE_COUNT,
     g_param_spec_int ("instance-count",
                       "Instance count",
                       "Number of instances of this scheduled transaction.",
                       0,
                       G_MAXINT16,
                       0,
                       G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_TEMPLATE_ACCOUNT,
     g_param_spec_object("template-account",
                        "Template account",
                        "Account which holds the template transactions.",
                        GNC_TYPE_ACCOUNT,
                        G_PARAM_READWRITE));
}

static void
xaccSchedXactionInit(SchedXaction *sx, QofBook *book)
{
    Account        *ra;
    const GUID *guid;

    qof_instance_init_data (&sx->inst, GNC_ID_SCHEDXACTION, book);

    /* create a new template account for our splits */
    sx->template_acct = xaccMallocAccount(book);
    guid = qof_instance_get_guid( sx );
    xaccAccountBeginEdit( sx->template_acct );
    xaccAccountSetName( sx->template_acct, guid_to_string( guid ));
    xaccAccountSetCommodity
    (sx->template_acct,
     gnc_commodity_table_lookup( gnc_commodity_table_get_table(book),
                                 "template", "template") );
    xaccAccountSetType( sx->template_acct, ACCT_TYPE_BANK );
    xaccAccountCommitEdit( sx->template_acct );
    ra = gnc_book_get_template_root( book );
    gnc_account_append_child( ra, sx->template_acct );
}

SchedXaction*
xaccSchedXactionMalloc(QofBook *book)
{
    SchedXaction *sx;

    g_return_val_if_fail (book, NULL);

    sx = g_object_new(GNC_TYPE_SCHEDXACTION, NULL);
    xaccSchedXactionInit( sx, book );
    qof_event_gen( &sx->inst, QOF_EVENT_CREATE , NULL);

    return sx;
}

static void
sxprivTransMapDelete( gpointer data, gpointer user_data )
{
    Transaction *t = (Transaction *) data;
    xaccTransBeginEdit( t );
    xaccTransDestroy( t );
    xaccTransCommitEdit( t );
    return;
}

static void
delete_template_trans(SchedXaction *sx)
{
    GList *templ_acct_splits, *curr_split_listref;
    Split *curr_split;
    Transaction *split_trans;
    GList *templ_acct_transactions = NULL;

    templ_acct_splits
    = xaccAccountGetSplitList(sx->template_acct);

    for (curr_split_listref = templ_acct_splits;
            curr_split_listref;
            curr_split_listref = curr_split_listref->next)
    {
        curr_split = (Split *) curr_split_listref->data;
        split_trans = xaccSplitGetParent(curr_split);
        if (! (g_list_find(templ_acct_transactions, split_trans)))
        {
            templ_acct_transactions
            = g_list_prepend(templ_acct_transactions, split_trans);
        }
    }

    g_list_foreach(templ_acct_transactions,
                   sxprivTransMapDelete,
                   NULL);

    return;
}

void
sx_set_template_account (SchedXaction *sx, Account *account)
{
    Account *old;

    old = sx->template_acct;
    sx->template_acct = account;
    if (old)
    {
        xaccAccountBeginEdit(old);
        xaccAccountDestroy(old);
    }
}

void
xaccSchedXactionDestroy( SchedXaction *sx )
{
    qof_instance_set_destroying( QOF_INSTANCE(sx), TRUE );
    gnc_sx_commit_edit( sx );
}

static void
xaccSchedXactionFree( SchedXaction *sx )
{
    GList *l;

    if ( sx == NULL ) return;

    qof_event_gen( &sx->inst, QOF_EVENT_DESTROY , NULL);

    if ( sx->name )
        g_free( sx->name );

    /*
     * we have to delete the transactions in the
     * template account ourselves
     */

    delete_template_trans( sx );

    /*
     * xaccAccountDestroy removes the account from
     * its group for us AFAICT
     */

    xaccAccountBeginEdit(sx->template_acct);
    xaccAccountDestroy(sx->template_acct);

    for ( l = sx->deferredList; l; l = l->next )
    {
        gnc_sx_destroy_temporal_state( l->data );
        l->data = NULL;
    }
    if ( sx->deferredList )
    {
        g_list_free( sx->deferredList );
        sx->deferredList = NULL;
    }

    /* qof_instance_release (&sx->inst); */
    g_object_unref( sx );
}

/* ============================================================ */

void
gnc_sx_begin_edit (SchedXaction *sx)
{
    qof_begin_edit (&sx->inst);
}

static void sx_free(QofInstance* inst )
{
    xaccSchedXactionFree( GNC_SX(inst) );
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
    g_critical("Failed to commit: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void commit_done(QofInstance *inst)
{
    qof_event_gen (inst, QOF_EVENT_MODIFY, NULL);
}

void
gnc_sx_commit_edit (SchedXaction *sx)
{
    if (!qof_commit_edit (QOF_INSTANCE(sx))) return;
    qof_commit_edit_part2 (&sx->inst, commit_err, commit_done, sx_free);
}

/* ============================================================ */

GList*
gnc_sx_get_schedule(const SchedXaction *sx)
{
    return sx->schedule;
}

void
gnc_sx_set_schedule(SchedXaction *sx, GList *schedule)
{
    g_return_if_fail(sx);
    gnc_sx_begin_edit(sx);
    sx->schedule = schedule;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

gchar *
xaccSchedXactionGetName( const SchedXaction *sx )
{
    return sx->name;
}

void
xaccSchedXactionSetName( SchedXaction *sx, const gchar *newName )
{
    g_return_if_fail( newName != NULL );
    gnc_sx_begin_edit(sx);
    if ( sx->name != NULL )
    {
        g_free( sx->name );
        sx->name = NULL;
    }
    sx->name = g_strdup( newName );
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

GDate*
xaccSchedXactionGetStartDate( SchedXaction *sx )
{
    return &sx->start_date;
}

void
xaccSchedXactionSetStartDate( SchedXaction *sx, GDate* newStart )
{
    gnc_sx_begin_edit(sx);
    sx->start_date = *newStart;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

gboolean
xaccSchedXactionHasEndDate( const SchedXaction *sx )
{
    return g_date_valid( &sx->end_date );
}

GDate*
xaccSchedXactionGetEndDate( SchedXaction *sx )
{
    return &sx->end_date;
}

void
xaccSchedXactionSetEndDate( SchedXaction *sx, GDate *newEnd )
{
    if ( g_date_valid( newEnd )
            && g_date_compare( newEnd, &sx->start_date ) < 0 )
    {
        /* XXX: I reject the bad data - is this the right
         * thing to do <rgmerk>.
         * This warning is only human readable - the caller
         * doesn't know the call failed.  This is bad
         */
        g_critical("New end date before start date");
        return;
    }

    gnc_sx_begin_edit(sx);
    sx->end_date = *newEnd;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

GDate*
xaccSchedXactionGetLastOccurDate( SchedXaction *sx )
{
    return &sx->last_date;
}

void
xaccSchedXactionSetLastOccurDate(SchedXaction *sx, GDate* new_last_occur)
{
    if (g_date_valid(&sx->last_date)
            && g_date_compare(&sx->last_date, new_last_occur) == 0)
        return;
    gnc_sx_begin_edit(sx);
    sx->last_date = *new_last_occur;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

gboolean
xaccSchedXactionHasOccurDef( const SchedXaction *sx )
{
    return ( xaccSchedXactionGetNumOccur( sx ) != 0 );
}

gint
xaccSchedXactionGetNumOccur( const SchedXaction *sx )
{
    return sx->num_occurances_total;
}

void
xaccSchedXactionSetNumOccur(SchedXaction *sx, gint new_num)
{
    if (sx->num_occurances_total == new_num)
        return;
    gnc_sx_begin_edit(sx);
    sx->num_occurances_remain = sx->num_occurances_total = new_num;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

gint
xaccSchedXactionGetRemOccur( const SchedXaction *sx )
{
    return sx->num_occurances_remain;
}

void
xaccSchedXactionSetRemOccur(SchedXaction *sx, gint num_remain)
{
    /* FIXME This condition can be tightened up */
    if (num_remain > sx->num_occurances_total)
    {
        g_warning("number remaining [%d] > total occurrences [%d]",
                  num_remain, sx->num_occurances_total);
    }
    else
    {
        if (num_remain == sx->num_occurances_remain)
            return;
        gnc_sx_begin_edit(sx);
        sx->num_occurances_remain = num_remain;
        qof_instance_set_dirty(&sx->inst);
        gnc_sx_commit_edit(sx);
    }
}


KvpValue *
xaccSchedXactionGetSlot( const SchedXaction *sx, const char *slot )
{
    if (!sx) return NULL;

    return kvp_frame_get_slot(sx->inst.kvp_data, slot);
}

void
xaccSchedXactionSetSlot( SchedXaction *sx,
                         const char *slot,
                         const KvpValue *value )
{
    if (!sx) return;

    gnc_sx_begin_edit(sx);
    kvp_frame_set_slot( sx->inst.kvp_data, slot, value );
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

gboolean
xaccSchedXactionGetEnabled( const SchedXaction *sx )
{
    return sx->enabled;
}

void
xaccSchedXactionSetEnabled( SchedXaction *sx, gboolean newEnabled)
{
    gnc_sx_begin_edit(sx);
    sx->enabled = newEnabled;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

void
xaccSchedXactionGetAutoCreate( const SchedXaction *sx,
                               gboolean *outAutoCreate,
                               gboolean *outNotify )
{
    if (outAutoCreate != NULL)
        *outAutoCreate = sx->autoCreateOption;
    if (outNotify != NULL)
        *outNotify     = sx->autoCreateNotify;
    return;
}

void
xaccSchedXactionSetAutoCreate( SchedXaction *sx,
                               gboolean newAutoCreate,
                               gboolean newNotify )
{

    gnc_sx_begin_edit(sx);
    sx->autoCreateOption = newAutoCreate;
    sx->autoCreateNotify = newNotify;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
    return;
}

gint
xaccSchedXactionGetAdvanceCreation( const SchedXaction *sx )
{
    return sx->advanceCreateDays;
}

void
xaccSchedXactionSetAdvanceCreation( SchedXaction *sx, gint createDays )
{
    gnc_sx_begin_edit(sx);
    sx->advanceCreateDays = createDays;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

gint
xaccSchedXactionGetAdvanceReminder( const SchedXaction *sx )
{
    return sx->advanceRemindDays;
}

void
xaccSchedXactionSetAdvanceReminder( SchedXaction *sx, gint reminderDays )
{
    gnc_sx_begin_edit(sx);
    sx->advanceRemindDays = reminderDays;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}


GDate
xaccSchedXactionGetNextInstance( SchedXaction *sx, void *stateData )
{
    GDate    last_occur, next_occur, tmpDate;

    g_date_clear( &last_occur, 1 );
    g_date_clear( &next_occur, 1 );
    g_date_clear( &tmpDate, 1 );

    if ( g_date_valid( &sx->last_date ) )
    {
        last_occur = sx->last_date;
    }

    if ( stateData != NULL )
    {
        temporalStateData *tsd = (temporalStateData*)stateData;
        last_occur = tsd->last_date;
    }

    if ( g_date_valid( &sx->start_date ) )
    {
        if ( g_date_valid(&last_occur) )
        {
            last_occur =
                ( g_date_compare( &last_occur,
                                  &sx->start_date ) > 0 ?
                  last_occur : sx->start_date );
        }
        else
        {
            /* Think about this for a second, and you realize that if the
             * start date is _today_, we need a last-occur date such that
             * the 'next instance' is after that date, and equal to the
             * start date... one day should be good.
             *
             * This only holds for the first instance [read: if the
             * last[-occur]_date is invalid] */
            last_occur = sx->start_date;
            g_date_subtract_days( &last_occur, 1 );
        }
    }

    recurrenceListNextInstance(sx->schedule, &last_occur, &next_occur);

    /* out-of-bounds check */
    if ( xaccSchedXactionHasEndDate( sx ) )
    {
        GDate *end_date = xaccSchedXactionGetEndDate( sx );
        if ( g_date_compare( &next_occur, end_date ) > 0 )
        {
            g_debug("next_occur past end date");
            g_date_clear( &next_occur, 1 );
        }
    }
    else if ( xaccSchedXactionHasOccurDef( sx ) )
    {
        if ( stateData )
        {
            temporalStateData *tsd = (temporalStateData*)stateData;
            if ( tsd->num_occur_rem == 0 )
            {
                g_debug("no more occurances remain");
                g_date_clear( &next_occur, 1 );
            }
        }
        else
        {
            if ( sx->num_occurances_remain == 0 )
            {
                g_date_clear( &next_occur, 1 );
            }
        }
    }

    return next_occur;
}

GDate
xaccSchedXactionGetInstanceAfter( SchedXaction *sx,
                                  GDate *date,
                                  void *stateData )
{
    GDate prev_occur, next_occur;

    g_date_clear( &prev_occur, 1 );
    if ( date )
    {
        prev_occur = *date;
    }

    if ( stateData != NULL )
    {
        temporalStateData *tsd = (temporalStateData*)stateData;
        prev_occur = tsd->last_date;
    }

    if ( ! g_date_valid( &prev_occur ) )
    {
        /* We must be at the beginning. */
        prev_occur = sx->start_date;
        g_date_subtract_days( &prev_occur, 1 );
    }

    recurrenceListNextInstance(sx->schedule, &prev_occur, &next_occur);

    if ( xaccSchedXactionHasEndDate( sx ) )
    {
        GDate *end_date;

        end_date = xaccSchedXactionGetEndDate( sx );
        if ( g_date_compare( &next_occur, end_date ) > 0 )
        {
            g_date_clear( &next_occur, 1 );
        }
    }
    else if ( xaccSchedXactionHasOccurDef( sx ) )
    {
        if ( stateData )
        {
            temporalStateData *tsd = (temporalStateData*)stateData;
            if ( tsd->num_occur_rem == 0 )
            {
                g_date_clear( &next_occur, 1 );
            }
        }
        else
        {
            if ( sx->num_occurances_remain == 0 )
            {
                g_date_clear( &next_occur, 1 );
            }
        }
    }
    return next_occur;
}

gint
gnc_sx_get_instance_count( const SchedXaction *sx, void *stateData )
{
    gint toRet = -1;
    temporalStateData *tsd;

    if ( stateData )
    {
        tsd = (temporalStateData*)stateData;
        toRet = tsd->num_inst;
    }
    else
    {
        toRet = sx->instance_num;
    }

    return toRet;
}

void
gnc_sx_set_instance_count(SchedXaction *sx, gint instance_num)
{
    g_return_if_fail(sx);
    if (sx->instance_num == instance_num)
        return;
    sx->instance_num = instance_num;
}

GList *
xaccSchedXactionGetSplits( const SchedXaction *sx )
{
    g_return_val_if_fail( sx, NULL );
    return xaccAccountGetSplitList(sx->template_acct);
}

static Split *
pack_split_info (TTSplitInfo *s_info, Account *parent_acct,
                 Transaction *parent_trans, QofBook *book)
{
    Split *split;
    KvpFrame *split_frame;
    KvpValue *tmp_value;
    const GUID *acc_guid;

    split = xaccMallocSplit(book);

    xaccSplitSetMemo(split,
                     gnc_ttsplitinfo_get_memo(s_info));

    xaccSplitSetAction(split,
                       gnc_ttsplitinfo_get_action(s_info));


    xaccAccountInsertSplit(parent_acct,
                           split);

    split_frame = xaccSplitGetSlots(split);

    tmp_value
    = kvp_value_new_string(gnc_ttsplitinfo_get_credit_formula(s_info));

    kvp_frame_set_slot_path(split_frame,
                            tmp_value,
                            GNC_SX_ID,
                            GNC_SX_CREDIT_FORMULA,
                            NULL);
    kvp_value_delete(tmp_value);

    tmp_value
    = kvp_value_new_string(gnc_ttsplitinfo_get_debit_formula(s_info));

    kvp_frame_set_slot_path(split_frame,
                            tmp_value,
                            GNC_SX_ID,
                            GNC_SX_DEBIT_FORMULA,
                            NULL);

    kvp_value_delete(tmp_value);

    acc_guid = qof_entity_get_guid(QOF_INSTANCE(gnc_ttsplitinfo_get_account(s_info)));

    tmp_value = kvp_value_new_guid(acc_guid);

    kvp_frame_set_slot_path(split_frame,
                            tmp_value,
                            GNC_SX_ID,
                            GNC_SX_ACCOUNT,
                            NULL);

    kvp_value_delete(tmp_value);

    return split;
}


void
xaccSchedXactionSetTemplateTrans(SchedXaction *sx, GList *t_t_list,
                                 QofBook *book)
{
    Transaction *new_trans;
    TTInfo *tti;
    TTSplitInfo *s_info;
    Split *new_split;
    GList *split_list;

    g_return_if_fail (book);

    /* delete any old transactions, if there are any */
    delete_template_trans( sx );

    for (; t_t_list != NULL; t_t_list = t_t_list->next)
    {
        tti = t_t_list->data;

        new_trans = xaccMallocTransaction(book);

        xaccTransBeginEdit(new_trans);

        xaccTransSetDescription(new_trans,
                                gnc_ttinfo_get_description(tti));

        xaccTransSetDatePostedSecs(new_trans, time(NULL));

        xaccTransSetNum(new_trans,
                        gnc_ttinfo_get_num(tti));
        xaccTransSetCurrency( new_trans,
                              gnc_ttinfo_get_currency(tti) );

        for (split_list = gnc_ttinfo_get_template_splits(tti);
                split_list;
                split_list = split_list->next)
        {
            s_info = split_list->data;
            new_split = pack_split_info(s_info, sx->template_acct,
                                        new_trans, book);
            xaccTransAppendSplit(new_trans, new_split);
        }
        xaccTransCommitEdit(new_trans);
    }
}

void*
gnc_sx_create_temporal_state( SchedXaction *sx )
{
    temporalStateData *toRet =
        g_new0( temporalStateData, 1 );
    toRet->last_date       = sx->last_date;
    toRet->num_occur_rem   = sx->num_occurances_remain;
    toRet->num_inst   = sx->instance_num;
    return (void*)toRet;
}

void
gnc_sx_incr_temporal_state( SchedXaction *sx, void *stateData )
{
    GDate unused;
    temporalStateData *tsd = (temporalStateData*)stateData;

    g_date_clear( &unused, 1 );
    tsd->last_date =
        xaccSchedXactionGetInstanceAfter( sx,
                                          &unused,
                                          stateData );
    if ( xaccSchedXactionHasOccurDef( sx ) )
    {
        tsd->num_occur_rem -= 1;
    }
    tsd->num_inst += 1;
}

void
gnc_sx_revert_to_temporal_state( SchedXaction *sx, void *stateData )
{
    temporalStateData *tsd = (temporalStateData*)stateData;
    gnc_sx_begin_edit(sx);
    sx->last_date        = tsd->last_date;
    sx->num_occurances_remain = tsd->num_occur_rem;
    sx->instance_num     = tsd->num_inst;
    qof_instance_set_dirty(&sx->inst);
    gnc_sx_commit_edit(sx);
}

void
gnc_sx_destroy_temporal_state( void *stateData )
{
    g_free( (temporalStateData*)stateData );
}

void*
gnc_sx_clone_temporal_state( void *stateData )
{
    temporalStateData *toRet, *tsd;
    tsd = (temporalStateData*)stateData;
    toRet = g_memdup( tsd, sizeof( temporalStateData ) );
    return (void*)toRet;
}

static
gint
_temporal_state_data_cmp( gconstpointer a, gconstpointer b )
{
    temporalStateData *tsd_a, *tsd_b;
    tsd_a = (temporalStateData*)a;
    tsd_b = (temporalStateData*)b;

    if ( !tsd_a && !tsd_b )
        return 0;
    if (tsd_a == tsd_b)
        return 0;
    if ( !tsd_a )
        return 1;
    if ( !tsd_b )
        return -1;
    return g_date_compare( &tsd_a->last_date,
                           &tsd_b->last_date );
}

/**
 * Adds an instance to the deferred list of the SX.  Added instances are
 * added in (date-)sorted order.
 **/
void
gnc_sx_add_defer_instance( SchedXaction *sx, void *deferStateData )
{
    sx->deferredList = g_list_insert_sorted( sx->deferredList,
                       deferStateData,
                       _temporal_state_data_cmp );
}

/**
 * Removes an instance from the deferred list.  If the instance is no longer
 * useful; gnc_sx_destroy_temporal_state() it.
 **/
void
gnc_sx_remove_defer_instance( SchedXaction *sx, void *deferStateData )
{
    GList *found_by_value;

    found_by_value = g_list_find_custom(
                         sx->deferredList, deferStateData, _temporal_state_data_cmp);
    if (found_by_value == NULL)
    {
        g_warning("unable to find deferred instance");
        return;
    }

    gnc_sx_destroy_temporal_state(found_by_value->data);
    sx->deferredList = g_list_delete_link(sx->deferredList, found_by_value);
}

/**
 * Returns the defer list from the SX; this is a (date-)sorted
 * temporal-state-data instance list.  The list should not be modified by the
 * caller; use the gnc_sx_{add,remove}_defer_instance() functions to modifiy
 * the list.
 **/
GList*
gnc_sx_get_defer_instances( SchedXaction *sx )
{
    return sx->deferredList;
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject SXDesc =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_SX_ID,
    DI(.type_label        = ) "Scheduled Transaction",
    DI(.create            = ) (gpointer)xaccSchedXactionMalloc,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) NULL,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean
SXRegister(void)
{
    static QofParam params[] =
    {
        {
            GNC_SX_NAME, QOF_TYPE_STRING, (QofAccessFunc)xaccSchedXactionGetName,
            (QofSetterFunc)xaccSchedXactionSetName
        },
        {
            GNC_SX_START_DATE, QOF_TYPE_DATE, (QofAccessFunc)xaccSchedXactionGetStartDate,
            (QofSetterFunc)xaccSchedXactionSetStartDate
        },
        {
            GNC_SX_LAST_DATE, QOF_TYPE_DATE, (QofAccessFunc)xaccSchedXactionGetLastOccurDate,
            (QofSetterFunc)xaccSchedXactionSetLastOccurDate
        },
        {
            GNC_SX_NUM_OCCUR, QOF_TYPE_INT64, (QofAccessFunc)xaccSchedXactionGetNumOccur,
            (QofSetterFunc)xaccSchedXactionSetNumOccur
        },
        {
            GNC_SX_REM_OCCUR, QOF_TYPE_INT64, (QofAccessFunc)xaccSchedXactionGetRemOccur,
            (QofSetterFunc)xaccSchedXactionSetRemOccur
        },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };
    qof_class_register(GNC_SX_ID, NULL, params);
    return qof_object_register(&SXDesc);
}
