/********************************************************************\
 * gnc-budget.c -- Implementation of the top level Budgeting API.   *
 * Copyright (C) 04 sep 2003    Darin Willits <darin@willits.ca>    *
 * Copyright (C) 2005-2006 Chris Shoemaker <c.shoemaker@cox.net>    *
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
#include <qof.h>
#include <qofbookslots.h>
#include <qofinstance-p.h>
#include <optional>
#include <unordered_map>
#include <vector>

#include "Account.h"

#include "guid.hpp"
#include "gnc-budget.h"
#include "gnc-commodity.h"

static QofLogModule log_module = GNC_MOD_ENGINE;

enum
{
    PROP_0,
    PROP_NAME,                  /* Table */
    PROP_DESCRIPTION,           /* Table */
    PROP_NUM_PERIODS,           /* Table */
    PROP_RUNTIME_0,
    PROP_RECURRENCE,            /* Cached pointer; Recurrence table holds budget guid */
};

struct budget_s
{
    QofInstance inst;
};

typedef struct
{
    QofInstanceClass parent_class;
} BudgetClass;

struct PeriodData
{
    std::string note;
    std::optional<gnc_numeric> opt_value;
    PeriodData () = default;
    PeriodData (const char* note, std::optional<gnc_numeric> opt_value)
        : note (note)
        , opt_value (opt_value) {};
};

using PeriodDataVec = std::vector<PeriodData>;
using AcctMap = std::unordered_map<const Account*, PeriodDataVec>;
using StringVec = std::vector<std::string>;

typedef struct GncBudgetPrivate
{
    /* The name is an arbitrary string assigned by the user. */
    const gchar *name;

    /* The description is an arbitrary string assigned by the user. */
    const gchar *description;

    /* Recurrence (period info) for the budget */
    Recurrence recurrence;

    AcctMap acct_map;

    /* Number of periods */
    guint  num_periods;
} GncBudgetPrivate;

#define GET_PRIVATE(o) \
    ((GncBudgetPrivate*)gnc_budget_get_instance_private((GncBudget*)o))

struct _GncBudgetClass
{
    QofInstanceClass parent_class;
};

/* GObject Initialization */
G_DEFINE_TYPE_WITH_PRIVATE(GncBudget, gnc_budget, QOF_TYPE_INSTANCE)

static void
gnc_budget_init(GncBudget* budget)
{
    GncBudgetPrivate* priv;
    GDate *date;

    priv = GET_PRIVATE(budget);
    priv->name = CACHE_INSERT(_("Unnamed Budget"));
    priv->description = CACHE_INSERT("");
    new (&priv->acct_map) AcctMap ();

    priv->num_periods = 12;
    date = gnc_g_date_new_today ();
    g_date_subtract_days(date, g_date_get_day(date) - 1);
    recurrenceSet(&priv->recurrence, 1, PERIOD_MONTH, date, WEEKEND_ADJ_NONE);
    g_date_free (date);
}

static void
gnc_budget_dispose (GObject *budgetp)
{
    G_OBJECT_CLASS(gnc_budget_parent_class)->dispose(budgetp);
}

static void
gnc_budget_finalize(GObject* budgetp)
{
    G_OBJECT_CLASS(gnc_budget_parent_class)->finalize(budgetp);
}

static void
gnc_budget_get_property( GObject* object,
                         guint prop_id,
                         GValue* value,
                         GParamSpec* pspec)
{
    GncBudget* budget;
    GncBudgetPrivate* priv;

    g_return_if_fail(GNC_IS_BUDGET(object));

    budget = GNC_BUDGET(object);
    priv = GET_PRIVATE(budget);
    switch ( prop_id )
    {
    case PROP_NAME:
        g_value_set_string(value, priv->name);
        break;
    case PROP_DESCRIPTION:
        g_value_set_string(value, priv->description);
        break;
    case PROP_NUM_PERIODS:
        g_value_set_uint(value, priv->num_periods);
        break;
    case PROP_RECURRENCE:
        /* TODO: Make this a BOXED type */
        g_value_set_pointer(value, &priv->recurrence);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_budget_set_property( GObject* object,
                         guint prop_id,
                         const GValue* value,
                         GParamSpec* pspec)
{
    GncBudget* budget;

    g_return_if_fail(GNC_IS_BUDGET(object));

    budget = GNC_BUDGET(object);
    if (prop_id < PROP_RUNTIME_0)
        g_assert (qof_instance_get_editlevel(budget));

    switch ( prop_id )
    {
    case PROP_NAME:
        gnc_budget_set_name(budget, g_value_get_string(value));
        break;
    case PROP_DESCRIPTION:
        gnc_budget_set_description(budget, g_value_get_string(value));
        break;
    case PROP_NUM_PERIODS:
        gnc_budget_set_num_periods(budget, g_value_get_uint(value));
        break;
    case PROP_RECURRENCE:
        gnc_budget_set_recurrence (budget, static_cast<Recurrence*>(g_value_get_pointer(value)));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_budget_class_init(GncBudgetClass* klass)
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_budget_dispose;
    gobject_class->finalize = gnc_budget_finalize;
    gobject_class->get_property = gnc_budget_get_property;
    gobject_class->set_property = gnc_budget_set_property;

    g_object_class_install_property(
        gobject_class,
        PROP_NAME,
        g_param_spec_string( "name",
                             "Budget Name",
                             "The name is an arbitrary string "
                             "assigned by the user.  It is intended "
                             "to be a short, 5 to 30 character long string "
                             "that is displayed by the GUI as the "
                             "budget mnemonic",
                             nullptr,
                             G_PARAM_READWRITE));

    g_object_class_install_property(
        gobject_class,
        PROP_DESCRIPTION,
        g_param_spec_string( "description",
                             "Budget Description",
                             "The description is an arbitrary string "
                             "assigned by the user.  It is intended "
                             "to be a longer, 1-5 sentence description of "
                             "what the budget is all about.",
                             nullptr,
                             G_PARAM_READWRITE));

    g_object_class_install_property(
        gobject_class,
        PROP_NUM_PERIODS,
        g_param_spec_uint( "num-periods",
                           "Number of Periods",
                           "The number of periods for this budget.",
                           0,
                           G_MAXUINT32,
                           12,
                           G_PARAM_READWRITE));

    g_object_class_install_property(
        gobject_class,
        PROP_RECURRENCE,
        g_param_spec_pointer( "recurrence",
                              "Budget Recurrence",
                              "about.",
                              G_PARAM_READWRITE));
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
    PERR ("Failed to commit: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void
gnc_budget_free(QofInstance *inst)
{
    GncBudget *budget;
    GncBudgetPrivate* priv;

    if (inst == nullptr)
        return;
    g_return_if_fail(GNC_IS_BUDGET(inst));

    budget = GNC_BUDGET(inst);
    priv = GET_PRIVATE(budget);

    /* We first send the message that this object is about to be
     * destroyed so that any GUI elements can remove it before it is
     * actually gone. */
    qof_event_gen( &budget->inst, QOF_EVENT_DESTROY, nullptr);

    CACHE_REMOVE(priv->name);
    CACHE_REMOVE(priv->description);
    priv->acct_map.~AcctMap();

    /* qof_instance_release (&budget->inst); */
    g_object_unref(budget);
}

static void noop (QofInstance *inst) {}

void
gnc_budget_begin_edit(GncBudget *bgt)
{
    qof_begin_edit(QOF_INSTANCE(bgt));
}

void
gnc_budget_commit_edit(GncBudget *bgt)
{
    if (!qof_commit_edit(QOF_INSTANCE(bgt))) return;
    qof_commit_edit_part2(QOF_INSTANCE(bgt), commit_err,
                          noop, gnc_budget_free);
}

GncBudget*
gnc_budget_new(QofBook *book)
{
    g_return_val_if_fail(book, nullptr);

    ENTER(" ");

    auto budget { static_cast<GncBudget*>(g_object_new(GNC_TYPE_BUDGET, nullptr)) };
    qof_instance_init_data (&budget->inst, GNC_ID_BUDGET, book);

    qof_event_gen( &budget->inst, QOF_EVENT_CREATE , nullptr);

    LEAVE(" ");
    return budget;
}

void
gnc_budget_destroy(GncBudget *budget)
{
    g_return_if_fail(GNC_IS_BUDGET(budget));
    gnc_budget_begin_edit(budget);
    qof_instance_set_dirty(&budget->inst);
    qof_instance_set_destroying(budget, TRUE);
    gnc_budget_commit_edit(budget);
}

/** Data structure for containing info while cloning budget values */
typedef struct
{
    const GncBudget* old_b;
    GncBudget* new_b;
    guint num_periods;
} CloneBudgetData_t;

static void
clone_budget_values_cb(Account* a, gpointer user_data)
{
    CloneBudgetData_t* data = (CloneBudgetData_t*)user_data;
    guint i;

    for ( i = 0; i < data->num_periods; ++i )
    {
        if ( gnc_budget_is_account_period_value_set(data->old_b, a, i) )
        {
            gnc_budget_set_account_period_value(data->new_b, a, i,
                                                gnc_budget_get_account_period_value(data->old_b, a, i));
        }
    }
}

GncBudget*
gnc_budget_clone(const GncBudget* old_b)
{
    GncBudget* new_b;
    Account* root;
    CloneBudgetData_t clone_data;

    g_return_val_if_fail(old_b != nullptr, nullptr);

    ENTER(" ");

    new_b = gnc_budget_new(qof_instance_get_book(old_b));
    gnc_budget_begin_edit(new_b);
    gnc_budget_set_name(new_b, gnc_budget_get_name(old_b));
    gnc_budget_set_description(new_b, gnc_budget_get_description(old_b));
    gnc_budget_set_recurrence(new_b, gnc_budget_get_recurrence(old_b));
    gnc_budget_set_num_periods(new_b, gnc_budget_get_num_periods(old_b));

    root = gnc_book_get_root_account(qof_instance_get_book(old_b));
    clone_data.old_b = old_b;
    clone_data.new_b = new_b;
    clone_data.num_periods = gnc_budget_get_num_periods(new_b);
    gnc_account_foreach_descendant(root, clone_budget_values_cb, &clone_data);

    gnc_budget_commit_edit(new_b);

    LEAVE(" ");

    return new_b;
}

void
gnc_budget_set_name(GncBudget* budget, const gchar* name)
{
    GncBudgetPrivate* priv;

    g_return_if_fail(GNC_IS_BUDGET(budget) && name);

    priv = GET_PRIVATE(budget);
    if ( name == priv->name ) return;

    gnc_budget_begin_edit(budget);
    CACHE_REPLACE(priv->name, name);
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen( &budget->inst, QOF_EVENT_MODIFY, nullptr);
}

const gchar*
gnc_budget_get_name(const GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), nullptr);
    return GET_PRIVATE(budget)->name;
}

void
gnc_budget_set_description(GncBudget* budget, const gchar* description)
{
    GncBudgetPrivate* priv;

    g_return_if_fail(GNC_IS_BUDGET(budget));
    g_return_if_fail(description);

    priv = GET_PRIVATE(budget);
    if ( description == priv->description ) return;
    gnc_budget_begin_edit(budget);
    CACHE_REPLACE(priv->description, description);
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen( &budget->inst, QOF_EVENT_MODIFY, nullptr);
}

const gchar*
gnc_budget_get_description(const GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), nullptr);
    return GET_PRIVATE(budget)->description;
}

void
gnc_budget_set_recurrence(GncBudget *budget, const Recurrence *r)
{
    GncBudgetPrivate* priv;

    g_return_if_fail(budget && r);
    priv = GET_PRIVATE(budget);

    gnc_budget_begin_edit(budget);
    priv->recurrence = *r;
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen(&budget->inst, QOF_EVENT_MODIFY, nullptr);
}

const Recurrence *
gnc_budget_get_recurrence(const GncBudget *budget)
{
    g_return_val_if_fail(budget, nullptr);
    return (&GET_PRIVATE(budget)->recurrence);
}

const GncGUID*
gnc_budget_get_guid(const GncBudget* budget)
{
    g_return_val_if_fail(budget, nullptr);
    g_return_val_if_fail(GNC_IS_BUDGET(budget), nullptr);
    return qof_instance_get_guid(QOF_INSTANCE(budget));
}

void
gnc_budget_set_num_periods(GncBudget* budget, guint num_periods)
{
    GncBudgetPrivate* priv;

    g_return_if_fail(GNC_IS_BUDGET(budget));
    g_return_if_fail(num_periods > 0);

    priv = GET_PRIVATE(budget);
    if ( priv->num_periods == num_periods ) return;

    gnc_budget_begin_edit(budget);
    priv->num_periods = num_periods;
    std::for_each (priv->acct_map.begin(), priv->acct_map.end(),
                   [num_periods](auto& it){ it.second.resize(num_periods); });
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen( &budget->inst, QOF_EVENT_MODIFY, nullptr);
}

guint
gnc_budget_get_num_periods(const GncBudget* budget)
{
    g_return_val_if_fail(GNC_IS_BUDGET(budget), 0);
    return GET_PRIVATE(budget)->num_periods;
}

static inline StringVec
make_period_data_path (const Account *account, guint period_num)
{
    gnc::GUID acct_guid {*(xaccAccountGetGUID (account))};
    return { acct_guid.to_string(), std::to_string (period_num) };
}

static inline StringVec
make_period_note_path (const Account *account, guint period_num)
{
    StringVec path { GNC_BUDGET_NOTES_PATH };
    StringVec data_path { make_period_data_path (account, period_num) };
    std::move (data_path.begin(), data_path.end(), std::back_inserter (path));
    return path;
}

static PeriodData& get_perioddata (const GncBudget *budget,
                                   const Account *account,
                                   guint period_num);

/* period_num is zero-based */
/* What happens when account is deleted, after we have an entry for it? */
void
gnc_budget_unset_account_period_value(GncBudget *budget, const Account *account,
                                      guint period_num)
{
    g_return_if_fail (budget != nullptr);
    g_return_if_fail (account != nullptr);
    g_return_if_fail (period_num < GET_PRIVATE(budget)->num_periods);

    auto& data = get_perioddata (budget, account, period_num);
    data.opt_value.reset();

    gnc_budget_begin_edit(budget);
    auto path = make_period_data_path (account, period_num);
    auto budget_kvp { QOF_INSTANCE (budget)->kvp_data };
    delete budget_kvp->set_path (path, nullptr);
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen( &budget->inst, QOF_EVENT_MODIFY, nullptr);

}

/* period_num is zero-based */
/* What happens when account is deleted, after we have an entry for it? */
void
gnc_budget_set_account_period_value(GncBudget *budget, const Account *account,
                                    guint period_num, gnc_numeric val)
{
    /* Watch out for an off-by-one error here:
     * period_num starts from 0 while num_periods starts from 1 */
    if (period_num >= GET_PRIVATE(budget)->num_periods)
    {
        PWARN("Period %i does not exist", period_num);
        return;
    }

    g_return_if_fail (budget != nullptr);
    g_return_if_fail (account != nullptr);

    auto& perioddata = get_perioddata (budget, account, period_num);
    auto budget_kvp { QOF_INSTANCE (budget)->kvp_data };
    auto path = make_period_data_path (account, period_num);

    gnc_budget_begin_edit(budget);
    if (gnc_numeric_check(val))
    {
        delete budget_kvp->set_path (path, nullptr);
        perioddata.opt_value.reset();
    }
    else
    {
        KvpValue* v = new KvpValue (val);
        delete budget_kvp->set_path (path, v);
        perioddata.opt_value = val;
    }
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen( &budget->inst, QOF_EVENT_MODIFY, nullptr);

}

gboolean
gnc_budget_is_account_period_value_set (const GncBudget *budget,
                                        const Account *account,
                                        guint period_num)
{
    g_return_val_if_fail (period_num < GET_PRIVATE(budget)->num_periods, false);
    return get_perioddata (budget, account, period_num).opt_value.has_value();
}

gnc_numeric
gnc_budget_get_account_period_value (const GncBudget *budget,
                                     const Account *account,
                                     guint period_num)
{
    g_return_val_if_fail (period_num < GET_PRIVATE(budget)->num_periods,
                          gnc_numeric_zero());
    auto& data = get_perioddata (budget, account, period_num);

    return data.opt_value.has_value() ? data.opt_value.value() : gnc_numeric_zero();
}

void
gnc_budget_set_account_period_note(GncBudget *budget, const Account *account,
                                    guint period_num, const gchar *note)
{
    /* Watch out for an off-by-one error here:
     * period_num starts from 0 while num_periods starts from 1 */
    if (period_num >= GET_PRIVATE(budget)->num_periods)
    {
        PWARN("Period %i does not exist", period_num);
        return;
    }

    g_return_if_fail (budget != nullptr);
    g_return_if_fail (account != nullptr);

    auto& perioddata = get_perioddata (budget, account, period_num);
    auto budget_kvp { QOF_INSTANCE (budget)->kvp_data };
    auto path = make_period_note_path (account, period_num);

    gnc_budget_begin_edit(budget);
    if (note == nullptr)
    {
        delete budget_kvp->set_path (path, nullptr);
        perioddata.note.clear ();
    }
    else
    {
        KvpValue* v = new KvpValue (g_strdup (note));

        delete budget_kvp->set_path (path, v);
        perioddata.note = note;
    }
    qof_instance_set_dirty(&budget->inst);
    gnc_budget_commit_edit(budget);

    qof_event_gen( &budget->inst, QOF_EVENT_MODIFY, nullptr);

}

const gchar *
gnc_budget_get_account_period_note (const GncBudget *budget,
                                    const Account *account, guint period_num)
{
    g_return_val_if_fail (period_num < GET_PRIVATE(budget)->num_periods, nullptr);
    auto& data = get_perioddata (budget, account, period_num);
    return data.note.empty () ? nullptr : data.note.c_str();
}

time64
gnc_budget_get_period_start_date(const GncBudget *budget, guint period_num)
{
    g_return_val_if_fail (GNC_IS_BUDGET(budget), 0);
    return recurrenceGetPeriodTime(&GET_PRIVATE(budget)->recurrence, period_num, FALSE);
}

time64
gnc_budget_get_period_end_date(const GncBudget *budget, guint period_num)
{
    g_return_val_if_fail (GNC_IS_BUDGET(budget), 0);
    return recurrenceGetPeriodTime(&GET_PRIVATE(budget)->recurrence, period_num, TRUE);
}

gnc_numeric
gnc_budget_get_account_period_actual_value(
    const GncBudget *budget, Account *acc, guint period_num)
{
    // FIXME: maybe zero is not best error return val.
    g_return_val_if_fail(GNC_IS_BUDGET(budget) && acc, gnc_numeric_zero());
    return recurrenceGetAccountPeriodValue(&GET_PRIVATE(budget)->recurrence,
                                           acc, period_num);
}

static PeriodData&
get_perioddata (const GncBudget *budget, const Account *account, guint period_num)
{
    GncBudgetPrivate *priv = GET_PRIVATE (budget);

    if (period_num >= priv->num_periods)
        throw std::out_of_range("period_num >= num_periods");

    auto& vec = priv->acct_map[account];

    if (vec.empty())
    {
        auto budget_kvp { QOF_INSTANCE (budget)->kvp_data };
        vec.reserve (priv->num_periods);

        for (guint i = 0; i < priv->num_periods; i++)
        {
            auto kval1 { budget_kvp->get_slot (make_period_data_path (account, i)) };
            auto kval2 { budget_kvp->get_slot (make_period_note_path (account, i)) };

            auto is_set = kval1 && kval1->get_type() == KvpValue::Type::NUMERIC;
            auto num = is_set ? std::make_optional (kval1->get<gnc_numeric>()) : std::nullopt;
            auto note = (kval2 && kval2->get_type() == KvpValue::Type::STRING) ?
                kval2->get<const char*>() : "";

            vec.emplace_back (note, num);
        }
    }

    return vec.at(period_num);
}

GncBudget*
gnc_budget_lookup (const GncGUID *guid, const QofBook *book)
{
    QofCollection *col;

    g_return_val_if_fail(guid, nullptr);
    g_return_val_if_fail(book, nullptr);
    col = qof_book_get_collection (book, GNC_ID_BUDGET);
    return GNC_BUDGET(qof_collection_lookup_entity (col, guid));
}

static void just_get_one(QofInstance *ent, gpointer data)
{
    GncBudget **bgt = (GncBudget**)data;
    if (bgt && !*bgt) *bgt = GNC_BUDGET(ent);
}

GncBudget*
gnc_budget_get_default (QofBook *book)
{
    QofCollection *col;
    GncBudget *bgt = nullptr;
    GncGUID *default_budget_guid = nullptr;

    g_return_val_if_fail(book, nullptr);

    qof_instance_get (QOF_INSTANCE (book),
                      "default-budget", &default_budget_guid,
                      nullptr);
    if (default_budget_guid)
    {
        col = qof_book_get_collection(book, GNC_ID_BUDGET);
        bgt = (GncBudget *) qof_collection_lookup_entity(col,
                                                         default_budget_guid);
    }

    /* Revert to 2.2.x behavior if the book has no default budget. */

    if ( bgt == nullptr )
    {
        col = qof_book_get_collection(book, GNC_ID_BUDGET);
        if (qof_collection_count(col) > 0)
        {
            qof_collection_foreach(col, just_get_one, &bgt);
        }
    }

    guid_free (default_budget_guid);
    return bgt;
}

static void
destroy_budget_on_book_close(QofInstance *ent, gpointer data)
{
    GncBudget* bgt = GNC_BUDGET(ent);

    gnc_budget_destroy(bgt);
}

/** Handles book end - frees all budgets from the book
 *
 * @param book Book being closed
 */
static void
gnc_budget_book_end(QofBook* book)
{
    QofCollection *col;

    col = qof_book_get_collection(book, GNC_ID_BUDGET);
    qof_collection_foreach(col, destroy_budget_on_book_close, nullptr);
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif

/* Define the QofObject. */
static QofObject budget_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_BUDGET,
    DI(.type_label        = ) "Budget",
    DI(.create            = ) (void*(*)(QofBook*)) gnc_budget_new,
    DI(.book_begin        = ) nullptr,
    DI(.book_end          = ) gnc_budget_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) qof_collection_foreach,
    DI(.printable         = ) (const char * (*)(gpointer)) gnc_budget_get_name,
    DI(.version_cmp       = ) (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};


/* Static wrapper getters for the recurrence params */
static PeriodType gnc_budget_get_rec_pt(const GncBudget *bgt)
{
    return recurrenceGetPeriodType(&(GET_PRIVATE(bgt)->recurrence));
}
static guint gnc_budget_get_rec_mult(const GncBudget *bgt)
{
    return recurrenceGetMultiplier(&(GET_PRIVATE(bgt)->recurrence));
}
static time64 gnc_budget_get_rec_time(const GncBudget *bgt)
{
    return recurrenceGetTime(&(GET_PRIVATE(bgt)->recurrence));
}

/* Register ourselves with the engine. */
gboolean gnc_budget_register (void)
{
    static QofParam params[] =
    {
        {
            "name", QOF_TYPE_STRING,
            (QofAccessFunc) gnc_budget_get_name,
            (QofSetterFunc) gnc_budget_set_name
        },
        {
            "description", QOF_TYPE_STRING,
            (QofAccessFunc) gnc_budget_get_description,
            (QofSetterFunc) gnc_budget_set_description
        },
        {
            "recurrence_period_type", QOF_TYPE_INT32,
            (QofAccessFunc) gnc_budget_get_rec_pt, nullptr
        },
        /* Signedness problem: Should be unsigned. */
        {
            "recurrence_multiplier", QOF_TYPE_INT32,
            (QofAccessFunc) gnc_budget_get_rec_mult, nullptr
        },
        /* This is the same way that SchedXaction.c uses QOF_TYPE_DATE
           but I don't think QOF actually supports a GDate, so I think
           this is wrong. */
        {
            "recurrence_date", QOF_TYPE_DATE,
            (QofAccessFunc) gnc_budget_get_rec_time, nullptr
        },
        /* Signedness problem: Should be unsigned. */
        {
            "num_periods", QOF_TYPE_INT32,
            (QofAccessFunc) gnc_budget_get_num_periods,
            (QofSetterFunc) gnc_budget_set_num_periods
        },
        {
            QOF_PARAM_BOOK, QOF_ID_BOOK,
            (QofAccessFunc) qof_instance_get_book, nullptr
        },
        {
            QOF_PARAM_GUID, QOF_TYPE_GUID,
            (QofAccessFunc) qof_instance_get_guid, nullptr
        },
        { nullptr },
    };

    qof_class_register(GNC_ID_BUDGET, (QofSortFunc) nullptr, params);
    return qof_object_register(&budget_object_def);
}
