/*
 * gnc-sx-instance-model.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 and/or version 3 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * As a special exception, permission is granted to link the binary module
 * resultant from this code with the OpenSSL project's "OpenSSL" library (or
 * modified versions of it that use the same license as the "OpenSSL"
 * library), and distribute the linked executable.  You must obey the GNU
 * General Public License in all respects for all of the code used other than
 * "OpenSSL". If you modify this file, you may extend this exception to your
 * version of the file, but you are not obligated to do so. If you do not
 * wish to do so, delete this exception statement from your version of this
 * file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glib-object.h>
#include <stdlib.h>

#include "Account.h"
#include "SX-book.h"
#include "SchedXaction.h"
#include "Scrub.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-date.h"
#include "gnc-event.h"
#include "gnc-exp-parser.h"
#include "gnc-glib-utils.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"
#include "qof.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.app-utils.sx"
static QofLogModule log_module = G_LOG_DOMAIN;

/** Report errors bilingual:
 *  in g_critical untranslated and
 *  in g_list_append translated.
 */
#define REPORT_ERROR(list, format, ...) do { \
    g_critical(format, __VA_ARGS__); \
    if (list != NULL) \
        *list = g_list_append(*list, g_strdup_printf(_(format), __VA_ARGS__)); \
} while (0)

static GObjectClass *parent_class = NULL;

typedef struct _SxTxnCreationData
{
    GncSxInstance *instance;
    GList **created_txn_guids;
    GList **creation_errors;
} SxTxnCreationData;

static void gnc_sx_instance_model_class_init (GncSxInstanceModelClass *klass);
static void gnc_sx_instance_model_init(GTypeInstance *instance, gpointer klass);
static GncSxInstanceModel* gnc_sx_instance_model_new(void);

static GncSxInstance* gnc_sx_instance_new(GncSxInstances *parent, GncSxInstanceState state, GDate *date, void *temporal_state, gint sequence_num);

static gint _get_vars_helper(Transaction *txn, void *var_hash_data);

static GncSxVariable* gnc_sx_variable_new(gchar *name);

static void _gnc_sx_instance_event_handler(QofInstance *ent, QofEventId event_type, gpointer user_data, gpointer evt_data);
static gnc_commodity* get_transaction_currency(SxTxnCreationData *creation_data, SchedXaction *sx, Transaction *template_txn);
/* ------------------------------------------------------------ */

typedef struct
{
    const char *name;
    gnc_numeric amount;
} ScrubItem;

static void
scrub_sx_split_numeric (Split* split, gboolean is_credit, GList **changes)
{
    const char *formula = is_credit ? "sx-credit-formula" : "sx-debit-formula";
    const char *numeric = is_credit ? "sx-credit-numeric" : "sx-debit-numeric";
    char *formval;
    gnc_numeric *numval = NULL;
    GHashTable *parser_vars = g_hash_table_new_full
        (g_str_hash, g_str_equal, g_free, (GDestroyNotify)g_free);
    char *error_loc;
    gnc_numeric amount = gnc_numeric_zero ();
    gboolean parse_result = FALSE;

    qof_instance_get (QOF_INSTANCE (split),
                      formula, &formval,
                      numeric, &numval,
                      NULL);

    parse_result = gnc_exp_parser_parse_separate_vars (formval, &amount,
                                                       &error_loc, parser_vars);

    if (!parse_result || g_hash_table_size (parser_vars) != 0)
        amount = gnc_numeric_zero ();

    if (!numval || !gnc_numeric_eq (amount, *numval))
    {
        ScrubItem *change = g_new (ScrubItem, 1);
        change->name = numeric;
        change->amount = amount;
        *changes = g_list_prepend (*changes, change);
    }

    g_hash_table_destroy (parser_vars);
    g_free (formval);
    g_free (numval);
}

/* Fixes error in pre-2.6.16 where the numeric slot wouldn't get changed if the
 * formula slot was edited.
 */
void
gnc_sx_scrub_split_numerics (gpointer psplit, gpointer puser)
{
    Split *split = GNC_SPLIT (psplit);
    Transaction *trans = xaccSplitGetParent (split);
    GList *changes = NULL;
    scrub_sx_split_numeric (split, TRUE, &changes);
    scrub_sx_split_numeric (split, FALSE, &changes);
    if (!changes)
        return;

    xaccTransBeginEdit (trans);
    for (GList *n = changes; n; n = n->next)
    {
        ScrubItem *change = n->data;
        qof_instance_set (QOF_INSTANCE (split),
                          change->name, &change->amount,
                          NULL);
    }
    xaccTransCommitEdit (trans);
    g_list_free_full (changes, g_free);
}

static void
_sx_var_to_raw_numeric(gchar *name, GncSxVariable *var, GHashTable *parser_var_hash)
{
    g_hash_table_insert(parser_var_hash, g_strdup(name), &var->value);
}

static void
_var_numeric_to_sx_var(gchar *name, gnc_numeric *num, GHashTable *sx_var_hash)
{
    gpointer p_var;
    if (!g_hash_table_lookup_extended(sx_var_hash, name, NULL, &p_var))
    {
        p_var = (gpointer)gnc_sx_variable_new(name);
        g_hash_table_insert(sx_var_hash, g_strdup(name), p_var);
    }
    ((GncSxVariable*)p_var)->value = *num;
}

static void
_wipe_parsed_sx_var(gchar *key, GncSxVariable *var, gpointer unused_user_data)
{
    var->value = gnc_numeric_error(GNC_ERROR_ARG);
}

static gboolean
split_is_marker(Split *split)
{
    gchar *credit_formula = NULL;
    gchar *debit_formula = NULL;
    gboolean split_is_marker = TRUE;

    qof_instance_get (QOF_INSTANCE (split),
                      "sx-credit-formula", &credit_formula,
                      "sx-debit-formula", &debit_formula,
                      NULL);

    if ((credit_formula && *credit_formula) ||
        (debit_formula && *debit_formula))
        split_is_marker = FALSE;

    g_free(credit_formula);
    g_free(debit_formula);
    return split_is_marker;
}

/**
 * @return caller-owned.
 **/
GHashTable*
gnc_sx_instance_get_variables_for_parser(GHashTable *instance_var_hash)
{
    GHashTable *parser_vars;

    parser_vars = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    g_hash_table_foreach(instance_var_hash, (GHFunc)_sx_var_to_raw_numeric, parser_vars);
    return parser_vars;
}

int
gnc_sx_parse_vars_from_formula(const char *formula,
                               GHashTable *var_hash,
                               gnc_numeric *result)
{
    gnc_numeric num;
    char *errLoc = NULL;
    int toRet = 0;
    GHashTable *parser_vars;

    // convert var_hash -> variables for the parser.
    parser_vars = gnc_sx_instance_get_variables_for_parser(var_hash);

    num = gnc_numeric_zero();
    if (!gnc_exp_parser_parse_separate_vars(formula, &num, &errLoc, parser_vars))
    {
        toRet = -1;
    }

    // convert back.
    g_hash_table_foreach(parser_vars, (GHFunc)_var_numeric_to_sx_var, var_hash);
    g_hash_table_destroy(parser_vars);

    if (result != NULL)
    {
        *result = num;
    }

    return toRet;
}

static GncSxVariable*
gnc_sx_variable_new(gchar *name)
{
    GncSxVariable *var = g_new0(GncSxVariable, 1);
    var->name = g_strdup(name);
    var->value = gnc_numeric_error(GNC_ERROR_ARG);
    var->editable = TRUE;
    return var;
}

GncSxVariable*
gnc_sx_variable_new_full(gchar *name, gnc_numeric value, gboolean editable)
{
    GncSxVariable *var = gnc_sx_variable_new(name);
    var->value = value;
    var->editable = editable;
    return var;
}

static GncSxVariable*
gnc_sx_variable_new_copy(GncSxVariable *to_copy)
{
    GncSxVariable *var = gnc_sx_variable_new(to_copy->name);
    var->value = to_copy->value;
    var->editable = to_copy->editable;
    return var;
}

void
gnc_sx_variable_free(GncSxVariable *var)
{
    g_free(var->name);
    g_free(var);
}

static inline gchar*
var_name_from_commodities(gnc_commodity* split_c, gnc_commodity* txn_c)
{
    const gchar* split_m = gnc_commodity_get_mnemonic(split_c);
    const gchar* txn_m = gnc_commodity_get_mnemonic(txn_c);
    gchar* var_name = g_strdup_printf ("%s -> %s",
                                       split_m ? split_m : "(null)",
                                       txn_m ? txn_m : "(null)");

    DEBUG("var_name is %s", var_name);
    return var_name;
}

static gint
_get_vars_helper(Transaction *txn, void *var_hash_data)
{
    GHashTable *var_hash = (GHashTable*)var_hash_data;
    GList *split_list;
    Split *s;
    gchar *credit_formula = NULL;
    gchar *debit_formula = NULL;
    gnc_commodity *txn_cmdty = get_transaction_currency(NULL, NULL, txn);

    split_list = xaccTransGetSplitList(txn);
    if (split_list == NULL)
    {
        return 1;
    }

    for ( ; split_list; split_list = split_list->next)
    {
        gnc_commodity *split_cmdty = NULL;
        GncGUID *acct_guid = NULL;
        Account *acct;
        gboolean split_is_marker = TRUE;

        s = (Split*)split_list->data;

        qof_instance_get (QOF_INSTANCE (s),
			  "sx-account", &acct_guid,
			  "sx-credit-formula", &credit_formula,
			  "sx-debit-formula", &debit_formula,
			  NULL);
        acct = xaccAccountLookup(acct_guid, gnc_get_current_book());
        guid_free (acct_guid);
        split_cmdty = xaccAccountGetCommodity(acct);
        // existing... ------------------------------------------
	if (credit_formula && strlen(credit_formula) != 0)
	{
	    gnc_sx_parse_vars_from_formula(credit_formula, var_hash, NULL);
	    split_is_marker = FALSE;
	}
	if (debit_formula && strlen(debit_formula) != 0)
	{
	    gnc_sx_parse_vars_from_formula(debit_formula, var_hash, NULL);
	    split_is_marker = FALSE;
	}
	g_free (credit_formula);
	g_free (debit_formula);

        if (split_is_marker)
            continue;

        if (! gnc_commodity_equal(split_cmdty, txn_cmdty))
        {
            GncSxVariable *var;
            gchar *var_name;

            var_name = var_name_from_commodities(split_cmdty, txn_cmdty);
            var = gnc_sx_variable_new(var_name);
            g_hash_table_insert(var_hash, g_strdup(var->name), var);
        }
    }

    return 0;
}

Account*
gnc_sx_get_template_transaction_account(const SchedXaction *sx)
{
    Account *template_root, *sx_template_acct;
    char sx_guid_str[GUID_ENCODING_LENGTH+1];

    template_root = gnc_book_get_template_root(gnc_get_current_book());
    guid_to_string_buff(xaccSchedXactionGetGUID(sx), sx_guid_str);
    sx_template_acct = gnc_account_lookup_by_name(template_root, sx_guid_str);
    return sx_template_acct;
}

void
gnc_sx_get_variables(SchedXaction *sx, GHashTable *var_hash)
{
    Account *sx_template_acct = gnc_sx_get_template_transaction_account(sx);
    xaccAccountForEachTransaction(sx_template_acct, _get_vars_helper, var_hash);
}

static void
_set_var_to_random_value(gchar *key, GncSxVariable *var, gpointer unused_user_data)
{
    /* This is used by dialog-sx-editor to plug in values as a simplistic way to
     * check balances. One possible variable is the number of periods in a
     * interest or future value calculation where the variable is used as an
     * exponent, so we want the numbers to be monotonically > 0 and not so large
     * that they'll cause overflows.
     */
    var->value = gnc_numeric_create(g_random_int_range(1, 1000), 1);
}

void
gnc_sx_randomize_variables(GHashTable *vars)
{
    g_hash_table_foreach(vars, (GHFunc)_set_var_to_random_value, NULL);
}

static void
_clone_sx_var_hash_entry(gpointer key, gpointer value, gpointer user_data)
{
    GHashTable *to = (GHashTable*)user_data;
    GncSxVariable *to_copy = (GncSxVariable*)value;
    GncSxVariable *var = gnc_sx_variable_new_copy(to_copy);
    g_hash_table_insert(to, g_strdup(key), var);
}

static GncSxInstance*
gnc_sx_instance_new(GncSxInstances *parent, GncSxInstanceState state, GDate *date, void *temporal_state, gint sequence_num)
{
    GncSxInstance *rtn = g_new0(GncSxInstance, 1);
    rtn->parent = parent;
    rtn->orig_state = state;
    rtn->state = state;
    g_date_clear(&rtn->date, 1);
    rtn->date = *date;
    rtn->temporal_state = gnc_sx_clone_temporal_state(temporal_state);

    if (! parent->variable_names_parsed)
    {
        parent->variable_names = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)gnc_sx_variable_free);
        gnc_sx_get_variables(parent->sx, parent->variable_names);
        g_hash_table_foreach(parent->variable_names, (GHFunc)_wipe_parsed_sx_var, NULL);
        parent->variable_names_parsed = TRUE;
    }

    rtn->variable_bindings = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, (GDestroyNotify)gnc_sx_variable_free);
    g_hash_table_foreach(parent->variable_names, _clone_sx_var_hash_entry, rtn->variable_bindings);

    {
        int instance_i_value;
        gnc_numeric i_num;
        GncSxVariable *as_var;

        instance_i_value = gnc_sx_get_instance_count(rtn->parent->sx, rtn->temporal_state);
        i_num = gnc_numeric_create(instance_i_value, 1);
        as_var = gnc_sx_variable_new_full("i", i_num, FALSE);

        g_hash_table_insert(rtn->variable_bindings, g_strdup("i"), as_var);
    }

    return rtn;
}

static gint
_compare_GncSxVariables(gconstpointer a, gconstpointer b)
{
    return strcmp(((const GncSxVariable*)a)->name, ((const GncSxVariable*)b)->name);
}

static void
_build_list_from_hash_elts(gpointer key, gpointer value, gpointer user_data)
{
    GList **list = (GList**)user_data;
    *list = g_list_prepend (*list, value);
}

GList *
gnc_sx_instance_get_variables(GncSxInstance *inst)
{
    GList *vars = NULL;
    g_hash_table_foreach(inst->variable_bindings, _build_list_from_hash_elts, &vars);
    return g_list_sort (vars, _compare_GncSxVariables);
}

static GncSxInstances*
_gnc_sx_gen_instances(gpointer *data, gpointer user_data)
{
    GncSxInstances *instances = g_new0(GncSxInstances, 1);
    GList *instlist = NULL;
    SchedXaction *sx = (SchedXaction*)data;
    const GDate *range_end = (const GDate*)user_data;
    GDate creation_end, remind_end;
    GDate cur_date;
    SXTmpStateData *temporal_state = gnc_sx_create_temporal_state(sx);

    instances->sx = sx;

    creation_end = *range_end;
    g_date_add_days(&creation_end, xaccSchedXactionGetAdvanceCreation(sx));
    remind_end = creation_end;
    g_date_add_days(&remind_end, xaccSchedXactionGetAdvanceReminder(sx));

    /* postponed */
    {
        GList *postponed = gnc_sx_get_defer_instances(sx);
        for ( ; postponed != NULL; postponed = postponed->next)
        {
            GDate inst_date;
            int seq_num;
            GncSxInstance *inst;

            g_date_clear(&inst_date, 1);
            inst_date = xaccSchedXactionGetNextInstance(sx, postponed->data);
            seq_num = gnc_sx_get_instance_count(sx, postponed->data);
            inst = gnc_sx_instance_new(instances, SX_INSTANCE_STATE_POSTPONED,
                                       &inst_date, postponed->data, seq_num);
            instlist = g_list_prepend (instlist, inst);
            gnc_sx_destroy_temporal_state(temporal_state);
            temporal_state = gnc_sx_clone_temporal_state(postponed->data);
            gnc_sx_incr_temporal_state(sx, temporal_state);
        }
    }

    /* to-create */
    g_date_clear(&cur_date, 1);
    cur_date = xaccSchedXactionGetNextInstance(sx, temporal_state);
    instances->next_instance_date = cur_date;
    while (g_date_valid(&cur_date) && g_date_compare(&cur_date, &creation_end) <= 0)
    {
        GncSxInstance *inst;
        int seq_num;
        seq_num = gnc_sx_get_instance_count(sx, temporal_state);
        inst = gnc_sx_instance_new(instances, SX_INSTANCE_STATE_TO_CREATE,
                                   &cur_date, temporal_state, seq_num);
        instlist = g_list_prepend (instlist, inst);
        gnc_sx_incr_temporal_state(sx, temporal_state);
        cur_date = xaccSchedXactionGetNextInstance(sx, temporal_state);
    }

    /* reminders */
    while (g_date_valid(&cur_date) &&
           g_date_compare(&cur_date, &remind_end) <= 0)
    {
        GncSxInstance *inst;
        int seq_num;
        seq_num = gnc_sx_get_instance_count(sx, temporal_state);
        inst = gnc_sx_instance_new(instances, SX_INSTANCE_STATE_REMINDER,
                                   &cur_date, temporal_state, seq_num);
        instlist = g_list_prepend (instlist, inst);
        gnc_sx_incr_temporal_state(sx, temporal_state);
        cur_date = xaccSchedXactionGetNextInstance(sx, temporal_state);
    }

    instances->instance_list = g_list_reverse (instlist);

    gnc_sx_destroy_temporal_state (temporal_state);

    return instances;
}

GncSxInstanceModel*
gnc_sx_get_current_instances(void)
{
    GDate now;
    g_date_clear(&now, 1);
    gnc_gdate_set_time64 (&now, gnc_time (NULL));
    return gnc_sx_get_instances(&now, FALSE);
}

GncSxInstanceModel*
gnc_sx_get_instances(const GDate *range_end, gboolean include_disabled)
{
    GList *all_sxes = gnc_book_get_schedxactions(gnc_get_current_book())->sx_list;
    GncSxInstanceModel *instances;

    g_assert(range_end != NULL);
    g_assert(g_date_valid(range_end));

    instances = gnc_sx_instance_model_new();
    instances->include_disabled = include_disabled;
    instances->range_end = *range_end;

    if (include_disabled)
    {
        instances->sx_instance_list = gnc_g_list_map(all_sxes, (GncGMapFunc)_gnc_sx_gen_instances, (gpointer)range_end);
    }
    else
    {
        GList *sx_iter = g_list_first(all_sxes);
        GList *enabled_sxes = NULL;

        for (; sx_iter != NULL; sx_iter = sx_iter->next)
        {
            SchedXaction *sx = (SchedXaction*)sx_iter->data;
            if (xaccSchedXactionGetEnabled(sx))
            {
                enabled_sxes = g_list_prepend (enabled_sxes, sx);
            }
        }
        enabled_sxes = g_list_reverse (enabled_sxes);
        instances->sx_instance_list = gnc_g_list_map(enabled_sxes, (GncGMapFunc)_gnc_sx_gen_instances, (gpointer)range_end);
        g_list_free(enabled_sxes);
    }

    return instances;
}
static GncSxInstanceModel*
gnc_sx_instance_model_new(void)
{
    return GNC_SX_INSTANCE_MODEL(g_object_new(GNC_TYPE_SX_INSTANCE_MODEL, NULL));
}

GType
gnc_sx_instance_model_get_type(void)
{
    static GType type = 0;
    if (type == 0)
    {
        static const GTypeInfo info =
            {
                sizeof (GncSxInstanceModelClass),
                NULL,   /* base_init */
                NULL,   /* base_finalize */
                (GClassInitFunc)gnc_sx_instance_model_class_init,   /* class_init */
                NULL,   /* class_finalize */
                NULL,   /* class_data */
                sizeof (GncSxInstanceModel),
                0,      /* n_preallocs */
                (GInstanceInitFunc)gnc_sx_instance_model_init    /* instance_init */
            };
        type = g_type_register_static (G_TYPE_OBJECT,
                                       "GncSxInstanceModelType",
                                       &info, 0);
    }
    return type;
}

static void
gnc_sx_instance_model_dispose(GObject *object)
{
    GncSxInstanceModel *model;
    g_return_if_fail(object != NULL);
    model = GNC_SX_INSTANCE_MODEL(object);

    g_return_if_fail(!model->disposed);
    model->disposed = TRUE;

    qof_event_unregister_handler(model->qof_event_handler_id);

    G_OBJECT_CLASS(parent_class)->dispose(object);
}

static void
gnc_sx_instance_free(GncSxInstance *instance)
{
    gnc_sx_destroy_temporal_state(instance->temporal_state);

    if (instance->variable_bindings != NULL)
    {
        g_hash_table_destroy(instance->variable_bindings);
    }
    instance->variable_bindings = NULL;

    g_free(instance);
}

static void
gnc_sx_instances_free(GncSxInstances *instances)
{
    GList *instance_iter;

    if (instances->variable_names != NULL)
    {
        g_hash_table_destroy(instances->variable_names);
    }
    instances->variable_names = NULL;

    instances->sx = NULL;

    for (instance_iter = instances->instance_list; instance_iter != NULL; instance_iter = instance_iter->next)
    {
        GncSxInstance *inst = (GncSxInstance*)instance_iter->data;
        gnc_sx_instance_free(inst);
    }
    g_list_free(instances->instance_list);
    instances->instance_list = NULL;

    g_free(instances);
}

static void
gnc_sx_instance_model_finalize (GObject *object)
{
    GncSxInstanceModel *model;
    GList *sx_list_iter;

    g_return_if_fail(object != NULL);

    model = GNC_SX_INSTANCE_MODEL(object);
    for (sx_list_iter = model->sx_instance_list; sx_list_iter != NULL; sx_list_iter = sx_list_iter->next)
    {
        GncSxInstances *instances = (GncSxInstances*)sx_list_iter->data;
        gnc_sx_instances_free(instances);
    }
    g_list_free(model->sx_instance_list);
    model->sx_instance_list = NULL;

    G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
gnc_sx_instance_model_class_init (GncSxInstanceModelClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    parent_class = g_type_class_peek_parent(klass);

    object_class->dispose = gnc_sx_instance_model_dispose;
    object_class->finalize = gnc_sx_instance_model_finalize;

    klass->removing_signal_id =
        g_signal_new("removing",
                     GNC_TYPE_SX_INSTANCE_MODEL,
                     G_SIGNAL_RUN_FIRST,
                     0, /* class offset */
                     NULL, /* accumulator */
                     NULL, /* accum data */
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    klass->updated_signal_id =
        g_signal_new("updated",
                     GNC_TYPE_SX_INSTANCE_MODEL,
                     G_SIGNAL_RUN_FIRST,
                     0, /* class offset */
                     NULL, /* accumulator */
                     NULL, /* accum data */
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    klass->added_signal_id =
        g_signal_new("added",
                     GNC_TYPE_SX_INSTANCE_MODEL,
                     G_SIGNAL_RUN_FIRST,
                     0, /* class offset */
                     NULL, /* accumulator */
                     NULL, /* accum data */
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);
}

static void
gnc_sx_instance_model_init(GTypeInstance *instance, gpointer klass)
{
    GncSxInstanceModel *inst = (GncSxInstanceModel*)instance;

    g_date_clear(&inst->range_end, 1);
    inst->sx_instance_list = NULL;
    inst->qof_event_handler_id = qof_event_register_handler(_gnc_sx_instance_event_handler, inst);
}

static gint
_gnc_sx_instance_find_by_sx(GncSxInstances *in_list_instances, SchedXaction *sx_to_find)
{
    if (in_list_instances->sx == sx_to_find)
        return 0;
    return -1;
}

static void
_gnc_sx_instance_event_handler(QofInstance *ent, QofEventId event_type, gpointer user_data, gpointer evt_data)
{
    GncSxInstanceModel *instances = GNC_SX_INSTANCE_MODEL(user_data);

    /* selection rules {
    //   (gnc_collection_get_schedxaction_list(book), GNC_EVENT_ITEM_ADDED)
    //   (gnc_collection_get_schedxaction_list(book), GNC_EVENT_ITEM_REMOVED)
    //   (GNC_IS_SX(ent), QOF_EVENT_MODIFIED)
    // } */
    if (!(GNC_IS_SX(ent) || GNC_IS_SXES(ent)))
        return;

    if (GNC_IS_SX(ent))
    {
        SchedXaction *sx;
        gboolean sx_is_in_model = FALSE;

        sx = GNC_SX(ent);
        // only send `updated` if it's actually in the model
        sx_is_in_model = (g_list_find_custom(instances->sx_instance_list, sx, (GCompareFunc)_gnc_sx_instance_find_by_sx) != NULL);
        if (event_type & QOF_EVENT_MODIFY)
        {
            if (sx_is_in_model)
            {
                if (instances->include_disabled || xaccSchedXactionGetEnabled(sx))
                {
                    g_signal_emit_by_name(instances, "updated", (gpointer)sx);
                }
                else
                {
                    /* the sx was enabled but is now disabled */
                    g_signal_emit_by_name(instances, "removing", (gpointer)sx);
                }
            }
            else
            {
                /* determine if this is a legitimate SX or just a "one-off" / being created */
                GList *all_sxes = gnc_book_get_schedxactions(gnc_get_current_book())->sx_list;
                if (g_list_find(all_sxes, sx) && (!instances->include_disabled && xaccSchedXactionGetEnabled(sx)))
                {
                    /* it's moved from disabled to enabled, add the instances */
                    instances->sx_instance_list
                        = g_list_append(instances->sx_instance_list,
                                        _gnc_sx_gen_instances((gpointer)sx, (gpointer) & instances->range_end));
                    g_signal_emit_by_name(instances, "added", (gpointer)sx);
                }
            }
        }
        /* else { unsupported event type; ignore } */
    }
    else if (GNC_IS_SXES(ent))
    {
        SchedXaction *sx = GNC_SX(evt_data);

        if (event_type & GNC_EVENT_ITEM_REMOVED)
        {
            GList *instances_link;
            instances_link = g_list_find_custom(instances->sx_instance_list, sx, (GCompareFunc)_gnc_sx_instance_find_by_sx);
            if (instances_link != NULL)
            {
                g_signal_emit_by_name(instances, "removing", (gpointer)sx);
            }
            else if (instances->include_disabled)
            {
                g_warning("could not remove instances that do not exist in the model");
            }
        }
        else if (event_type & GNC_EVENT_ITEM_ADDED)
        {
            if (instances->include_disabled || xaccSchedXactionGetEnabled(sx))
            {
                /* generate instances, add to instance list, emit update. */
                instances->sx_instance_list
                    = g_list_append(instances->sx_instance_list,
                                    _gnc_sx_gen_instances((gpointer)sx, (gpointer) & instances->range_end));
                g_signal_emit_by_name(instances, "added", (gpointer)sx);
            }
        }
        /* else { g_critical("unsupported event type [%d]\n", event_type); } */
    }
}

typedef struct _HashListPair
{
    GHashTable *hash;
    GList *list;
} HashListPair;

static void
_find_unreferenced_vars(gchar *key,
                        gpointer value,
                        HashListPair *cb_pair)
{
    if (cb_pair->hash ==  NULL ||
        !g_hash_table_lookup_extended(cb_pair->hash, key, NULL, NULL))
    {
        DEBUG("variable [%s] not found", key);
        cb_pair->list = g_list_prepend (cb_pair->list, key);
    }
}

void
gnc_sx_instance_model_update_sx_instances(GncSxInstanceModel *model, SchedXaction *sx)
{
    GncSxInstances *existing, *new_instances;
    GList *link;

    link = g_list_find_custom(model->sx_instance_list, sx, (GCompareFunc)_gnc_sx_instance_find_by_sx);
    if (link == NULL)
    {
        g_critical("couldn't find sx [%p]\n", sx);
        return;
    }

    // merge the new instance data into the existing structure, mutating as little as possible.
    existing = (GncSxInstances*)link->data;
    new_instances = _gnc_sx_gen_instances((gpointer)sx, &model->range_end);
    existing->sx = new_instances->sx;
    existing->next_instance_date = new_instances->next_instance_date;
    {
        GList *existing_iter, *new_iter;
        gboolean existing_remain, new_remain;

        // step through the lists pairwise, and retain the existing
        // instance if the dates align, as soon as they don't stop and
        // cleanup.
        existing_iter = existing->instance_list;
        new_iter = new_instances->instance_list;
        for (; existing_iter != NULL && new_iter != NULL; existing_iter = existing_iter->next, new_iter = new_iter->next)
        {
            GncSxInstance *existing_inst, *new_inst;
            gboolean same_instance_date;
            existing_inst = (GncSxInstance*)existing_iter->data;
            new_inst = (GncSxInstance*)new_iter->data;

            same_instance_date = g_date_compare(&existing_inst->date, &new_inst->date) == 0;
            if (!same_instance_date)
                break;
        }

        existing_remain = (existing_iter != NULL);
        new_remain = (new_iter != NULL);

        if (existing_remain)
        {
            // delete excess
            gnc_g_list_cut(&existing->instance_list, existing_iter);
            g_list_foreach(existing_iter, (GFunc)gnc_sx_instance_free, NULL);
        }

        if (new_remain)
        {
            // append new
            GList *new_iter_iter;
            gnc_g_list_cut(&new_instances->instance_list, new_iter);

            for (new_iter_iter = new_iter; new_iter_iter != NULL; new_iter_iter = new_iter_iter->next)
            {
                GncSxInstance *inst = (GncSxInstance*)new_iter_iter->data;
                inst->parent = existing;
                existing->instance_list = g_list_append(existing->instance_list, new_iter_iter->data);
            }
            g_list_free(new_iter);
        }
    }

    // handle variables
    {
        GList *removed_var_names = NULL, *added_var_names = NULL;
        GList *inst_iter = NULL;

        if (existing->variable_names != NULL)
        {
            HashListPair removed_cb_data;
            removed_cb_data.hash = new_instances->variable_names;
            removed_cb_data.list = NULL;
            g_hash_table_foreach(existing->variable_names, (GHFunc)_find_unreferenced_vars, &removed_cb_data);
            removed_var_names = g_list_reverse (removed_cb_data.list);
        }
        DEBUG("%d removed variables", g_list_length(removed_var_names));

        if (new_instances->variable_names != NULL)
        {
            HashListPair added_cb_data;
            added_cb_data.hash = existing->variable_names;
            added_cb_data.list = NULL;
            g_hash_table_foreach(new_instances->variable_names, (GHFunc)_find_unreferenced_vars, &added_cb_data);
            added_var_names = g_list_reverse (added_cb_data.list);
        }
        DEBUG("%d added variables", g_list_length(added_var_names));

        if (existing->variable_names != NULL)
        {
            g_hash_table_destroy(existing->variable_names);
        }
        existing->variable_names = new_instances->variable_names;
        new_instances->variable_names = NULL;

        for (inst_iter = existing->instance_list; inst_iter != NULL; inst_iter = inst_iter->next)
        {
            GList *var_iter;
            GncSxInstance *inst = (GncSxInstance*)inst_iter->data;

            for (var_iter = removed_var_names; var_iter != NULL; var_iter = var_iter->next)
            {
                gchar *to_remove_key = (gchar*)var_iter->data;
                g_hash_table_remove(inst->variable_bindings, to_remove_key);
            }

            for (var_iter = added_var_names; var_iter != NULL; var_iter = var_iter->next)
            {
                gchar *to_add_key = (gchar*)var_iter->data;
                if (!g_hash_table_lookup_extended(
                        inst->variable_bindings, to_add_key, NULL, NULL))
                {
                    GncSxVariable *parent_var
                        = g_hash_table_lookup(existing->variable_names, to_add_key);
                    GncSxVariable *var_copy;

                    g_assert(parent_var != NULL);
                    var_copy = gnc_sx_variable_new_copy(parent_var);
                    g_hash_table_insert(inst->variable_bindings, g_strdup(to_add_key), var_copy);
                }
            }
        }
    }
    gnc_sx_instances_free(new_instances);
}

void
gnc_sx_instance_model_remove_sx_instances(GncSxInstanceModel *model, SchedXaction *sx)
{
    GList *instance_link = NULL;

    instance_link = g_list_find_custom(model->sx_instance_list, sx, (GCompareFunc)_gnc_sx_instance_find_by_sx);
    if (instance_link == NULL)
    {
        g_warning("instance not found!\n");
        return;
    }

    model->sx_instance_list = g_list_remove_link(model->sx_instance_list, instance_link);
    gnc_sx_instances_free((GncSxInstances*)instance_link->data);
}

static void
increment_sx_state(GncSxInstance *inst, GDate **last_occur_date, int *instance_count, int *remain_occur_count)
{
    if (!g_date_valid(*last_occur_date)
        || (g_date_valid(*last_occur_date)
            && g_date_compare(*last_occur_date, &inst->date) <= 0))
    {
        *last_occur_date = &inst->date;
    }

    *instance_count = gnc_sx_get_instance_count(inst->parent->sx, inst->temporal_state) + 1;

    if (*remain_occur_count > 0)
    {
        *remain_occur_count -= 1;
    }
}

static gboolean
_get_template_split_account(const SchedXaction* sx,
			    const Split *template_split,
			    Account **split_acct,
			    GList **creation_errors)
{
    gboolean success = TRUE;
    GncGUID *acct_guid = NULL;
    qof_instance_get (QOF_INSTANCE (template_split),
		      "sx-account", &acct_guid,
		      NULL);
    *split_acct = xaccAccountLookup(acct_guid, gnc_get_current_book());
    if (!*split_acct && sx && creation_errors)
    {
        char guid_str[GUID_ENCODING_LENGTH+1];
/* Translators: A list of error messages from the Scheduled Transactions (SX).
   They might appear in their editor or in "Since last run".                  */
        gchar* err = N_("Unknown account for guid [%s], cancelling SX [%s] creation.");
        guid_to_string_buff((const GncGUID*)acct_guid, guid_str);
        REPORT_ERROR(creation_errors, err, guid_str, xaccSchedXactionGetName(sx));
        success = FALSE;
    }

    guid_free (acct_guid);
    return success;
}

static void
_get_sx_formula_value(const SchedXaction* sx,
		      const Split *template_split,
		      gnc_numeric *numeric,
		      GList **creation_errors,
		      const char *formula_key,
		      const char* numeric_key,
		      GHashTable *variable_bindings)
{

    char *formula_str = NULL, *parseErrorLoc = NULL;
    gnc_numeric *numeric_val = NULL;
    qof_instance_get (QOF_INSTANCE (template_split),
		      formula_key, &formula_str,
		      numeric_key, &numeric_val,
		      NULL);

    if ((variable_bindings == NULL ||
         g_hash_table_size (variable_bindings) == 0) &&
        numeric_val != NULL &&
	gnc_numeric_check(*numeric_val) == GNC_ERROR_OK &&
	!gnc_numeric_zero_p(*numeric_val))
    {
        /* If there are no variables to parse and we had a valid numeric stored
         * then we can skip parsing the formula, which might save some
         * localization problems with separators. */
	numeric->num = numeric_val->num;
	numeric->denom = numeric_val->denom;
        g_free (formula_str);
        g_free (numeric_val);
        return;
    }

    if (formula_str != NULL && strlen(formula_str) != 0)
    {
        GHashTable *parser_vars = NULL;
        if (variable_bindings)
        {
            parser_vars = gnc_sx_instance_get_variables_for_parser(variable_bindings);
        }
        if (!gnc_exp_parser_parse_separate_vars(formula_str,
                                                numeric,
                                                &parseErrorLoc,
                                                parser_vars))
        {
            gchar *err = N_("Error parsing SX [%s] key [%s]=formula [%s] at [%s]: %s.");
            REPORT_ERROR(creation_errors, err,
                    xaccSchedXactionGetName(sx),
                    formula_key,
                    formula_str,
                    parseErrorLoc,
                    gnc_exp_parser_error_string());
       }

        if (parser_vars != NULL)
        {
            g_hash_table_destroy(parser_vars);
        }
    }
    g_free (formula_str);
    g_free (numeric_val);
}

static void
_get_credit_formula_value(GncSxInstance *instance,
                          const Split *template_split, gnc_numeric *credit_num,
                          GList **creation_errors)
{
    _get_sx_formula_value(instance->parent->sx, template_split, credit_num,
                          creation_errors, "sx-credit-formula",
                          "sx-credit-numeric", instance->variable_bindings);
}

static void
_get_debit_formula_value(GncSxInstance *instance, const Split *template_split,
                         gnc_numeric *debit_num, GList **creation_errors)
{
    _get_sx_formula_value(instance->parent->sx, template_split, debit_num,
                          creation_errors, "sx-debit-formula",
                          "sx-debit-numeric", instance->variable_bindings);
}

static gnc_numeric
split_apply_formulas (const Split *split, SxTxnCreationData* creation_data)
{
    gnc_numeric credit_num = gnc_numeric_zero();
    gnc_numeric debit_num = gnc_numeric_zero();
    gnc_numeric final;
    gint gncn_error;
    SchedXaction *sx = creation_data->instance->parent->sx;

    _get_credit_formula_value(creation_data->instance, split, &credit_num,
                              creation_data->creation_errors);
    _get_debit_formula_value(creation_data->instance, split, &debit_num,
                             creation_data->creation_errors);

    final = gnc_numeric_sub_fixed(debit_num, credit_num);

    gncn_error = gnc_numeric_check(final);
    if (gncn_error != GNC_ERROR_OK)
    {
        gchar *err = N_("Error %d in SX [%s] final gnc_numeric value, using 0 instead.");
        REPORT_ERROR(creation_data->creation_errors, err,
                        gncn_error, xaccSchedXactionGetName(sx));
        final = gnc_numeric_zero();
    }
    return final;
}

static void
split_apply_exchange_rate (Split *split, GHashTable *bindings,
                           gnc_commodity *txn_cmdty,
                           gnc_commodity *split_cmdty, gnc_numeric *final)
{
    gchar *exchange_rate_var_name;
    GncSxVariable *exchange_rate_var;
    gnc_numeric amt;
    gnc_numeric exchange_rate = gnc_numeric_create (1, 1);

    exchange_rate_var_name = var_name_from_commodities(split_cmdty, txn_cmdty);
    exchange_rate_var =
        (GncSxVariable*)g_hash_table_lookup(bindings,
                                            exchange_rate_var_name);

    if (exchange_rate_var != NULL)
    {
        exchange_rate = exchange_rate_var->value;
        DEBUG("exchange_rate is %s", gnc_numeric_to_string (exchange_rate));
    }
    g_free (exchange_rate_var_name);

    if (!gnc_commodity_is_currency (split_cmdty))
        amt = gnc_numeric_div(*final, exchange_rate,
                              gnc_commodity_get_fraction (split_cmdty),
                              GNC_HOW_RND_ROUND_HALF_UP);
    else
        amt = gnc_numeric_mul(*final, exchange_rate, 1000,
                              GNC_HOW_RND_ROUND_HALF_UP);


    DEBUG("amount is %s for memo split '%s'", gnc_numeric_to_string (amt),
            xaccSplitGetMemo (split));
    xaccSplitSetAmount(split, amt); /* marks split dirty */

}
/* If the template_txn was created from the SX Editor then it has the default
 * currency even if none of its splits do; if the template_txn was created from
 * a non-currency register then it will be requesting backwards prices. Check
 * that the template_txn currency is in at least one split; if it's not a
 * currency and one of the splits is, use that currency. If there are no
 * currencies at all assume that the user knew what they were doing and return
 * the template_transaction's commodity.
 *
 * Since we're going through the split commodities anyway, check that they all
 * have usable values. If we find an error return NULL as a signal to
 * create_each_transaction_helper to bail out.
 */

static gnc_commodity*
get_transaction_currency(SxTxnCreationData *creation_data,
                         SchedXaction *sx, Transaction *template_txn)
{
    gnc_commodity *first_currency = NULL, *first_cmdty = NULL,
        *fallback_cmdty = NULL;
    gboolean err_flag = FALSE, txn_cmdty_in_splits = FALSE;
    gnc_commodity *txn_cmdty = xaccTransGetCurrency (template_txn);
    GList* txn_splits = xaccTransGetSplitList (template_txn);
    GList** creation_errors =
        creation_data ? creation_data->creation_errors : NULL;

    if (txn_cmdty)
        DEBUG("Template txn currency is %s.",
                gnc_commodity_get_mnemonic (txn_cmdty));
    else
        DEBUG("No template txn currency.");

    for (;txn_splits; txn_splits = txn_splits->next)
    {
        Split* t_split = (Split*)txn_splits->data;
        Account* split_account = NULL;
        gnc_commodity *split_cmdty = NULL;

        if (!_get_template_split_account(sx, t_split, &split_account,
                                         creation_errors))
        {
            err_flag = TRUE;
            break;
        }
        /* Don't consider the commodity of a transaction that has
         * neither a credit nor a debit formula. */

        if (!fallback_cmdty)
            fallback_cmdty = xaccAccountGetCommodity (split_account);

        if (split_is_marker(t_split))
             continue;

        split_cmdty = xaccAccountGetCommodity (split_account);
        if (!txn_cmdty)
            txn_cmdty = split_cmdty;
        if (!first_cmdty)
            first_cmdty = split_cmdty;
        if (gnc_commodity_equal (split_cmdty, txn_cmdty))
            txn_cmdty_in_splits = TRUE;
        if (!first_currency && gnc_commodity_is_currency (split_cmdty))
            first_currency = split_cmdty;
    }
    if (err_flag)
    {
        g_critical("Error in SX transaction [%s], split missing account: "
                   "Creation aborted.", xaccSchedXactionGetName(sx));
        return NULL;
    }
    if (first_currency &&
        (!txn_cmdty_in_splits || !gnc_commodity_is_currency (txn_cmdty)))
        return first_currency;
    if (!txn_cmdty_in_splits && first_cmdty)
        return first_cmdty;
    if (txn_cmdty)
        return txn_cmdty;
    return fallback_cmdty;
}

static gboolean
create_each_transaction_helper(Transaction *template_txn, void *user_data)
{
    Transaction *new_txn;
    GList *txn_splits, *template_splits, *node;
    Split *copying_split;
    SxTxnCreationData *creation_data = (SxTxnCreationData*)user_data;
    SchedXaction *sx = creation_data->instance->parent->sx;
    gnc_commodity *txn_cmdty = get_transaction_currency (creation_data,
                                                         sx, template_txn);

    /* No txn_cmdty means there was a defective split. Bail. */
    if (txn_cmdty == NULL)
         return FALSE;

    /* FIXME: In general, this should [correctly] deal with errors such
       as not finding the appropriate Accounts and not being able to
       parse the formula|credit/debit strings. */

    new_txn = xaccTransCloneNoKvp(template_txn);
    xaccTransBeginEdit(new_txn);

    DEBUG("creating template txn desc [%s] for sx [%s]",
            xaccTransGetDescription(new_txn),
            xaccSchedXactionGetName(sx));


    /* Bug#500427: copy the notes, if any */
    if (xaccTransGetNotes(template_txn) != NULL)
        xaccTransSetNotes (new_txn, xaccTransGetNotes (template_txn));

    xaccTransSetDate(new_txn,
                     g_date_get_day(&creation_data->instance->date),
                     g_date_get_month(&creation_data->instance->date),
                     g_date_get_year(&creation_data->instance->date));

    xaccTransSetDateEnteredSecs(new_txn, gnc_time(NULL));
    template_splits = xaccTransGetSplitList(template_txn);
    txn_splits = xaccTransGetSplitList(new_txn);
    if ((template_splits == NULL) || (txn_splits == NULL))
    {
        g_critical("transaction w/o splits for sx [%s]",
                   xaccSchedXactionGetName(sx));
        xaccTransDestroy(new_txn);
        xaccTransCommitEdit(new_txn);
        return FALSE;
    }

    if (txn_cmdty == NULL)
    {
        xaccTransDestroy(new_txn);
        xaccTransCommitEdit(new_txn);
        return FALSE;
    }
    xaccTransSetCurrency(new_txn, txn_cmdty);

    for (;
         txn_splits && template_splits;
         txn_splits = txn_splits->next, template_splits = template_splits->next)
    {
        const Split *template_split;
        Account *split_acct;
        gnc_commodity *split_cmdty = NULL;

        /* FIXME: Ick.  This assumes that the split lists will be ordered
           identically. :( They are, but we'd rather not have to count on
           it. --jsled */
        template_split = (Split*)template_splits->data;
        copying_split = (Split*)txn_splits->data;

        _get_template_split_account(sx, template_split, &split_acct,
                                    creation_data->creation_errors);

        split_cmdty = xaccAccountGetCommodity(split_acct);
        xaccSplitSetAccount(copying_split, split_acct);

        {
            gnc_numeric final = split_apply_formulas(template_split,
                                                     creation_data);
            xaccSplitSetValue(copying_split, final);
            DEBUG("value is %s for memo split '%s'",
                    gnc_numeric_to_string (final),
                    xaccSplitGetMemo (copying_split));
            if (! gnc_commodity_equal(split_cmdty, txn_cmdty))
            {
                split_apply_exchange_rate(copying_split,
                                          creation_data->instance->variable_bindings,
                                          txn_cmdty, split_cmdty, &final);
            }

            xaccSplitScrub(copying_split);
        }
    }


    {
	qof_instance_set (QOF_INSTANCE (new_txn),
			  "from-sched-xaction",
			  xaccSchedXactionGetGUID(creation_data->instance->parent->sx),
			  NULL);
    }

    xaccTransCommitEdit(new_txn);

    if (creation_data->created_txn_guids != NULL)
    {
        *creation_data->created_txn_guids
            = g_list_append(*(creation_data->created_txn_guids),
                            (gpointer)xaccTransGetGUID(new_txn));
    }

    return FALSE;
}

static void
create_transactions_for_instance(GncSxInstance *instance, GList **created_txn_guids, GList **creation_errors)
{
    SxTxnCreationData creation_data;
    Account *sx_template_account;

    sx_template_account = gnc_sx_get_template_transaction_account(instance->parent->sx);

    creation_data.instance = instance;
    creation_data.created_txn_guids = created_txn_guids;
    creation_data.creation_errors = creation_errors;
    /* Don't update the GUI for every transaction, it can really slow things
     * down.
     */
    qof_event_suspend();
    xaccAccountForEachTransaction(sx_template_account,
                                  create_each_transaction_helper,
                                  &creation_data);
    qof_event_resume();
}

void
gnc_sx_instance_model_effect_change(GncSxInstanceModel *model,
                                    gboolean auto_create_only,
                                    GList **created_transaction_guids,
                                    GList **creation_errors)
{
    GList *iter;

    if (qof_book_is_readonly(gnc_get_current_book()))
    {
        /* Is the book read-only? Then don't change anything here. */
        return;
    }

    for (iter = model->sx_instance_list; iter != NULL; iter = iter->next)
    {
        GList *instance_iter;
        GncSxInstances *instances = (GncSxInstances*)iter->data;
        GDate *last_occur_date;
        gint instance_count = 0;
        gint remain_occur_count = 0;

        // If there are no instances, then skip; specifically, skip
        // re-setting SchedXaction fields, which will dirty the book
        // spuriously.
        if (g_list_length(instances->instance_list) == 0)
            continue;

        last_occur_date = (GDate*) xaccSchedXactionGetLastOccurDate(instances->sx);
        instance_count = gnc_sx_get_instance_count(instances->sx, NULL);
        remain_occur_count = xaccSchedXactionGetRemOccur(instances->sx);

        for (instance_iter = instances->instance_list; instance_iter != NULL; instance_iter = instance_iter->next)
        {
            GncSxInstance *inst = (GncSxInstance*)instance_iter->data;
            gboolean sx_is_auto_create;
            GList *instance_errors = NULL;

            xaccSchedXactionGetAutoCreate(inst->parent->sx, &sx_is_auto_create, NULL);
            if (auto_create_only && !sx_is_auto_create)
            {
                if (inst->state != SX_INSTANCE_STATE_TO_CREATE)
                {
                    break;
                }
                continue;
            }

            if (inst->orig_state == SX_INSTANCE_STATE_POSTPONED
                && inst->state != SX_INSTANCE_STATE_POSTPONED)
            {
                // remove from postponed list
                g_assert(inst->temporal_state != NULL);
                gnc_sx_remove_defer_instance(inst->parent->sx,
                                             inst->temporal_state);
            }

            switch (inst->state)
            {
                case SX_INSTANCE_STATE_CREATED:
                    // nop: we've already processed this.
                    break;
                case SX_INSTANCE_STATE_IGNORED:
                    increment_sx_state(inst, &last_occur_date, &instance_count, &remain_occur_count);
                    break;
                case SX_INSTANCE_STATE_POSTPONED:
                    if (inst->orig_state != SX_INSTANCE_STATE_POSTPONED)
                    {
                        gnc_sx_add_defer_instance(instances->sx,
                                                  gnc_sx_clone_temporal_state (inst->temporal_state));
                    }
                    increment_sx_state(inst, &last_occur_date, &instance_count, &remain_occur_count);
                    break;
                case SX_INSTANCE_STATE_TO_CREATE:
                    create_transactions_for_instance (inst,
                                                      created_transaction_guids,
                                                      &instance_errors);
                    if (instance_errors == NULL)
                    {
                        increment_sx_state (inst, &last_occur_date,
                                            &instance_count,
                                            &remain_occur_count);
                        gnc_sx_instance_model_change_instance_state
                            (model, inst, SX_INSTANCE_STATE_CREATED);
                    }
                    else
                        *creation_errors = g_list_concat (*creation_errors,
                                                          instance_errors);
                    break;
                case SX_INSTANCE_STATE_REMINDER:
                    // do nothing
                    // assert no non-remind instances after this?
                    break;
                default:
                    g_assert_not_reached();
                    break;
            }
        }

        xaccSchedXactionSetLastOccurDate(instances->sx, last_occur_date);
        gnc_sx_set_instance_count(instances->sx, instance_count);
        xaccSchedXactionSetRemOccur(instances->sx, remain_occur_count);
    }
}

void
gnc_sx_instance_model_change_instance_state(GncSxInstanceModel *model,
                                            GncSxInstance *instance,
                                            GncSxInstanceState new_state)
{
    if (instance->state == new_state)
        return;

    instance->state = new_state;

    // ensure 'remind' constraints are met:
    {
        GList *inst_iter;
        inst_iter = g_list_find(instance->parent->instance_list, instance);
        g_assert(inst_iter != NULL);
        if (instance->state != SX_INSTANCE_STATE_REMINDER)
        {
            // iterate backwards, making sure reminders are changed to 'postponed'
            for (inst_iter = inst_iter->prev; inst_iter != NULL; inst_iter = inst_iter->prev)
            {
                GncSxInstance *prev_inst = (GncSxInstance*)inst_iter->data;
                if (prev_inst->state != SX_INSTANCE_STATE_REMINDER)
                    continue;
                prev_inst->state = SX_INSTANCE_STATE_POSTPONED;
            }
        }
        else
        {
            // iterate forward, make sure transactions are set to 'remind'
            for (inst_iter = inst_iter->next; inst_iter != NULL; inst_iter = inst_iter->next)
            {
                GncSxInstance *next_inst = (GncSxInstance*)inst_iter->data;
                if (next_inst->state == SX_INSTANCE_STATE_REMINDER)
                    continue;
                next_inst->state = SX_INSTANCE_STATE_REMINDER;
            }
        }
    }

    g_signal_emit_by_name(model, "updated", (gpointer)instance->parent->sx);
}

void
gnc_sx_instance_model_set_variable(GncSxInstanceModel *model,
                                   GncSxInstance *instance,
                                   GncSxVariable *variable,
                                   gnc_numeric *new_value)
{

    if (gnc_numeric_equal(variable->value, *new_value))
        return;
    variable->value = *new_value;
    g_signal_emit_by_name(model, "updated", (gpointer)instance->parent->sx);
}

static void
_list_from_hash_elts(gpointer key, gpointer value, GList **result_list)
{
    *result_list = g_list_prepend (*result_list, value);
}

GList*
gnc_sx_instance_model_check_variables(GncSxInstanceModel *model)
{
    GList *rtn = NULL;
    GList *sx_iter, *inst_iter, *var_list = NULL, *var_iter;

    for (sx_iter = model->sx_instance_list; sx_iter != NULL; sx_iter = sx_iter->next)
    {
        GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
        for (inst_iter = instances->instance_list; inst_iter != NULL; inst_iter = inst_iter->next)
        {
            GncSxInstance *inst = (GncSxInstance*)inst_iter->data;

            if (inst->state != SX_INSTANCE_STATE_TO_CREATE)
                continue;

            g_hash_table_foreach(inst->variable_bindings, (GHFunc)_list_from_hash_elts, &var_list);
            for (var_iter = var_list; var_iter != NULL; var_iter = var_iter->next)
            {
                GncSxVariable *var = (GncSxVariable*)var_iter->data;
                if (gnc_numeric_check(var->value) != GNC_ERROR_OK)
                {
                    GncSxVariableNeeded *need = g_new0(GncSxVariableNeeded, 1);
                    need->instance = inst;
                    need->variable = var;
                    rtn = g_list_prepend (rtn, need);
                }
            }
            g_list_free(var_list);
            var_list = NULL;
        }
    }
    return rtn;
}

void
gnc_sx_instance_model_summarize(GncSxInstanceModel *model, GncSxSummary *summary)
{
    GList *sx_iter, *inst_iter;

    g_return_if_fail(model != NULL);
    g_return_if_fail(summary != NULL);

    summary->need_dialog = FALSE;
    summary->num_instances = 0;
    summary->num_to_create_instances = 0;
    summary->num_auto_create_instances = 0;
    summary->num_auto_create_no_notify_instances = 0;

    for (sx_iter = model->sx_instance_list; sx_iter != NULL; sx_iter = sx_iter->next)
    {
        GncSxInstances *instances = (GncSxInstances*)sx_iter->data;
        gboolean sx_is_auto_create = FALSE, sx_notify = FALSE;
        xaccSchedXactionGetAutoCreate(instances->sx, &sx_is_auto_create, &sx_notify);
        for (inst_iter = instances->instance_list; inst_iter != NULL; inst_iter = inst_iter->next)
        {
            GncSxInstance *inst = (GncSxInstance*)inst_iter->data;
            summary->num_instances++;

            if (inst->state == SX_INSTANCE_STATE_TO_CREATE)
            {
                if (sx_is_auto_create)
                {
                    if (!sx_notify)
                    {
                        summary->num_auto_create_no_notify_instances++;
                    }
                    else
                    {
                        summary->num_auto_create_instances++;
                    }
                }
                else
                {
                    summary->num_to_create_instances++;
                }
            }
        }
    }

    // if all the instances are 'auto-create, no-notify', then we don't need
    // the dialog.
    summary->need_dialog
        = (summary->num_instances != 0
           && summary->num_auto_create_no_notify_instances != summary->num_instances);
}

void
gnc_sx_summary_print(const GncSxSummary *summary)
{
    PINFO("num_instances: %d", summary->num_instances);
    PINFO("num_to_create: %d", summary->num_to_create_instances);
    PINFO("num_auto_create_instances: %d", summary->num_auto_create_instances);
    PINFO("num_auto_create_no_notify_instances: %d", summary->num_auto_create_no_notify_instances);
    PINFO("need dialog? %s", summary->need_dialog ? "true" : "false");
}

static void gnc_numeric_free(gpointer data)
{
    gnc_numeric *p = (gnc_numeric*) data;
    g_free(p);
}

GHashTable* gnc_g_hash_new_guid_numeric()
{
    return g_hash_table_new_full (guid_hash_to_guint, guid_g_hash_table_equal,
                                  NULL, gnc_numeric_free);
}

typedef struct
{
    GHashTable *hash;
    GList **creation_errors;
    const SchedXaction *sx;
    gnc_numeric count;
} SxCashflowData;

static void add_to_hash_amount(GHashTable* hash, const GncGUID* guid, const gnc_numeric* amount)
{
    /* Do we have a number belonging to this GUID in the hash? If yes,
     * modify it in-place; if not, insert the new element into the
     * hash. */
    gnc_numeric* elem = g_hash_table_lookup(hash, guid);
    gchar guidstr[GUID_ENCODING_LENGTH+1];
    guid_to_string_buff(guid, guidstr);
    if (!elem)
    {
        elem = g_new0(gnc_numeric, 1);
        *elem = gnc_numeric_zero();
        g_hash_table_insert(hash, (gpointer) guid, elem);
    }

    /* Check input arguments for sanity */
    if (gnc_numeric_check(*amount) != GNC_ERROR_OK)
    {
        g_critical("Oops, the given amount [%s] has the error code %d, at guid [%s].",
                   gnc_num_dbg_to_string(*amount),
                   gnc_numeric_check(*amount),
                   guidstr);
        return;
    }
    if (gnc_numeric_check(*elem) != GNC_ERROR_OK)
    {
        g_critical("Oops, the account's amount [%s] has the error code %d, at guid [%s].",
                   gnc_num_dbg_to_string(*elem),
                   gnc_numeric_check(*elem),
                   guidstr);
        return;
    }

    /* Watch out - don't use gnc_numeric_add_fixed here because it
     * will refuse to add 1/5+1/10; instead, we have to use the flags
     * as given here explicitly. Eventually, add the given amount to
     * the entry in the hash. */
    *elem = gnc_numeric_add(*elem, *amount,
                            GNC_DENOM_AUTO,
                            GNC_HOW_DENOM_REDUCE | GNC_HOW_RND_NEVER);

    /* Check for sanity of the output. */
    if (gnc_numeric_check(*elem) != GNC_ERROR_OK)
    {
        g_critical("Oops, after addition at guid [%s] the resulting amount [%s] has the error code %d; added amount = [%s].",
                   guidstr,
                   gnc_num_dbg_to_string(*elem),
                   gnc_numeric_check(*elem),
                   gnc_num_dbg_to_string(*amount));
        return;
    }

    /* In case anyone wants to see this in the debug log. */
    DEBUG("Adding to guid [%s] the value [%s]. Value now [%s].",
            guidstr,
            gnc_num_dbg_to_string(*amount),
            gnc_num_dbg_to_string(*elem));
}

static gboolean
create_cashflow_helper(Transaction *template_txn, void *user_data)
{
    SxCashflowData *creation_data = user_data;
    GList *template_splits;
    const gnc_commodity *first_cmdty = NULL;

    DEBUG("Evaluating txn desc [%s] for sx [%s]",
            xaccTransGetDescription(template_txn),
            xaccSchedXactionGetName(creation_data->sx));

    template_splits = xaccTransGetSplitList(template_txn);

    if (template_splits == NULL)
    {
        g_critical("transaction w/o splits for sx [%s]",
                   xaccSchedXactionGetName(creation_data->sx));
        return FALSE;
    }

    for (;
         template_splits;
         template_splits = template_splits->next)
    {
        Account *split_acct;
        const gnc_commodity *split_cmdty = NULL;
        const Split *template_split = (const Split*) template_splits->data;

        /* Get the account that should be used for this split. */
        if (!_get_template_split_account(creation_data->sx, template_split, &split_acct, creation_data->creation_errors))
        {
            DEBUG("Could not find account for split");
            break;
        }

        /* The split's account also has some commodity */
        split_cmdty = xaccAccountGetCommodity(split_acct);
        if (first_cmdty == NULL)
        {
            first_cmdty = split_cmdty;
            //xaccTransSetCurrency(new_txn, first_cmdty);
        }

        {
            gnc_numeric credit_num = gnc_numeric_zero();
            gnc_numeric debit_num = gnc_numeric_zero();
            gnc_numeric final_once, final;
            gint gncn_error;

            /* Credit value */
            _get_sx_formula_value(creation_data->sx, template_split,
				  &credit_num, creation_data->creation_errors,
				  "sx-credit-formula", "sx-credit-numeric",
				  NULL);
            /* Debit value */
            _get_sx_formula_value(creation_data->sx, template_split,
				  &debit_num, creation_data->creation_errors,
				  "sx-debit-formula", "sx-debit-numeric", NULL);

            /* The resulting cash flow number: debit minus credit,
             * multiplied with the count factor. */
            final_once = gnc_numeric_sub_fixed( debit_num, credit_num );
            /* Multiply with the count factor. */
            final = gnc_numeric_mul(final_once, creation_data->count,
                                    gnc_numeric_denom(final_once),
                                    GNC_HOW_RND_ROUND_HALF_UP);

            gncn_error = gnc_numeric_check(final);
            if (gncn_error != GNC_ERROR_OK)
            {
                gchar* err = N_("Error %d in SX [%s] final gnc_numeric value, using 0 instead.");
                REPORT_ERROR(creation_data->creation_errors, err,
                             gncn_error, xaccSchedXactionGetName(creation_data->sx));
                final = gnc_numeric_zero();
            }

            /* Print error message if we would have needed an exchange rate */
            if (! gnc_commodity_equal(split_cmdty, first_cmdty))
            {
                gchar *err = N_("No exchange rate available in SX [%s] for %s -> %s, value is zero.");
                REPORT_ERROR(creation_data->creation_errors, err,
                             xaccSchedXactionGetName(creation_data->sx),
                             gnc_commodity_get_mnemonic(split_cmdty),
                             gnc_commodity_get_mnemonic(first_cmdty));
                final = gnc_numeric_zero();
            }

            /* And add the resulting value to the hash */
            add_to_hash_amount(creation_data->hash, xaccAccountGetGUID(split_acct), &final);
        }
    }

    return FALSE;
}

static void
instantiate_cashflow_internal(const SchedXaction* sx,
                              GHashTable* map,
                              GList **creation_errors, gint count)
{
    SxCashflowData create_cashflow_data;
    Account* sx_template_account = gnc_sx_get_template_transaction_account(sx);

    if (!sx_template_account)
    {
        g_critical("Huh? No template account for the SX %s", xaccSchedXactionGetName(sx));
        return;
    }

    if (!xaccSchedXactionGetEnabled(sx))
    {
        DEBUG("Skipping non-enabled SX [%s]",
                xaccSchedXactionGetName(sx));
        return;
    }

    create_cashflow_data.hash = map;
    create_cashflow_data.creation_errors = creation_errors;
    create_cashflow_data.sx = sx;
    create_cashflow_data.count = gnc_numeric_create(count, 1);

    /* The cash flow numbers are in the transactions of the template
     * account, so run this foreach on the transactions. */
    xaccAccountForEachTransaction(sx_template_account,
                                  create_cashflow_helper,
                                  &create_cashflow_data);
}

typedef struct
{
    GHashTable *hash;
    GList **creation_errors;
    const GDate *range_start;
    const GDate *range_end;
} SxAllCashflow;

static void instantiate_cashflow_cb(gpointer data, gpointer _user_data)
{
    const SchedXaction* sx = (const SchedXaction*) data;
    SxAllCashflow* userdata = (SxAllCashflow*) _user_data;
    gint count;

    g_assert(sx);
    g_assert(userdata);

    /* How often does this particular SX occur in the date range? */
    count = gnc_sx_get_num_occur_daterange(sx, userdata->range_start,
                                           userdata->range_end);
    if (count > 0)
    {
        /* If it occurs at least once, calculate ("instantiate") its
         * cash flow and add it to the result
         * g_hash<GUID,gnc_numeric> */
        instantiate_cashflow_internal(sx,
                                      userdata->hash,
                                      userdata->creation_errors,
                                      count);
    }
}

void gnc_sx_all_instantiate_cashflow(GList *all_sxes,
                                     const GDate *range_start, const GDate *range_end,
                                     GHashTable* map, GList **creation_errors)
{
    SxAllCashflow userdata;
    userdata.hash = map;
    userdata.creation_errors = creation_errors;
    userdata.range_start = range_start;
    userdata.range_end = range_end;

    /* The work is done in the callback for each SX */
    g_list_foreach(all_sxes, instantiate_cashflow_cb, &userdata);
}


GHashTable* gnc_sx_all_instantiate_cashflow_all(GDate range_start, GDate range_end)
{
    GHashTable *result_map = gnc_g_hash_new_guid_numeric();
    GList *all_sxes = gnc_book_get_schedxactions(gnc_get_current_book())->sx_list;
    gnc_sx_all_instantiate_cashflow(all_sxes,
                                    &range_start, &range_end,
                                    result_map, NULL);
    return result_map;
}
