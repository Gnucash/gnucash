/* 
 * gnc-sx-instance-model.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#include "config.h"
#include <glib.h>
#include <glib-object.h>

#include "Account.h"
#include "SchedXaction.h"
#include "Split.h"
#include "SX-book.h"
#include "Transaction.h"
#include "gnc-book.h"
#include "gnc-commodity.h"
#include "gnc-event.h"
#include "gnc-exp-parser.h"
#include "gnc-glib-utils.h"
#include "gnc-sx-instance-model.h"
#include "gnc-ui-util.h"
#include "qof.h"

static void gnc_sx_instance_model_class_init (GncSxInstanceModelClass *klass);
static void gnc_sx_instance_model_init(GTypeInstance *instance, gpointer klass);
static GncSxInstanceModel* gnc_sx_instance_model_new(void);

static GncSxInstance* gnc_sx_instance_new(GncSxInstances *parent, GncSxInstanceState state, GDate *date, void *temporal_state, gint sequence_num);

static void sxsl_get_sx_vars(SchedXaction *sx, GHashTable *var_hash);
static gint _get_vars_helper(Transaction *txn, void *var_hash_data);
static int parse_vars_from_formula(const char *formula, GHashTable *varHash, gnc_numeric *result);

static GncSxVariable* gnc_sx_variable_new(gchar *name);

static void _gnc_sx_instance_event_handler(QofEntity *ent, QofEventId event_type, gpointer user_data, gpointer evt_data);

/* ------------------------------------------------------------ */

static void
_sx_var_to_raw_numeric(gchar *name, GncSxVariable *var, GHashTable *parser_var_hash)
{
     g_hash_table_insert(parser_var_hash, name, &var->value);
}

static void
_var_numeric_to_sx_var(gchar *name, gnc_numeric *num, GHashTable *sx_var_hash)
{
     gpointer p_var;
     if (!g_hash_table_lookup_extended(sx_var_hash, name, NULL, &p_var))
     {
          p_var = (gpointer)gnc_sx_variable_new(name);
          g_hash_table_insert(sx_var_hash, name, p_var);
     }
     ((GncSxVariable*)p_var)->value = *num;
}

static void
_wipe_parsed_sx_var(gchar *key, GncSxVariable *var, gpointer unused_user_data)
{
     var->value = gnc_numeric_error(GNC_ERROR_ARG);
}

/**
 * @return caller-owned.
 **/
GHashTable*
gnc_sx_instance_get_variables_for_parser(GHashTable *instance_var_hash)
{
     GHashTable *parser_vars;
     parser_vars = g_hash_table_new(g_str_hash, g_str_equal);
     g_hash_table_foreach(instance_var_hash, (GHFunc)_sx_var_to_raw_numeric, parser_vars);
     return parser_vars;
}

static int
parse_vars_from_formula(const char *formula,
                        GHashTable *var_hash,
                        gnc_numeric *result)
{
     gnc_numeric num;
     char *errLoc;
     int toRet = 0;
     GHashTable *parser_vars;

     // convert var_hash -> variables for the parser.
     parser_vars = gnc_sx_instance_get_variables_for_parser(var_hash);

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
     var->name = name;
     var->value = gnc_numeric_error(GNC_ERROR_ARG);
     var->editable = TRUE;
     return var;
}

static gint
_get_vars_helper(Transaction *txn, void *var_hash_data)
{
     GHashTable *var_hash = (GHashTable*)var_hash_data;
     GList *split_list;
     kvp_frame *kvpf;
     kvp_value *kvp_val;
     Split *s;
     char *str;
     gnc_commodity *first_cmdty = NULL;

     split_list = xaccTransGetSplitList(txn);
     if (split_list == NULL)
     {
          return 1;
     }

     for ( ; split_list; split_list = split_list->next)
     {
          gnc_commodity *split_cmdty = NULL;
          GUID *acct_guid;
          Account *acct;

          s = (Split*)split_list->data;
          kvpf = xaccSplitGetSlots(s);
          kvp_val = kvp_frame_get_slot_path(kvpf,
                                            GNC_SX_ID,
                                            GNC_SX_ACCOUNT,
                                            NULL);
          acct_guid = kvp_value_get_guid(kvp_val);
          acct = xaccAccountLookup(acct_guid, gnc_get_current_book());
          split_cmdty = xaccAccountGetCommodity(acct);
          if (first_cmdty == NULL)
          {
               first_cmdty = split_cmdty;
          }
                
          if (! gnc_commodity_equal(split_cmdty, first_cmdty))
          {
               GncSxVariable *var;
               GString *var_name;

               var_name = g_string_sized_new(16);
               g_string_printf(var_name, "%s -> %s",
                               gnc_commodity_get_mnemonic(split_cmdty),
                               gnc_commodity_get_mnemonic(first_cmdty));
               var = gnc_sx_variable_new(g_strdup(var_name->str));
               g_hash_table_insert(var_hash, var->name, var);
               g_string_free(var_name, TRUE);
          }

          // existing... ------------------------------------------
          kvp_val = kvp_frame_get_slot_path(kvpf,
                                            GNC_SX_ID,
                                            GNC_SX_CREDIT_FORMULA,
                                            NULL);
          if (kvp_val != NULL)
          {
               str = kvp_value_get_string(kvp_val);
               if (str && strlen(str) != 0)
               {
                    parse_vars_from_formula(str, var_hash, NULL);
               }
          }

          kvp_val = kvp_frame_get_slot_path(kvpf,
                                            GNC_SX_ID,
                                            GNC_SX_DEBIT_FORMULA,
                                            NULL);
          if (kvp_val != NULL)
          {
               str = kvp_value_get_string(kvp_val);
               if (str && strlen(str) != 0)
               {
                    parse_vars_from_formula(str, var_hash, NULL);
               }
          }
     }

     return 0;
}

Account*
gnc_sx_get_template_transaction_account(SchedXaction *sx)
{
     AccountGroup *template_group;
     Account *sx_template_acct;
     const char *sx_guid_str;

     template_group = gnc_book_get_template_group(gnc_get_current_book());
     sx_guid_str = guid_to_string(xaccSchedXactionGetGUID(sx));
     /* Get account named after guid string. */
     sx_template_acct = xaccGetAccountFromName(template_group, sx_guid_str);
     return sx_template_acct;
}

static void
sxsl_get_sx_vars(SchedXaction *sx, GHashTable *var_hash)
{
     Account *sx_template_acct;
     sx_template_acct = gnc_sx_get_template_transaction_account(sx);
     xaccAccountForEachTransaction(sx_template_acct, _get_vars_helper, var_hash);
}

static void
_clone_sx_var_hash_entry(gpointer key, gpointer value, gpointer user_data)
{
     GHashTable *to = (GHashTable*)user_data;
     GncSxVariable *to_copy = (GncSxVariable*)value;
     GncSxVariable *var = gnc_sx_variable_new(to_copy->name);
     var->value = to_copy->value;
     var->editable = to_copy->editable;
     g_hash_table_insert(to, key, var);
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
          parent->variable_names = g_hash_table_new(g_str_hash, g_str_equal);
          sxsl_get_sx_vars(parent->sx, parent->variable_names);
          g_hash_table_foreach(parent->variable_names, (GHFunc)_wipe_parsed_sx_var, NULL);
          parent->variable_names_parsed = TRUE;

          // @@fixme: add sequence_num as `i`, non-editable
     }

     rtn->variable_bindings = g_hash_table_new(g_str_hash, g_str_equal);
     g_hash_table_foreach(parent->variable_names, _clone_sx_var_hash_entry, rtn->variable_bindings);
     return rtn;
}

static void
_build_list_from_hash_elts(gpointer key, gpointer value, gpointer user_data)
{
     GList **list = (GList**)user_data;
     *list = g_list_append(*list, value);
}

GList *
gnc_sx_instance_get_variables(GncSxInstance *inst)
{
     GList *vars = NULL;
     g_hash_table_foreach(inst->variable_bindings, _build_list_from_hash_elts, &vars);
     
     // @@fixme sort by name
     return vars;
}

static GncSxInstances*
_gnc_sx_gen_instances(gpointer *data, gpointer user_data)
{
     GncSxInstances *instances = g_new0(GncSxInstances, 1);
     SchedXaction *sx = (SchedXaction*)data;
     GDate *range_end = (GDate*)user_data;
     GDate creation_end, remind_end;
     GDate cur_date;
     void *sequence_ctx;

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
               inst = gnc_sx_instance_new(instances, SX_INSTANCE_STATE_POSTPONED, &inst_date, postponed->data, seq_num);
               //inst->temporal_state = postponed->data;
               instances->list = g_list_append(instances->list, inst);
          }
     }

     /* to-create */
     g_date_clear(&cur_date, 1);
     sequence_ctx = gnc_sx_create_temporal_state(sx);
     cur_date = xaccSchedXactionGetInstanceAfter(sx, &cur_date, sequence_ctx);
     instances->next_instance_date = cur_date;
     while (g_date_valid(&cur_date) && g_date_compare(&cur_date, &creation_end) <= 0)
     {
          GncSxInstance *inst;
          int seq_num;
          seq_num = gnc_sx_get_instance_count(sx, sequence_ctx);
          inst = gnc_sx_instance_new(instances, SX_INSTANCE_STATE_TO_CREATE, &cur_date, sequence_ctx, seq_num);
          instances->list = g_list_append(instances->list, inst);
          gnc_sx_incr_temporal_state(sx, sequence_ctx);
          cur_date = xaccSchedXactionGetInstanceAfter(sx, &cur_date, sequence_ctx);
     }

     /* reminders */
     while (g_date_valid(&cur_date) && g_date_compare(&cur_date, &remind_end) <= 0)
     {
          GncSxInstance *inst;
          int seq_num;
          seq_num = gnc_sx_get_instance_count(sx, sequence_ctx);
          inst = gnc_sx_instance_new(instances, SX_INSTANCE_STATE_REMINDER, &cur_date, sequence_ctx, seq_num);
          instances->list = g_list_append(instances->list, inst);
          gnc_sx_incr_temporal_state(sx, sequence_ctx);
          cur_date = xaccSchedXactionGetInstanceAfter(sx, &cur_date, sequence_ctx);
     }

     return instances;
}

GncSxInstanceModel*
gnc_sx_get_instances(GDate *range_end)
{
     GncSxInstanceModel *instances;
     GList *sxes;

     g_assert(range_end != NULL);
     g_assert(g_date_valid(range_end));

     instances = gnc_sx_instance_model_new();
     instances->range_end = *range_end;
     sxes = gnc_book_get_schedxactions(gnc_get_current_book())->sx_list;
     instances->sx_instance_list = gnc_g_list_map(sxes, (GncGMapFunc)_gnc_sx_gen_instances, range_end);

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
     if (type == 0) {
          static const GTypeInfo info = {
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
gnc_sx_instance_model_dispose (GObject *object)
{
     printf("dispose\n");
}

static void
gnc_sx_instance_model_finalize (GObject *object)
{
     printf("finalize\n");
}

static void
gnc_sx_instance_model_class_init (GncSxInstanceModelClass *klass)
{
     GObjectClass *object_class = G_OBJECT_CLASS(klass);
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
                       g_cclosure_marshal_VOID__VOID,
                       G_TYPE_NONE,
                       0, NULL);

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
_gnc_sx_instance_find_by_sx(GncSxInstances *in_list_instances, GncSxInstances *to_find)
{
     if (in_list_instances->sx == to_find->sx)
          return 0;
     return -1;
}

static void
_gnc_sx_instance_event_handler(QofEntity *ent, QofEventId event_type, gpointer user_data, gpointer evt_data)
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
          sx = GNC_SX(ent);
          if (event_type & QOF_EVENT_MODIFY)
          {
               GncSxInstances *new_instances;
               GList *link;

               new_instances = _gnc_sx_gen_instances((gpointer)sx, &instances->range_end);

               link = g_list_find_custom(instances->sx_instance_list, new_instances, (GCompareFunc)_gnc_sx_instance_find_by_sx);
               g_assert(link != NULL);
               // @fixme g_object_unref(link->data);
               link->data = new_instances;
               g_signal_emit_by_name(instances, "updated"); // , new_instances[->sx].
          }
          /* else { unsupported event type; ignore } */
     }
     else if (GNC_IS_SXES(ent))
     {
          SchedXactions *sxes = GNC_SXES(ent);
          SchedXaction *sx = GNC_SX(evt_data);

          sxes = NULL;
          if (event_type & GNC_EVENT_ITEM_REMOVED)
          {
               gpointer sx_instance_to_remove = NULL;
               GList *list;

               /* find, remove, update */
               for (list = instances->sx_instance_list; list != NULL; list = list->next)
               {
                    if (sx == ((GncSxInstances*)list->data)->sx)
                    {
                         sx_instance_to_remove = list->data;
                         break;
                    }
               }
               if (sx_instance_to_remove != NULL)
               {
                    g_signal_emit_by_name(instances, "removing", GUINT_TO_POINTER(GPOINTER_TO_UINT(((GncSxInstances*)sx_instance_to_remove)->sx)));
                    instances->sx_instance_list = g_list_remove(instances->sx_instance_list, sx_instance_to_remove);
                    g_signal_emit_by_name(instances, "updated"); // @@fixme remove when callers support "removing"
               }
               // @@fixme: uh, actually remove...?
               else { printf("err\n"); }
          }
          else if (event_type & GNC_EVENT_ITEM_ADDED)
          {
               /* generate instances, add to instance list, emit update. */
               instances->sx_instance_list
                    = g_list_append(instances->sx_instance_list,
                                    (*_gnc_sx_gen_instances)((gpointer)sx, (gpointer)&instances->range_end));
               g_signal_emit_by_name(instances, "added", GUINT_TO_POINTER(GPOINTER_TO_UINT(sx)));
               g_signal_emit_by_name(instances, "updated"); // @fixme remove when callers look for "added".
          }
          /* else { printf("unsupported event type [%d]\n", event_type); } */
     }
}
