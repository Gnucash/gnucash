/*
 * Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
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

/** @addtogroup budget
 *     @{ */

#include <config.h>

#include "gnc-tree-model-budget.h"
#include "gnc-budget.h"
#include "gnc-ui-util.h"

struct _GncBudgetListItem
{
    GObject parent_instance;
    GncGUID guid;
    gchar *name;
    gchar *description;
};

G_DEFINE_TYPE (GncBudgetListItem, gnc_budget_list_item, G_TYPE_OBJECT)

enum
{
    PROP_0,
    PROP_NAME,
    PROP_DESCRIPTION,
    N_PROPERTIES
};

static GParamSpec *properties[N_PROPERTIES] = { NULL };

static void
budget_list_item_get_property (GObject *object, guint property_id,
                               GValue *value, GParamSpec *pspec)
{
    GncBudgetListItem *item = GNC_BUDGET_LIST_ITEM (object);

    switch (property_id)
    {
    case PROP_NAME:
        g_value_set_string (value, item->name);
        break;
    case PROP_DESCRIPTION:
        g_value_set_string (value, item->description);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

static void
budget_list_item_finalize (GObject *object)
{
    GncBudgetListItem *item = GNC_BUDGET_LIST_ITEM (object);

    g_clear_pointer (&item->name, g_free);
    g_clear_pointer (&item->description, g_free);
    G_OBJECT_CLASS (gnc_budget_list_item_parent_class)->finalize (object);
}

static void
gnc_budget_list_item_class_init (GncBudgetListItemClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->get_property = budget_list_item_get_property;
    object_class->finalize = budget_list_item_finalize;
    properties[PROP_NAME] =
        g_param_spec_string ("name", "Name", "The budget name", NULL,
                             G_PARAM_READABLE | G_PARAM_STATIC_STRINGS);
    properties[PROP_DESCRIPTION] =
        g_param_spec_string ("description", "Description", "The budget description", NULL,
                             G_PARAM_READABLE | G_PARAM_STATIC_STRINGS);
    g_object_class_install_properties (object_class, N_PROPERTIES, properties);
}

static void
gnc_budget_list_item_init (GncBudgetListItem *item)
{
}

static GncBudgetListItem *
budget_list_item_new (GncBudget *budget)
{
    GncBudgetListItem *item = g_object_new (GNC_TYPE_BUDGET_LIST_ITEM, NULL);

    item->guid = *gnc_budget_get_guid (budget);
    item->name = g_strdup (gnc_budget_get_name (budget));
    item->description = g_strdup (gnc_budget_get_description (budget));
    return item;
}

/* Add a budget object to the list model. */
static void
add_budget_to_model (QofInstance *data, gpointer user_data)
{
    GncBudget *budget = GNC_BUDGET (data);
    GListStore *store = G_LIST_STORE (user_data);

    g_return_if_fail (GNC_IS_BUDGET (budget));
    g_return_if_fail (store != NULL);

    GncBudgetListItem *item = budget_list_item_new (budget);
    g_list_store_append (store, item);
    g_object_unref (item);
}

GListModel *
gnc_budget_list_model_new (QofBook *book)
{
    GListStore *store = g_list_store_new (GNC_TYPE_BUDGET_LIST_ITEM);

    qof_collection_foreach (qof_book_get_collection (book, GNC_ID_BUDGET),
                            add_budget_to_model, store);
    return G_LIST_MODEL (store);
}

GncBudget *
gnc_budget_list_item_get_budget (GncBudgetListItem *item)
{
    g_return_val_if_fail (GNC_IS_BUDGET_LIST_ITEM (item), NULL);

    return gnc_budget_lookup (&item->guid, gnc_get_current_book ());
}

const gchar *
gnc_budget_list_item_get_name (GncBudgetListItem *item)
{
    g_return_val_if_fail (GNC_IS_BUDGET_LIST_ITEM (item), NULL);

    return item->name;
}

const gchar *
gnc_budget_list_item_get_description (GncBudgetListItem *item)
{
    g_return_val_if_fail (GNC_IS_BUDGET_LIST_ITEM (item), NULL);

    return item->description;
}

guint
gnc_budget_list_model_get_position (GListModel *model, GncBudget *budget)
{
    const GncGUID *guid;
    guint count;

    g_return_val_if_fail (G_IS_LIST_MODEL (model), G_MAXUINT);
    g_return_val_if_fail (GNC_IS_BUDGET (budget), G_MAXUINT);

    guid = gnc_budget_get_guid (budget);
    count = g_list_model_get_n_items (model);

    for (guint index = 0; index < count; index++)
    {
        GncBudgetListItem *item = GNC_BUDGET_LIST_ITEM (
            g_list_model_get_item (model, index));
        gboolean found = guid_equal (guid, &item->guid);

        g_object_unref (item);
        if (found)
            return index;
    }

    return G_MAXUINT;
}

/** @} */
