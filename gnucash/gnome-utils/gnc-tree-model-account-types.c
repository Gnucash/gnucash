/*
 * gnc-tree-model-account-types.c -- GTK4 list model for account types.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2005, 2006 Chris Shoemaker <c.shoemaker@cox.net>
 * Copyright (C) 2006 Eskil Bylund <eskil.bylund@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-tree-model-account-types.h"
#include "Account.h"

enum
{
    PROP_0,
    PROP_ACCOUNT_TYPE,
    PROP_NAME,
    PROP_SELECTED,
    N_PROPERTIES
};

struct _GncAccountTypeItem
{
    GObject parent_instance;

    GNCAccountType account_type;
    gchar *name;
    gboolean selected;
};

G_DEFINE_TYPE (GncAccountTypeItem, gnc_account_type_item, G_TYPE_OBJECT)

static GParamSpec *properties[N_PROPERTIES];

static void
account_type_item_set_property (GObject *object, guint property_id,
                                const GValue *value, GParamSpec *pspec)
{
    GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (object);

    switch (property_id)
    {
    case PROP_ACCOUNT_TYPE:
        item->account_type = g_value_get_int (value);
        g_free (item->name);
        item->name = g_strdup (xaccAccountGetTypeStr (item->account_type));
        break;
    case PROP_SELECTED:
        gnc_account_type_item_set_selected (item, g_value_get_boolean (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

static void
account_type_item_get_property (GObject *object, guint property_id,
                                GValue *value, GParamSpec *pspec)
{
    GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (object);

    switch (property_id)
    {
    case PROP_ACCOUNT_TYPE:
        g_value_set_int (value, item->account_type);
        break;
    case PROP_NAME:
        g_value_set_string (value, item->name);
        break;
    case PROP_SELECTED:
        g_value_set_boolean (value, item->selected);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

static void
account_type_item_finalize (GObject *object)
{
    GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (object);

    g_clear_pointer (&item->name, g_free);
    G_OBJECT_CLASS (gnc_account_type_item_parent_class)->finalize (object);
}

static void
gnc_account_type_item_class_init (GncAccountTypeItemClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->set_property = account_type_item_set_property;
    object_class->get_property = account_type_item_get_property;
    object_class->finalize = account_type_item_finalize;

    properties[PROP_ACCOUNT_TYPE] =
        g_param_spec_int ("account-type", "Account Type",
                          "GnuCash account type", ACCT_TYPE_NONE,
                          NUM_ACCOUNT_TYPES - 1, ACCT_TYPE_NONE,
                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY |
                          G_PARAM_STATIC_STRINGS);
    properties[PROP_NAME] =
        g_param_spec_string ("name", "Name", "Localized account type name",
                             NULL, G_PARAM_READABLE | G_PARAM_STATIC_STRINGS);
    properties[PROP_SELECTED] =
        g_param_spec_boolean ("selected", "Selected",
                              "Whether this account type is selected", FALSE,
                              G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
    g_object_class_install_properties (object_class, N_PROPERTIES, properties);
}

static void
gnc_account_type_item_init (GncAccountTypeItem *item)
{
    item->account_type = ACCT_TYPE_NONE;
}

static GncAccountTypeItem *
account_type_item_new (GNCAccountType account_type)
{
    return g_object_new (GNC_TYPE_ACCOUNT_TYPE_ITEM,
                         "account-type", account_type,
                         NULL);
}

static gint
account_type_item_compare (gconstpointer left, gconstpointer right,
                           gpointer user_data)
{
    const GncAccountTypeItem *left_item = left;
    const GncAccountTypeItem *right_item = right;

    (void)user_data;
    return g_utf8_collate (left_item->name, right_item->name);
}

static GListModel *
account_type_list_new (guint32 types, gboolean with_placeholder)
{
    GListStore *store = g_list_store_new (GNC_TYPE_ACCOUNT_TYPE_ITEM);

    for (gint type = ACCT_TYPE_NONE + 1; type < NUM_ACCOUNT_TYPES; type++)
    {
        GncAccountTypeItem *item;

        if ((types & (1u << type)) == 0)
            continue;

        item = account_type_item_new (type);
        g_list_store_insert_sorted (store, item, account_type_item_compare, NULL);
        g_object_unref (item);
    }

    if (with_placeholder && types != 0)
    {
        GncAccountTypeItem *item = account_type_item_new (ACCT_TYPE_NONE);

        g_list_store_insert (store, 0, item);
        g_object_unref (item);
    }

    return G_LIST_MODEL (store);
}

GListModel *
gnc_account_type_list_new (guint32 types)
{
    return account_type_list_new (types, FALSE);
}

GListModel *
gnc_account_type_list_new_with_placeholder (guint32 types)
{
    return account_type_list_new (types, TRUE);
}

GNCAccountType
gnc_account_type_item_get_account_type (GncAccountTypeItem *item)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT_TYPE_ITEM (item), ACCT_TYPE_NONE);

    return item->account_type;
}

const gchar *
gnc_account_type_item_get_name (GncAccountTypeItem *item)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT_TYPE_ITEM (item), NULL);

    return item->name;
}

gboolean
gnc_account_type_item_get_selected (GncAccountTypeItem *item)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT_TYPE_ITEM (item), FALSE);

    return item->selected;
}

void
gnc_account_type_item_set_selected (GncAccountTypeItem *item,
                                    gboolean selected)
{
    g_return_if_fail (GNC_IS_ACCOUNT_TYPE_ITEM (item));

    selected = !!selected;
    if (item->selected == selected)
        return;

    item->selected = selected;
    g_object_notify_by_pspec (G_OBJECT (item), properties[PROP_SELECTED]);
}
