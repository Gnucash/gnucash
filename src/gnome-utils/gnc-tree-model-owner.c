/*
 * gnc-tree-model-owner.c -- GtkTreeModel implementation to
 *	display owners in a GtkTreeView.
 *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-model-owner.h"
#include "gnc-component-manager.h"
#include "gncOwner.h"
#include "gnc-commodity.h"
#include "gnc-prefs.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-gobject-utils.h"
#include "gnc-ui-balances.h"
#include "gnc-ui-util.h"

#define TREE_MODEL_OWNER_CM_CLASS "tree-model-owner"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_model_owner_class_init (GncTreeModelOwnerClass *klass);
static void gnc_tree_model_owner_init (GncTreeModelOwner *model);
static void gnc_tree_model_owner_finalize (GObject *object);
static void gnc_tree_model_owner_dispose (GObject *object);

/** Implementation of GtkTreeModel  **************************************/
static void gnc_tree_model_owner_tree_model_init (GtkTreeModelIface *iface);
static GtkTreeModelFlags gnc_tree_model_owner_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_owner_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_owner_get_column_type (GtkTreeModel *tree_model,
        int index);
static gboolean gnc_tree_model_owner_get_iter (GtkTreeModel *tree_model,
        GtkTreeIter *iter,
        GtkTreePath *path);
static GtkTreePath *gnc_tree_model_owner_get_path (GtkTreeModel *tree_model,
        GtkTreeIter *iter);
static void gnc_tree_model_owner_get_value (GtkTreeModel *tree_model,
        GtkTreeIter *iter,
        int column,
        GValue *value);
static gboolean	gnc_tree_model_owner_iter_next (GtkTreeModel *tree_model,
        GtkTreeIter *iter);
static gboolean	gnc_tree_model_owner_iter_children (GtkTreeModel *tree_model,
        GtkTreeIter *iter,
        GtkTreeIter *parent);
static gboolean	gnc_tree_model_owner_iter_has_child (GtkTreeModel *tree_model,
        GtkTreeIter *iter);
static int gnc_tree_model_owner_iter_n_children (GtkTreeModel *tree_model,
        GtkTreeIter *iter);
static gboolean	gnc_tree_model_owner_iter_nth_child (GtkTreeModel *tree_model,
        GtkTreeIter *iter,
        GtkTreeIter *parent,
        int n);
static gboolean	gnc_tree_model_owner_iter_parent (GtkTreeModel *tree_model,
        GtkTreeIter *iter,
        GtkTreeIter *child);

/** Component Manager Callback ******************************************/
static void gnc_tree_model_owner_event_handler (QofInstance *entity,
        QofEventId event_type,
        GncTreeModelOwner *model,
        GncEventData *ed);

/** The instance private data for an owner tree model. */
typedef struct GncTreeModelOwnerPrivate
{
    QofBook *book;
    GncOwnerType owner_type;
    OwnerList *owner_list;
    gint event_handler_id;
    const gchar *negative_color;
} GncTreeModelOwnerPrivate;

#define GNC_TREE_MODEL_OWNER_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_MODEL_OWNER, GncTreeModelOwnerPrivate))


/************************************************************/
/*           Owner Tree Model - Misc Functions            */
/************************************************************/

/** Tell the GncTreeModelOwner code to update the color that it will
 *  use for negative numbers.  This function will iterate over all
 *  existing models and update their setting.
 *
 *  @internal
 */
static void
gnc_tree_model_owner_update_color (gpointer gsettings, gchar *key, gpointer user_data)
{
    GncTreeModelOwnerPrivate *priv;
    GncTreeModelOwner *model;
    gboolean use_red;

    g_return_if_fail(GNC_IS_TREE_MODEL_OWNER(user_data));
    model = user_data;
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
    use_red = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    priv->negative_color = use_red ? "red" : NULL;
}
/************************************************************/
/*               g_object required functions                */
/************************************************************/

/** A pointer to the parent class of an owner tree model. */
static GtkObjectClass *parent_class = NULL;

GType
gnc_tree_model_owner_get_type (void)
{
    static GType gnc_tree_model_owner_type = 0;

    if (gnc_tree_model_owner_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeModelOwnerClass), /* class_size */
            NULL,                            /* base_init */
            NULL,                            /* base_finalize */
            (GClassInitFunc) gnc_tree_model_owner_class_init,
            NULL,                            /* class_finalize */
            NULL,                            /* class_data */
            sizeof (GncTreeModelOwner),      /* */
            0,                               /* n_preallocs */
            (GInstanceInitFunc) gnc_tree_model_owner_init
        };

        static const GInterfaceInfo tree_model_info =
        {
            (GInterfaceInitFunc) gnc_tree_model_owner_tree_model_init,
            NULL,
            NULL
        };

        gnc_tree_model_owner_type = g_type_register_static (GNC_TYPE_TREE_MODEL,
                                    GNC_TREE_MODEL_OWNER_NAME,
                                    &our_info, 0);

        g_type_add_interface_static (gnc_tree_model_owner_type,
                                     GTK_TYPE_TREE_MODEL,
                                     &tree_model_info);
    }

    return gnc_tree_model_owner_type;
}

static void
gnc_tree_model_owner_class_init (GncTreeModelOwnerClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    o_class = G_OBJECT_CLASS (klass);

    /* GObject signals */
    o_class->finalize = gnc_tree_model_owner_finalize;
    o_class->dispose = gnc_tree_model_owner_dispose;

    g_type_class_add_private(klass, sizeof(GncTreeModelOwnerPrivate));
}

static void
gnc_tree_model_owner_init (GncTreeModelOwner *model)
{
    GncTreeModelOwnerPrivate *priv;
    gboolean red;

    ENTER("model %p", model);
    while (model->stamp == 0)
    {
        model->stamp = g_random_int ();
    }

    red = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);

    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
    priv->book       = NULL;
    priv->owner_list = NULL;
    priv->owner_type = GNC_OWNER_NONE;
    priv->negative_color = red ? "red" : NULL;

    gnc_prefs_register_cb(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                          gnc_tree_model_owner_update_color,
                          model);

    LEAVE(" ");
}

static void
gnc_tree_model_owner_finalize (GObject *object)
{
    GncTreeModelOwnerPrivate *priv;
    GncTreeModelOwner *model;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL_OWNER (object));

    ENTER("model %p", object);

    model = GNC_TREE_MODEL_OWNER (object);
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    priv->book       = NULL;
    priv->owner_list = NULL;

    if (G_OBJECT_CLASS (parent_class)->finalize)
        G_OBJECT_CLASS(parent_class)->finalize (object);
    LEAVE(" ");
}

static void
gnc_tree_model_owner_dispose (GObject *object)
{
    GncTreeModelOwnerPrivate *priv;
    GncTreeModelOwner *model;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL_OWNER (object));

    ENTER("model %p", object);

    model = GNC_TREE_MODEL_OWNER (object);
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    if (priv->event_handler_id)
    {
        qof_event_unregister_handler (priv->event_handler_id);
        priv->event_handler_id = 0;
    }

    gnc_prefs_remove_cb_by_func(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED,
                                gnc_tree_model_owner_update_color,
                                model);

    if (G_OBJECT_CLASS (parent_class)->dispose)
        G_OBJECT_CLASS (parent_class)->dispose (object);
    LEAVE(" ");
}


/************************************************************/
/*                   New Model Creation                     */
/************************************************************/

GtkTreeModel *
gnc_tree_model_owner_new (GncOwnerType owner_type)
{
    GncTreeModelOwner *model;
    GncTreeModelOwnerPrivate *priv;
    const GList *item;

    ENTER("owner_type %d", owner_type);
    item = gnc_gobject_tracking_get_list(GNC_TREE_MODEL_OWNER_NAME);
    for ( ; item; item = g_list_next(item))
    {
        model = (GncTreeModelOwner *)item->data;
        priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
        if (priv->owner_type == owner_type)
        {
            g_object_ref(G_OBJECT(model));
            LEAVE("returning existing model %p", model);
            return GTK_TREE_MODEL(model);
        }
    }

    model = g_object_new (GNC_TYPE_TREE_MODEL_OWNER,
                          NULL);

    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
    priv->book = gnc_get_current_book();
    priv->owner_type = owner_type;
    priv->owner_list = gncBusinessGetOwnerList (priv->book, gncOwnerTypeToQofIdType(owner_type), TRUE);

    priv->event_handler_id = qof_event_register_handler
                             ((QofEventHandler)gnc_tree_model_owner_event_handler, model);

    LEAVE("model %p", model);
    return GTK_TREE_MODEL (model);
}


/************************************************************/
/*        Gnc Tree Model Debugging Utility Function         */
/************************************************************/

#define ITER_STRING_LEN 128

static const gchar *
iter_to_string (GtkTreeIter *iter)
{
#ifdef G_THREADS_ENABLED
#ifndef HAVE_GLIB_2_32
    static GStaticPrivate gtmits_buffer_key = G_STATIC_PRIVATE_INIT;
    gchar *string;

    string = g_static_private_get (&gtmits_buffer_key);
    if (string == NULL)
    {
        string = g_malloc(ITER_STRING_LEN + 1);
        g_static_private_set (&gtmits_buffer_key, string, g_free);
    }
#else
    static GPrivate gtmits_buffer_key = G_PRIVATE_INIT(g_free);
    gchar *string;

    string = g_private_get (&gtmits_buffer_key);
    if (string == NULL)
    {
        string = g_malloc(ITER_STRING_LEN + 1);
        g_private_set (&gtmits_buffer_key, string);
    }
#endif
#else
    static char string[ITER_STRING_LEN + 1];
#endif

    if (iter)
        snprintf(string, ITER_STRING_LEN,
                 "[stamp:%x data:%p (%s), %p, %d]",
                 iter->stamp, iter->user_data,
                 gncOwnerGetName ((GncOwner *) iter->user_data),
                 iter->user_data2, GPOINTER_TO_INT(iter->user_data3));
    else
        strcpy(string, "(null)");
    return string;
}


/************************************************************/
/*       Gtk Tree Model Required Interface Functions        */
/************************************************************/

static void
gnc_tree_model_owner_tree_model_init (GtkTreeModelIface *iface)
{
    iface->get_flags       = gnc_tree_model_owner_get_flags;
    iface->get_n_columns   = gnc_tree_model_owner_get_n_columns;
    iface->get_column_type = gnc_tree_model_owner_get_column_type;
    iface->get_iter        = gnc_tree_model_owner_get_iter;
    iface->get_path        = gnc_tree_model_owner_get_path;
    iface->get_value       = gnc_tree_model_owner_get_value;
    iface->iter_next       = gnc_tree_model_owner_iter_next;
    iface->iter_children   = gnc_tree_model_owner_iter_children;
    iface->iter_has_child  = gnc_tree_model_owner_iter_has_child;
    iface->iter_n_children = gnc_tree_model_owner_iter_n_children;
    iface->iter_nth_child  = gnc_tree_model_owner_iter_nth_child;
    iface->iter_parent     = gnc_tree_model_owner_iter_parent;
}

static GtkTreeModelFlags
gnc_tree_model_owner_get_flags (GtkTreeModel *tree_model)
{
    return 0;
}

static int
gnc_tree_model_owner_get_n_columns (GtkTreeModel *tree_model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_OWNER(tree_model), -1);

    return GNC_TREE_MODEL_OWNER_NUM_COLUMNS;
}

static GType
gnc_tree_model_owner_get_column_type (GtkTreeModel *tree_model,
                                      int index)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (tree_model), G_TYPE_INVALID);
    g_return_val_if_fail ((index < GNC_TREE_MODEL_OWNER_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

    switch (index)
    {
    case GNC_TREE_MODEL_OWNER_COL_NAME:
    case GNC_TREE_MODEL_OWNER_COL_TYPE:
    case GNC_TREE_MODEL_OWNER_COL_CURRENCY:
    case GNC_TREE_MODEL_OWNER_COL_ID:
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_NAME:
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_1:
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_2:
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_3:
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_4:
    case GNC_TREE_MODEL_OWNER_COL_PHONE:
    case GNC_TREE_MODEL_OWNER_COL_FAX:
    case GNC_TREE_MODEL_OWNER_COL_EMAIL:
    case GNC_TREE_MODEL_OWNER_COL_BALANCE:
    case GNC_TREE_MODEL_OWNER_COL_BALANCE_REPORT:
    case GNC_TREE_MODEL_OWNER_COL_NOTES:

    case GNC_TREE_MODEL_OWNER_COL_COLOR_BALANCE:
        return G_TYPE_STRING;

    case GNC_TREE_MODEL_OWNER_COL_ACTIVE:
        return G_TYPE_BOOLEAN;

    default:
        g_assert_not_reached ();
        return G_TYPE_INVALID;
    }
}

static gboolean
gnc_tree_model_owner_get_iter (GtkTreeModel *tree_model,
                               GtkTreeIter *iter,
                               GtkTreePath *path)
{
    GncTreeModelOwnerPrivate *priv;
    GncTreeModelOwner *model;
    GncOwner *owner;
    gint *indices;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (tree_model), FALSE);

    {
        gchar *path_string = gtk_tree_path_to_string(path);
        ENTER("model %p, iter %p, path %s", tree_model, iter, path_string);
        g_free(path_string);
    }

    model = GNC_TREE_MODEL_OWNER (tree_model);
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    /* We keep a simple list of owners, not a tree, so only depth 1 is valid */
    if (gtk_tree_path_get_depth (path) != 1)
    {
        LEAVE("bad depth");
        return FALSE;
    }

    indices = gtk_tree_path_get_indices (path);

    owner = g_list_nth_data (priv->owner_list, indices[0]);
    if (owner == NULL)
    {
        iter->stamp = 0;
        LEAVE("bad index");
        return FALSE;
    }

    iter->stamp = model->stamp;
    iter->user_data = owner;
    iter->user_data2 = GINT_TO_POINTER (indices[0]);
    iter->user_data3 = NULL;

    LEAVE("iter %s", iter_to_string (iter));
    return TRUE;
}

static GtkTreePath *
gnc_tree_model_owner_get_path (GtkTreeModel *tree_model,
                               GtkTreeIter *iter)
{
    GncTreeModelOwner *model = GNC_TREE_MODEL_OWNER (tree_model);
    GncTreeModelOwnerPrivate *priv;
    GncOwner *owner;
    GtkTreePath *path;
    gint i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), NULL);
    g_return_val_if_fail (iter != NULL, NULL);
    g_return_val_if_fail (iter->user_data != NULL, NULL);
    g_return_val_if_fail (iter->stamp == model->stamp, NULL);

    ENTER("model %p, iter %s", model, iter_to_string(iter));

    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
    if (priv->owner_list == NULL)
    {
        LEAVE("failed (1)");
        return NULL;
    }

    owner = (GncOwner *) iter->user_data;

    path = gtk_tree_path_new ();
    i = g_list_index (priv->owner_list, owner);
    if (i == -1)
    {
        gtk_tree_path_free (path);
        LEAVE("failed (3)");
        return NULL;
    }
    gtk_tree_path_prepend_index (path, i);

    {
        gchar *path_string = gtk_tree_path_to_string(path);
        LEAVE("path (4) %s", path_string);
        g_free(path_string);
    }
    return path;
}

static void
gnc_tree_model_owner_set_color(GncTreeModelOwner *model,
                               gboolean negative,
                               GValue *value)
{
    GncTreeModelOwnerPrivate *priv;

    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
    if (negative)
        g_value_set_static_string (value, priv->negative_color);
    else
        g_value_set_static_string (value, NULL);
}

static void
gnc_tree_model_owner_get_value (GtkTreeModel *tree_model,
                                GtkTreeIter *iter,
                                int column,
                                GValue *value)
{
    GncTreeModelOwner *model = GNC_TREE_MODEL_OWNER (tree_model);
    GncOwner *owner;
    gboolean negative; /* used to set "deficit style" also known as red numbers */
    gchar *string = NULL;

    g_return_if_fail (GNC_IS_TREE_MODEL_OWNER (model));
    g_return_if_fail (iter != NULL);
    g_return_if_fail (iter->user_data != NULL);
    g_return_if_fail (iter->stamp == model->stamp);

    ENTER("model %p, iter %s, col %d", tree_model,
          iter_to_string(iter), column);

    owner = (GncOwner *) iter->user_data;

    switch (column)
    {
    case GNC_TREE_MODEL_OWNER_COL_NAME:
        g_value_init (value, G_TYPE_STRING);
        g_value_set_string (value, gncOwnerGetName (owner));
        break;
    case GNC_TREE_MODEL_OWNER_COL_TYPE:
        g_value_init (value, G_TYPE_STRING);
        g_value_set_string (value,
                            gncOwnerTypeToQofIdType (gncOwnerGetType (owner)));
        break;
    case GNC_TREE_MODEL_OWNER_COL_ID:
        g_value_init (value, G_TYPE_STRING);
        g_value_set_string (value, gncOwnerGetID (owner));
        break;
    case GNC_TREE_MODEL_OWNER_COL_CURRENCY:
        g_value_init (value, G_TYPE_STRING);
        g_value_set_string (value,
                            gnc_commodity_get_fullname(gncOwnerGetCurrency (owner)));
        break;
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_NAME:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetName (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_1:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetAddr1 (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_2:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetAddr2 (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_3:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetAddr3 (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_ADDRESS_4:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetAddr4 (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_PHONE:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetPhone (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_FAX:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetFax (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;
    case GNC_TREE_MODEL_OWNER_COL_EMAIL:
        g_value_init (value, G_TYPE_STRING);
        string = g_strdup (gncAddressGetEmail (gncOwnerGetAddr (owner)));
        if (string)
            g_value_take_string (value, string);
        else
            g_value_set_static_string (value, "");
        break;

    case GNC_TREE_MODEL_OWNER_COL_BALANCE:
        g_value_init (value, G_TYPE_STRING);
        string = gnc_ui_owner_get_print_balance(owner, &negative);
        g_value_take_string (value, string);
        break;

    case GNC_TREE_MODEL_OWNER_COL_BALANCE_REPORT:
        g_value_init (value, G_TYPE_STRING);
        string = gnc_ui_owner_get_print_report_balance(owner, &negative);
        g_value_take_string (value, string);
        break;
    case GNC_TREE_MODEL_OWNER_COL_COLOR_BALANCE:
        g_value_init (value, G_TYPE_STRING);
        string = gnc_ui_owner_get_print_balance(owner, &negative);
        gnc_tree_model_owner_set_color(model, negative, value);
        g_free(string);
        break;

    case GNC_TREE_MODEL_OWNER_COL_NOTES:
        g_value_init (value, G_TYPE_STRING);
        switch (gncOwnerGetType (owner))
        {
        case GNC_OWNER_NONE:
        case GNC_OWNER_UNDEFINED:
        case GNC_OWNER_EMPLOYEE:
        case GNC_OWNER_JOB:
        default:
            g_value_set_static_string (value, "");
            break;
        case GNC_OWNER_VENDOR:
            g_value_set_string (value, gncVendorGetNotes (gncOwnerGetVendor (owner)));
            break;
        case GNC_OWNER_CUSTOMER:
            g_value_set_string (value, gncCustomerGetNotes (gncOwnerGetCustomer (owner)));
            break;
        }
        break;

    case GNC_TREE_MODEL_OWNER_COL_ACTIVE:
        g_value_init (value, G_TYPE_BOOLEAN);
        g_value_set_boolean (value, gncOwnerGetActive (owner));
        break;

    default:
        g_assert_not_reached ();
    }
    LEAVE(" ");
}

static gboolean
gnc_tree_model_owner_iter_next (GtkTreeModel *tree_model,
                                GtkTreeIter *iter)
{
    GncTreeModelOwner *model = GNC_TREE_MODEL_OWNER (tree_model);
    GncTreeModelOwnerPrivate *priv;
    GncOwner *owner;
    gint i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);
    g_return_val_if_fail (iter->user_data != NULL, FALSE);
    g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

    ENTER("model %p, iter %s", tree_model, iter_to_string (iter));

    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    /* Get the *next* sibling owner. */
    i = GPOINTER_TO_INT (iter->user_data2);
    owner = g_list_nth_data (priv->owner_list, i + 1);
    if (owner == NULL)
    {
        iter->stamp = 0;
        LEAVE("failed (3)");
        return FALSE;
    }

    iter->user_data = owner;
    iter->user_data2 = GINT_TO_POINTER (i + 1);
    iter->user_data3 = NULL;

    LEAVE("iter %s", iter_to_string(iter));
    return TRUE;
}

static gboolean
gnc_tree_model_owner_iter_children (GtkTreeModel *tree_model,
                                    GtkTreeIter *iter,
                                    GtkTreeIter *parent_iter)
{
    GncTreeModelOwnerPrivate *priv;
    GncTreeModelOwner *model;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (tree_model), FALSE);
    ENTER("model %p, iter %p (to be filed in), parent %s",
          tree_model, iter, (parent_iter ? iter_to_string(parent_iter) : "(null)"));

    model = GNC_TREE_MODEL_OWNER (tree_model);
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    /* Owner lists don't have children, so this function call only
     * makes sense if no parent_iter was supplied. In that case,
     * return the first owner in the list */
    if (!parent_iter)
    {
        iter->user_data = g_list_nth_data (priv->owner_list, 0);
        iter->user_data2 = GINT_TO_POINTER (0);
        iter->user_data3 = NULL;
        iter->stamp = model->stamp;
        LEAVE("iter (2) %s", iter_to_string(iter));
        return TRUE;
    }
    else
    {
        iter->stamp = 0;
        LEAVE("failed (owners don't have children)");
        return FALSE;
    }
}

static gboolean
gnc_tree_model_owner_iter_has_child (GtkTreeModel *tree_model,
                                     GtkTreeIter *iter)
{
    /* Owner lists don't have children, so always return false */
    return FALSE;
}

static int
gnc_tree_model_owner_iter_n_children (GtkTreeModel *tree_model,
                                      GtkTreeIter *iter)
{
    GncTreeModelOwner *model;
    GncTreeModelOwnerPrivate *priv;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (tree_model), -1);

    model = GNC_TREE_MODEL_OWNER (tree_model);
    priv  = GNC_TREE_MODEL_OWNER_GET_PRIVATE (model);

    /* Owner lists don't have children, so always return 0, except for
     * the special case this request comes for the special "root" iter
     * (NULL). For that exception we return the size of the ower list.
     */
    if (iter == NULL)
        return (gint) g_list_length (priv->owner_list);

    g_return_val_if_fail (
        GNC_TREE_MODEL_OWNER (tree_model)->stamp == iter->stamp, -1);

    return 0;
}

static gboolean
gnc_tree_model_owner_iter_nth_child (GtkTreeModel *tree_model,
                                     GtkTreeIter *iter,
                                     GtkTreeIter *parent_iter,
                                     int n)
{
    GncTreeModelOwner *model;
    GncTreeModelOwnerPrivate *priv;

    if (parent_iter)
    {
        gchar *parent_string;
        parent_string = g_strdup(iter_to_string(parent_iter));
        ENTER("model %p, iter %s, parent_iter %s, n %d",
              tree_model, iter_to_string(iter),
              parent_string, n);
        g_free(parent_string);
    }
    else
    {
        ENTER("model %p, iter %s, parent_iter (null), n %d",
              tree_model, iter_to_string(iter), n);
    }
    gnc_leave_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (tree_model), FALSE);

    model = GNC_TREE_MODEL_OWNER (tree_model);
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    /* Owner lists don't have children, so this function call only
     * makes sense if no parent_iter was supplied. In that case,
     * return the first owner in the list */
    if (!parent_iter)
    {
        iter->user_data = g_list_nth_data (priv->owner_list, n);
        iter->user_data2 = GINT_TO_POINTER (n);
        iter->user_data3 = NULL;
        iter->stamp = model->stamp;
        LEAVE("iter (2) %s", iter_to_string(iter));
        return TRUE;
    }
    else
    {
        iter->stamp = 0;
        LEAVE("failed (owners don't have children)");
        return FALSE;
    }
}

static gboolean
gnc_tree_model_owner_iter_parent (GtkTreeModel *tree_model,
                                  GtkTreeIter *iter,
                                  GtkTreeIter *child)
{
    /* Owner lists don't have children, so always return false */
    iter->stamp = 0;
    return FALSE;
}


/************************************************************/
/*            Owner Tree View Filter Functions            */
/************************************************************/

/*
 * Convert a model/iter pair to a gnucash owner.  This routine should
 * only be called from an owner tree view filter function.
 */
GncOwner *
gnc_tree_model_owner_get_owner (GncTreeModelOwner *model,
                                GtkTreeIter *iter)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), NULL);
    g_return_val_if_fail (iter != NULL, NULL);
    g_return_val_if_fail (iter->user_data != NULL, NULL);
    g_return_val_if_fail (iter->stamp == model->stamp, NULL);

    return (GncOwner *) iter->user_data;
}

/*
 * Convert a model/owner pair into a gtk_tree_model_iter.  This
 * routine should only be called from the file
 * gnc-tree-view-owner.c.
 */
gboolean
gnc_tree_model_owner_get_iter_from_owner (GncTreeModelOwner *model,
        GncOwner *owner,
        GtkTreeIter *iter)
{
    GncTreeModelOwnerPrivate *priv;
    GList *owner_in_list;

    ENTER("model %p, owner %p, iter %p", model, owner, iter);
    gnc_leave_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), FALSE);
    gnc_leave_return_val_if_fail ((owner != NULL), FALSE);
    gnc_leave_return_val_if_fail ((iter != NULL), FALSE);


    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);
    owner_in_list = g_list_find_custom (priv->owner_list, (gconstpointer)owner, (GCompareFunc)gncOwnerGCompareFunc);
    if (owner_in_list)
    {
        iter->stamp = model->stamp;
        iter->user_data = owner_in_list->data;
        iter->user_data2 = GINT_TO_POINTER (g_list_position (priv->owner_list, owner_in_list));
        iter->user_data3 = NULL;
        LEAVE("iter %s", iter_to_string (iter));
        return TRUE;
    }
    else
    {
        iter->stamp = 0;
        iter->user_data = NULL;
        LEAVE("Owner not found in list");
        return FALSE;
    }
}

/*
 * Convert a model/owner pair into a gtk_tree_model_path.  This
 * routine should only be called from the file
 * gnc-tree-view-owner.c.
 */
GtkTreePath *
gnc_tree_model_owner_get_path_from_owner (GncTreeModelOwner *model,
        GncOwner *owner)
{
    GtkTreeIter tree_iter;
    GtkTreePath *tree_path;

    ENTER("model %p, owner %p", model, owner);
    gnc_leave_return_val_if_fail (GNC_IS_TREE_MODEL_OWNER (model), NULL);
    gnc_leave_return_val_if_fail (owner != NULL, NULL);

    if (!gnc_tree_model_owner_get_iter_from_owner (model, owner,
            &tree_iter))
    {
        LEAVE("no iter");
        return NULL;
    }

    tree_path = gtk_tree_model_get_path (GTK_TREE_MODEL(model), &tree_iter);
    if (tree_path)
    {
        gchar *path_string = gtk_tree_path_to_string(tree_path);
        LEAVE("path (2) %s", path_string);
        g_free(path_string);
    }
    else
    {
        LEAVE("no path");
    }
    return tree_path;
}

/************************************************************/
/*   Owner Tree Model - Engine Event Handling Functions   */
/************************************************************/

static void
increment_stamp(GncTreeModelOwner *model)
{
    do model->stamp++;
    while (!model->stamp);
}

/** This function is the handler for all event messages from the
 *  engine.  Its purpose is to update the owner tree model any time
 *  an owner is added to the engine or deleted from the engine.
 *  This change to the model is then propagated to any/all overlying
 *  filters and views.  This function listens to the ADD, REMOVE, and
 *  DESTROY events.
 *
 *  @internal
 *
 *  @warning There is a "Catch 22" situation here.
 *  gtk_tree_model_row_deleted() can't be called until after the item
 *  has been deleted from the real model (which is the engine's
 *  owner tree for us), but once the owner has been deleted from
 *  the engine we have no way to determine the path to pass to
 *  row_deleted().  This is a PITA, but the only other choice is to
 *  have this model mirror the engine's owners instead of
 *  referencing them directly.
 *
 *  @param entity The guid of the affected item.
 *
 *  @param type The type of the affected item.  This function only
 *  cares about items of type "owner".
 *
 *  @param event type The type of the event. This function only cares
 *  about items of type ADD, REMOVE, MODIFY, and DESTROY.
 *
 *  @param user_data A pointer to the owner tree model.
 */
static void
gnc_tree_model_owner_event_handler (QofInstance *entity,
                                    QofEventId event_type,
                                    GncTreeModelOwner *model,
                                    GncEventData *ed)
{
    GncTreeModelOwnerPrivate *priv;
    GtkTreePath *path = NULL;
    GtkTreeIter iter;
    GncOwner owner;

    g_return_if_fail(model);         /* Required */

    if (!GNC_IS_OWNER(entity))
        return;

    ENTER("entity %p of type %d, model %p, event_data %p",
          entity, event_type, model, ed);
    priv = GNC_TREE_MODEL_OWNER_GET_PRIVATE(model);

    qofOwnerSetEntity (&owner, entity);
    if (gncOwnerGetType(&owner) != priv->owner_type)
    {
        LEAVE("model type and owner type differ");
        return;
    }

    if (qof_instance_get_book (entity) != priv->book)
    {
        LEAVE("not in this book");
        return;
    }

    /* What to do, that to do. */
    switch (event_type)
    {
    case QOF_EVENT_ADD:
        /* Tell the filters/views where the new owner was added. */
        DEBUG("add owner %p (%s)", &owner, gncOwnerGetName(&owner));
        /* First update our copy of the owner list. This isn't done automatically */
        priv->owner_list = gncBusinessGetOwnerList (priv->book,
                           gncOwnerTypeToQofIdType(priv->owner_type), TRUE);
        increment_stamp(model);
        if (!gnc_tree_model_owner_get_iter_from_owner (model, &owner, &iter))
        {
            LEAVE("can't generate iter");
            break;
        }
        path = gnc_tree_model_owner_get_path(GTK_TREE_MODEL(model), &iter);
        if (!path)
        {
            DEBUG("can't generate path");
            break;
        }
        gtk_tree_model_row_inserted (GTK_TREE_MODEL(model), path, &iter);
        break;

    case QOF_EVENT_REMOVE:
        if (!ed) /* Required for a remove. */
            break;
        DEBUG("remove owner %d (%s) from owner_list %p", ed->idx,
              gncOwnerGetName(&owner), priv->owner_list);
        path = gtk_tree_path_new();
        if (!path)
        {
            DEBUG("can't generate path");
            break;
        }
        increment_stamp(model);
        gtk_tree_path_append_index (path, ed->idx);
        gtk_tree_model_row_deleted (GTK_TREE_MODEL(model), path);
        break;

    case QOF_EVENT_MODIFY:
        DEBUG("modify  owner %p (%s)", &owner, gncOwnerGetName(&owner));
        if (!gnc_tree_model_owner_get_iter_from_owner (model, &owner, &iter))
        {
            LEAVE("can't generate iter");
            return;
        }
        path = gnc_tree_model_owner_get_path(GTK_TREE_MODEL(model), &iter);
        if (!path)
        {
            DEBUG("can't generate path");
            break;
        }
        gtk_tree_model_row_changed(GTK_TREE_MODEL(model), path, &iter);
        break;

    default:
        LEAVE("unknown event type");
        return;
    }

    if (path)
        gtk_tree_path_free(path);
    LEAVE(" ");
    return;
}
