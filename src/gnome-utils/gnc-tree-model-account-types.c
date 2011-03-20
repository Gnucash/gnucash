/*
 * gnc-tree-model-account-types.c -- GtkTreeModel implementation
 *	to display account types in a GtkTreeView.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2005, 2006 Chris Shoemaker <c.shoemaker@cox.net>
 * Copyright (C) 2006 Eskil Bylund <eskil.bylund@gmail.com>
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

#include "qof.h"
#include "gnc-tree-model-account-types.h"
#include "Account.h"

static QofLogModule log_module = GNC_MOD_GUI;
static GtkTreeModel *account_types_tree_model = NULL;

#define TYPE_MASK "type-mask"

/* Functions for the type system */
static void
gnc_tree_model_account_types_class_init (GncTreeModelAccountTypesClass *klass);
static void
gnc_tree_model_account_types_init (GncTreeModelAccountTypes * model);
static void
gnc_tree_model_account_types_finalize (GObject * object);


/* Functions implementing GtkTreeModel */
static void
gnc_tree_model_account_types_tree_model_init (GtkTreeModelIface * iface);

typedef struct GncTreeModelAccountTypesPrivate
{
    guint32 selected;
} GncTreeModelAccountTypesPrivate;

#define GNC_TREE_MODEL_ACCOUNT_TYPES_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, \
                                 GncTreeModelAccountTypesPrivate))

static GObjectClass *parent_class = NULL;

GType
gnc_tree_model_account_types_get_type (void)
{
    static GType gnc_tree_model_account_types_type = 0;

    if (gnc_tree_model_account_types_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeModelAccountTypesClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_tree_model_account_types_class_init,
            NULL,
            NULL,
            sizeof (GncTreeModelAccountTypes),
            0,
            (GInstanceInitFunc) gnc_tree_model_account_types_init
        };

        static const GInterfaceInfo tree_model_info =
        {
            (GInterfaceInitFunc) gnc_tree_model_account_types_tree_model_init,
            NULL,
            NULL
        };

        gnc_tree_model_account_types_type =
            g_type_register_static (G_TYPE_OBJECT,
                                    "GncTreeModelAccountTypes",
                                    &our_info, 0);

        g_type_add_interface_static (gnc_tree_model_account_types_type,
                                     GTK_TYPE_TREE_MODEL, &tree_model_info);
    }

    return gnc_tree_model_account_types_type;
}

static void
gnc_tree_model_account_types_class_init (GncTreeModelAccountTypesClass * klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_tree_model_account_types_finalize;

    g_type_class_add_private(klass, sizeof(GncTreeModelAccountTypesPrivate));
}

static void
gnc_tree_model_account_types_init (GncTreeModelAccountTypes * model)
{
    while (model->stamp == 0)
    {
        model->stamp = g_random_int ();
    }
}

static void
gnc_tree_model_account_types_finalize (GObject * object)
{
    GncTreeModelAccountTypes *model;
    GncTreeModelAccountTypesPrivate *priv;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (object));

    model = GNC_TREE_MODEL_ACCOUNT_TYPES (object);
    priv = GNC_TREE_MODEL_ACCOUNT_TYPES_GET_PRIVATE (model);

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

GtkTreeModel *
gnc_tree_model_account_types_new (guint32 selected)
{
    GncTreeModelAccountTypes *model;
    GncTreeModelAccountTypesPrivate *priv;

    model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT_TYPES, NULL);
    priv = GNC_TREE_MODEL_ACCOUNT_TYPES_GET_PRIVATE(model);
    priv->selected = selected;

    return GTK_TREE_MODEL (model);
}

static GtkTreeModel *
gnc_tree_model_account_types_master(void)
{
    if (!account_types_tree_model)
        account_types_tree_model = gnc_tree_model_account_types_new(0);
    return account_types_tree_model;
}


static gboolean
gnc_tree_model_account_types_is_valid (GtkTreeModel *model,
                                       GtkTreeIter *iter, gpointer data)
{
    GNCAccountType type;
    GObject *f_model = G_OBJECT (data);
    guint32 valid_types = GPOINTER_TO_UINT (g_object_get_data (
            f_model, TYPE_MASK));

    gtk_tree_model_get (model, iter,
                        GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE, &type, -1);
    return (valid_types & (1 << type)) ? TRUE : FALSE;
}

GtkTreeModel *
gnc_tree_model_account_types_valid (void)
{
    return gnc_tree_model_account_types_filter_using_mask(
               xaccAccountTypesValid());
}

GtkTreeModel *
gnc_tree_model_account_types_filter_using_mask (guint32 types)
{
    GtkTreeModel *f_model;

    f_model = gtk_tree_model_filter_new (gnc_tree_model_account_types_master (),
                                         NULL);
    g_object_set_data (G_OBJECT (f_model), TYPE_MASK, GUINT_TO_POINTER (types));
    gtk_tree_model_filter_set_visible_func (
        GTK_TREE_MODEL_FILTER (f_model), gnc_tree_model_account_types_is_valid,
        f_model, NULL);

    return f_model;
}

void
gnc_tree_model_account_types_set_mask (GtkTreeModel *f_model,
                                       guint32 types)
{
    g_return_if_fail (f_model);

    g_object_set_data (G_OBJECT (f_model), TYPE_MASK, GUINT_TO_POINTER (types));
    gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (f_model));
}

guint32
gnc_tree_model_account_types_get_mask (GtkTreeModel *f_model)
{
    g_return_val_if_fail (f_model, 0);

    return GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (f_model), TYPE_MASK));
}

guint32
gnc_tree_model_account_types_get_selected (GncTreeModelAccountTypes * model)
{
    GncTreeModelAccountTypesPrivate *priv;

    g_return_val_if_fail (model != NULL, 0);

    priv = GNC_TREE_MODEL_ACCOUNT_TYPES_GET_PRIVATE(model);
    return priv->selected;
}

void
gnc_tree_model_account_types_set_selected (GncTreeModelAccountTypes * model,
        guint32 selected)
{
    GncTreeModelAccountTypesPrivate *priv;

    g_return_if_fail (model != NULL);

    priv = GNC_TREE_MODEL_ACCOUNT_TYPES_GET_PRIVATE(model);
    priv->selected = selected;
}

guint32
gnc_tree_model_account_types_get_selection (GtkTreeSelection *sel)
{
    GtkTreeModel *f_model, *model;
    GtkTreePath *path;
    GtkTreeView *view;
    GList *list, *node;
    guint32 bits = 0;

    g_return_val_if_fail(GTK_IS_TREE_SELECTION(sel), 0);
    view = gtk_tree_selection_get_tree_view(sel);
    g_return_val_if_fail (view, 0);

    /* circumvent a bug in gtk+ not always filling f_model */
    f_model = NULL;
    list = gtk_tree_selection_get_selected_rows(sel, &f_model);
    if (!f_model)
        f_model = gtk_tree_view_get_model(view);

    model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(f_model));
    if (model != account_types_tree_model)
        PERR("TreeSelection's TreeModel is not the account-types Model");
    else
    {
        for (node = list; node; node = node->next)
        {
            path = gtk_tree_model_filter_convert_path_to_child_path(
                       GTK_TREE_MODEL_FILTER(f_model), (GtkTreePath*)node->data);
            if (!path || gtk_tree_path_get_depth(path) != 1)
            {
                PERR("Invalid Account-types TreePath.");
                continue;
            }
            bits |= (1 << gtk_tree_path_get_indices(path)[0]);
        }
    }

    g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
    g_list_free (list);

    return bits;
}

GNCAccountType
gnc_tree_model_account_types_get_selection_single(GtkTreeSelection *sel)
{
    gint i;
    guint32 selected = gnc_tree_model_account_types_get_selection(sel);

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
        if (selected & (1 << i))
            return i;
    return ACCT_TYPE_NONE;
}

void
gnc_tree_model_account_types_set_selection (GtkTreeSelection *sel,
        guint32 selected)
{
    GtkTreePath *path, *f_path;
    GtkTreeModelFilter *f_model;
    gint i;
    GtkTreeView *view;

    g_return_if_fail(GTK_IS_TREE_SELECTION(sel));
    view = gtk_tree_selection_get_tree_view(sel);
    g_return_if_fail (view);
    f_model = GTK_TREE_MODEL_FILTER(gtk_tree_view_get_model(view));
    g_return_if_fail(gtk_tree_model_filter_get_model(f_model) ==
                     account_types_tree_model);
    gtk_tree_selection_unselect_all(sel);
    path = gtk_tree_path_new_first();

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
    {
        if (selected & (1 << i))
        {
            f_path = gtk_tree_model_filter_convert_child_path_to_path(
                         f_model, path);
            gtk_tree_selection_select_path(sel, f_path);
            gtk_tree_view_scroll_to_cell(view, f_path, NULL, FALSE, 0.0, 0.0);
        }
        gtk_tree_path_next(path);
    }
    gtk_tree_path_free(path);
}


/* Static functions implementing GtkTreeModel */

static GtkTreeModelFlags
gnc_tree_model_account_types_get_flags (GtkTreeModel * tree_model)
{
    return GTK_TREE_MODEL_ITERS_PERSIST | GTK_TREE_MODEL_LIST_ONLY;
}

static int
gnc_tree_model_account_types_get_n_columns (GtkTreeModel * tree_model)
{
    return GNC_TREE_MODEL_ACCOUNT_TYPES_NUM_COLUMNS;
}

static GType
gnc_tree_model_account_types_get_column_type (GtkTreeModel * tree_model,
        int index)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model),
                         G_TYPE_INVALID);
    g_return_val_if_fail((index < GNC_TREE_MODEL_ACCOUNT_TYPES_NUM_COLUMNS)
                         && (index >= 0), G_TYPE_INVALID);

    switch (index)
    {
    case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE:
        return G_TYPE_INT;
    case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME:
        return G_TYPE_STRING;
    case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_SELECTED:
        return G_TYPE_BOOLEAN;
    default:
        g_assert_not_reached ();
        return G_TYPE_INVALID;
    }
}

static gboolean
gnc_tree_model_account_types_get_iter (GtkTreeModel * tree_model,
                                       GtkTreeIter * iter, GtkTreePath * path)
{
    GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES(tree_model);
    gint i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model), FALSE);
    g_return_val_if_fail (gtk_tree_path_get_depth (path) > 0, FALSE);

    i = gtk_tree_path_get_indices (path)[0];

    if (i > ACCT_TYPE_NONE && i < NUM_ACCOUNT_TYPES)
    {
        iter->stamp = model->stamp;
        iter->user_data = GINT_TO_POINTER (i);
        return TRUE;
    }

    iter->stamp = 0;
    return FALSE;
}

static GtkTreePath *
gnc_tree_model_account_types_get_path (GtkTreeModel * tree_model,
                                       GtkTreeIter * iter)
{
    GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES(tree_model);
    GtkTreePath *path;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model), NULL);
    g_return_val_if_fail (iter != NULL, NULL);
    g_return_val_if_fail (iter->stamp == model->stamp, NULL);

    path = gtk_tree_path_new ();

    gtk_tree_path_append_index (path, GPOINTER_TO_INT (iter->user_data));

    return path;
}

static void
gnc_tree_model_account_types_get_value (GtkTreeModel * tree_model,
                                        GtkTreeIter * iter, int column,
                                        GValue * value)
{
    GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES(tree_model);
    GncTreeModelAccountTypesPrivate *priv;

    g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model));
    g_return_if_fail (iter != NULL);
    g_return_if_fail (iter->stamp == model->stamp);

    priv = GNC_TREE_MODEL_ACCOUNT_TYPES_GET_PRIVATE(model);
    switch (column)
    {
    case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_TYPE:
        g_value_init (value, G_TYPE_INT);
        g_value_set_int (value, GPOINTER_TO_INT (iter->user_data));
        break;
    case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_NAME:
        g_value_init (value, G_TYPE_STRING);
        g_value_set_string (value, xaccAccountGetTypeStr (
                                GPOINTER_TO_INT (iter->user_data)));
        break;
    case GNC_TREE_MODEL_ACCOUNT_TYPES_COL_SELECTED:
        g_value_init (value, G_TYPE_BOOLEAN);
        g_value_set_boolean (value, priv->selected &
                             (1 << GPOINTER_TO_INT (iter->user_data)));
        break;
    default:
        g_assert_not_reached ();
    }
}

static gboolean
gnc_tree_model_account_types_iter_next (GtkTreeModel * tree_model,
                                        GtkTreeIter * iter)
{
    GncTreeModelAccountTypes *model = GNC_TREE_MODEL_ACCOUNT_TYPES(tree_model);

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (model), FALSE);
    g_return_val_if_fail (iter != NULL, FALSE);
    g_return_val_if_fail (iter->stamp == model->stamp, FALSE);

    if (GPOINTER_TO_INT (iter->user_data) < NUM_ACCOUNT_TYPES - 1)
    {
        iter->user_data = GINT_TO_POINTER(
                              GPOINTER_TO_INT(iter->user_data) + 1);
        return TRUE;
    }

    iter->stamp = 0;
    return FALSE;
}

static gboolean
gnc_tree_model_account_types_iter_children (GtkTreeModel * tree_model,
        GtkTreeIter * iter,
        GtkTreeIter * parent)
{

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES(tree_model), FALSE);

    if (parent != NULL)
        return FALSE;

    iter->stamp = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model)->stamp;
    iter->user_data = GINT_TO_POINTER (0);

    return TRUE;
}

static gboolean
gnc_tree_model_account_types_iter_has_child (GtkTreeModel * tree_model,
        GtkTreeIter * iter)
{
    return FALSE;
}

static int
gnc_tree_model_account_types_iter_n_children (GtkTreeModel * tree_model,
        GtkTreeIter * iter)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model), -1);

    if (iter == NULL)
        return NUM_ACCOUNT_TYPES;

    g_return_val_if_fail (
        GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model)->stamp == iter->stamp, -1);

    return 0;
}

static gboolean
gnc_tree_model_account_types_iter_nth_child (GtkTreeModel * tree_model,
        GtkTreeIter * iter,
        GtkTreeIter * parent, int n)
{
    GncTreeModelAccountTypes *model;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT_TYPES (tree_model), FALSE);

    if (parent != NULL)
        return FALSE;

    model = GNC_TREE_MODEL_ACCOUNT_TYPES (tree_model);

    if (n > ACCT_TYPE_NONE && n < NUM_ACCOUNT_TYPES)
    {
        iter->stamp = model->stamp;
        iter->user_data = GINT_TO_POINTER (n);
        return TRUE;
    }

    iter->stamp = 0;
    return FALSE;
}

static gboolean
gnc_tree_model_account_types_iter_parent (GtkTreeModel * tree_model,
        GtkTreeIter * iter,
        GtkTreeIter * child)
{
    return FALSE;
}

static void
gnc_tree_model_account_types_tree_model_init (GtkTreeModelIface * iface)
{
    iface->get_flags = gnc_tree_model_account_types_get_flags;
    iface->get_n_columns = gnc_tree_model_account_types_get_n_columns;
    iface->get_column_type = gnc_tree_model_account_types_get_column_type;
    iface->get_iter = gnc_tree_model_account_types_get_iter;
    iface->get_path = gnc_tree_model_account_types_get_path;
    iface->get_value = gnc_tree_model_account_types_get_value;
    iface->iter_next = gnc_tree_model_account_types_iter_next;
    iface->iter_children = gnc_tree_model_account_types_iter_children;
    iface->iter_has_child = gnc_tree_model_account_types_iter_has_child;
    iface->iter_n_children = gnc_tree_model_account_types_iter_n_children;
    iface->iter_nth_child = gnc_tree_model_account_types_iter_nth_child;
    iface->iter_parent = gnc_tree_model_account_types_iter_parent;
}

