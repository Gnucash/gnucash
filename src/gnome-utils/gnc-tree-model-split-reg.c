/********************************************************************\
 * gnc-tree-model-split-reg.c -- GtkTreeView implementation to      *
 *                     display registers   in a GtkTreeView.        *
 *                                                                  *
 * Copyright (C) 2012 Robert Fewell                                 *
 * Copyright (C) 2006-2007 Chris Shoemaker <c.shoemaker@cox.net>    *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>

#include "gnc-tree-model-split-reg.h"
#include "gnc-component-manager.h"
#include "gnc-commodity.h"
#include "gnc-gconf-utils.h"
#include "gnc-engine.h"
#include "gnc-event.h"
#include "gnc-gobject-utils.h"
#include "Query.h"
#include "Transaction.h"

#include "gnc-ui-util.h"

#define TREE_MODEL_SPLIT_REG_CM_CLASS "tree-model-split-reg"

/** Static Globals *******************************************************/
static QofLogModule log_module = GNC_MOD_GUI;

/** Declarations *********************************************************/
static void gnc_tree_model_split_reg_class_init (GncTreeModelSplitRegClass *klass);
static void gnc_tree_model_split_reg_init (GncTreeModelSplitReg *model);
static void gnc_tree_model_split_reg_finalize (GObject *object);
static void gnc_tree_model_split_reg_dispose (GObject *object);

/** Implementation of GtkTreeModel  **************************************/
static void gnc_tree_model_split_reg_tree_model_init (GtkTreeModelIface *iface);

static GtkTreeModelFlags gnc_tree_model_split_reg_get_flags (GtkTreeModel *tree_model);
static int gnc_tree_model_split_reg_get_n_columns (GtkTreeModel *tree_model);
static GType gnc_tree_model_split_reg_get_column_type (GtkTreeModel *tree_model, int index);
static gboolean gnc_tree_model_split_reg_get_iter (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreePath *path);
static GtkTreePath *gnc_tree_model_split_reg_get_path (GtkTreeModel *tree_model, GtkTreeIter *iter);
static void gnc_tree_model_split_reg_get_value (GtkTreeModel *tree_model, GtkTreeIter *iter, int column, GValue *value);
static gboolean	gnc_tree_model_split_reg_iter_next (GtkTreeModel *tree_model, GtkTreeIter *iter);
static gboolean	gnc_tree_model_split_reg_iter_children (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *parent);
static gboolean	gnc_tree_model_split_reg_iter_has_child (GtkTreeModel *tree_model, GtkTreeIter *iter);
static int gnc_tree_model_split_reg_iter_n_children (GtkTreeModel *tree_model, GtkTreeIter *iter);
static gboolean	gnc_tree_model_split_reg_iter_nth_child (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *parent, int n);
static gboolean	gnc_tree_model_split_reg_iter_parent (GtkTreeModel *tree_model, GtkTreeIter *iter, GtkTreeIter *child);

/** Component Manager Callback ******************************************/
static void gnc_tree_model_split_reg_event_handler (QofInstance *entity, QofEventId event_type, GncTreeModelSplitReg *model, GncEventData *ed);

static void create_completion_models(GncTreeModelSplitReg *model);

/** The instance private data for the split register tree model. */
struct GncTreeModelSplitRegPrivate
{
    QofBook *book;
    Account *anchor;
    Query   *query;

    GList *tlist;
    GList *slist;

    gboolean use_colors;
    gboolean double_line;
    gboolean alt_colors_by_txn;


    GtkListStore *description_list;
    GtkListStore *notes_list;
    GtkListStore *memo_list;
    GtkListStore *num_list;
    GtkListStore *numact_list;

    gint event_handler_id;
};


#define GREENROW "#BFDEB9"
#define TANROW "#F6FFDA"
#define SPLITROW "#EDE7D3"
#define YELLOWROW "#FFEF98"


#define TROW1 0x1
#define TROW2 0x2
#define SPLIT 0x4
#define BLANK 0x8
#define IS_TROW1(x) (GPOINTER_TO_INT((x)->user_data) & TROW1)
#define IS_TROW2(x) (GPOINTER_TO_INT((x)->user_data) & TROW2)
#define IS_BLANK(x) (GPOINTER_TO_INT((x)->user_data) & BLANK)
#define IS_SPLIT(x) (GPOINTER_TO_INT((x)->user_data) & SPLIT)
#define IS_BLANK_SPLIT(x) (IS_BLANK(x) && IS_SPLIT(x))
#define IS_BLANK_TRANS(x) (IS_BLANK(x) && !IS_SPLIT(x))
/* Meaning of user_data fields in iter struct:
 *
 * user_data:  a bitfield for BLANK, SPLIT, TROW1, TROW2
 * user_data2: a pointer to a node in a GList of Transactions               
 * user_data3: a pointer to a node in a GList of Splits.
 *            
 */

/* FIXME ***********************
#define VALID_ITER(model, iter) \
    (GNC_IS_TREE_MODEL_SPLIT_REG(model) &&                            \
     ((iter) && (iter)->user_data2) &&                                  \
     ((iter)->stamp == (model)->stamp) &&                               \
     (!IS_SPLIT(iter) ^ ((iter)->user_data3 != NULL)) &&                \
     (!IS_BLANK_SPLIT(iter) ||                                          \
      ((iter)->user_data2 == (model)->priv->bsplit_parent_node))        \
     )
*/
static GtkTreeIter
make_iter(GncTreeModelSplitReg *model, gint f, GList *tnode, GList *snode)
{
    GtkTreeIter iter, *iter_p;
    iter_p = &iter;
    iter.stamp = model->stamp;
    iter.user_data = GINT_TO_POINTER(f);
    iter.user_data2 = tnode;
    iter.user_data3 = snode;

/*    if (!VALID_ITER(model, &iter)) PERR("Making invalid iter"); */
    return iter;
}



#define GNC_TREE_MODEL_SPLIT_REG_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_MODEL_SPLIT_REG, GncTreeModelSplitRegPrivate))

/************************************************************/
/*               g_object required functions                */
/************************************************************/

/** A pointer to the parent class of the split register tree model. */
static GtkObjectClass *parent_class = NULL;

GType
gnc_tree_model_split_reg_get_type (void)
{
    static GType gnc_tree_model_split_reg_type = 0;

    if (gnc_tree_model_split_reg_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeModelSplitRegClass),                /* class_size */
            NULL,                                              /* base_init */
            NULL,                                              /* base_finalize */
            (GClassInitFunc) gnc_tree_model_split_reg_class_init,
            NULL,                                               /* class_finalize */
            NULL,                                               /* class_data */
            sizeof (GncTreeModelSplitReg),                      /* */
            0,                                                  /* n_preallocs */
            (GInstanceInitFunc) gnc_tree_model_split_reg_init
        };

        static const GInterfaceInfo tree_model_info =
        {
            (GInterfaceInitFunc) gnc_tree_model_split_reg_tree_model_init,
            NULL,
            NULL
        };

        gnc_tree_model_split_reg_type = g_type_register_static (GNC_TYPE_TREE_MODEL,
                                      GNC_TREE_MODEL_SPLIT_REG_NAME,
                                      &our_info, 0);

        g_type_add_interface_static (gnc_tree_model_split_reg_type,
                                     GTK_TYPE_TREE_MODEL,
                                     &tree_model_info);
    }

    return gnc_tree_model_split_reg_type;
}



static void
gnc_tree_model_split_reg_class_init (GncTreeModelSplitRegClass *klass)
{
    GObjectClass *o_class;

    parent_class = g_type_class_peek_parent (klass);

    o_class = G_OBJECT_CLASS (klass);

    /* GObject signals */
    o_class->finalize = gnc_tree_model_split_reg_finalize;
    o_class->dispose = gnc_tree_model_split_reg_dispose;
}



static void
gnc_tree_model_split_reg_init (GncTreeModelSplitReg *model)
{
    GncTreeModelSplitRegPrivate *priv;

    ENTER("model %p", model);
    while (model->stamp == 0)
    {
        model->stamp = g_random_int ();
    }

    model->priv = g_new0 (GncTreeModelSplitRegPrivate, 1);
    LEAVE(" ");
}



static void
gnc_tree_model_split_reg_finalize (GObject *object)
{
    GncTreeModelSplitRegPrivate *priv;
    GncTreeModelSplitReg *model;

    ENTER("model %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (object));

    model = GNC_TREE_MODEL_SPLIT_REG (object);
    priv = model->priv;

    if (priv->event_handler_id) {
        qof_event_unregister_handler(priv->event_handler_id);
        priv->event_handler_id = 0;
    }

    priv->book = NULL;
    g_list_free(priv->tlist);
    priv->tlist = NULL;
    g_list_free(priv->slist);
    priv->slist = NULL;
    qof_query_destroy(priv->query);
    priv->query = NULL;

/*FIXME Other stuff here */

    g_free(priv);

    if (G_OBJECT_CLASS (parent_class)->finalize)
        G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}



static void
gnc_tree_model_split_reg_dispose (GObject *object)
{
    GncTreeModelSplitRegPrivate *priv;
    GncTreeModelSplitReg *model;

/* Nor sure about this */

    ENTER("model %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (object));

    model = GNC_TREE_MODEL_SPLIT_REG (object);
    priv = model->priv;

    if (priv->event_handler_id)
    {
        qof_event_unregister_handler (priv->event_handler_id);
        priv->event_handler_id = 0;
    }

    if (G_OBJECT_CLASS (parent_class)->dispose)
        G_OBJECT_CLASS (parent_class)->dispose (object);
    LEAVE(" ");
}



/************************************************************/
/*                   New Model Creation                     */
/************************************************************/
static GncTreeModelSplitReg *
gnc_tree_model_split_reg_new (GList *slist)
{
    GncTreeModelSplitReg *model;
    GncTreeModelSplitRegPrivate *priv;
    GList *tlist;
    const GList *item;

    ENTER("Create Model");

/* This needs fixing 
    item = gnc_gobject_tracking_get_list(GNC_TREE_MODEL_SPLIT_REG_NAME);
    for ( ; item; item = g_list_next(item))
    {
        model = (GncTreeModelSplitReg *)item->data;
        priv = GNC_TREE_MODEL_SPLIT_REG_GET_PRIVATE(model);
        if (priv->anchor == anchor)
        {
            g_object_ref(G_OBJECT(model));
            LEAVE("returning existing model %p", model);
            return model;
        }
    }
*/


    model = g_object_new (GNC_TYPE_TREE_MODEL_SPLIT_REG, NULL);

    /* Get a list of Unique Transactions, upsets the list order !! */
    tlist = xaccSplitListGetUniqueTransactions(slist);

    /* Default Sort Transactions by date */
    tlist = g_list_sort (tlist, (GCompareFunc) xaccTransOrder );

    priv = model->priv;
    priv->book = gnc_get_current_book();
    priv->tlist = tlist;

    priv->use_colors = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "use_theme_colors", NULL);
    priv->double_line = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "double_line_mode", NULL);
    priv->alt_colors_by_txn = gnc_gconf_get_bool(GCONF_GENERAL_REGISTER, "alternate_color_by_transaction", NULL);

    create_completion_models(model);

/* More stuff here ? */

    priv->event_handler_id = qof_event_register_handler
                             ((QofEventHandler)gnc_tree_model_split_reg_event_handler, model);

    LEAVE("model %p", model);
    return model;
}



GncTreeModelSplitReg *
gnc_tree_model_split_reg_new_from_query(Query *query)
{
    GncTreeModelSplitReg *model;
    GList *slist;
/*

Not sure if required or what to put in here
    slist = 
    model = gnc_tree_model_split_reg_new(slist);
    model->priv->query = query;
*/
    model = gnc_tree_model_split_reg_new(NULL);
    return model;

}



GncTreeModelSplitReg *
gnc_tree_model_split_reg_new_from_account(Account *acc)
{
    GncTreeModelSplitReg *model;
    Query *q;
    GSList  *p1, *p2;
    QofBook *book;

    GList *slist;

    q = qof_query_create_for(GNC_ID_SPLIT);
    book = gnc_get_current_book();
    qof_query_set_book (q, book);

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    xaccQueryAddSingleAccountMatch (q, acc, QOF_QUERY_AND);

    /* Run the query */
    slist = qof_query_run(q);

    model = gnc_tree_model_split_reg_new(slist);
    model->priv->anchor = acc;
    model->priv->query = q;
    return model;
}



/************************************************************/
/*        Gnc Tree Model Debugging Utility Function         */
/************************************************************/
#define ITER_STRING_LEN 128

static const gchar *
iter_to_string (GtkTreeIter *iter)
{
#ifdef G_THREADS_ENABLED
    static GStaticPrivate gtmits_buffer_key = G_STATIC_PRIVATE_INIT;
    gchar *string;

    string = g_static_private_get (&gtmits_buffer_key);
    if (string == NULL)
    {
        string = g_malloc(ITER_STRING_LEN + 1);
        g_static_private_set (&gtmits_buffer_key, string, g_free);
    }
#else
    static char string[ITER_STRING_LEN + 1];
#endif

    if (iter)
        snprintf(
            string, ITER_STRING_LEN,
            "[stamp:%x data:%d, %p (%p:%s), %p (%p:%s)]",
            iter->stamp, GPOINTER_TO_INT(iter->user_data),
            iter->user_data2,
            iter->user_data2 ? ((GList *) iter->user_data2)->data : 0,
            iter->user_data2 ?
            (QOF_INSTANCE(((GList *) iter->user_data2)->data))->e_type : "",
            iter->user_data3,
            iter->user_data3 ? ((GList *) iter->user_data3)->data : 0,
            iter->user_data3 ?
            (QOF_INSTANCE(((GList *) iter->user_data3)->data))->e_type : "");
    else
        strcpy(string, "(null)");
    return string;
}



/************************************************************/
/*       Gtk Tree Model Required Interface Functions        */
/************************************************************/
static void
gnc_tree_model_split_reg_tree_model_init (GtkTreeModelIface *iface)
{
    iface->get_flags       = gnc_tree_model_split_reg_get_flags;
    iface->get_n_columns   = gnc_tree_model_split_reg_get_n_columns;
    iface->get_column_type = gnc_tree_model_split_reg_get_column_type;
    iface->get_iter        = gnc_tree_model_split_reg_get_iter;
    iface->get_path        = gnc_tree_model_split_reg_get_path;
    iface->get_value       = gnc_tree_model_split_reg_get_value;
    iface->iter_next       = gnc_tree_model_split_reg_iter_next;
    iface->iter_children   = gnc_tree_model_split_reg_iter_children;
    iface->iter_has_child  = gnc_tree_model_split_reg_iter_has_child;
    iface->iter_n_children = gnc_tree_model_split_reg_iter_n_children;
    iface->iter_nth_child  = gnc_tree_model_split_reg_iter_nth_child;
    iface->iter_parent     = gnc_tree_model_split_reg_iter_parent;
}



static GtkTreeModelFlags
gnc_tree_model_split_reg_get_flags (GtkTreeModel *tree_model)
{
    /* Returns a set of flags supported by this interface. The flags
       are a bitwise combination of GtkTreeModelFlags. The flags supported
       should not change during the lifecycle of the tree_model. */
    return 0;
}



static int
gnc_tree_model_split_reg_get_n_columns (GtkTreeModel *tree_model)
{
    /* Returns the number of columns supported by tree_model. */
    g_return_val_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(tree_model), -1);

    return GNC_TREE_MODEL_SPLIT_REG_NUM_COLUMNS;
}



static GType
gnc_tree_model_split_reg_get_column_type (GtkTreeModel *tree_model,
                                        int index)
{
    /* Returns the type of the column. */
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (tree_model), G_TYPE_INVALID);
    g_return_val_if_fail ((index < GNC_TREE_MODEL_SPLIT_REG_NUM_COLUMNS) && (index >= 0), G_TYPE_INVALID);

    switch (index)
    {
    case GNC_TREE_MODEL_SPLIT_REG_COL_GUID:
        return G_TYPE_POINTER;

    case GNC_TREE_MODEL_SPLIT_REG_COL_DATE:
    case GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT:
    case GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES:
    case GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID:

    case GNC_TREE_MODEL_SPLIT_REG_COL_COLOR:
        return G_TYPE_STRING;

    default:
        g_assert_not_reached ();
        return G_TYPE_INVALID;
    }
}



static gboolean
gnc_tree_model_split_reg_get_iter (GtkTreeModel *tree_model,
                                 GtkTreeIter *iter,
                                 GtkTreePath *path)
{
    /* Sets iter to a valid iterator pointing to path. */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    GList *tnode;
    SplitList *slist;
    GList *snode;
    gint depth, *indices, flags;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (tree_model), FALSE);

    {
        gchar *path_string = gtk_tree_path_to_string(path);
        ENTER("model %p, iter %p, path %s", tree_model, iter, path_string);
        g_free(path_string);
    }

/* g_print("gnc_tree_model_split_reg_get_iter\n"); */

    depth = gtk_tree_path_get_depth (path);
/* g_print("Depth is %u\n", depth); */
    indices = gtk_tree_path_get_indices (path);

    tnode = g_list_nth(model->priv->tlist, indices[0]);

    if (!tnode) {
        DEBUG("path index off end of tlist");
        goto fail;
    }

    slist = xaccTransGetSplitList(tnode->data);

/* g_print("Num of Splits is %d\n\n", xaccTransCountSplits(tnode->data)); */

    if(depth != 3 )
        snode = g_list_nth(slist, 0);
    else
        snode = g_list_nth(slist, indices[2]);

    if (!snode) {
        DEBUG("path index off end of slist");
        goto fail;
    }

    if (depth == 1)       /* Trans Row 1 */
        flags = TROW1;

    else if (depth == 2)  /* Trans Row 2 */
        flags = TROW2;

    else if (depth == 3)  /* Split */
        flags = SPLIT;

    else {
        DEBUG("Invalid path depth");
        goto fail;
    }

    *iter = make_iter(model, flags, tnode, snode);
/*    g_assert(VALID_ITER(model, iter)); */
    LEAVE("True");
    return TRUE;
 fail:
    iter->stamp = 0;
    LEAVE("False");
    return FALSE;
}



static GtkTreePath *
gnc_tree_model_split_reg_get_path (GtkTreeModel *tree_model,
                                 GtkTreeIter *iter)
{
    /* Returns a newly-created GtkTreePath referenced by iter. 
       This path should be freed with gtk_tree_path_free(). */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    GtkTreePath *path;
    gint tpos, spos;
    GList *tnode, *snode;
    GList *slist;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (model), NULL);

/* g_print("gnc_tree_model_split_reg_get_path\n"); */
    ENTER("model %p, iter %s", model, iter_to_string(iter));
/*    g_assert(VALID_ITER(model, iter)); */

    path = gtk_tree_path_new();

    tnode = iter->user_data2;

    slist = xaccTransGetSplitList(tnode->data);
    snode = iter->user_data3;

    /* Level 1 */
    tpos = g_list_position(model->priv->tlist, tnode);
    if (tpos == -1)
        goto fail;
    gtk_tree_path_append_index(path, tpos);

    /* Level 2 - All ways 0 */
    if(IS_TROW2(iter))
        gtk_tree_path_append_index(path, 0);

    /* Level 3 */
    if(IS_SPLIT(iter))
    {
        spos = g_list_position(slist, snode);

        if (spos == -1)
            goto fail;
        gtk_tree_path_append_index(path, 0); /* Add the Level 2 part */
        gtk_tree_path_append_index(path, spos);
    }

    {
        gchar *path_string = gtk_tree_path_to_string(path);
/* g_print("Returned get path is %s\n", gtk_tree_path_to_string(path)); */
        LEAVE("get path  %s", path_string);
        g_free(path_string);
    }
    return path;

 fail:
    LEAVE("No Valid Path");
    return NULL;
}



static gchar*
get_row_color(GncTreeModelSplitReg *model, gboolean is_trow1, gboolean is_trow2, gboolean is_split, gint num)
{

    gchar *cell_color = NULL;

    if(!model->priv->use_colors)
    {
        if(model->priv->double_line)
        {
            if(model->priv->alt_colors_by_txn)
            {
                if(num % 2 == 0)
                {
                    if(is_trow1 || is_trow2)
                        cell_color = (gchar*)GREENROW;
                }
                else 
                {
                    if(is_trow1 || is_trow2)
                        cell_color = (gchar*)TANROW;
                }
            }
            else
            {
                if(is_trow1)
                    cell_color = (gchar*)GREENROW;
                else if(is_trow2)
                    cell_color = (gchar*)TANROW;
            }
        }
        else
        {
            if(num % 2 == 0)
            {
                if(is_trow1)
                    cell_color = (gchar*)GREENROW;
                else if(is_trow2)
                    cell_color = (gchar*)TANROW;
            }
            else
            {
                if(is_trow1)
                    cell_color = (gchar*)TANROW;
                else if(is_trow2)
                    cell_color = (gchar*)GREENROW;
            }
        }
        if(is_split)
            cell_color = (gchar*)SPLITROW;
    }
    else
        cell_color = (gchar*)NULL;

    return cell_color;
}



static void
gnc_tree_model_split_reg_get_value (GtkTreeModel *tree_model,
                                  GtkTreeIter *iter,
                                  int column,
                                  GValue *value)
{
    /* Initializes and sets value to that at column. When done with value,
       g_value_unset() needs to be called to free any allocated memory. */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    Account *account;
    Transaction *trans;
    Split *split;
    const GncGUID *guid;
    GList *tnode, *snode;
    gint depth, *indices;

    g_return_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (model));
/* g_print("gnc_tree_model_split_reg_get_value\n"); */
    ENTER("model %p, iter %s, col %d", tree_model, iter_to_string(iter), column);

    account = model->priv->anchor;

    tnode = (GList *) iter->user_data2;
    trans = (Transaction *) tnode->data;

    snode = (GList *) iter->user_data3;
    split = (Split *) snode->data;

    g_value_init(value, gnc_tree_model_split_reg_get_column_type(tree_model, column));

    indices = gtk_tree_path_get_indices (gtk_tree_model_get_path (GTK_TREE_MODEL(model), iter));

    switch (column)
    {
    case GNC_TREE_MODEL_SPLIT_REG_COL_GUID:
        guid = qof_entity_get_guid(QOF_INSTANCE(trans));
        g_value_set_pointer(value, (gpointer) guid);
        break;

    case GNC_TREE_MODEL_SPLIT_REG_COL_DATE:
        break;

    case GNC_TREE_MODEL_SPLIT_REG_COL_COLOR:
            g_value_set_string (value, get_row_color(model, IS_TROW1(iter), IS_TROW2(iter), IS_SPLIT(iter), indices[0]));
        break;

    case GNC_TREE_MODEL_SPLIT_REG_COL_NUMACT:
        break;

    case GNC_TREE_MODEL_SPLIT_REG_COL_DESCNOTES:
        break;

    case GNC_TREE_MODEL_SPLIT_REG_COL_TRANSVOID:
        break;

    default:
        g_assert_not_reached ();
    }

    LEAVE(" ");
}



static gboolean
gnc_tree_model_split_reg_iter_next (GtkTreeModel *tree_model,
                                  GtkTreeIter *iter)
{
    /* Sets iter to point to the node following it at the current level.
       If there is no next iter, FALSE is returned and iter is set to be
       invalid */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    Transaction *trans;
    SplitList *slist;
    GList *tnode, *snode;
    gint flags = TROW1;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (model), FALSE);
/* g_print("gnc_tree_model_split_reg_iter_next\n"); */
    ENTER("model %p, iter %s", tree_model, iter_to_string(iter));

    if (IS_TROW2(iter)) {
        LEAVE("Transaction row 2 never has a next");
        goto fail;
    }

    if (IS_TROW1(iter)) {

        tnode = iter->user_data2;
        tnode = g_list_next(tnode);

        if (!tnode) {
           LEAVE("last trans has no next");
           goto fail;
        }

        trans = tnode->data;

        slist = xaccTransGetSplitList(trans);
        snode = g_list_nth(slist, 0);

        flags = TROW1;
    }

    if (IS_SPLIT(iter)) {

        tnode = iter->user_data2;
        snode = iter->user_data3;
        snode = g_list_next(snode);

        if (!snode) {
           LEAVE("last split has no next");
           goto fail;
        }

        flags = SPLIT;
    }

    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("iter %s", iter_to_string(iter));
    return TRUE;
 fail:
    iter->stamp = 0;
    return FALSE;
}



static gboolean
gnc_tree_model_split_reg_iter_children (GtkTreeModel *tree_model,
                                      GtkTreeIter *iter,
                                      GtkTreeIter *parent_iter)
{
    /* Sets iter to point to the first child of parent. If parent has no children,
       FALSE is returned and iter is set to be invalid. parent will remain a valid
       node after this function has been called. */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    GList *tnode, *snode;
    gint flags = TROW1;
    Transaction *trans;
    SplitList *slist;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (tree_model), FALSE);
    ENTER("model %p, iter %p (to be filed in), parent %s",
          tree_model, iter, (parent_iter ? iter_to_string(parent_iter) : "(null)"));

/* g_print("gnc_tree_model_split_reg_iter_children\n"); */

    if (!parent_iter) {
        /* Get the very first iter */
        tnode = model->priv->tlist;
        if (tnode) {

            slist = xaccTransGetSplitList(tnode->data);
            snode = g_list_nth(slist, 0);
        
            *iter = make_iter(model, flags, tnode, snode);
            LEAVE("iter (2) %s", iter_to_string(iter));
            return TRUE;
        } else {
            PERR("We should never have a NULL trans list.");
            goto fail;
        }
    }

/*    g_assert(VALID_ITER(model, parent_iter)); */

    if(IS_TROW1(parent_iter))
    {
        tnode = parent_iter->user_data2;
        snode = parent_iter->user_data3;
        flags = TROW2;
    }

    if(IS_TROW2(parent_iter))
    {
        tnode = parent_iter->user_data2;

        slist = xaccTransGetSplitList(tnode->data);
        snode = g_list_nth(slist, 0);
        flags = SPLIT;
    }

    if(IS_SPLIT(parent_iter))
        goto fail;

    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("iter %s", iter_to_string(iter));
    return TRUE;
 fail:
    LEAVE("iter has no children");
    iter->stamp = 0;
    return FALSE;
}



static gboolean
gnc_tree_model_split_reg_iter_has_child (GtkTreeModel *tree_model,
                                       GtkTreeIter *iter)
{
    /* Returns TRUE if iter has children, FALSE otherwise. */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    GList *tnode;
    Transaction *trans;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (tree_model), FALSE);

    ENTER("model %p, iter %s", tree_model, iter_to_string(iter));
/* g_print("gnc_tree_model_split_reg_iter_has_child\n"); */

    tnode = iter->user_data2;
    trans = tnode->data;

    if (IS_TROW1(iter))
    {
        LEAVE("Transaction Row 1 is yes");
        return TRUE;
    }

    if (IS_TROW2(iter))
    {
        LEAVE("Transaction Row 2 is yes");
        return TRUE;
    }

    LEAVE("We have no child");
    return FALSE;
}



static int
gnc_tree_model_split_reg_iter_n_children (GtkTreeModel *tree_model,
                                        GtkTreeIter *iter)
{
    /* Returns the number of children that iter has. As a special case,
       if iter is NULL, then the number of toplevel nodes is returned.  */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    Transaction *trans;
    GList *tnode;
    int i;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (tree_model), FALSE);
    ENTER("model %p, iter %s", tree_model, iter_to_string(iter));
/* g_print("gnc_tree_model_split_reg_iter_n_children\n"); */

    if (iter == NULL) {
        i = g_list_length(model->priv->tlist);
        LEAVE("toplevel count is %d", i);
        return i;
    }

    if(IS_SPLIT(iter))
        i = 0;

    if(IS_TROW1(iter))
        i = 1;

    if(IS_TROW2(iter))
    {
        tnode = iter->user_data2;
        trans = tnode->data;
        i = xaccTransCountSplits(trans);
    }

    LEAVE("The number of children iter has is %d", i);
    return i;
}



static gboolean
gnc_tree_model_split_reg_iter_nth_child (GtkTreeModel *tree_model,
                                       GtkTreeIter *iter,
                                       GtkTreeIter *parent_iter,
                                       int n)
{
    /* Sets iter to be the child of parent, using the given index. */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    Transaction *trans;
    SplitList *slist;
    GList *tnode, *snode;
    gint flags = TROW1;

    ENTER("model %p, iter %s, n %d", tree_model, iter_to_string(parent_iter), n);
    g_return_val_if_fail (GNC_IS_TREE_MODEL_SPLIT_REG (tree_model), FALSE);

/* g_print("gnc_tree_model_split_reg_iter_nth_child %d\n", n); */

    if (parent_iter == NULL) {  /* Top-level */
        tnode = g_list_nth(model->priv->tlist, n);

        if (!tnode) {
            PERR("Trans list should never be NULL.");
            goto fail;
        }
        trans = tnode->data;

        slist = xaccTransGetSplitList(trans);
        snode = g_list_nth(slist, 0);

        *iter = make_iter(model, flags, tnode, snode);
        LEAVE("iter (2) %s", iter_to_string(iter));
        return TRUE;
    }

    DEBUG("parent iter %s", iter_to_string(parent_iter));
/*    g_assert(VALID_ITER(model, parent_iter)); */

    if (IS_SPLIT(parent_iter))
        goto fail;  /* Splits have no children */


    if(IS_TROW1(parent_iter) && (n != 0))
        goto fail;

    tnode = parent_iter->user_data2;
    trans = tnode->data;
    snode = parent_iter->user_data3;
    flags = TROW2;

    if(IS_TROW2(parent_iter) && (n > xaccTransCountSplits(trans)))
        goto fail;
    else
    {

        slist = xaccTransGetSplitList(trans);
        snode = g_list_nth(slist, n);
        flags = SPLIT;
    }

    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("iter of child with index %u is %s", n, iter_to_string(iter));
    return TRUE;
 fail:
    LEAVE("iter has no child with index %u", n);
    iter->stamp = 0;
    return FALSE;
}



static gboolean
gnc_tree_model_split_reg_iter_parent (GtkTreeModel *tree_model,
                                    GtkTreeIter *iter,
                                    GtkTreeIter *child)
{
    /* Sets iter to be the parent of child. If child is at the toplevel,
       and doesn't have a parent, then iter is set to an invalid iterator
       and FALSE is returned. child will remain a valid node after this 
       function has been called. */
    GncTreeModelSplitReg *model = GNC_TREE_MODEL_SPLIT_REG (tree_model);
    GList *tnode, *snode;
    gint flags = TROW1;

    ENTER("model %p, child %s", tree_model, iter_to_string(child));

/* g_print("gnc_tree_model_split_reg_iter_parent\n"); */

/*    g_assert(VALID_ITER(model, child)); */

    tnode = child->user_data2;
    snode = child->user_data3;

    if(IS_TROW1(child))
        goto fail;

    if(IS_TROW2(child))
        flags = TROW1;

    if(IS_SPLIT(child))
        flags = TROW2;

    *iter = make_iter(model, flags, tnode, snode);
    LEAVE("parent iter is %s", iter_to_string(iter));
    return TRUE;
 fail:
    LEAVE("we have no parent");
    iter->stamp = 0;
    return FALSE;
}




/*###################################################################*/

gboolean
gnc_tree_model_split_reg_get_split_and_trans (
    GncTreeModelSplitReg *model, GtkTreeIter *iter,
    gboolean *is_trow1, gboolean *is_trow2, gboolean *is_split,
    gboolean *is_blank, Split **split, Transaction **trans)
{
    GList *node;

/*    g_return_val_if_fail(VALID_ITER(model, iter), FALSE); */
    ENTER("");
    if (is_trow1)
        *is_trow1 = !!IS_TROW1(iter);
    if (is_trow2)
        *is_trow2 = !!IS_TROW2(iter);
    if (is_split)
        *is_split = !!IS_SPLIT(iter);
    if (is_blank)
        *is_blank = !!IS_BLANK(iter);

    if (trans) {
        node = iter->user_data2;
        *trans = node ? (Transaction *) node->data : NULL;
    }
    if (split) {
        node = iter->user_data3;
        *split = node ? (Split *) node->data : NULL;
    }
    LEAVE("");
    return TRUE;
}


/*###################################################################*/

Account *
gnc_tree_model_split_reg_get_anchor(GncTreeModelSplitReg *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(model), NULL);
    return model->priv->anchor;
}


GtkListStore *
gnc_tree_model_split_reg_get_description_list(GncTreeModelSplitReg *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(model), NULL);
    return model->priv->description_list;
}


GtkListStore *
gnc_tree_model_split_reg_get_notes_list(GncTreeModelSplitReg *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(model), NULL);
    return model->priv->notes_list;
}


GtkListStore *
gnc_tree_model_split_reg_get_memo_list(GncTreeModelSplitReg *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(model), NULL);
    return model->priv->memo_list;
}


GtkListStore *
gnc_tree_model_split_reg_get_numact_list(GncTreeModelSplitReg *model)
{
    g_return_val_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(model), NULL);
    return model->priv->numact_list;
}


/* Return TRUE if string all ready exists in the list */
static gboolean
check_for_duplicates(GtkListStore *liststore, const gchar *string)
{
    GtkTreeIter iter;
    gboolean valid;

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (liststore), &iter);
    while (valid)
    {
        gchar *text;
        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL (liststore), &iter, 0, &text, -1);

        if(!(g_strcmp0(text, string)))
        {
            g_free(text);
            return TRUE;
        }
        g_free(text);

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (liststore), &iter);
    }
    return FALSE;
}



/* Create the Auto Complete List Stores.... */
static void
create_completion_models(GncTreeModelSplitReg *model)
{

    GncTreeModelSplitRegPrivate *priv;
    GtkListStore *description_list, *notes_list, *memo_list, *num_list;
    GtkTreeIter d_iter, n_iter, m_iter, num_iter;
    GList *tlist, *tnode, *slist, *snode;
    int cnt, nSplits;

    priv = model->priv;
    tlist = priv->tlist;

    description_list = gtk_list_store_new(1, G_TYPE_STRING);
    notes_list = gtk_list_store_new(1, G_TYPE_STRING);
    memo_list = gtk_list_store_new(1, G_TYPE_STRING);
    num_list = gtk_list_store_new(1, G_TYPE_STRING);

    for (tnode = tlist; tnode; tnode = tnode->next)
    {
        Transaction *trans;
        Split       *split;
        const gchar *string;

        trans = tnode->data;

        nSplits = xaccTransCountSplits(trans);
        slist = xaccTransGetSplitList(trans);
    
        /* Add to the Description list */
        string = xaccTransGetDescription(trans);
        if(g_strcmp0(string, ""))
        {
            if(check_for_duplicates(description_list, string) == FALSE)
            {
                gtk_list_store_append(description_list, &d_iter);
                gtk_list_store_set(description_list, &d_iter, 0, string , -1);
            }
        }

        /* Add to the Notes list */
        string = xaccTransGetNotes(trans);
        if(g_strcmp0(string, ""))
        {
            if(check_for_duplicates(notes_list, string) == FALSE)
            {
                gtk_list_store_append(notes_list, &n_iter);
                gtk_list_store_set(notes_list, &n_iter, 0, string, -1);
            }
        }

        /* Add to the Num list */
        string = xaccTransGetNum(trans);
        if(g_strcmp0(string, ""))
        {
            if(check_for_duplicates(num_list, string) == FALSE)
            {
                gtk_list_store_prepend(num_list, &num_iter);
                gtk_list_store_set(num_list, &num_iter, 0, string, -1);
            }
        }

        /* Loop through the list of splits for each Transaction - **do not free the list** */
        snode = slist;
        cnt = 0;
        while (cnt < nSplits)
        {
            split = snode->data;

            /* Add to the Memo list */
            string = xaccSplitGetMemo(split);
            if(g_strcmp0(string, ""))
            {
                if(check_for_duplicates(memo_list, string) == FALSE)
                {
                    gtk_list_store_append(memo_list, &m_iter);
                    gtk_list_store_set(memo_list, &m_iter, 0, string, -1);
                }
            }
            cnt++;
            snode = snode->next;
         }
    }

    priv->description_list = description_list;
    priv->notes_list = notes_list;
    priv->memo_list = memo_list;
    priv->num_list = num_list;
    priv->numact_list = gtk_list_store_new(1, G_TYPE_STRING);
}


/* Update the model with entries for the Number field ... */
void
gnc_tree_model_split_reg_get_num_list(GncTreeModelSplitReg *model)
{
    GncTreeModelSplitRegPrivate *priv;
    GtkListStore *store, *num_list;
    GtkTreeIter iter, num_iter;
    gboolean valid;

    priv = model->priv;

    store = priv->numact_list;
    num_list = priv->num_list;

    /* Clear the liststore */
    gtk_list_store_clear (store);

    valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (num_list), &num_iter);
    while (valid)
    {
        gchar *text;

        // Walk through the list, reading each row
        gtk_tree_model_get (GTK_TREE_MODEL (num_list), &num_iter, 0, &text, -1);

        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0, text, -1);
        g_free(text);

        valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (num_list), &num_iter);
    }
    priv->numact_list = store;
}


/* Update the model with entries for the Action field ... */
void
gnc_tree_model_split_reg_get_action_list(GncTreeModelSplitReg *model)
{
    GncTreeModelSplitRegPrivate *priv;
    GtkListStore *store;
    GtkTreeIter iter;

    priv = model->priv;
    store = priv->numact_list;

    /* Clear the liststore */
    gtk_list_store_clear (store);

/*FIXME This is a temp hack to give a value, this would come from ledger ? */
    model->type = CREDIT_REGISTER2;

    /* setup strings in the action pull-down */
    switch (model->type)
    {
    case BANK_REGISTER2:
        /* broken ! FIXME bg */
    case SEARCH_LEDGER2:

        /* Translators: This string has a context prefix; the translation
        	must only contain the part after the | character. */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, Q_("Action Column|Deposit"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Withdraw"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Check"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Interest"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("ATM Deposit"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("ATM Draw"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Teller"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Charge"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Payment"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Receipt"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Increase"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Decrease"), -1);
        /* Action: Point Of Sale */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("POS"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Phone"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Online"), -1);
        /* Action: Automatic Deposit ?!? */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("AutoDep"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Wire"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Credit"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Direct Debit"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Transfer"), -1);
        break;
    case CASH_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Increase"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Decrease"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        break;
    case ASSET_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Fee"), -1);
        break;
    case CREDIT_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("ATM Deposit"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("ATM Draw"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Credit"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Fee"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Interest"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Online"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        break;
    case LIABILITY_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Loan"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Interest"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Payment"), -1);
        break;
    case RECEIVABLE_REGISTER2:
    case PAYABLE_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Invoice"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Payment"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Interest"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Credit"), -1);
        break;
    case INCOME_LEDGER2:
    case INCOME_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Increase"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Decrease"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Interest"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Payment"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Rebate"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Paycheck"), -1);
        break;
    case EXPENSE_REGISTER2:
    case TRADING_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Increase"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Decrease"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        break;
    case GENERAL_LEDGER2:
    case EQUITY_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Equity"), -1);
        break;
    case STOCK_REGISTER2:
    case PORTFOLIO_LEDGER2:
    case CURRENCY_REGISTER2:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Price"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Fee"), -1);
        /* Action: Dividend */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Dividend"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Interest"), -1);
        /* Action: Long Term Capital Gains */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("LTCG"), -1);
        /* Action: Short Term Capital Gains */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("STCG"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Income"), -1);
        /* Action: Distribution */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Dist"), -1);
        /* Translators: This string has a disambiguation prefix */
        gtk_list_store_insert_with_values (store, &iter, 100, 0, Q_("Action Column|Split"), -1);
        break;

    default:
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Increase"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Decrease"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Buy"), -1);
        gtk_list_store_insert_with_values (store, &iter, 100, 0, _("Sell"), -1);
        break;
    }
    priv->numact_list = store;
}


/* Return the GtkListstore of Accounts */
GtkListStore *
gnc_tree_model_split_reg_get_acct_list(GncTreeModelSplitReg *model)
{
    GncTreeModelSplitRegPrivate *priv;
    Account *root;
    Account *acc;
    GtkListStore *store;
    GtkTreeIter iter;
    GList *accts, *ptr;
    gboolean valid;
    const gchar *name, *fname;
    gint i;

    priv = model->priv;

    /* Store is short name, full name and account pointer */
    store = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_POINTER );

    root = gnc_book_get_root_account( gnc_get_current_book());
/*FIXME This does not look sorted to me, need to look at this */
    accts = gnc_account_get_descendants_sorted( root );

    for (ptr = accts, i = 0; ptr; ptr = g_list_next(ptr), i++)
    {
        acc = ptr->data;

        if(!(acc == model->priv->anchor))
        {
            fname = gnc_account_get_full_name(acc);
            name = xaccAccountGetName(acc);
            gtk_list_store_append(store, &iter);
            gtk_list_store_set(store, &iter, 0, name, 1, fname, 2, acc, -1);
        }
    }

    g_list_free( accts );

    return store;
}


/*******************************************************************/
/*   Split Register Tree Model - Engine Event Handling Functions   */
/*******************************************************************/

/** This function is the handler for all event messages from the
 *  engine.  Its purpose is to update the account tree model any time
 *  an account is added to the engine or deleted from the engine.
 *  This change to the model is then propagated to any/all overlying
 *  filters and views.  This function listens to the ADD, REMOVE, and
 *  DESTROY events.
 *
 *  @internal
 *
 *  @warning There is a "Catch 22" situation here.
 *  gtk_tree_model_row_deleted() can't be called until after the item
 *  has been deleted from the real model (which is the engine's
 *  account tree for us), but once the account has been deleted from
 *  the engine we have no way to determine the path to pass to
 *  row_deleted().  This is a PITA, but the only other choice is to
 *  have this model mirror the engine's accounts instead of
 *  referencing them directly.
 *
 *  @param entity The guid of the affected item.
 *
 *  @param type The type of the affected item.  This function only
 *  cares about items of type "account".
 *
 *  @param event type The type of the event. This function only cares
 *  about items of type ADD, REMOVE, MODIFY, and DESTROY.
 *
 *  @param user_data A pointer to the split register tree model.
 */
static void
gnc_tree_model_split_reg_event_handler (QofInstance *entity,
                                      QofEventId event_type,
                                      GncTreeModelSplitReg *model,
                                      GncEventData *event_data)
{
    GncTreeModelSplitRegPrivate *priv = model->priv;
    GncEventData *ed = event_data;
    GtkTreeIter iter;
    GtkTreePath *path;
    Transaction *trans;
    Split *split = NULL;
    QofIdType type;
    const gchar *name = NULL;
    GList *tnode;

    g_return_if_fail(GNC_IS_TREE_MODEL_SPLIT_REG(model));

    if (qof_instance_get_book(entity) != priv->book)
        return;
    type = entity->e_type;

    if (g_strcmp0(type, GNC_ID_SPLIT) == 0) {
        /* Get the split.*/
        split = (Split *) entity;

        switch (event_type) {
        case QOF_EVENT_MODIFY:

            break;
        default:
            DEBUG("ignored event for %p (%s)", split, name);
        }
    } else if (g_strcmp0(type, GNC_ID_TRANS) == 0) {
        /* Get the trans.*/
        trans = (Transaction *) entity;

        switch (event_type) {
        case GNC_EVENT_ITEM_ADDED:
            split = (Split *) ed->node;


            break;
        case GNC_EVENT_ITEM_REMOVED:
            split = (Split *) ed->node;


            break;
        case QOF_EVENT_MODIFY:


            break;
        case QOF_EVENT_DESTROY:


            break;
        default:
            DEBUG("ignored event for %p (%s)", trans, name);
        }
    } else if (g_strcmp0(type, GNC_ID_ACCOUNT) == 0) {
        switch (event_type) {
            Account *acc;
        case GNC_EVENT_ITEM_ADDED:
            split = (Split *) ed;

            break;
        default:
            ;
        }
    }
}
