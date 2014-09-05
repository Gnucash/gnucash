/********************************************************************\
 * gnc-combott.c -- Basic simulation of ComboBox with tooltips for  *
 *                  each item.                                      *
 * Copyright (c) 2012 Robert Fewell                                 *
 *                                                                  *
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
 * This widget requires external ListStore which has two columns.   *
 * By default, column 0 holds the text to display and column 1 the  *
 * per item tooltip but these can be specified if the liststore has *
 * a different format.                                              *
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
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
#include <gtk/gtk.h>
#include "gnc-combott.h"
#include <strings.h>
#include <string.h>

enum
{
    CHANGED,
    LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_MODEL,
    PROP_ACTIVE,
    PROP_TEXT_COL,
    PROP_TIP_COL,
};

#define GNC_COMBOTT_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_COMBOTT, GncCombottPrivate))

static guint combott_signals[LAST_SIGNAL] = {0,};

typedef struct GncCombottPrivate
{
    GtkTreeModel  *model;
    GtkWidget     *button;
    GtkWidget     *label;
    GtkWidget     *menu;
    GtkTreeIter    active_iter;
    gint           active;

    gint	   text_col;
    gint	   tip_col;

    gint           max_number_char;
    gint           num_items;

    gint           x;
    gint           y;
    gint           width;
    gint           height;

} GncCombottPrivate;

/** Declarations *********************************************************/
static void gctt_init (GncCombott *combott);

static void gctt_class_init (GncCombottClass *klass);

static void gctt_set_property (GObject *object,
                               guint param_id,
                               const GValue *value,
                               GParamSpec *pspec);

static void gctt_get_property (GObject *object,
                               guint param_id,
                               GValue *value,
                               GParamSpec *pspec);

static void gctt_finalize (GObject *object);

static void gctt_combott_menu_position (GtkMenu *menu,
                                        gint *x,
                                        gint *y,
                                        gint *push_in,
                                        gpointer user_data);

static void gctt_changed (GncCombott *combott);
static void gctt_set_model (GncCombott *combott, GtkTreeModel *model);
static void gctt_refresh_menu (GncCombott *combott, GtkTreeModel *model);
static void gctt_rebuild_menu (GncCombott *combott, GtkTreeModel *model);

static gboolean which_tooltip_cb (GtkWidget  *widget, gint x, gint y,
                                  gboolean keyboard_mode, GtkTooltip *tooltip, gpointer user_data);
static gboolean button_press_cb (GtkWidget *widget, GdkEvent *event, gpointer *user_data );
static void button_getsize_cb (GtkWidget *widget, GtkAllocation *allocation, gpointer *user_data);
static void menu_getsize_cb (GtkWidget *widget, GtkAllocation *allocation, gpointer *user_data);
static void menuitem_response_cb (GtkMenuItem *item, gpointer *user_data);


/************************************************************/
/*               g_object required functions                */
/************************************************************/
static GObjectClass *parent_class = NULL;

GType
gnc_combott_get_type (void)
{
    static GType combott_type = 0;

    if (!combott_type)
    {
        static const GTypeInfo combott_info =
        {
            sizeof (GncCombottClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gctt_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncCombott),
            0,              /* n_preallocs */
            (GInstanceInitFunc) gctt_init,
        };

        combott_type = g_type_register_static (GTK_TYPE_HBOX,
                                               "GncCombott",
                                               &combott_info, 0);
    }
    return combott_type;
}


static void
gctt_class_init (GncCombottClass *klass)
{
    GObjectClass            *gobject_class;

    parent_class = g_type_class_peek_parent (klass);
    gobject_class = G_OBJECT_CLASS (klass);

    gobject_class->set_property = gctt_set_property;
    gobject_class->get_property = gctt_get_property;
    gobject_class->finalize = gctt_finalize;

    klass->changed = gctt_changed;

    combott_signals[CHANGED] =
        g_signal_new ("changed",
                      G_OBJECT_CLASS_TYPE (klass),
                      G_SIGNAL_RUN_LAST,
                      G_STRUCT_OFFSET (GncCombottClass, changed),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE, 0);

    g_object_class_install_property (
        gobject_class,
        PROP_MODEL,
        g_param_spec_object ("model",
                             "Combott model",
                             "The model for the combo tooltip",
                             GTK_TYPE_TREE_MODEL,
                             G_PARAM_READWRITE));

    g_object_class_install_property (
        gobject_class,
        PROP_TEXT_COL,
        g_param_spec_int ("text-col",
                          "text column",
                          "Column for the text",
                          0,
                          G_MAXINT,
                          0,
                          G_PARAM_READWRITE));

    g_object_class_install_property (
        gobject_class,
        PROP_TIP_COL,
        g_param_spec_int ("tip-col",
                          "tip column",
                          "Column for the tip",
                          0,
                          G_MAXINT,
                          1,
                          G_PARAM_READWRITE));

    g_type_class_add_private(klass, sizeof(GncCombottPrivate));
}


static void
gctt_init (GncCombott *combott)
{
    GtkWidget *hbox;
    GtkWidget *label;
    GtkWidget *arrow;
    GtkWidget *button;
    GtkWidget *sep;

    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    priv->active = 0;
    priv->text_col = 0;
    priv->tip_col = 1;

    hbox = gtk_hbox_new(FALSE, 0);

    arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_OUT);
    gtk_box_pack_end (GTK_BOX (hbox), arrow, FALSE, FALSE, 0);

    sep = gtk_vseparator_new();
    gtk_box_pack_end (GTK_BOX (hbox), sep, FALSE, FALSE, 0);

    label = gtk_label_new(NULL);
    gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
    priv->label = label;

    button = gtk_button_new();
    gtk_container_add(GTK_CONTAINER(button), GTK_WIDGET(hbox));
    priv->button = button;

    gtk_container_add(GTK_CONTAINER(combott), GTK_WIDGET(button));

    g_signal_connect (button, "event",
                      G_CALLBACK (button_press_cb), combott);

    gtk_widget_set_has_tooltip (GTK_WIDGET(combott), TRUE);

    g_signal_connect(G_OBJECT(combott), "query-tooltip",
                     G_CALLBACK(which_tooltip_cb), combott);

    g_signal_connect(G_OBJECT(combott), "size-allocate",
                     G_CALLBACK(button_getsize_cb), combott);

    gtk_widget_show(GTK_WIDGET(priv->button));
}


static void
gctt_set_property (GObject      *object,
                   guint         param_id,
                   const GValue *value,
                   GParamSpec   *pspec)
{
    GncCombott *combott = GNC_COMBOTT (object);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    switch (param_id)
    {
    case PROP_MODEL:
        gctt_set_model (combott, g_value_get_object (value));
        break;

    case PROP_ACTIVE:
        gnc_combott_set_active (combott, g_value_get_int (value));
        break;

    case PROP_TEXT_COL:
        priv->text_col = g_value_get_int (value);
        break;

    case PROP_TIP_COL:
        priv->tip_col = g_value_get_int (value);
        gctt_refresh_menu(combott, priv->model);
        break;

    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gctt_get_property (GObject    *object,
                   guint       param_id,
                   GValue     *value,
                   GParamSpec *pspec)
{
    GncCombott *combott = GNC_COMBOTT (object);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    switch (param_id)
    {
    case PROP_MODEL:
        g_value_take_object (value, priv->model);
        break;

    case PROP_ACTIVE:
        g_value_set_int (value, gnc_combott_get_active (combott));
        break;

    case PROP_TEXT_COL:
        g_value_set_int (value, priv->text_col);
        break;

    case PROP_TIP_COL:
        g_value_set_int (value, priv->tip_col);
        break;

    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
        break;
    }
}


static void
gctt_finalize (GObject *object)
{
    GncCombott *combott;
    GncCombottPrivate *priv;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_COMBOTT (object));

    combott = GNC_COMBOTT (object);
    priv = GNC_COMBOTT_GET_PRIVATE (combott);

    if (priv->model)
    {
        priv->model = NULL;
    }

    if (priv->menu)
    {
        priv->menu = NULL;
    }

    G_OBJECT_CLASS (parent_class)->finalize (object);
}


static void
gctt_set_model (GncCombott *combott, GtkTreeModel *model)
{
    GncCombottPrivate *priv;

    g_return_if_fail (GNC_IS_COMBOTT (combott));
    g_return_if_fail (model == NULL || GTK_IS_TREE_MODEL (model));

    priv = GNC_COMBOTT_GET_PRIVATE (combott);

    gctt_rebuild_menu(combott, model);

    priv->model = model;
    g_object_ref (priv->model);
}


static void
gctt_rebuild_menu (GncCombott *combott, GtkTreeModel *model)
{
    GncCombottPrivate *priv;
    GtkTreeIter iter;
    GtkWidget *menu_items;
    gboolean valid;
    gint num = 1;
    gint items = 0;

    g_return_if_fail (GNC_IS_COMBOTT (combott));
    g_return_if_fail (model == NULL || GTK_IS_TREE_MODEL (model));

    priv = GNC_COMBOTT_GET_PRIVATE (combott);

    priv->menu = NULL;

    priv->menu = gtk_menu_new();

    valid = gtk_tree_model_get_iter_first (model, &iter);
    while (valid)
    {
        GtkWidget *label;

        /* Walk through the list, reading each row */
        gchar *str_data;
        gchar *tip_data;
        gtk_tree_model_get (model, &iter,
                            priv->text_col, &str_data,
                            priv->tip_col, &tip_data,
                            -1);

        /* Create a new menu-item with a name... */
        menu_items = gtk_menu_item_new_with_label (str_data);

        /* Get widget width to max number of characters in list */
        if(strlen(str_data) > num)
            num = strlen(str_data);

        /* Add the tooltip to the child label */
        label = gtk_bin_get_child(GTK_BIN(menu_items));
        gtk_widget_set_tooltip_text (label, tip_data);
        gtk_misc_set_alignment (GTK_MISC(label), 0, 0.5);

        /* ...and add it to the menu. */
        gtk_menu_shell_append (GTK_MENU_SHELL (priv->menu), menu_items);
        g_signal_connect (menu_items, "activate",
                          G_CALLBACK (menuitem_response_cb),
                          combott);

        /* Show the widget */
        gtk_widget_show (menu_items);

        g_free (str_data);
        g_free (tip_data);
        items++;
        valid = gtk_tree_model_iter_next (model, &iter);
    }

    g_signal_connect(G_OBJECT(priv->menu), "size-allocate", G_CALLBACK(menu_getsize_cb), combott);

    /* Set widget width to max number of characters in list */
    priv->max_number_char = num;
    gtk_label_set_width_chars(GTK_LABEL(priv->label), priv->max_number_char);

    priv->num_items = items;
}


static void
gctt_refresh_menu (GncCombott *combott, GtkTreeModel *model)
{
    GncCombottPrivate *priv;

    g_return_if_fail (GNC_IS_COMBOTT (combott));
    g_return_if_fail (model == NULL || GTK_IS_TREE_MODEL (model));

    priv = GNC_COMBOTT_GET_PRIVATE (combott);

    gctt_rebuild_menu(combott, model);
}


static void
gctt_changed(GncCombott *combott)
{
    /*
    g_print("Changed Signal\n");
    */
}


static void
gctt_combott_menu_position (GtkMenu  *menu,
                            gint     *x,
                            gint     *y,
                            gint     *push_in,
                            gpointer  user_data)
{
    GncCombott *combott = GNC_COMBOTT (user_data);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);
    gint sx, sy;
    GtkWidget *child;
    GtkRequisition req;

    child = GTK_BIN (priv->button)->child;

    sx = sy = 0;

    if (!gtk_widget_get_has_window (child))
    {
        sx += child->allocation.x;
        sy += child->allocation.y;
    }

    gdk_window_get_root_coords (child->window, sx, sy, &sx, &sy);

    sx -= GTK_WIDGET (priv->button)->style->xthickness;

    gtk_widget_size_request (GTK_WIDGET (menu), &req);

    if (gtk_widget_get_direction (GTK_WIDGET (priv->button)) == GTK_TEXT_DIR_LTR)
        *x = sx;
    else
        *x = sx + child->allocation.width - req.width;

    if(priv->active == -1 || priv->active == 0)
        *y = sy;
    else
        *y = sy - ((req.height / priv->num_items) * (priv->active - 1));

    *push_in = FALSE;
}


static void
button_getsize_cb (GtkWidget *widget, GtkAllocation *allocation, gpointer *user_data)
{
    GncCombott *combott = GNC_COMBOTT (user_data);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    priv->width = allocation->width;
    priv->height = allocation->height;
    priv->x = allocation->x;
    priv->y = allocation->y;
}


static void
menu_getsize_cb (GtkWidget *widget, GtkAllocation *allocation, gpointer *user_data)
{
    GncCombott *combott = GNC_COMBOTT (user_data);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    /* Set the menu width */
    gtk_widget_set_size_request (widget, priv->width - 6, allocation->height);
}


static gboolean
which_tooltip_cb (GtkWidget  *widget, gint x, gint y, gboolean keyboard_mode, GtkTooltip *tooltip, gpointer user_data)
{
    gchar *text = "";

    GncCombott *combott = GNC_COMBOTT (user_data);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    if(!priv->active == 0)
    {
        gtk_tree_model_get( priv->model, &priv->active_iter, priv->tip_col, &text, -1 );
        if(g_strcmp0(text, "") && (text != NULL))
        {
            gchar *label = "";
            gtk_tooltip_set_text (tooltip, text);
            g_free(text);
            return TRUE;
        }
        else
        {
            g_free(text);
            return FALSE;
        }
    }
    return FALSE;
}


static gboolean
button_press_cb (GtkWidget *widget, GdkEvent *event, gpointer *user_data )
{
    GncCombott *combott = GNC_COMBOTT (user_data);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    if(priv->model != NULL)
    {
        if (event->type == GDK_BUTTON_PRESS)
        {
            GdkEventButton *bevent = (GdkEventButton *) event;

            gtk_menu_popup (GTK_MENU (priv->menu),
                            NULL, NULL,
                            gctt_combott_menu_position, combott,
                            bevent->button, bevent->time);

            /* Tell calling code that we have handled this event; the buck
             * stops here. */
            return TRUE;
        }
    }
    /* Tell calling code that we have not handled this event; pass it on. */
    return FALSE;
}


static void
menuitem_response_cb (GtkMenuItem *item, gpointer *user_data )
{
    const gchar *label_text;
    GtkTreeIter iter, iter_now = {0, NULL, NULL, NULL};
    gboolean valid;
    gint active = 1;
    gint active_now = 1;

    GncCombott *combott = GNC_COMBOTT (user_data);
    GncCombottPrivate *priv = GNC_COMBOTT_GET_PRIVATE (combott);

    label_text = gtk_menu_item_get_label (item);

    /* Set the button Label */
    gtk_label_set_text(GTK_LABEL(priv->label), label_text);
    gtk_misc_set_alignment (GTK_MISC(priv->label), 0, 0.5);

    /* Get the coresponding entry in the list store */
    valid = gtk_tree_model_get_iter_first (priv->model, &iter);
    while (valid)
    {
        /* Walk through the list, reading each row */
        gchar *str_data;
        gchar *tip_data;
        gtk_tree_model_get (priv->model, &iter,
                            priv->text_col, &str_data,
                            priv->tip_col, &tip_data,
                            -1);
        if(!g_strcmp0(str_data, label_text))
        {
            active_now = active;
            iter_now = iter;
        }

        g_free (str_data);
        g_free (tip_data);
        active ++;
        valid = gtk_tree_model_iter_next (priv->model, &iter);
    }

    /* Emit Changed signal if we have selected a new entry */
    if(priv->active != active_now)
    {
        priv->active = active_now;
        priv->active_iter = iter_now;

        g_signal_emit (combott, combott_signals[CHANGED], 0);
    }
}


GncCombott *
gnc_combott_new (void)
{
    GObject *hbox;
    hbox = g_object_new (GNC_TYPE_COMBOTT, NULL);
    return GNC_COMBOTT (hbox);
}


gint
gnc_combott_get_active (GncCombott *combott)
{
    GncCombottPrivate *priv;
    gint result;

    g_return_val_if_fail (GNC_IS_COMBOTT (combott), 0);

    priv = GNC_COMBOTT_GET_PRIVATE (combott);

    result = priv->active - 1;

    return result;
}


void
gnc_combott_set_active (GncCombott *combott, gint index)
{
    GncCombottPrivate *priv;
    GtkTreeIter iter;
    gboolean valid = TRUE;
    gint active = 1;
    gint num = 1;

    g_return_if_fail (GNC_IS_COMBOTT (combott));
    g_return_if_fail (index >= -1);

    priv = GNC_COMBOTT_GET_PRIVATE (combott);

    /* Do we have a valid model */
    if (priv->model != NULL)
    {
        /* Is index the same as current option */
        if(index + 1 != priv->active)
        {
            /* Set button label to blank for no selection */
            if(index == -1)
            {
                priv->active = 0;
                gtk_label_set_text(GTK_LABEL(priv->label), "");
                g_signal_emit (combott, combott_signals[CHANGED], 0);
            }
            else
            {
                /* Get the coresponding entry in the list store */
                valid = gtk_tree_model_get_iter_first (priv->model, &iter);
                while (valid)
                {
                    /* Walk through the list, reading each row */
                    gchar *str_data;
                    gchar *tip_data;
                    /* Make sure you terminate calls to gtk_tree_model_get()
                     * with a '-1' value */
                    gtk_tree_model_get (priv->model, &iter,
                                        priv->text_col, &str_data,
                                        priv->tip_col, &tip_data,
                                        -1);

                    if(index + 1 == active)
                    {
                        priv->active = index + 1;
                        priv->active_iter = iter;
                        gtk_label_set_text(GTK_LABEL(priv->label), str_data);
                        gtk_misc_set_alignment (GTK_MISC(priv->label), 0, 0.5);
                        g_signal_emit (combott, combott_signals[CHANGED], 0);
                    }

                    g_free (str_data);
                    g_free (tip_data);
                    active ++;
                    valid = gtk_tree_model_iter_next (priv->model, &iter);
                }
            }
        }
    }
}
