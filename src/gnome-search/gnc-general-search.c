/*
 * gnc-general-select.c --  Widget to pop-up a search dialog and show
 *			the selected item.
 *
 * Copyright (C) 2001 Free Software Foundation
 * All rights reserved.
 *
 * Derek Atkins <warlord@MIT.EDU>
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-component-manager.h"
#include "QueryCore.h"
#include "QueryObject.h"
#include "gncObject.h"
#include "gnc-general-search.h"

#define GNCGENERALSEARCH_CLASS	"gnc-general-search-widget"

/* Signal codes */
enum
{
    SELECTION_CHANGED,
    LAST_SIGNAL
};

/* Columns used in GtkEntryCompletion's model */
enum
{
    GSL_COLUMN_TEXT,
    GSL_COLUMN_QOFOBJECT,
    GSL_N_COLUMNS
};

static void gnc_general_search_init         (GNCGeneralSearch      *gsl);
static void gnc_general_search_class_init   (GNCGeneralSearchClass *class);
static void gnc_general_search_destroy      (GtkObject             *object);

typedef struct _GNCGeneralSearchPrivate GNCGeneralSearchPrivate;

struct _GNCGeneralSearchPrivate
{
    GUID			guid;
    GNCIdTypeConst		type;
    GNCSearchCB		search_cb;
    gpointer		user_data;
    GNCSearchWindow *	sw;
    const QofParam * get_guid;
    gint			component_id;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_GENERAL_SEARCH, GNCGeneralSearchPrivate))

static GtkHBoxClass *parent_class;
static guint general_search_signals[LAST_SIGNAL];


/**
 * gnc_general_search_get_type:
 *
 * Returns the GtkType for the GNCGeneralSearch widget
 */
GType
gnc_general_search_get_type (void)
{
    static GType general_search_type = 0;

    if (!general_search_type)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GNCGeneralSearchClass),    /* class_size */
            NULL,   			   /* base_init */
            NULL,				   /* base_finalize */
            (GClassInitFunc) gnc_general_search_class_init,
            NULL,				   /* class_finalize */
            NULL,				   /* class_data */
            sizeof (GNCGeneralSearch),	   /* */
            0,				   /* n_preallocs */
            (GInstanceInitFunc) gnc_general_search_init,
        };

        general_search_type = g_type_register_static (GTK_TYPE_HBOX,
                              "GNCGeneralSearch",
                              &our_info, 0);
    }

    return general_search_type;
}

static void
gnc_general_search_class_init (GNCGeneralSearchClass *klass)
{
    GtkObjectClass *object_class = (GtkObjectClass *) klass;

    object_class = (GtkObjectClass*) klass;

    parent_class = gtk_type_class (gtk_hbox_get_type ());

    general_search_signals[SELECTION_CHANGED] =
        g_signal_new("changed",
                     G_TYPE_FROM_CLASS(object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET(GNCGeneralSearchClass, changed),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__VOID,
                     G_TYPE_NONE, 0);

    object_class->destroy = gnc_general_search_destroy;

    klass->changed = NULL;

    g_type_class_add_private(klass, sizeof(GNCGeneralSearchPrivate));
}

static void
gnc_general_search_init (GNCGeneralSearch *gsl)
{
    gsl->selected_item = NULL;
}

static void
gnc_general_search_destroy (GtkObject *object)
{
    GNCGeneralSearch *gsl;
    GNCGeneralSearchPrivate *priv;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_GENERAL_SEARCH (object));

    gsl = GNC_GENERAL_SEARCH (object);

    gsl->entry = NULL;
    gsl->button = NULL;

    priv = _PRIVATE(gsl);
    /* Clear the callbacks */
    if (priv->sw)
    {
        gnc_search_dialog_set_select_cb (priv->sw, NULL, NULL, FALSE);
        gnc_search_dialog_disconnect (priv->sw, gsl);
        priv->sw = NULL;
    }
    if (priv->component_id)
    {
        /* Unregister ourselves */
        gnc_unregister_gui_component (priv->component_id);
        priv->component_id = 0;
    }

    if (GTK_OBJECT_CLASS (parent_class)->destroy)
        GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

/* The "selection" contents have changed.  Change the text. */
static void
reset_selection_text (GNCGeneralSearch *gsl)
{
    GNCGeneralSearchPrivate *priv;
    const char *text;

    priv = _PRIVATE(gsl);
    if (gsl->selected_item == NULL)
        text = "";
    else
        text = gncObjectPrintable (priv->type, gsl->selected_item);

    gtk_entry_set_text(GTK_ENTRY(gsl->entry), text);
}

/* We've got a refresh event */
static void
refresh_handler (GHashTable *changes, gpointer data)
{
    GNCGeneralSearch *gsl = data;
    GNCGeneralSearchPrivate *priv;
    const EventInfo *info;

    priv = _PRIVATE(gsl);
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &priv->guid);
        if (info)
        {
            if (info->event_mask & QOF_EVENT_DESTROY)
                gsl->selected_item = NULL;
            reset_selection_text (gsl);
        }
    }
}

/* The user has selected from the search dialog */
static void
new_item_selected_cb (gpointer item, gpointer user_data)
{
    GNCGeneralSearch *gsl = user_data;
    gnc_general_search_set_selected (gsl, item);
}

/* The search dialog has closed; let's forget about her */
static int
on_close_cb (GtkDialog *dialog, gpointer user_data)
{
    GNCGeneralSearch *gsl = user_data;
    GNCGeneralSearchPrivate *priv;

    priv = _PRIVATE(gsl);
    priv->sw = NULL;
    return FALSE;
}

/* The user clicked on the button.  Pop up the selection window */
static void
search_cb(GtkButton * button, gpointer user_data)
{
    GNCGeneralSearch *gsl = user_data;
    GNCGeneralSearchPrivate *priv;
    GNCSearchWindow *sw;

    priv = _PRIVATE(gsl);
    if (priv->sw)
    {
        gnc_search_dialog_raise (priv->sw);
        return;
    }

    sw = (priv->search_cb)(gsl->selected_item, priv->user_data);

    /* NULL means nothing to 'select' */
    if (sw == NULL)
        return;

    /* Ok, save this search window and setup callbacks */
    priv->sw = sw;

    /* Catch when the search dialog closes */
    gnc_search_dialog_connect_on_close (sw, G_CALLBACK (on_close_cb),
                                        gsl);

    /* Catch the selection */
    gnc_search_dialog_set_select_cb (sw, new_item_selected_cb,
                                     gsl, gsl->allow_clear);

}

/**  The completion attached to search edit widget has selected a
 *   match.  This function extracts the completed string from the
 *   completion code's temporary model, and uses that to set the iterator
 *   and object data of the selection for use when the user leaves the widget.
 *   This should always point to a valid iterator since the user
 *   made the selection from a list of available object names.
 *
 *   @param completion Unused.
 *
 *   @param comp_model A temporary model used by completion code that
 *   contains only the current matches.
 *
 *   @param comp_iter The iter in the completion's temporary model
 *   that represents the user selected match.
 *
 *   @param cbe A pointer to a currency entry widget. */
static gboolean
gnc_gsl_match_selected_cb (GtkEntryCompletion *completion,
                           GtkTreeModel       *comp_model,
                           GtkTreeIter        *comp_iter,
                           GNCGeneralSearch   *gsl)
{
    QofObject * qofobject;

    gtk_tree_model_get(comp_model, comp_iter, GSL_COLUMN_QOFOBJECT, &qofobject, -1);
    gnc_general_search_set_selected (gsl, qofobject);
    return FALSE;
}

/**  The focus left the general search edit widget, so reset the widget to
 *   its last known good value.  If the widget value contained a valid
 *   value then this is a noop.  Otherwise the widget will be reset
 *   to the last user selected value.  This latter state will occur
 *   if the user has typed characters directly into the widget but not
 *   selected a completion.
 *
 *   @param entry The entry widget in which the user is typing.
 *
 *   @param event Unused.
 *
 *   @param gsl A pointer to a general search widget. */
static gboolean
gnc_gsl_focus_out_cb (GtkEntry         *entry,
                      GdkEventFocus    *event,
                      GNCGeneralSearch *gsl)
{
    const gchar	*text;
    GtkEntryCompletion *completion;
    GtkTreeModel *model;
    GtkTreeIter iter;
    gchar *lc_text, *tree_string, *lc_tree_string;
    gboolean match, valid_iter;
    QofObject *qofobject;
    gpointer selected_item = NULL;

    /* Attempt to match the current text to a qofobject. */
    completion = gtk_entry_get_completion(entry);
    model = gtk_entry_completion_get_model(completion);

    /* Return if completion tree is empty */
    valid_iter = gtk_tree_model_get_iter_first(model, &iter);
    if (!valid_iter)
        return FALSE;

    text = gtk_entry_get_text(entry);
    lc_text = g_utf8_strdown(text, -1);

    /* The last, valid selected entry can match the entered text
     * No need to search further in that case */
    if (gsl->selected_item)
    {
        GNCGeneralSearchPrivate *	priv;

        priv = _PRIVATE(gsl);
        tree_string = g_strdup(qof_object_printable(priv->type, gsl->selected_item));
        lc_tree_string = g_utf8_strdown(tree_string, -1);
        match = g_utf8_collate(lc_text, lc_tree_string) == 0;
        g_free(tree_string);
        g_free(lc_tree_string);
        if (match)
            selected_item = gsl->selected_item;
    }

    /* Otherwise, find a match in the completion list */
    while (valid_iter && !selected_item)
    {
        gtk_tree_model_get(model, &iter, GSL_COLUMN_TEXT, &tree_string, -1);
        lc_tree_string = g_utf8_strdown(tree_string, -1);
        match = g_utf8_collate(lc_text, lc_tree_string) == 0;
        g_free(tree_string);
        g_free(lc_tree_string);
        if (match)
        {
            gtk_tree_model_get(model, &iter, GSL_COLUMN_QOFOBJECT, &qofobject, -1);
            selected_item = qofobject;
        }
        else
            valid_iter = gtk_tree_model_iter_next(model, &iter);
    }

    g_free(lc_text);
    gnc_general_search_set_selected (gsl, selected_item);
    return FALSE;
}

static void
create_children (GNCGeneralSearch *gsl,
                 const char       *label,
                 gboolean          text_editable,
                 GNCIdTypeConst    type,
                 QofBook          *book)
{
    GtkListStore *	list_store;
    QueryNew *	q;
    GtkTreeIter iter;
    GList * list, * it;
    GtkEntryCompletion *completion;

    /* Add a text entry box */
    gsl->entry = gtk_entry_new ();
    if (!text_editable)
        gtk_editable_set_editable (GTK_EDITABLE (gsl->entry), FALSE);
    gtk_box_pack_start (GTK_BOX (gsl), gsl->entry, TRUE, TRUE, 0);


    /* Setup a GtkEntryCompletion auxiliary widget for our Entry box
     * This requires an internal table ("model") with the possible
     * auto-completion text entries */

    /* Query for the requested object type */
    q = qof_query_create_for (type);
    qof_query_add_boolean_match(q, g_slist_prepend
                                (NULL, QOF_PARAM_ACTIVE), TRUE, QOF_QUERY_AND);
    qof_query_set_book (q, book);
    list = qof_query_run(q);

    /* Setup the internal model */
    list_store = gtk_list_store_new (GSL_N_COLUMNS, G_TYPE_STRING, G_TYPE_OBJECT);
    for (it = list; it != NULL ; it = it->next)
    {
        char * name;

        name = g_strdup(qof_object_printable(type, it->data));
        /* Add a new row to the model */
        if (name)
        {
            gtk_list_store_append (list_store, &iter);
            gtk_list_store_set (list_store, &iter,
                                GSL_COLUMN_TEXT, name,
                                GSL_COLUMN_QOFOBJECT, G_OBJECT(it->data),
                                -1);
            g_free(name);
        }

    }

    gncQueryDestroy(q);

    /* Add the GtkEntryCompletion widget */
    completion = gtk_entry_completion_new();
    gtk_entry_completion_set_model(completion, GTK_TREE_MODEL(list_store));
    gtk_entry_completion_set_text_column(completion, 0);
    gtk_entry_completion_set_inline_completion(completion, TRUE);
    gtk_entry_set_completion(GTK_ENTRY(gsl->entry), completion);

    g_signal_connect (G_OBJECT (completion), "match_selected",
                      G_CALLBACK (gnc_gsl_match_selected_cb), gsl);
    g_signal_connect (G_OBJECT (gsl->entry), "focus-out-event",
                      G_CALLBACK (gnc_gsl_focus_out_cb), gsl);

    g_object_unref(completion);
    gtk_widget_show (gsl->entry);

    /* Add the search button */
    gsl->button = gtk_button_new_with_label (label);
    gtk_box_pack_start (GTK_BOX (gsl), gsl->button, FALSE, FALSE, 0);
    g_signal_connect (G_OBJECT (gsl->button), "clicked",
                      G_CALLBACK (search_cb), gsl);
    gtk_widget_show (gsl->button);
}

/**
 * gnc_general_search_new:
 *
 * Creates a new GNCGeneralSearch widget which can be used to provide
 * an easy way to choose selections.
 *
 * @param type The type of object that this widget will be used for.
 * This parameter is a GNCIdTypeConst.
 * @param label The label for the GtkButton child widget.
 * @param text_editable switch to enable or disable direct text entry
 * @param search_cb The callback function to use when an object has been
 * selected in the search dialog. This dialog is created when clicking on
 * the GtkButton child widget.
 * @param user_data Generic pointer to context relevant data that can be
 * used by callback functions later on. At present, depending on the context
 * this can be a QofBook, a GncISI structure or a InvoiceWindow structure.
 * @param book Pointer to the QofBook for this search widget. This is used for
 * the autocompletion in the text entry widget.
 *
 * @return a GNCGeneralSearch widget.
 */
GtkWidget *
gnc_general_search_new (GNCIdTypeConst type,
                        const char    *label,
                        gboolean       text_editable,
                        GNCSearchCB    search_cb,
                        gpointer       user_data,
                        QofBook       *book)
{
    GNCGeneralSearch *gsl;
    GNCGeneralSearchPrivate *priv;
    const QofParam *get_guid;

    g_return_val_if_fail (type && label && search_cb, NULL);

    get_guid = qof_class_get_parameter (type, QOF_PARAM_GUID);
    g_return_val_if_fail (get_guid, NULL);

    gsl = g_object_new (GNC_TYPE_GENERAL_SEARCH, NULL);

    create_children (gsl, label, text_editable, type, book);

    priv = _PRIVATE(gsl);
    priv->type = type;
    priv->search_cb = search_cb;
    priv->user_data = user_data;
    priv->get_guid = get_guid;
    priv->component_id =
        gnc_register_gui_component (GNCGENERALSEARCH_CLASS,
                                    refresh_handler, NULL, gsl);

    return GTK_WIDGET (gsl);
}

/**
 * gnc_general_search_set_selected:
 * @gsl: the general selection widget
 * @selection: the selection to point to
 *
 * Sets the selection value of the widget to a particular pointer.
 *
 * Returns nothing.
 */
void
gnc_general_search_set_selected (GNCGeneralSearch *gsl, gpointer selection)
{
    GNCGeneralSearchPrivate *priv;

    g_return_if_fail(gsl != NULL);
    g_return_if_fail(GNC_IS_GENERAL_SEARCH(gsl));

    priv = _PRIVATE(gsl);
    if (selection != gsl->selected_item)
    {
        gsl->selected_item = selection;
        g_signal_emit(gsl,
                      general_search_signals[SELECTION_CHANGED], 0);
    }
    reset_selection_text (gsl);

    gnc_gui_component_clear_watches (priv->component_id);

    if (selection)
    {
        const QofParam *get_guid = priv->get_guid;
        priv->guid = * ((GUID *)(get_guid->param_getfcn
                                 (gsl->selected_item, get_guid)));
        gnc_gui_component_watch_entity
        (priv->component_id, &(priv->guid),
         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    }
    else
        priv->guid = *xaccGUIDNULL ();
}

/**
 * gnc_general_search_get_selected:
 * @gsl: the general selection widget
 *
 * Returns the current selection by the widget.
 */
gpointer
gnc_general_search_get_selected (GNCGeneralSearch *gsl)
{
    g_return_val_if_fail(gsl != NULL, NULL);
    g_return_val_if_fail(GNC_IS_GENERAL_SEARCH(gsl), NULL);

    return gsl->selected_item;
}

void
gnc_general_search_allow_clear (GNCGeneralSearch *gsl, gboolean allow_clear)
{
    g_return_if_fail (GNC_IS_GENERAL_SEARCH (gsl));
    gsl->allow_clear = allow_clear;
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
