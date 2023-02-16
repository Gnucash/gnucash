/********************************************************************\
 * gnc-report-combo.c -- report select widget for GnuCash           *
 *                                                                  *
 * Copyright (C) 2022 Bob Fewell                                    *
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
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-report-combo.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"
#include "dialog-utils.h"

/** The debugging module used by this file. */
__attribute__((unused)) static QofLogModule log_module = GNC_MOD_GUI;

static void gnc_report_combo_init       (GncReportCombo      *grc);
static void gnc_report_combo_class_init (GncReportComboClass *klass);
static void gnc_report_combo_dispose    (GObject *object);
static void gnc_report_combo_finalize   (GObject *object);

#define GNC_REPORT_COMBO_PATH "gnc-report-combo-path"

enum
{
    RC_NAME,
    RC_GUID,
    RC_MISSING
};

/** The instance private data for a content plugin. */
typedef struct _GncReportComboPrivate
{
    GtkWidget   *combo;
    GtkWidget   *warning_image;

    const gchar *rpt_guids;

    gboolean     block_signal;
    gboolean     popup_shown;

    gchar       *active_report_guid;
    gchar       *active_report_name;

} GncReportComboPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncReportCombo, gnc_report_combo, GTK_TYPE_BOX)

#define GET_PRIVATE(o)  \
   ((GncReportComboPrivate*)g_type_instance_get_private ((GTypeInstance*)o, GNC_TYPE_REPORT_COMBO))

enum
{
    SIGNAL_0,
    CHANGED,
    LAST_SIGNAL
};

static guint report_combo_signals [LAST_SIGNAL] = {0};

enum
{
    PROP_0,
    PROP_POPUP_SHOWN,
    N_PROPERTIES
};

static GParamSpec *report_combo_properties [N_PROPERTIES] = {NULL,};

static void
gnc_report_combo_get_property (GObject    *object,
                               guint       property_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
    GncReportCombo        *grc = GNC_REPORT_COMBO(object);
    GncReportComboPrivate *priv = GET_PRIVATE(grc);

    switch (property_id)
    {
    case PROP_POPUP_SHOWN:
        g_value_set_boolean (value, priv->popup_shown);
        break;

    default:
        /* We don't have any other property... */
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
        break;
    }
}

/** Initialize the GncReportCombo class object.
 *
 *  @internal
 *
 *  @param klass A pointer to the newly created class object.
 */
static void
gnc_report_combo_class_init (GncReportComboClass *klass)
{
    GObjectClass   *object_class = G_OBJECT_CLASS(klass);

    object_class->get_property = gnc_report_combo_get_property;
    object_class->dispose  = gnc_report_combo_dispose;
    object_class->finalize = gnc_report_combo_finalize;

    report_combo_signals [CHANGED] =
        g_signal_new ("changed",
                      G_OBJECT_CLASS_TYPE(object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET(GncReportComboClass, changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    report_combo_properties [PROP_POPUP_SHOWN] =
        g_param_spec_boolean ("popup-shown",
                              "State of PopUp",
                              "State of PopUp",
                              FALSE /* default value */,
                              G_PARAM_READABLE);

    g_object_class_install_properties (object_class,
                                       N_PROPERTIES,
                                       report_combo_properties);
}

/** Initialize a GncReportCombo object.
 *
 *  @internal
 *
 *  @param grc A pointer to the newly created object.
 */
static void
gnc_report_combo_init (GncReportCombo *grc)
{
    GncReportComboPrivate *priv;

    g_return_if_fail (grc != NULL);
    g_return_if_fail (GNC_IS_REPORT_COMBO(grc));

    priv = GET_PRIVATE(grc);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(grc), "gnc-id-report-combo");

    priv->block_signal = FALSE;
    priv->active_report_guid = NULL;
    priv->active_report_name = NULL;
    priv->popup_shown = FALSE;
}

/** Dispopse the GncReportCombo object. This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_report_combo_dispose (GObject *object)
{
    /* Do not free the private data structure itself. It is part of
     * a larger memory block allocated by the type system. */

    G_OBJECT_CLASS (gnc_report_combo_parent_class)->dispose (object);
}

/** Finalize the GncReportCombo object.  This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's finalize function.
 *
 *  @param object The object being finalized.
 *
 *  @internal
 */
static void
gnc_report_combo_finalize (GObject *object)
{
    GncReportComboPrivate *priv;
    GncReportCombo *grc;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_REPORT_COMBO(object));

    grc = GNC_REPORT_COMBO(object);
    priv = GET_PRIVATE(grc);

    g_free (priv->active_report_guid);
    g_free (priv->active_report_name);

    G_OBJECT_CLASS (gnc_report_combo_parent_class)->finalize (object);
}

/** This function sets the active combo entry based on the private
 *  report guid and also checks to see if the report guid is in the
 *  list of reports, if not a warning image and tooltip is shown.
 *
 *  @internal
 *
 *  @param grc The report combo.
 *
 *  @return TRUE if report guid is in the list, other wise FALSE.
 */
static gboolean
select_active_and_check_exists (GncReportCombo *grc)
{
    GncReportComboPrivate *priv = GET_PRIVATE(grc);
    GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(priv->combo));
    GtkTreeIter iter;
    gboolean valid_iter = gtk_tree_model_get_iter_first (model, &iter);
    gchar *tmp;

    while (valid_iter)
    {
        gchar *guid;
        gtk_tree_model_get (model, &iter, RC_GUID, &guid, -1);

        if (g_strcmp0 (priv->active_report_guid, guid) == 0)
        {
            gtk_combo_box_set_active_iter (GTK_COMBO_BOX(priv->combo), &iter);
            g_free (guid);
            return TRUE;
        }
        g_free (guid);
        valid_iter = gtk_tree_model_iter_next (model, &iter);
    }

    if (priv->active_report_name)
        tmp = g_strdup (priv->active_report_name);
    else
        tmp = g_strdup (_("Selected Report is Missing"));

    gtk_list_store_prepend (GTK_LIST_STORE(model), &iter);
    gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                        RC_NAME, tmp,
                        RC_GUID, priv->active_report_guid,
                        RC_MISSING, TRUE,
                        -1);

    g_free (tmp);
    gtk_combo_box_set_active_iter (GTK_COMBO_BOX(priv->combo), &iter);
    return FALSE;
}

static void
update_report_list (GncReportCombo *grc, GSList *report_list)
{
    GncReportComboPrivate *priv = GET_PRIVATE(grc);
    GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(priv->combo));

    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(model),
                                          RC_NAME, GTK_SORT_ASCENDING);

    gtk_list_store_clear (GTK_LIST_STORE(model));

    if (report_list)
    {
        GtkTreeIter iter;

        for (GSList* node = report_list; node != NULL; node = g_slist_next (node))
        {
            ReportListEntry *rle = node->data;

            gtk_list_store_append (GTK_LIST_STORE(model), &iter);
            gtk_list_store_set (GTK_LIST_STORE(model), &iter,
                                RC_NAME, rle->report_name,
                                RC_GUID, rle->report_guid,
                                RC_MISSING, FALSE,
                                -1);
            g_free (rle->report_name);
            g_free (rle->report_guid);
            g_free (rle);
        }
    }
    g_slist_free (report_list);
}

static void
update_warning_tooltip (GncReportCombo *grc)
{
    GncReportComboPrivate *priv = GET_PRIVATE(grc);
    gchar *tool_tip;

    if (priv->active_report_name)
        /* Translators: %s is the report name. */
        tool_tip = g_strdup_printf (_("'%s' is missing"),
                                      priv->active_report_name);
    else
        /* Translators: %s is the internal report guid. */
        tool_tip = g_strdup_printf (_("Report with GUID '%s' is missing"),
                                       priv->active_report_guid);

    gtk_widget_show (priv->warning_image);
    gtk_widget_set_tooltip_text (priv->warning_image, tool_tip);
    g_free (tool_tip);
}

void
gnc_report_combo_set_active (GncReportCombo *grc,
                             const char* active_report_guid,
                             const char* active_report_name)
{
    GncReportComboPrivate *priv;

    g_return_if_fail (grc != NULL);
    g_return_if_fail (GNC_IS_REPORT_COMBO(grc));

    priv = GET_PRIVATE(grc);

    g_free (priv->active_report_guid);

    priv->active_report_guid = g_strdup (active_report_guid);

    g_free (priv->active_report_name);

    priv->active_report_name = g_strdup (active_report_name);

    priv->block_signal = TRUE;

    if (!select_active_and_check_exists (grc))
        update_warning_tooltip (grc);

    priv->block_signal = FALSE;
}

gchar *
gnc_report_combo_get_active_guid (GncReportCombo *grc)
{
    GncReportComboPrivate *priv;
    gchar *guid = NULL;
    GtkTreeIter iter;

    g_return_val_if_fail (grc != NULL, NULL);
    g_return_val_if_fail (GNC_IS_REPORT_COMBO(grc), NULL);

    priv = GET_PRIVATE(grc);

    if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX(priv->combo), &iter))
    {
        GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(priv->combo));
        gtk_tree_model_get (model, &iter, RC_GUID, &guid, -1);
    }
    return guid;
}

gchar *
gnc_report_combo_get_active_name (GncReportCombo *grc)
{
    GncReportComboPrivate *priv;
    gchar *name = NULL;
    GtkTreeIter iter;

    g_return_val_if_fail (grc != NULL, NULL);
    g_return_val_if_fail (GNC_IS_REPORT_COMBO(grc), NULL);

    priv = GET_PRIVATE(grc);

    if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX(priv->combo), &iter))
    {
        GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(priv->combo));
        gtk_tree_model_get (model, &iter, RC_NAME, &name, -1);
    }
    return name;
}

gchar*
gnc_report_combo_get_active_guid_name (GncReportCombo *grc)
{
    GncReportComboPrivate *priv;
    gchar *report = NULL;
    GtkTreeIter iter;

    g_return_val_if_fail (grc != NULL, NULL);
    g_return_val_if_fail (GNC_IS_REPORT_COMBO(grc), NULL);

    priv = GET_PRIVATE(grc);

    if (gtk_combo_box_get_active_iter (GTK_COMBO_BOX(priv->combo), &iter))
    {
        GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX(priv->combo));
        gchar *report_guid;
        gchar *report_name;
        gtk_tree_model_get (model, &iter, RC_NAME, &report_name,
                                          RC_GUID, &report_guid,
                                          -1);

        report = g_strconcat (report_guid, "/", report_name, NULL);
        g_free (report_guid);
        g_free (report_name);
    }
    return report;
}

void
gnc_report_combo_set_active_guid_name (GncReportCombo *grc,
                                       const gchar *guid_name)
{
    g_return_if_fail (grc != NULL);
    g_return_if_fail (GNC_IS_REPORT_COMBO(grc));

    if (guid_name && *guid_name)
    {
        gchar *guid = NULL;
        gchar *name = g_strstr_len (guid_name, -1, "/");

        if (name)
        {
            guid = g_strndup (guid_name, (name - guid_name));
            gnc_report_combo_set_active (grc, guid, name + 1);
        }
        g_free (guid);
    }
}

gboolean
gnc_report_combo_is_warning_visible_for_active (GncReportCombo *grc)
{
    GncReportComboPrivate *priv;

    g_return_val_if_fail (grc != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_REPORT_COMBO(grc), FALSE);

    priv = GET_PRIVATE(grc);

    return gtk_widget_is_visible (GTK_WIDGET(priv->warning_image));
}

static void
combo_changed_cb (GtkComboBox *widget, gpointer user_data)
{
    GncReportCombo        *grc = GNC_REPORT_COMBO(user_data);
    GncReportComboPrivate *priv = GET_PRIVATE(grc);
    GtkTreeIter            iter;

    if (gtk_combo_box_get_active_iter (widget, &iter))
    {
        GtkTreeModel *model = gtk_combo_box_get_model (widget);
        gboolean missing;
        gtk_tree_model_get (model, &iter, RC_MISSING, &missing, -1);
        // set visibility of the warning image
        gtk_widget_set_visible (priv->warning_image, missing);

        if (!priv->block_signal)
            g_signal_emit (grc, report_combo_signals [CHANGED], 0);

        gtk_widget_queue_resize (GTK_WIDGET(widget));
    }
}

static void
combo_popped_cb (GObject    *gobject,
                 GParamSpec *pspec,
                 gpointer    user_data)
{
    GncReportCombo        *grc = GNC_REPORT_COMBO(user_data);
    GncReportComboPrivate *priv = GET_PRIVATE(grc);
    gboolean popup_shown;

    g_object_get (G_OBJECT(gobject), "popup-shown", &popup_shown, NULL);

    priv->popup_shown = popup_shown;
    g_object_notify (G_OBJECT(grc), "popup-shown");
}

void
gnc_report_combo_refresh (GncReportCombo *grc, GSList *report_list)
{
    GncReportComboPrivate *priv;

    g_return_if_fail (grc != NULL);
    g_return_if_fail (GNC_IS_REPORT_COMBO(grc));
    g_return_if_fail (report_list != NULL);

    priv = GET_PRIVATE(grc);

    priv->block_signal = TRUE;

    update_report_list (grc, report_list);

    if (!select_active_and_check_exists (grc))
        update_warning_tooltip (grc);

    priv->block_signal = FALSE;
}

/*  Create a new GncReportCombo widget which can be used to select
 *  a report from a GtkComboBox.
 *
 *  @return A GncReportCombo widget.
 */
GtkWidget *
gnc_report_combo_new (GSList *report_list)
{
    GncReportCombo *grc;
    GncReportComboPrivate *priv;
    GtkListStore *store;
    GtkCellRenderer *renderer;

    store = gtk_list_store_new (3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_BOOLEAN);
    grc = g_object_new (GNC_TYPE_REPORT_COMBO, NULL);

    priv = GET_PRIVATE(grc);

    priv->combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL(store));
    g_object_unref (store);

    renderer = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(priv->combo), renderer, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT(priv->combo), renderer,
                                    "text", RC_NAME, NULL);

    gtk_box_pack_start (GTK_BOX(grc), GTK_WIDGET(priv->combo), TRUE, TRUE, 0);
    priv->warning_image = gtk_image_new_from_icon_name ("dialog-warning",
                                                        GTK_ICON_SIZE_SMALL_TOOLBAR);
    gtk_box_pack_start (GTK_BOX(grc), GTK_WIDGET(priv->warning_image), FALSE, FALSE, 6);
    gtk_widget_set_no_show_all (GTK_WIDGET(priv->warning_image), TRUE);
    gtk_widget_hide (GTK_WIDGET(priv->warning_image));

    update_report_list (grc, report_list);

    g_signal_connect (G_OBJECT(priv->combo), "changed",
                      G_CALLBACK(combo_changed_cb), grc);

    g_signal_connect (G_OBJECT(priv->combo), "notify::popup-shown",
                      G_CALLBACK(combo_popped_cb), grc);

    gtk_widget_show_all (GTK_WIDGET(grc));

    return GTK_WIDGET(grc);
}
