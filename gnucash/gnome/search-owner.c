/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "gncOwner.h"

#include "business-gnome-utils.h"
#include "search-owner.h"
#include "search-core-utils.h"
#include "gnc-gtk-utils.h"

#define d(x)

#define OWNER_SEARCH_VALUE "owner-search-value"

static GtkDropDown *
owner_search_dropdown_new (void)
{
    GtkStringList *model = gtk_string_list_new (NULL);
    GtkDropDown *dropdown = gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL);
    return dropdown;
}

static void
owner_search_dropdown_add (GtkDropDown *dropdown, const gchar *label, gint value)
{
    GtkStringList *model = GTK_STRING_LIST (gtk_drop_down_get_model (dropdown));
    guint position = g_list_model_get_n_items (G_LIST_MODEL (model));
    GtkStringObject *item;

    gtk_string_list_append (model, label);
    item = GTK_STRING_OBJECT (g_list_model_get_item (G_LIST_MODEL (model), position));
    g_object_set_data (G_OBJECT (item), OWNER_SEARCH_VALUE, GINT_TO_POINTER (value));
    g_object_unref (item);
}

static gint
owner_search_dropdown_get_active (GtkDropDown *dropdown)
{
    GListModel *model = gtk_drop_down_get_model (dropdown);
    guint position = gtk_drop_down_get_selected (dropdown);
    GObject *item;
    gint value;

    if (!model || position == GTK_INVALID_LIST_POSITION)
        return 0;
    item = g_list_model_get_item (model, position);
    value = GPOINTER_TO_INT (g_object_get_data (item, OWNER_SEARCH_VALUE));
    g_object_unref (item);
    return value;
}

static void
owner_search_dropdown_set_active (GtkDropDown *dropdown, gint value)
{
    GListModel *model = gtk_drop_down_get_model (dropdown);
    guint count = g_list_model_get_n_items (model);

    for (guint i = 0; i < count; i++)
    {
        GObject *item = g_list_model_get_item (model, i);
        gint item_value = GPOINTER_TO_INT (g_object_get_data (item, OWNER_SEARCH_VALUE));
        g_object_unref (item);
        if (item_value == value)
        {
            gtk_drop_down_set_selected (dropdown, i);
            return;
        }
    }
    gtk_drop_down_set_selected (dropdown, GTK_INVALID_LIST_POSITION);
}
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_owner_finalize   (GObject *obj);

struct _GNCSearchOwner
{
    GNCSearchCoreType parent_instance;

    QofGuidMatch    how;

    GncOwner    owner;
    GtkWindow * parent;
    GtkWidget * owner_box;
    GtkWidget * owner_choice;
} GNCSearchOwnerPrivate;

G_DEFINE_TYPE(GNCSearchOwner, gnc_search_owner, GNC_TYPE_SEARCH_CORE_TYPE)

enum
{
    LAST_SIGNAL
};

#if LAST_SIGNAL > 0
static guint signals[LAST_SIGNAL] = { 0 };
#endif

static void
gnc_search_owner_class_init (GNCSearchOwnerClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = gnc_search_owner_finalize;

    /* override methods */
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_owner_init (GNCSearchOwner *o)
{
}

static void
gnc_search_owner_finalize (GObject *obj)
{
    g_assert (GNC_IS_SEARCH_OWNER (obj));

    G_OBJECT_CLASS (gnc_search_owner_parent_class)->finalize(obj);
}

/**
 * gnc_search_owner_new:
 *
 * Create a new GNCSearchOwner object.
 *
 * Return value: A new #GNCSearchOwner object.
 **/
GNCSearchOwner *
gnc_search_owner_new (void)
{
    GNCSearchOwner *o = g_object_new(gnc_search_owner_get_type (), NULL);
    return o;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fi), FALSE);

    if (fi->owner.owner.undefined == NULL)
    {
        valid = FALSE;
        gnc_error_dialog (GTK_WINDOW(fi->parent), "%s", _("You have not selected an owner"));
    }

    /* XXX */

    return valid;
}

static int
owner_changed_cb (GtkWidget *widget, gpointer data)
{
    GNCSearchOwner *fe = data;

    gnc_owner_get_owner (fe->owner_choice, &(fe->owner));
    return FALSE;
}

static void
set_owner_widget (GNCSearchOwner *fe)
{
    /* Remove the old choice widget */
    if (fe->owner_choice)
        gtk_box_remove (GTK_BOX(fe->owner_box), GTK_WIDGET(fe->owner_choice));

    /* Create a new choice widget */
    fe->owner_choice =
        gnc_owner_select_create (NULL, fe->owner_box,
                                 gnc_get_current_book(), &(fe->owner));

    /* Setup the "changed" callback */
    g_signal_connect (G_OBJECT (fe->owner_choice), "changed",
                      G_CALLBACK (owner_changed_cb), fe);

    gtk_widget_set_visible (fe->owner_choice, TRUE);
}

static void
type_combo_changed (GtkDropDown *dropdown, GParamSpec *pspec, GNCSearchOwner *fe)
{
    GncOwnerType type;
    (void)pspec;

    type = (GncOwnerType)owner_search_dropdown_get_active (dropdown);
    if (type != gncOwnerGetType (&(fe->owner)))
    {
        fe->owner.type = type;
        fe->owner.owner.undefined = NULL;
        set_owner_widget (fe);
    }
    else if (fe->owner_choice == NULL)
        set_owner_widget (fe);
}

static GtkWidget *
make_type_menu (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GtkDropDown *dropdown;
    GncOwnerType type = gncOwnerGetType (&(fi->owner));

    dropdown = owner_search_dropdown_new ();
    owner_search_dropdown_add (dropdown, _("Customer"), GNC_OWNER_CUSTOMER);
    owner_search_dropdown_add (dropdown, _("Vendor"), GNC_OWNER_VENDOR);
    owner_search_dropdown_add (dropdown, _("Employee"), GNC_OWNER_EMPLOYEE);
    owner_search_dropdown_add (dropdown, _("Job"), GNC_OWNER_JOB);
    g_signal_connect (dropdown, "notify::selected", G_CALLBACK (type_combo_changed), fe);
    owner_search_dropdown_set_active (dropdown, type);
    return GTK_WIDGET (dropdown);
}

static void
owner_search_how_changed (GtkDropDown *dropdown, GParamSpec *pspec,
                          GNCSearchOwner *fi)
{
    (void)pspec;
    fi->how = (QofGuidMatch)owner_search_dropdown_get_active (dropdown);
}

static GtkWidget *
make_how_menu (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GtkDropDown *dropdown;

    dropdown = owner_search_dropdown_new ();
    owner_search_dropdown_add (dropdown, _("is"), QOF_GUID_MATCH_ANY);
    owner_search_dropdown_add (dropdown, _("is not"), QOF_GUID_MATCH_NONE);
    g_signal_connect (dropdown, "notify::selected", G_CALLBACK (owner_search_how_changed), fi);
    owner_search_dropdown_set_active (dropdown, fi->how ? fi->how : QOF_GUID_MATCH_ANY);

    return GTK_WIDGET(dropdown);
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_OWNER (fi));

    fi->parent = GTK_WINDOW(parent);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *how_menu, *type_menu, *box;
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the "how" option menu. */
    how_menu = make_how_menu (fe);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(how_menu));
    gtk_box_set_spacing (GTK_BOX(box), 3);

    /* Create the owner box */
    fi->owner_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (fi->owner_box), FALSE);

    /* Build and connect the "type" option menu.
     * Note that this will build the owner_choice and
     * put it in the owner_box we just created.
     */
    type_menu = make_type_menu (fe);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(type_menu));
    gtk_box_set_spacing (GTK_BOX(box), 3);
    /* connect the owner box */
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(fi->owner_box));
    gtk_box_set_spacing (GTK_BOX(box), 3);
    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    const GncGUID *guid;
    GList *l = NULL;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fi), NULL);

    guid = gncOwnerGetGUID (&(fi->owner));
    l = g_list_prepend (l, (gpointer)guid);

    return qof_query_guid_predicate (fi->how, l);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchOwner *se, *fse = (GNCSearchOwner *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_OWNER (fse), NULL);

    se = gnc_search_owner_new ();
    se->how = fse->how;
    gncOwnerCopy (&(fse->owner), &(se->owner));

    return (GNCSearchCoreType *)se;
}
