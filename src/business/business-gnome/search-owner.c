/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#include "QueryCore.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "gncOwner.h"

#include "business-gnome-utils.h"
#include "search-owner.h"
#include "search-core-utils.h"

#define d(x)

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_owner_class_init	(GNCSearchOwnerClass *class);
static void gnc_search_owner_init	(GNCSearchOwner *gspaper);
static void gnc_search_owner_finalize	(GObject *obj);

#define _PRIVATE(x) (((GNCSearchOwner *)(x))->priv)

typedef struct _GNCSearchOwnerPrivate
{
    GncOwner	owner;
    GtkWidget *	owner_box;
    GtkWidget *	owner_choice;
} GNCSearchOwnerPrivate;

#define GNC_SEARCH_OWNER_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_OWNER, GNCSearchOwnerPrivate))

static GNCSearchCoreTypeClass *parent_class;

enum
{
    LAST_SIGNAL
};

#if LAST_SIGNAL > 0
static guint signals[LAST_SIGNAL] = { 0 };
#endif

GType
gnc_search_owner_get_type (void)
{
    static GType type = 0;

    if (!type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSearchOwnerClass),    /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_search_owner_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSearchOwner),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_search_owner_init,
        };

        type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
                                       "GNCSearchOwner",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_owner_class_init (GNCSearchOwnerClass *class)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

    object_class = G_OBJECT_CLASS (class);
    parent_class = gtk_type_class(gnc_search_core_type_get_type ());

    object_class->finalize = gnc_search_owner_finalize;

    /* override methods */
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;

    g_type_class_add_private(class, sizeof(GNCSearchOwnerPrivate));
}

static void
gnc_search_owner_init (GNCSearchOwner *o)
{
}

static void
gnc_search_owner_finalize (GObject *obj)
{
    GNCSearchOwner *o;
    GNCSearchOwnerPrivate *priv;

    g_assert (IS_GNCSEARCH_OWNER (obj));

    o = GNCSEARCH_OWNER(obj);
    priv = GNC_SEARCH_OWNER_GET_PRIVATE(o);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
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
    GNCSearchOwnerPrivate *priv;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_OWNER (fi), FALSE);

    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fi);
    if (priv->owner.owner.undefined == NULL)
    {
        valid = FALSE;
        gnc_error_dialog (NULL, "%s", _("You have not selected an owner"));
    }

    /* XXX */

    return valid;
}

static int
owner_changed_cb (GtkWidget *widget, gpointer data)
{
    GNCSearchOwner *fe = data;
    GNCSearchOwnerPrivate *priv;

    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fe);
    gnc_owner_get_owner (priv->owner_choice, &(priv->owner));
    return FALSE;
}

static void
set_owner_widget (GNCSearchOwner *fe)
{
    GNCSearchOwnerPrivate *priv;

    /* Remove the old choice widget */
    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fe);
    if (priv->owner_choice)
        gtk_container_remove (GTK_CONTAINER (priv->owner_box), priv->owner_choice);

    /* Create a new choice widget */
    priv->owner_choice =
        gnc_owner_select_create (NULL, priv->owner_box,
                                 gnc_get_current_book(), &(priv->owner));

    /* Setup the "changed" callback */
    g_signal_connect (G_OBJECT (priv->owner_choice), "changed",
                      G_CALLBACK (owner_changed_cb), fe);

    gtk_widget_show_all (priv->owner_choice);
}

static void
type_combo_changed (GtkWidget *widget, GNCSearchOwner *fe)
{
    GNCSearchOwnerPrivate *priv;
    GncOwnerType type;

    g_return_if_fail(GTK_IS_COMBO_BOX(widget));

    type = gnc_combo_box_search_get_active(GTK_COMBO_BOX(widget));

    /* If the type changed or if we don't have a type create the owner_choice */
    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fe);
    if (type != gncOwnerGetType (&(priv->owner)))
    {
        priv->owner.type = type;
        priv->owner.owner.undefined = NULL;
        set_owner_widget (fe);
    }
    else if (priv->owner_choice == NULL)
        set_owner_widget (fe);
}

static GtkWidget *
make_type_menu (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GNCSearchOwnerPrivate *priv;
    GtkComboBox *combo;
    GncOwnerType type;

    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fi);
    type = gncOwnerGetType (&(priv->owner));

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("Customer"), GNC_OWNER_CUSTOMER);
    gnc_combo_box_search_add(combo, _("Vendor"), GNC_OWNER_VENDOR);
    gnc_combo_box_search_add(combo, _("Employee"), GNC_OWNER_EMPLOYEE);
    gnc_combo_box_search_add(combo, _("Job"), GNC_OWNER_JOB);

    g_signal_connect (combo, "changed", G_CALLBACK (type_combo_changed), fe);
    gnc_combo_box_search_set_active(combo, type);

    return GTK_WIDGET(combo);


}

static GtkWidget *
make_how_menu (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());
    gnc_combo_box_search_add(combo, _("is"), GUID_MATCH_ANY);
    gnc_combo_box_search_add(combo, _("is not"), GUID_MATCH_NONE);
    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : GUID_MATCH_ANY);

    return GTK_WIDGET(combo);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *how_menu, *type_menu, *box;
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GNCSearchOwnerPrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_OWNER (fi), NULL);

    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fi);
    box = gtk_hbox_new (FALSE, 3);

    /* Build and connect the "how" option menu. */
    how_menu = make_how_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), how_menu, FALSE, FALSE, 3);

    /* Create the owner box */
    priv->owner_box = gtk_hbox_new (FALSE, 0);

    /* Build and connect the "type" option menu.
     * Note that this will build the owner_choice and
     * put it in the owner_box we just created.
     */
    type_menu = make_type_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), type_menu, FALSE, FALSE, 3);

    /* connect the owner box */
    gtk_box_pack_start (GTK_BOX (box), priv->owner_box, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchOwner *fi = (GNCSearchOwner *)fe;
    GNCSearchOwnerPrivate *priv;
    const GncGUID *guid;
    GList *l = NULL;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_OWNER (fi), NULL);

    priv = GNC_SEARCH_OWNER_GET_PRIVATE(fi);
    guid = gncOwnerGetGUID (&(priv->owner));
    l = g_list_prepend (l, (gpointer)guid);

    return gncQueryGUIDPredicate (fi->how, l);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchOwner *se, *fse = (GNCSearchOwner *)fe;
    GNCSearchOwnerPrivate *se_priv, *fse_priv;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_OWNER (fse), NULL);

    se = gnc_search_owner_new ();
    se->how = fse->how;
    se_priv = GNC_SEARCH_OWNER_GET_PRIVATE(se);
    fse_priv = GNC_SEARCH_OWNER_GET_PRIVATE(fse);
    gncOwnerCopy (&(fse_priv->owner), &(se_priv->owner));

    return (GNCSearchCoreType *)se;
}
