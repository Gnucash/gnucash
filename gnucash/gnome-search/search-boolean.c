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

#include "search-boolean.h"
#include "search-core-utils.h"

#define d(x)

static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_boolean_class_init	(GNCSearchBooleanClass *klass);
static void gnc_search_boolean_init	(GNCSearchBoolean *gspaper);
static void gnc_search_boolean_finalize	(GObject *obj);

typedef struct _GNCSearchBooleanPrivate GNCSearchBooleanPrivate;

struct _GNCSearchBooleanPrivate
{
    GtkWindow *parent;
    gpointer dummy;
};

G_DEFINE_TYPE_WITH_PRIVATE(GNCSearchBoolean, gnc_search_boolean, GNC_TYPE_SEARCH_CORE_TYPE)

#define _PRIVATE(o) \
   ((GNCSearchBooleanPrivate*)g_type_instance_get_private((GTypeInstance*)o, GNC_TYPE_SEARCH_BOOLEAN))

static GNCSearchCoreTypeClass *parent_class;

static void
gnc_search_boolean_class_init (GNCSearchBooleanClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_boolean_finalize;

    /* override methods */
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_boolean_init (GNCSearchBoolean *o)
{
    o->value = TRUE;
}

static void
gnc_search_boolean_finalize (GObject *obj)
{
    GNCSearchBoolean *o = (GNCSearchBoolean *)obj;
    g_assert (IS_GNCSEARCH_BOOLEAN (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_boolean_new:
 *
 * Create a new GNCSearchBoolean object.
 *
 * Return value: A new #GNCSearchBoolean object.
 **/
GNCSearchBoolean *
gnc_search_boolean_new (void)
{
    GNCSearchBoolean *o = g_object_new(GNC_TYPE_SEARCH_BOOLEAN, NULL);
    return o;
}

void
gnc_search_boolean_set_value (GNCSearchBoolean *fi, gboolean value)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_BOOLEAN (fi));

    fi->value = value;
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;
    GNCSearchBooleanPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_BOOLEAN (fi));

    priv = _PRIVATE(fi);
    priv->parent = GTK_WINDOW(parent);
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fi), FALSE);

    /* XXX */

    return valid;
}

static void
toggle_changed (GtkToggleButton *button, GNCSearchBoolean *fe)
{
    fe->value = gtk_toggle_button_get_active (button);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *toggle, *box;
    GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the toggle */
    toggle = gtk_check_button_new ();
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (toggle), fi->value);
    g_signal_connect (G_OBJECT (toggle), "toggled", G_CALLBACK (toggle_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchBoolean *fi = (GNCSearchBoolean *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fi), NULL);

    return qof_query_boolean_predicate (QOF_COMPARE_EQUAL, fi->value);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchBoolean *se, *fse = (GNCSearchBoolean *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_BOOLEAN (fse), NULL);

    se = gnc_search_boolean_new ();
    gnc_search_boolean_set_value (se, fse->value);

    return (GNCSearchCoreType *)se;
}
