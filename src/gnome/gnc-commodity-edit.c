/*
 * gnc-commodity-edit.c --  Commodity editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

/*
 * Commodity editor widget
 *
 * Authors: Dave Peticolas <dave@krondo.com>
 */

#include <config.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-commodity-edit.h"
#include "dialog-commodity.h"
#include "guile-util.h"
#include "messages.h"


static void gnc_commodity_edit_init         (GNCCommodityEdit      *gce);
static void gnc_commodity_edit_class_init   (GNCCommodityEditClass *class);
static void gnc_commodity_edit_destroy      (GtkObject             *object);

static GtkHBoxClass *parent_class;

/**
 * gnc_commodity_edit_get_type:
 *
 * Returns the GtkType for the GNCCommodityEdit widget
 */
guint
gnc_commodity_edit_get_type (void)
{
	static guint commodity_edit_type = 0;

	if (!commodity_edit_type){
		GtkTypeInfo commodity_edit_info = {
			"GNCCommodityEdit",
			sizeof (GNCCommodityEdit),
			sizeof (GNCCommodityEditClass),
			(GtkClassInitFunc) gnc_commodity_edit_class_init,
			(GtkObjectInitFunc) gnc_commodity_edit_init,
			NULL,
			NULL,
		};

		commodity_edit_type = gtk_type_unique (gtk_hbox_get_type (),
                                                       &commodity_edit_info);
	}

	return commodity_edit_type;
}

static void
gnc_commodity_edit_forall (GtkContainer *container, gboolean include_internals,
                           GtkCallback callback, gpointer callback_data)
{
	g_return_if_fail (container != NULL);
	g_return_if_fail (GNC_IS_COMMODITY_EDIT (container));
	g_return_if_fail (callback != NULL);

	/* Let GtkBox handle things only if the internal widgets need
	 * to be poked. */
        if (!include_internals)
                return;

        if (!GTK_CONTAINER_CLASS (parent_class)->forall)
                return;

        GTK_CONTAINER_CLASS (parent_class)->forall (container,
                                                    include_internals,
                                                    callback,
                                                    callback_data);
}

static void
gnc_commodity_edit_class_init (GNCCommodityEditClass *class)
{
	GtkObjectClass *object_class = (GtkObjectClass *) class;
	GtkContainerClass *container_class = (GtkContainerClass *) class;

	object_class = (GtkObjectClass*) class;

	parent_class = gtk_type_class (gtk_hbox_get_type ());

	container_class->forall = gnc_commodity_edit_forall;

	object_class->destroy = gnc_commodity_edit_destroy;
}

static void
gnc_commodity_edit_init (GNCCommodityEdit *gce)
{
        gce->selected_commodity = NULL;
}

static void
gnc_commodity_edit_destroy (GtkObject *object)
{
        GNCCommodityEdit *gce;

	g_return_if_fail (object != NULL);
	g_return_if_fail (GNC_IS_COMMODITY_EDIT (object));

        gce = GNC_COMMODITY_EDIT (object);

        gce->entry = NULL;
        gce->button = NULL;

	if (GTK_OBJECT_CLASS (parent_class)->destroy)
		GTK_OBJECT_CLASS (parent_class)->destroy (object);
}

static void
select_currency_cb(GtkButton * button, gpointer user_data)
{
        GNCCommodityEdit *gce = user_data;
        gnc_commodity *new_commodity;
        GtkWidget *toplevel;

        toplevel = gtk_widget_get_toplevel (GTK_WIDGET (button));

        new_commodity = gnc_ui_select_commodity_modal(gce->selected_commodity,
                                                      toplevel);

        /* NULL return means cancel; no change */
        if (new_commodity == NULL)
                return;

        gnc_commodity_edit_set_commodity (gce, new_commodity);
}

static void
create_children (GNCCommodityEdit *gce)
{
        gce->entry = gtk_entry_new ();
        gtk_entry_set_editable (GTK_ENTRY (gce->entry), FALSE);
	gtk_box_pack_start (GTK_BOX (gce), gce->entry, TRUE, TRUE, 0);
        gtk_widget_show (gce->entry);

        gce->button = gtk_button_new_with_label (_("Select..."));
	gtk_box_pack_start (GTK_BOX (gce), gce->button, FALSE, FALSE, 0);
        gtk_signal_connect (GTK_OBJECT (gce->button), "clicked",
                            select_currency_cb, gce);
        gtk_widget_show (gce->button);
}

/**
 * gnc_commodity_edit_new:
 *
 * Creates a new GNCCommodityEdit widget which can be used to provide
 * an easy way to enter ISO commodity codes.
 *
 * Returns a GNCCommodityEdit widget.
 */
GtkWidget *
gnc_commodity_edit_new (void)
{
	GNCCommodityEdit *gce;

	gce = gtk_type_new (gnc_commodity_edit_get_type ());

	create_children (gce);

	return GTK_WIDGET (gce);
}

/**
 * gnc_commodity_edit_set_commodity:
 * @gce: the commodity editor widget
 * @commodity: the commodity code to select
 *
 * Sets the commodity value of the widget to a particular commodity.
 *
 * Returns nothing.
 */
void
gnc_commodity_edit_set_commodity (GNCCommodityEdit *gce,
                                  gnc_commodity *commodity)
{
        const char *text;

        g_return_if_fail(gce != NULL);
        g_return_if_fail(GNC_IS_COMMODITY_EDIT(gce));

        gce->selected_commodity = commodity;

        if (commodity == NULL)
                text = "";
        else
                text = gnc_commodity_get_printname(commodity);

        gtk_entry_set_text(GTK_ENTRY(gce->entry), text);
}

/**
 * gnc_commodity_edit_get_commodity:
 * @gce: the commodity editor widget
 *
 * Returns the commodity currently selected by the widget.
 */
gnc_commodity *
gnc_commodity_edit_get_commodity (GNCCommodityEdit *gce)
{
        g_return_val_if_fail(gce != NULL, NULL);
        g_return_val_if_fail(GNC_IS_COMMODITY_EDIT(gce), NULL);

        return gce->selected_commodity;
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
