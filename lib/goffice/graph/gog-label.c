/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-label.c
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-label.h>
#include <goffice/graph/gog-outlined-object.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-theme.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-renderer.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/go-data.h>

// #include <src/gui-util.h>
#include <gui-util.h>
#include <glib/gi18n.h>

#include <gsf/gsf-impl-utils.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkalignment.h>

struct _GogLabel {
	GogOutlinedObject	base;

	GogDatasetElement text;
	gboolean	  allow_markup;
};
typedef GogStyledObjectClass GogLabelClass;

enum {
	LABEL_PROP_0,
	LABEL_PROP_ALLOW_MARKUP,
};

static GType gog_label_view_get_type (void);
static GObjectClass *label_parent_klass;
static GogViewClass *lview_parent_klass;

static void
gog_label_set_property (GObject *obj, guint param_id,
			     GValue const *value, GParamSpec *pspec)
{
	GogLabel *label = GOG_LABEL (obj);

	switch (param_id) {
	case LABEL_PROP_ALLOW_MARKUP :
		label->allow_markup = g_value_get_boolean (value);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 return; /* NOTE : RETURN */
	}
	gog_object_emit_changed (GOG_OBJECT (obj), FALSE);
}

static void
gog_label_get_property (GObject *obj, guint param_id,
			     GValue *value, GParamSpec *pspec)
{
	GogLabel *label = GOG_LABEL (obj);

	switch (param_id) {
	case LABEL_PROP_ALLOW_MARKUP :
		g_value_set_boolean (value, label->allow_markup);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_label_finalize (GObject *obj)
{
	gog_dataset_finalize (GOG_DATASET (obj));
	(*label_parent_klass->finalize) (obj);
}

static gpointer
gog_label_editor (GogObject *gobj, GogDataAllocator *dalloc, GnmCmdContext *cc)
{
	static guint label_pref_page = 0;
	GtkWidget *notebook = gtk_notebook_new ();
	GtkWidget *hbox = gtk_hbox_new (FALSE, 12);
	GtkWidget *alignment = gtk_alignment_new (0, 0, 1, 0);

	gtk_container_set_border_width (GTK_CONTAINER (alignment), 12);
	gtk_box_pack_start (GTK_BOX (hbox), 
		gtk_label_new_with_mnemonic (_("_Text:")), FALSE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (hbox), 
		gog_data_allocator_editor (dalloc, GOG_DATASET (gobj), 0, GOG_DATA_SCALAR),
		TRUE, TRUE, 0);
	gtk_container_add (GTK_CONTAINER (alignment), hbox);
	gtk_widget_show_all (alignment);
	gtk_notebook_prepend_page (GTK_NOTEBOOK (notebook), alignment,
		gtk_label_new (_("Data")));
	gog_styled_object_editor (GOG_STYLED_OBJECT (gobj), cc, notebook);
	gog_style_handle_notebook (notebook, &label_pref_page);
	return notebook;
}

static void
gog_label_init_style (GogStyledObject *gso, GogStyle *style)
{
	style->interesting_fields = GOG_STYLE_OUTLINE | GOG_STYLE_FILL | GOG_STYLE_FONT;
	gog_theme_fillin_style (gog_object_get_theme (GOG_OBJECT (gso)),
		style, GOG_OBJECT (gso), 0, FALSE);
}

static void
gog_label_class_init (GogLabelClass *klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) klass;
	GogObjectClass *gog_klass = (GogObjectClass *) klass;
	GogStyledObjectClass *style_klass = (GogStyledObjectClass *) klass;

	label_parent_klass = g_type_class_peek_parent (klass);
	gobject_klass->set_property = gog_label_set_property;
	gobject_klass->get_property = gog_label_get_property;
	gobject_klass->finalize	    = gog_label_finalize;
	g_object_class_install_property (gobject_klass, LABEL_PROP_ALLOW_MARKUP,
		g_param_spec_boolean ("allow-markup", "allow-markup",
			"Support basic html-ish markup",
			TRUE, G_PARAM_READWRITE|GOG_PARAM_PERSISTENT));

	gog_klass->editor	= gog_label_editor;
	gog_klass->view_type	= gog_label_view_get_type ();
	style_klass->init_style = gog_label_init_style;
}

static void
gog_label_dims (GogDataset const *set, int *first, int *last)
{
	*first = *last = 0;
}

static GogDatasetElement *
gog_label_get_elem (GogDataset const *set, int dim_i)
{
	GogLabel *label = GOG_LABEL (set);
	return &label->text;
}

static void
gog_label_dim_changed (GogDataset *set, int dim_i)
{
	gog_object_emit_changed (GOG_OBJECT (set), TRUE);
}

static void
gog_label_dataset_init (GogDatasetClass *iface)
{
	iface->dims	   = gog_label_dims;
	iface->get_elem	   = gog_label_get_elem;
	iface->dim_changed = gog_label_dim_changed;
}

GSF_CLASS_FULL (GogLabel, gog_label,
		gog_label_class_init, NULL,
		GOG_OUTLINED_OBJECT_TYPE, 0,
		GSF_INTERFACE (gog_label_dataset_init, GOG_DATASET_TYPE))

/************************************************************************/

typedef GogOutlinedView		GogLabelView;
typedef GogOutlinedViewClass	GogLabelViewClass;

#define GOG_LABEL_VIEW_TYPE	(gog_label_view_get_type ())
#define GOG_LABEL_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_LABEL_VIEW_TYPE, GogLabelView))
#define IS_GOG_LABEL_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_LABEL_VIEW_TYPE))

static void
gog_label_view_size_request (GogView *v, GogViewRequisition *req)
{
	GogLabel *l = GOG_LABEL (v->model);

	req->w = req->h = 0.;
	if (l->text.data != NULL) {
		char const *text = go_data_scalar_get_str (GO_DATA_SCALAR (l->text.data));
		if (text != NULL) {
			gog_renderer_push_style (v->renderer, l->base.base.style);
			gog_renderer_measure_text (v->renderer, text, req);
			gog_renderer_pop_style (v->renderer);
		}
	}
	lview_parent_klass->size_request (v, req);
}

static void
gog_label_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GogLabel *l = GOG_LABEL (view->model);
	GogOutlinedObject *goo = GOG_OUTLINED_OBJECT (view->model);
	GogStyle *style = l->base.base.style;

	gog_renderer_push_style (view->renderer, style);
	if (l->text.data != NULL) {
		char const *text = go_data_scalar_get_str (GO_DATA_SCALAR (l->text.data));
		if (text != NULL) {
			double outline = gog_renderer_line_size (
				view->renderer, goo->base.style->outline.width);
			if (style->fill.type != GOG_FILL_STYLE_NONE || outline > 0.) {
				GogViewRequisition req;
				GogViewAllocation rect;
				double pad_x = gog_renderer_pt2r_x (view->renderer, goo->padding_pts);
				double pad_y = gog_renderer_pt2r_y (view->renderer, goo->padding_pts);
				
				gog_renderer_measure_text (view->renderer, text, &req);
				rect = view->allocation;
				rect.w = req.w + 2. * outline + pad_x;
				rect.h = req.h + 2. * outline + pad_y;
				gog_renderer_draw_sharp_rectangle (view->renderer, &rect, NULL);
			}
			gog_renderer_draw_text (view->renderer, text,
						&view->residual, GTK_ANCHOR_NW, NULL);
		}
	}
	gog_renderer_pop_style (view->renderer);
}

static void
gog_label_view_class_init (GogLabelViewClass *gview_klass)
{
	GogViewClass *view_klass    = (GogViewClass *) gview_klass;

	lview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->size_request    = gog_label_view_size_request;
	view_klass->render	    = gog_label_view_render;
}

static GSF_CLASS (GogLabelView, gog_label_view,
	   gog_label_view_class_init, NULL,
	   GOG_OUTLINED_VIEW_TYPE)
