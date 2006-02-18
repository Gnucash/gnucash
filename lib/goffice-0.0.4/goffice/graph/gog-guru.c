/* vim: set sw=8: */

/*
 * go-graph-guru.c:  The Graph guru
 *
 * Copyright (C) 2000-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#include <goffice/goffice-config.h>
#include <goffice/goffice-priv.h>

#include <goffice/graph/gog-guru.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-chart.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-reg-curve.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-plot-engine.h>
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/gog-control-foocanvas.h>
#include <goffice/gtk/goffice-gtk.h>

#include <glib/gi18n.h>

#include <libxml/parser.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-pixbuf.h>
#include <goffice/cut-n-paste/foocanvas/foo-canvas-rect-ellipse.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtknotebook.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkbutton.h>
#include <gtk/gtkentry.h>
#include <gtk/gtktreeselection.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtktreestore.h>
#include <gtk/gtkimage.h>
#include <gtk/gtkimagemenuitem.h>
#include <gtk/gtkframe.h>
#include <gtk/gtkviewport.h>
#include <gtk/gtkvbox.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkcellrendererpixbuf.h>
#include <gtk/gtkstock.h>
#include <gtk/gtkliststore.h>
#include <string.h>

typedef struct _GraphGuruState		GraphGuruState;
typedef struct _GraphGuruTypeSelector	GraphGuruTypeSelector;

struct _GraphGuruState {
	GogGraph    *graph;
	GogChart    *chart;
	GogPlot	    *plot;

	GOCmdContext	 *cc;
	GogDataAllocator *dalloc;
	GClosure         *register_closure;

	/* GUI accessors */
	GladeXML    *gui;
	GtkWidget   *dialog;
	GtkWidget   *button_cancel;
	GtkWidget   *button_navigate;
	GtkWidget   *button_ok;
	GtkNotebook *steps;
	GtkWidget   *add_menu, *delete_button;

	FooCanvasItem	  *sample_graph_item;

	GtkContainer  	  *prop_container;
	GtkTreeSelection  *prop_selection;
	GtkTreeView	  *prop_view;
	GtkTreeStore	  *prop_model;
	GtkTreeIter        prop_iter;
	GogObject	  *prop_object;

	GraphGuruTypeSelector *type_selector;

	struct {
		GtkWidget *inc, *dec, *first, *last, *menu;
	} prec;
	/* internal state */
	int current_page, initial_page;
	gboolean valid;
	gboolean updating;
	gboolean fmt_page_initialized;
	gboolean editing;

	/* hackish reuse of State as a closure */
	GogObject *search_target, *new_child;
};

struct _GraphGuruTypeSelector {
	GladeXML    	*gui;
	GtkWidget	*canvas;
	GtkWidget	*sample_button;
	GtkLabel	*label;
	GtkTreeView	*list_view;
	GtkListStore	*model;
	FooCanvasItem *selector;

	FooCanvasItem *sample_graph_item;

	GraphGuruState *state;

	FooCanvasGroup *graph_group;

	xmlNode const *plots;
	GogPlotFamily	*current_family;
	GogPlotType	*current_type;
	FooCanvasGroup const *current_family_item;
	FooCanvasItem const  *current_minor_item;

	int max_priority_so_far;
};

enum {
	PLOT_FAMILY_TYPE_IMAGE,
	PLOT_FAMILY_TYPE_NAME,
	PLOT_FAMILY_TYPE_CANVAS_GROUP,
	PLOT_FAMILY_NUM_COLUMNS
};
enum {
	PLOT_ATTR_NAME,
	PLOT_ATTR_OBJECT,
	PLOT_ATTR_NUM_COLUMNS
};

#define MINOR_PIXMAP_WIDTH	64
#define MINOR_PIXMAP_HEIGHT	60
#define BORDER	5

#define PLOT_TYPE_KEY		"plot_type"
#define REG_CURVE_TYPE_KEY	"reg_curve_type"
#define FIRST_MINOR_TYPE	"first_minor_type"
#define ROLE_KEY		"role"
#define STATE_KEY		"plot_type"

static GdkPixbuf *
get_pixbuf (char const *image_file)
{
	static GHashTable *cache = NULL;
	GdkPixbuf *pixbuf;

	if (cache != NULL) {
		pixbuf = g_hash_table_lookup (cache, image_file);
		if (pixbuf != NULL)
			return pixbuf;
	} else
		cache = g_hash_table_new_full (g_str_hash, g_str_equal,
					       NULL, g_object_unref);

	pixbuf = go_pixbuf_new_from_file (image_file);
	g_hash_table_insert (cache, (gpointer)image_file, pixbuf);

	return pixbuf;
}

static void
get_pos (int col, int row, double *x, double *y)
{
	*x = (col-1) * (MINOR_PIXMAP_WIDTH + BORDER) + BORDER;
	*y = (row-1) * (MINOR_PIXMAP_HEIGHT + BORDER) + BORDER;
}

/*
 * graph_typeselect_minor :
 *
 * @typesel :
 * @item : A CanvasItem in the selector.
 *
 * Moves the typesel::selector overlay above the @item.
 * Assumes that the item is visible
 */
static void
graph_typeselect_minor (GraphGuruTypeSelector *typesel, FooCanvasItem *item)
{
	GraphGuruState *s = typesel->state;
	GogPlotType *type;
	double x1, y1, x2, y2;
	gboolean enable_next_button;
	GogPlot *plot;

	if (typesel->current_minor_item == item)
		return;

	type = g_object_get_data (G_OBJECT (item), PLOT_TYPE_KEY);

	g_return_if_fail (type != NULL);

	typesel->current_type = type;
	typesel->current_minor_item = item;
	foo_canvas_item_get_bounds (item, &x1, &y1, &x2, &y2);
	foo_canvas_item_set (FOO_CANVAS_ITEM (typesel->selector),
		"x1", x1-1., "y1", y1-1.,
		"x2", x2+1., "y2", y2+1.,
		NULL);
	gtk_label_set_text (typesel->label, _(type->description));
	gtk_widget_set_sensitive (typesel->sample_button, TRUE);

	enable_next_button = (s->plot == NULL);

	plot = gog_plot_new_by_type (type);

	g_return_if_fail (plot != NULL);

	if (s->chart != NULL) {
		GogObject *obj = GOG_OBJECT (s->chart);
		gog_object_clear_parent (obj);
		g_object_unref (obj);
		s->chart = GOG_CHART (gog_object_add_by_name (
				GOG_OBJECT (s->graph), "Chart", NULL));
	}

	gog_object_add_by_name (GOG_OBJECT (s->chart),
		"Plot", GOG_OBJECT (s->plot = plot));
	gog_plot_guru_helper (plot);
#if 0
	if (s->original_plot != NULL &&
	    !gog_plot_make_similar (s->plot, s->original_plot))
		return;
#endif

	if (s->dalloc != NULL)
		gog_data_allocator_allocate (s->dalloc, s->plot);

	if (s->current_page == 0 && enable_next_button)
		gtk_widget_set_sensitive (s->button_navigate, TRUE);
}

static gboolean
graph_typeselect_minor_x_y (GraphGuruTypeSelector *typesel,
			    double x, double y)
{
	FooCanvasItem *item = foo_canvas_get_item_at (
		FOO_CANVAS (typesel->canvas), x, y);

	if (item != NULL) {
		if(item != typesel->selector)
			graph_typeselect_minor (typesel, item);
		foo_canvas_item_grab_focus (item);
		return TRUE;
	}

	return FALSE;
}

static gboolean
cb_key_press_event (G_GNUC_UNUSED GtkWidget *wrapper,
		    GdkEventKey *event,
		    GraphGuruTypeSelector *typesel)
{
	GtkCornerType corner;
	int col, row;
	double x, y;
	GogPlotType const *type = g_object_get_data (
		G_OBJECT (typesel->current_minor_item), PLOT_TYPE_KEY);

	g_return_val_if_fail (type != NULL, FALSE);

	col = type->col;
	row = type->row;

	switch (event->keyval){
	case GDK_KP_Left:	case GDK_Left:
		corner = GTK_CORNER_BOTTOM_RIGHT;
		--col;
		break;

	case GDK_KP_Up:	case GDK_Up:
		corner = GTK_CORNER_BOTTOM_RIGHT;
		--row;
		break;

	case GDK_KP_Right:	case GDK_Right:
		corner = GTK_CORNER_TOP_LEFT;
		++col;
		break;

	case GDK_KP_Down:	case GDK_Down:
		corner = GTK_CORNER_TOP_LEFT;
		++row;
		break;

	default:
		return FALSE;
	}

	get_pos (col, row, &x, &y);
	graph_typeselect_minor_x_y (typesel, x, y);

	return TRUE;
}

static gint
cb_button_press_event (GtkWidget *widget, GdkEventButton *event,
		       GraphGuruTypeSelector *typesel)
{
	if (event->button == 1) {
		FooCanvas *c = FOO_CANVAS (widget);
		double x, y;

		foo_canvas_window_to_world (c, event->x, event->y, &x, &y);

		graph_typeselect_minor_x_y (typesel, x, y);
	}

	return FALSE;
}

static void
cb_selection_changed (GraphGuruTypeSelector *typesel)
{
	GtkTreeSelection *selection = gtk_tree_view_get_selection (typesel->list_view);
	GtkTreeIter  iter;
	FooCanvasItem *item;
	FooCanvasGroup *group;

	if (typesel->current_family_item != NULL)
		foo_canvas_item_hide (FOO_CANVAS_ITEM (typesel->current_family_item));
	if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
		return;
	gtk_tree_model_get (GTK_TREE_MODEL (typesel->model), &iter,
		PLOT_FAMILY_TYPE_CANVAS_GROUP, &group,
		-1);

	foo_canvas_item_show (FOO_CANVAS_ITEM (group));
	typesel->current_family_item = group;

	foo_canvas_item_hide (FOO_CANVAS_ITEM (typesel->selector));
	item = g_object_get_data (G_OBJECT (group), FIRST_MINOR_TYPE);
	if (item != NULL)
		graph_typeselect_minor (typesel, item);
	foo_canvas_item_show (FOO_CANVAS_ITEM (typesel->selector));
}

static void
cb_typesel_sample_plot_resize (FooCanvas *canvas,
			       GtkAllocation *alloc, GraphGuruTypeSelector *typesel)
{
	/* Use 96dpi and 50% zoom. Hard code the zoom because it is not
	 * active when sample button is not depressed */
	if (typesel->sample_graph_item != NULL)
		foo_canvas_item_set (typesel->sample_graph_item,
			"w", (double)alloc->width,
			"h", (double)alloc->height,
			NULL);
}

static void
cb_sample_pressed (GraphGuruTypeSelector *typesel)
{
	if (typesel->current_family_item == NULL)
		return;

	if (typesel->sample_graph_item == NULL) {
		GtkAllocation *size = &GTK_WIDGET (typesel->canvas)->allocation;
		typesel->sample_graph_item = foo_canvas_item_new (typesel->graph_group,
			GOG_CONTROL_FOOCANVAS_TYPE,
			"model", typesel->state->graph,
			NULL);
		cb_typesel_sample_plot_resize (FOO_CANVAS (typesel->canvas),
					       size, typesel);

		g_return_if_fail (typesel->sample_graph_item != NULL);
	}

	foo_canvas_item_hide (FOO_CANVAS_ITEM (typesel->current_family_item));
	foo_canvas_item_hide (FOO_CANVAS_ITEM (typesel->selector));
	foo_canvas_item_show (FOO_CANVAS_ITEM (typesel->graph_group));
}

static void
cb_sample_released (GraphGuruTypeSelector *typesel)
{
	if (typesel->current_family_item == NULL)
		return;

	foo_canvas_item_hide (FOO_CANVAS_ITEM (typesel->graph_group));
	foo_canvas_item_show (FOO_CANVAS_ITEM (typesel->current_family_item));
	foo_canvas_item_show (FOO_CANVAS_ITEM (typesel->selector));
}

typedef struct {
	GraphGuruTypeSelector	*typesel;
	FooCanvasGroup		*group;
	FooCanvasItem		*current_item;
	GogPlotType 		*current_type;
	int col, row;
} type_list_closure;

static void
cb_plot_types_init (char const *id, GogPlotType *type,
		    type_list_closure *closure)
{
	double x1, y1, w, h;
	FooCanvasItem *item;
	int col, row;
	GdkPixbuf *image = get_pixbuf (type->sample_image_file);

	g_return_if_fail (image != NULL);

	col = type->col;
	row = type->row;
	get_pos (col, row, &x1, &y1);
	w = gdk_pixbuf_get_width (image);
	if (w > MINOR_PIXMAP_WIDTH)
		w = MINOR_PIXMAP_WIDTH;
	h = gdk_pixbuf_get_height (image);
	if (h > MINOR_PIXMAP_HEIGHT)
		h = MINOR_PIXMAP_HEIGHT;

	item = foo_canvas_item_new (closure->group,
		foo_canvas_pixbuf_get_type (),
		"x",	 x1,	"y",	  y1,
		"width", w,	"height", h,
		"pixbuf",	image,
		"point_ignores_alpha", TRUE,
		NULL);
	g_object_set_data (G_OBJECT (item), PLOT_TYPE_KEY, (gpointer)type);

	if (closure->current_type == NULL ||
	    closure->row > row ||
	    (closure->row == row && closure->col > col)) {
		closure->current_type = type;
		closure->current_item = item;
		closure->col = col;
		closure->row = row;
	}
}

static void
cb_plot_families_init (char const *id, GogPlotFamily *family,
		       GraphGuruTypeSelector *typesel)
{
	FooCanvasGroup		*group;
	GtkTreeIter		 iter;
	type_list_closure	 closure;

	if (g_hash_table_size (family->types) <= 0)
		return;

	/* Define a canvas group for all the minor types */
	group = FOO_CANVAS_GROUP (foo_canvas_item_new (
		foo_canvas_root (FOO_CANVAS (typesel->canvas)),
		foo_canvas_group_get_type (),
		"x", 0.0,
		"y", 0.0,
		NULL));
	foo_canvas_item_hide (FOO_CANVAS_ITEM (group));

	gtk_list_store_append (typesel->model, &iter);
	gtk_list_store_set (typesel->model, &iter,
		PLOT_FAMILY_TYPE_IMAGE,		get_pixbuf (family->sample_image_file),
		PLOT_FAMILY_TYPE_NAME,		_(family->name),
		PLOT_FAMILY_TYPE_CANVAS_GROUP,	group,
		-1);

	if (typesel->max_priority_so_far < family->priority) {
		typesel->max_priority_so_far = family->priority;
		gtk_tree_selection_select_iter (
			gtk_tree_view_get_selection (typesel->list_view), &iter);
	}

	closure.typesel = typesel;
	closure.group	= group;
	closure.current_type = NULL;
	closure.current_item = NULL;

	/* Init the list and the canvas group for each family */
	g_hash_table_foreach (family->types,
		(GHFunc) cb_plot_types_init, &closure);
	g_object_set_data (G_OBJECT (group), FIRST_MINOR_TYPE,
		closure.current_item);
}

static void
cb_canvas_realized (GtkLayout *widget)
{
	gdk_window_set_back_pixmap (widget->bin_window, NULL, FALSE);
}

static void
graph_guru_state_destroy (GraphGuruState *state)
{
	g_return_if_fail (state != NULL);

	if (state->graph != NULL) {
		g_object_unref (state->graph);
		state->graph = NULL;
	}

	if (state->gui != NULL) {
		g_object_unref (state->gui);
		state->gui = NULL;
	}

	if (state->register_closure != NULL) {
		g_closure_unref (state->register_closure);
		state->register_closure = NULL;
	}

	state->dialog = NULL;
	g_free (state);
}

static void populate_graph_item_list (GogObject *obj, GogObject *select,
				      GraphGuruState *s, GtkTreeIter *parent,
				      gboolean insert);

static void
cb_graph_guru_add_item (GtkWidget *w, GraphGuruState *s)
{
	gog_object_add_by_role (s->prop_object, 
		g_object_get_data (G_OBJECT (w), ROLE_KEY), NULL);
}

static void
cb_graph_guru_delete_item (GraphGuruState *s)
{
	if (s->prop_object != NULL) {
		GtkTreeIter iter;
		GogObject *obj = s->prop_object;
		gboolean have_iter;

		/* store parent iter */
		have_iter =
		  gtk_tree_model_iter_parent (GTK_TREE_MODEL (s->prop_model),
					      &iter, &s->prop_iter);
		gog_object_clear_parent (obj);
		g_object_unref (obj);
		/* then select the parent after we delete */
		if (have_iter)
		  gtk_tree_selection_select_iter (s->prop_selection, &iter);
	}
}

static void
update_prec_menu (GraphGuruState *s, gboolean inc_ok, gboolean dec_ok)
{
	gtk_widget_set_sensitive (s->prec.first,    inc_ok);
	gtk_widget_set_sensitive (s->prec.inc,	    inc_ok);
	gtk_widget_set_sensitive (s->prec.dec,	    dec_ok);
	gtk_widget_set_sensitive (s->prec.last,	    dec_ok);
	gtk_widget_set_sensitive (s->prec.menu,	    dec_ok | inc_ok);
}

static gboolean
cb_reordered_find (GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
		   GraphGuruState *s)
{
	GogObject *obj;
	gtk_tree_model_get (model, iter, PLOT_ATTR_OBJECT, &obj, -1);
	if (obj == s->search_target) {
		gtk_tree_store_move_after (s->prop_model, &s->prop_iter, iter);
		return TRUE;
	}

	return FALSE;
}
static void
reorder (GraphGuruState *s, gboolean inc, gboolean goto_max)
{
	gboolean inc_ok, dec_ok;
	GogObject *after;

	g_return_if_fail (s->search_target == NULL);

	after = gog_object_reorder (s->prop_object, inc, goto_max);
	if (after != NULL) {
		s->search_target = after;
		gtk_tree_model_foreach (GTK_TREE_MODEL (s->prop_model),
			(GtkTreeModelForeachFunc) cb_reordered_find, s);
		s->search_target = NULL;
	} else
		gtk_tree_store_move_after (s->prop_model, &s->prop_iter, NULL);

	gog_object_can_reorder (s->prop_object, &inc_ok, &dec_ok);
	update_prec_menu (s, inc_ok, dec_ok);
}
static void cb_prec_first (GraphGuruState *s) { reorder (s, TRUE,  TRUE); }
static void cb_prec_inc	  (GraphGuruState *s) { reorder (s, TRUE,  FALSE); }
static void cb_prec_dec   (GraphGuruState *s) { reorder (s, FALSE, FALSE); }
static void cb_prec_last  (GraphGuruState *s) { reorder (s, FALSE, TRUE); }

struct type_menu_create {
	GraphGuruState *state;
	GtkWidget   *menu;
	gboolean non_blank;
};


static gint
cb_cmp_plot_type (GogPlotType const *a, GogPlotType const *b)
{
	if (a->row == b->row)
		return a->col - b->col;
	return a->row - b->row;
}

static void
cb_plot_type_list (char const *id, GogPlotType *type, GSList **list)
{
	*list = g_slist_insert_sorted (*list, type,
		(GCompareFunc) cb_cmp_plot_type);
}

static void
cb_graph_guru_add_plot (GtkWidget *w, GraphGuruState *s)
{
	GogPlotType *type = g_object_get_data (G_OBJECT (w), PLOT_TYPE_KEY);
	GogPlot *plot = gog_plot_new_by_type (type);
	gog_object_add_by_name (GOG_OBJECT (s->prop_object),
		"Plot", GOG_OBJECT (plot));
	gog_plot_guru_helper (plot);
	/* as a convenience add a series to the newly created plot */
	gog_object_add_by_name (GOG_OBJECT (plot), "Series", NULL);
}

static void
cb_plot_family_menu_create (char const *id, GogPlotFamily *family,
			    struct type_menu_create *closure)
{
	GtkWidget *w, *menu;
	GSList *ptr, *types = NULL;
	GogPlotType *type;
	GogAxisSet axis_set;

	if (g_hash_table_size (family->types) <= 0)
		return;

	axis_set = gog_chart_get_axis_set (GOG_CHART (closure->state->prop_object));

	if (axis_set != GOG_AXIS_SET_UNKNOWN &&
	    family->axis_set != axis_set)
		return;

	menu = gtk_image_menu_item_new_with_label (_(family->name));
	gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (menu),
		gtk_image_new_from_pixbuf (
			get_pixbuf (family->sample_image_file)));
	gtk_menu_shell_append (GTK_MENU_SHELL (closure->menu), menu);
	closure->non_blank = TRUE;

	w = gtk_menu_new ();
	gtk_menu_item_set_submenu (GTK_MENU_ITEM (menu), w);

	menu = w;
	g_hash_table_foreach (family->types,
		(GHFunc) cb_plot_type_list, &types);
	for (ptr = types ; ptr != NULL ; ptr = ptr->next) {
		type = ptr->data;
		w = gtk_image_menu_item_new_with_label (_(type->name));
		gtk_image_menu_item_set_image (GTK_IMAGE_MENU_ITEM (w),
			gtk_image_new_from_pixbuf (
				get_pixbuf (type->sample_image_file)));
		g_object_set_data (G_OBJECT (w), PLOT_TYPE_KEY, type);
		g_signal_connect (G_OBJECT (w),
			"activate",
			G_CALLBACK (cb_graph_guru_add_plot), closure->state);
		gtk_menu_shell_append (GTK_MENU_SHELL (menu), w);
	}
	g_slist_free (types);
}

/* return TRUE if there are plot types to add */
static GtkWidget *
plot_type_menu_create (GraphGuruState *s)
{
	struct type_menu_create closure;
	closure.state = s;
	closure.menu = gtk_menu_new ();
	closure.non_blank = FALSE;

	g_hash_table_foreach ((GHashTable *)gog_plot_families (),
		(GHFunc) cb_plot_family_menu_create, &closure);

	if (closure.non_blank)
		return closure.menu;
	gtk_object_destroy (GTK_OBJECT (closure.menu));
	return NULL;
}

static void
cb_graph_guru_add_reg_curve (GtkWidget *w, GraphGuruState *s)
{
	GogRegCurveType *type = g_object_get_data (G_OBJECT (w), REG_CURVE_TYPE_KEY);
	GogRegCurve *curve = gog_reg_curve_new_by_type (type);
	gog_object_add_by_name (GOG_OBJECT (s->prop_object),
		"Regression curve", GOG_OBJECT (curve));
}

static void
cb_reg_curve_type_menu_create (char const *id, GogRegCurveType *type,
			    struct type_menu_create *closure)
{
	GtkWidget *menu;

	menu = gtk_menu_item_new_with_label (_(type->name));
	g_object_set_data (G_OBJECT (menu), REG_CURVE_TYPE_KEY, type);
	g_signal_connect (G_OBJECT (menu),
		"activate",
		G_CALLBACK (cb_graph_guru_add_reg_curve), closure->state);
	gtk_menu_shell_append (GTK_MENU_SHELL (closure->menu), menu);
	closure->non_blank = TRUE;
}

static GtkWidget *
reg_curve_type_menu_create (GraphGuruState *s)
{
	struct type_menu_create closure;
	closure.state = s;
	closure.menu = gtk_menu_new ();
	closure.non_blank = FALSE;

	g_hash_table_foreach ((GHashTable *)gog_reg_curve_types (),
		(GHFunc) cb_reg_curve_type_menu_create, &closure);

	if (closure.non_blank)
		return closure.menu;
	gtk_object_destroy (GTK_OBJECT (closure.menu));
	return NULL;
}

static void
cb_attr_tree_selection_change (GraphGuruState *s)
{
	gboolean add_ok = FALSE;
	gboolean delete_ok = FALSE;
	gboolean inc_ok = FALSE;
	gboolean dec_ok = FALSE;
	GtkTreeModel *model;
	GogObject  *obj = NULL;
	GtkWidget *w, *notebook;
	GtkTreePath *path;

	if (gtk_tree_selection_get_selected (s->prop_selection, &model, &s->prop_iter))
		gtk_tree_model_get (model, &s->prop_iter,
				    PLOT_ATTR_OBJECT, &obj,
				    -1);

	if (s->prop_object == obj)
		return;

	if (obj) {
		path = gtk_tree_model_get_path (GTK_TREE_MODEL (s->prop_model), &s->prop_iter);
		gtk_tree_view_scroll_to_cell (s->prop_view, path, NULL, FALSE, 0, 0);
		gtk_tree_path_free (path);
	}

	/* remove the old prop page */
	s->prop_object = obj;
	w = gtk_bin_get_child (GTK_BIN (s->prop_container));
	if (w != NULL)
		gtk_container_remove (s->prop_container, w);

	if (s->prop_object != NULL) {
		/* Setup up the additions menu */
		GSList *additions = gog_object_possible_additions (s->prop_object);
		if (additions != NULL) {
			GogObjectRole const *role;
			GSList *ptr;
			GtkWidget *tmp, *menu;

			menu = gtk_menu_new ();
			for (ptr = additions ; ptr != NULL ; ptr = ptr->next) {
				role = ptr->data;
				/* somewhat hackish, but I do not see a need
				 * for anything more general yet */
				if (!strcmp (role->id, "Regression curve")) {
					GtkWidget *submenu = reg_curve_type_menu_create (s);
					if (submenu != NULL) {
						tmp = gtk_menu_item_new_with_label (_(role->id));
						gtk_menu_item_set_submenu (GTK_MENU_ITEM (tmp), submenu);
					} else 
						continue;
				} else if (strcmp (role->id, "Plot")) {
					tmp = gtk_menu_item_new_with_label (_(role->id));
					g_object_set_data (G_OBJECT (tmp), ROLE_KEY,
						(gpointer)role);
					g_signal_connect (G_OBJECT (tmp),
						"activate",
						G_CALLBACK (cb_graph_guru_add_item), s);
				} else {
					GtkWidget *submenu = plot_type_menu_create (s);
					if (submenu != NULL) {
						tmp = gtk_menu_item_new_with_label (_(role->id));
						gtk_menu_item_set_submenu (GTK_MENU_ITEM (tmp), submenu);
					} else 
						continue;
				}

				gtk_menu_shell_append (GTK_MENU_SHELL (menu), tmp);

			}
			add_ok = (additions != NULL);
			g_slist_free (additions);

			gtk_menu_item_set_submenu (GTK_MENU_ITEM (s->add_menu), menu);
			gtk_widget_show_all (s->add_menu);
		}

		/* if we ever go back to the typeselector be sure to 
		 * add the plot to the last selected chart */
		s->chart = (GogChart *)
			gog_object_get_parent_typed (obj, GOG_CHART_TYPE);
		s->plot = (GogPlot *)
			gog_object_get_parent_typed (obj, GOG_PLOT_TYPE);

		if (s->plot == NULL) {
			if (s->chart == NULL && s->graph != NULL) {
				GSList *charts = gog_object_get_children (GOG_OBJECT (s->graph),
					gog_object_find_role_by_name (GOG_OBJECT (s->graph), "Chart"));
				if (charts != NULL && charts->next == NULL)
					s->chart = charts->data;
				g_slist_free (charts);
			}
			if (s->chart != NULL) {
				GSList *plots = gog_object_get_children (GOG_OBJECT (s->chart),
					gog_object_find_role_by_name (GOG_OBJECT (s->chart), "Plot"));
				if (plots != NULL && plots->next == NULL)
					s->plot = plots->data;
				g_slist_free (plots);
			}
		}
		gtk_widget_set_sensitive (s->button_navigate, s->chart != NULL);

		delete_ok = gog_object_is_deletable (s->prop_object);
		gog_object_can_reorder (obj, &inc_ok, &dec_ok);

		/* create a prefs page for the graph obj */
		notebook = gog_object_get_editor (obj, s->dalloc, s->cc);
		gtk_widget_show (notebook);
		gtk_container_add (s->prop_container, notebook);
	}

	gtk_widget_set_sensitive (s->delete_button, delete_ok);
	gtk_widget_set_sensitive (s->add_menu,	    add_ok);
	update_prec_menu (s, inc_ok, dec_ok);
}

static gboolean
cb_find_renamed_item (GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
		      GraphGuruState *s)
{
	GogObject *obj;

	gtk_tree_model_get (model, iter, PLOT_ATTR_OBJECT, &obj, -1);
	if (obj == s->search_target) {
		s->search_target = NULL;
		gtk_tree_store_set (s->prop_model, iter,
			PLOT_ATTR_NAME, gog_object_get_name (obj),
			-1);
		return TRUE;
	}

	return FALSE;
}

static void
cb_obj_name_changed (GogObject *obj, GraphGuruState *s)
{
	s->search_target = obj;
	gtk_tree_model_foreach (GTK_TREE_MODEL (s->prop_model),
		(GtkTreeModelForeachFunc) cb_find_renamed_item, s);
}

static gboolean
cb_find_child_added (GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
		     GraphGuruState *s)
{
	GogObject *obj;

	gtk_tree_model_get (model, iter, PLOT_ATTR_OBJECT, &obj, -1);
	if (obj == s->search_target) {
		s->search_target = NULL;
		populate_graph_item_list (s->new_child, s->new_child, s, iter, TRUE);
		return TRUE;
	}

	return FALSE;
}

static void
cb_obj_child_added (GogObject *parent, GogObject *child, GraphGuruState *s)
{
	s->search_target = parent;
	s->new_child = child;
	gtk_tree_model_foreach (GTK_TREE_MODEL (s->prop_model),
		(GtkTreeModelForeachFunc) cb_find_child_added, s);
	s->new_child = NULL;
}

static gboolean
cb_find_child_removed (GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
		       GraphGuruState *s)
{
	GogObject *obj;
	GtkWidget *w;

	gtk_tree_model_get (model, iter, PLOT_ATTR_OBJECT, &obj, -1);
	if (obj == s->search_target) {
		s->search_target = NULL;
		/* remove the tree element and the prop page */
		gtk_tree_store_remove (s->prop_model, iter);
		w = gtk_bin_get_child (GTK_BIN (s->prop_container));
		if (w != NULL)
			gtk_container_remove (s->prop_container, w);
		return TRUE;
	}

	return FALSE;
}
static void
cb_obj_child_removed (GogObject *parent, GogObject *child, GraphGuruState *s)
{
	s->search_target = child;
	gtk_tree_model_foreach (GTK_TREE_MODEL (s->prop_model),
		(GtkTreeModelForeachFunc) cb_find_child_removed, s);

	if (s->chart == (gpointer)child) {
		s->chart = NULL;
		s->plot = NULL;
		gtk_widget_set_sensitive (s->button_navigate, FALSE);
	} else if (s->plot == (gpointer)child)
		s->plot = NULL;
}

static void
populate_graph_item_list (GogObject *obj, GogObject *select, GraphGuruState *s,
			  GtkTreeIter *parent, gboolean insert)
{
	GSList *children, *ptr;
	GtkTreeIter iter;
	GtkTreePath *path;
	GClosure *closure;

	if (insert) {
		GogObject *gparent = gog_object_get_parent (obj);
		gint i = g_slist_index (gparent->children, obj);
		if (i > 0) {
			GtkTreeIter sibling;
			if (gtk_tree_model_iter_nth_child (GTK_TREE_MODEL (s->prop_model),
				&sibling, parent, i-1)) {
			  gtk_tree_store_insert_after (s->prop_model, &iter,
				parent, &sibling);
			} else {
			  gtk_tree_store_append (s->prop_model, &iter, parent);
			}
		} else
			gtk_tree_store_prepend (s->prop_model, &iter, parent);
	} else
		gtk_tree_store_append (s->prop_model, &iter, parent);

	gtk_tree_store_set (s->prop_model, &iter,
		PLOT_ATTR_OBJECT,	obj,
		PLOT_ATTR_NAME, gog_object_get_name (obj),
		-1);

	/* remove the signal handlers when the guru goes away */
	closure = g_cclosure_new (G_CALLBACK (cb_obj_name_changed), s, NULL);
	g_object_watch_closure (G_OBJECT (s->prop_view), closure);
	g_signal_connect_closure (G_OBJECT (obj),
		"name-changed",
		closure, FALSE);
	closure = g_cclosure_new (G_CALLBACK (cb_obj_child_added), s, NULL);
	g_object_watch_closure (G_OBJECT (s->prop_view), closure);
	g_signal_connect_closure (G_OBJECT (obj),
		"child-added",
		closure, FALSE);
	closure = g_cclosure_new (G_CALLBACK (cb_obj_child_removed), s, NULL);
	g_object_watch_closure (G_OBJECT (s->prop_view), closure);
	g_signal_connect_closure (G_OBJECT (obj),
		"child-removed",
		closure, FALSE);

	children = gog_object_get_children (obj, NULL);
	for (ptr = children ; ptr != NULL ; ptr = ptr->next)
		populate_graph_item_list (ptr->data, select, s, &iter, FALSE);
	g_slist_free (children);

	/* ensure that new items are visible */
	path = gtk_tree_model_get_path (
		GTK_TREE_MODEL (s->prop_model), &iter);
	gtk_tree_view_expand_to_path (s->prop_view, path);
	gtk_tree_path_free (path);

	if (obj == select)
		/* select after expanding so that we do not lose selection due
		 * to visibility */
		gtk_tree_selection_select_iter (s->prop_selection, &iter);
}

static gboolean
cb_find_item (GtkTreeModel *model, GtkTreePath *path, GtkTreeIter *iter,
	      GraphGuruState *s)
{
	GogObject *obj;

	gtk_tree_model_get (model, iter, PLOT_ATTR_OBJECT, &obj, -1);
	if (obj == s->search_target) {
		gtk_tree_selection_select_iter (s->prop_selection, iter);
		return TRUE;
	}

	return FALSE;
}

static gint
cb_canvas_select_item (FooCanvas *canvas, GdkEventButton *event,
		       GraphGuruState *s)
{
	GogView *view;
	GogRenderer *rend;
	double x, y, item_x, item_y;

	g_return_val_if_fail (FOO_IS_CANVAS (canvas), FALSE);

	if (canvas->current_item == NULL)
		return FALSE;

	g_object_get (G_OBJECT (s->sample_graph_item), "renderer", &rend, NULL);
	g_object_get (G_OBJECT (rend), "view", &view, NULL);
	g_object_unref (G_OBJECT (rend));
	foo_canvas_window_to_world (canvas, event->x, event->y, &x, &y);
	g_object_get (G_OBJECT(s->sample_graph_item), "x", &item_x, "y", &item_y, NULL);
	gog_view_info_at_point (view,
		(x - item_x) * canvas->pixels_per_unit,	
		(y - item_y) * canvas->pixels_per_unit,
		s->prop_object, &s->search_target, NULL);
	g_object_unref (G_OBJECT (view));
	if (s->search_target == NULL)
		return FALSE;

	gtk_tree_model_foreach (GTK_TREE_MODEL (s->prop_model),
		(GtkTreeModelForeachFunc) cb_find_item, s);
	s->search_target = NULL;
	return TRUE;
}

static void
cb_sample_plot_resize (FooCanvas *canvas,
		       GtkAllocation *alloc, GraphGuruState *state)
{
	double aspect_ratio;
	double width, height, x, y;

	gog_graph_get_size (state->graph, &width, &height);
       	aspect_ratio = width / height;

	if (alloc->width > alloc->height * aspect_ratio) {
		height = alloc->height;
		width = height * aspect_ratio;
		x = (alloc->width - width) / 2.0;
		y = 0.0;
	} else {
		width = alloc->width;
		height = width / aspect_ratio;
		x = 0.0;
		y = (alloc->height - height) / 2.0;
	}
	
	foo_canvas_item_set (state->sample_graph_item,
		"w", width,
		"h", height,
		"x", x,
		"y", y,
		NULL);
}

static void
graph_guru_init_format_page (GraphGuruState *s)
{
	GtkWidget *w;
	GtkTreeViewColumn *column;

	if (s->fmt_page_initialized)
		return;
	s->fmt_page_initialized = TRUE;
	s->add_menu	 = glade_xml_get_widget (s->gui, "add_menu");
	s->delete_button = glade_xml_get_widget (s->gui, "delete");
	s->prec.menu  = glade_xml_get_widget (s->gui, "precedence_menu");
	s->prec.inc   = glade_xml_get_widget (s->gui, "inc_precedence");
	s->prec.dec   = glade_xml_get_widget (s->gui, "dec_precedence");
	s->prec.first = glade_xml_get_widget (s->gui, "first_precedence");
	s->prec.last  = glade_xml_get_widget (s->gui, "last_precedence");

	g_signal_connect_swapped (G_OBJECT (s->delete_button),
		"clicked",
		G_CALLBACK (cb_graph_guru_delete_item), s);
	g_signal_connect_swapped (G_OBJECT (s->prec.first),
		"activate",
		G_CALLBACK (cb_prec_first), s);
	g_signal_connect_swapped (G_OBJECT (s->prec.inc),
		"activate",
		G_CALLBACK (cb_prec_inc), s);
	g_signal_connect_swapped (G_OBJECT (s->prec.dec),
		"activate",
		G_CALLBACK (cb_prec_dec), s);
	g_signal_connect_swapped (G_OBJECT (s->prec.last),
		"activate",
		G_CALLBACK (cb_prec_last), s);

	/* Load up the sample view and make it fill the entire canvas */
	w = glade_xml_get_widget (s->gui, "sample_canvas");
	s->sample_graph_item = foo_canvas_item_new (
		foo_canvas_root (FOO_CANVAS (w)), GOG_CONTROL_FOOCANVAS_TYPE,
		"model", s->graph,
		NULL);
	cb_sample_plot_resize (FOO_CANVAS (w), &w->allocation, s);
	g_signal_connect (G_OBJECT (w),
		"size_allocate",
		G_CALLBACK (cb_sample_plot_resize), s);
	g_signal_connect_after (G_OBJECT (w),
		"button_press_event",
		G_CALLBACK (cb_canvas_select_item), s);
	gtk_widget_show (w);

	w = glade_xml_get_widget (s->gui, "prop_alignment");
	s->prop_container = GTK_CONTAINER (w);
	s->prop_model = gtk_tree_store_new (PLOT_ATTR_NUM_COLUMNS,
				    G_TYPE_STRING, G_TYPE_POINTER);
	s->prop_view = GTK_TREE_VIEW (gtk_tree_view_new_with_model (
			GTK_TREE_MODEL (s->prop_model)));
	s->prop_selection = gtk_tree_view_get_selection (s->prop_view);
	gtk_tree_selection_set_mode (s->prop_selection, GTK_SELECTION_BROWSE);
	g_signal_connect_swapped (s->prop_selection,
		"changed",
		G_CALLBACK (cb_attr_tree_selection_change), s);

	column = gtk_tree_view_column_new_with_attributes (_("Name"),
		gtk_cell_renderer_text_new (),
		"text", PLOT_ATTR_NAME, NULL);
	gtk_tree_view_append_column (s->prop_view, column);

	gtk_tree_view_set_headers_visible (s->prop_view, FALSE);

	gtk_tree_store_clear (s->prop_model);
	populate_graph_item_list (GOG_OBJECT (s->graph), GOG_OBJECT (s->graph),
				  s, NULL, FALSE);
	gtk_tree_view_expand_all (s->prop_view);

	w = glade_xml_get_widget (s->gui, "attr_window");
	gtk_container_add (GTK_CONTAINER (w), GTK_WIDGET (s->prop_view));
	gtk_widget_show_all (w);
}

static void
graph_guru_set_page (GraphGuruState *s, int page)
{
	char *name;

	if (s->current_page == page)
		return;

	switch (page) {
	case 0: name = _("Step 1 of 2: Select Chart Type");
		gtk_widget_set_sensitive (s->button_navigate, s->plot != NULL);
		gtk_button_set_label (GTK_BUTTON (s->button_navigate),
				      GTK_STOCK_GO_FORWARD);
		break;

	case 1:
		if (s->initial_page == 0) {
			name = _("Step 2 of 2: Customize Chart");
			gtk_widget_set_sensitive (s->button_navigate, s->chart != NULL);
			gtk_button_set_label (GTK_BUTTON (s->button_navigate),
					      GTK_STOCK_GO_BACK);
		} else {
			name = _("Customize Chart");
			gtk_widget_hide	(s->button_navigate);
		}
		graph_guru_init_format_page (s);
		break;

	default:
		g_warning ("Invalid Chart Guru page");
		return;
	}

	s->current_page = page;
	gtk_notebook_set_current_page (s->steps, page - s->initial_page);
	gtk_window_set_title (GTK_WINDOW (s->dialog), name);
}

static void
cb_graph_guru_clicked (GtkWidget *button, GraphGuruState *s)
{
	if (s->dialog == NULL)
		return;

	if (button == s->button_navigate) {
		graph_guru_set_page (s, (s->current_page + 1) % 2);
		return;
	} else if (button == s->button_ok &&
		   s->register_closure != NULL && s->graph != NULL) {
		/* Invoking closure */
		GValue instance_and_params[2];
		gpointer data = s->register_closure->is_invalid ? 
			NULL : s->register_closure->data;
		
		instance_and_params[0].g_type = 0;
		g_value_init (&instance_and_params[0], GOG_GRAPH_TYPE);
		g_value_set_instance (&instance_and_params[0], s->graph);

		instance_and_params[1].g_type = 0;
		g_value_init (&instance_and_params[1], G_TYPE_POINTER);
		g_value_set_pointer (&instance_and_params[1], data);

		g_closure_set_marshal (s->register_closure,
				       g_cclosure_marshal_VOID__POINTER);

		g_closure_invoke (s->register_closure,
				  NULL,
				  2,
				  instance_and_params,
				  NULL);
		g_value_unset (instance_and_params + 0);
	}

	gtk_widget_destroy (GTK_WIDGET (s->dialog));
}

static GtkWidget *
graph_guru_init_button (GraphGuruState *s, char const *widget_name)
{
	GtkWidget *button = glade_xml_get_widget (s->gui, widget_name);
	g_signal_connect (G_OBJECT (button),
		"clicked",
		G_CALLBACK (cb_graph_guru_clicked), s);
	return button;
}

static GtkWidget *
graph_guru_init_ok_button (GraphGuruState *s)
{
	GtkButton *button = GTK_BUTTON (glade_xml_get_widget 
				       (s->gui, "button_ok"));
	
	if (s->editing) {
		gtk_button_set_label (button, GTK_STOCK_APPLY);
		gtk_button_set_use_stock (button, TRUE);
	} else {
		gtk_button_set_use_stock (button, FALSE);
		gtk_button_set_use_underline (button, TRUE);
		gtk_button_set_label (button, _("_Insert"));
	}
	g_signal_connect (G_OBJECT (button),
		"clicked",
		G_CALLBACK (cb_graph_guru_clicked), s);
	return GTK_WIDGET (button);
}

static void
typesel_set_selection_color (GraphGuruTypeSelector *typesel)
{
	GtkWidget *w = gtk_entry_new ();
	GdkColor  *color = &w->style->base [GTK_WIDGET_HAS_FOCUS (typesel->canvas)
		? GTK_STATE_SELECTED : GTK_STATE_ACTIVE];
	guint32    select_color = 0;

	select_color |= ((color->red >> 8) & 0xff)   << 24;
	select_color |= ((color->green >> 8) & 0xff) << 16;
	select_color |= ((color->blue >> 8) & 0xff)  << 8;
	select_color |= 0x40; /* alpha of 25% */
	foo_canvas_item_set (typesel->selector,
		"fill_color_rgba",	select_color,
		NULL);
	gtk_object_destroy (GTK_OBJECT (w));
}

static GtkWidget *
graph_guru_type_selector_new (GraphGuruState *s)
{
	GtkTreeSelection *selection;
	GraphGuruTypeSelector *typesel;
	GtkWidget *selector;
	GladeXML *gui;

	gui = go_libglade_new ("gog-guru-type-selector.glade", "type_selector", NULL, s->cc);

	typesel = g_new0 (GraphGuruTypeSelector, 1);
	typesel->state = s;
	typesel->current_family_item = NULL;
	typesel->current_minor_item = NULL;
	typesel->current_type = NULL;
	typesel->sample_graph_item = NULL;
	typesel->max_priority_so_far = -1;

	selector = glade_xml_get_widget (gui, "type_selector");

	/* List of family types */
	typesel->model = gtk_list_store_new (PLOT_FAMILY_NUM_COLUMNS,
					     GDK_TYPE_PIXBUF,
					     G_TYPE_STRING,
					     G_TYPE_POINTER);
	gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (typesel->model),
		PLOT_FAMILY_TYPE_NAME, GTK_SORT_ASCENDING);

	typesel->list_view = GTK_TREE_VIEW (glade_xml_get_widget (gui, "type_treeview"));
	gtk_tree_view_set_model (typesel->list_view, GTK_TREE_MODEL (typesel->model));
	gtk_tree_view_append_column (typesel->list_view,
		gtk_tree_view_column_new_with_attributes ("",
			gtk_cell_renderer_pixbuf_new (),
			"pixbuf", PLOT_FAMILY_TYPE_IMAGE,
			NULL));
	gtk_tree_view_append_column (typesel->list_view,
		gtk_tree_view_column_new_with_attributes (_("_Plot Type"),
			gtk_cell_renderer_text_new (),
			"text", PLOT_FAMILY_TYPE_NAME,
			NULL));


	/* Setup an canvas to display the sample image & the sample plot. */
	typesel->canvas = foo_canvas_new ();
	typesel->graph_group = FOO_CANVAS_GROUP (foo_canvas_item_new (
		foo_canvas_root (FOO_CANVAS (typesel->canvas)),
		foo_canvas_group_get_type (),
		"x", 0.0,
		"y", 0.0,
		NULL));
	g_object_connect (typesel->canvas,
		"signal::realize", G_CALLBACK (cb_canvas_realized), typesel,
		"signal::size_allocate", G_CALLBACK (cb_typesel_sample_plot_resize), typesel,
		"signal_after::key_press_event", G_CALLBACK (cb_key_press_event), typesel,
		"signal::button_press_event", G_CALLBACK (cb_button_press_event), typesel,
		"swapped_signal::focus_in_event", G_CALLBACK (typesel_set_selection_color), typesel,
		"swapped_signal::focus_out_event", G_CALLBACK (typesel_set_selection_color), typesel,
		NULL);
	gtk_widget_set_size_request (typesel->canvas,
		MINOR_PIXMAP_WIDTH*3 + BORDER*5,
		MINOR_PIXMAP_HEIGHT*3 + BORDER*5);
	foo_canvas_scroll_to (FOO_CANVAS (typesel->canvas), 0, 0);
	gtk_container_add (GTK_CONTAINER (glade_xml_get_widget (gui, "canvas_container")), 
			   typesel->canvas);

	/* Init the list and the canvas group for each family */
	g_hash_table_foreach ((GHashTable *)gog_plot_families (),
		(GHFunc) cb_plot_families_init, typesel);

	selection = gtk_tree_view_get_selection (typesel->list_view);
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
	g_signal_connect_swapped (selection,
		"changed",
		G_CALLBACK (cb_selection_changed), typesel);

	/* The alpha blended selection box */
	typesel->selector = foo_canvas_item_new (
		foo_canvas_root (FOO_CANVAS (typesel->canvas)),
		foo_canvas_rect_get_type (),
		"outline_color_rgba",	0x000000ff,	/* black */
		"width_pixels", 1,
		NULL);
	typesel_set_selection_color (typesel);

	/* Setup the description label */
	typesel->label = GTK_LABEL (glade_xml_get_widget (gui, "description_label"));

	/* Set up sample button */
	typesel->sample_button = glade_xml_get_widget (gui, "sample_button");
	g_signal_connect_swapped (G_OBJECT (typesel->sample_button),
		"pressed",
		G_CALLBACK (cb_sample_pressed), typesel);
	g_signal_connect_swapped (G_OBJECT (typesel->sample_button),
		"released",
		G_CALLBACK (cb_sample_released), typesel);

	g_object_set_data_full (G_OBJECT (selector),
		"state", typesel, (GDestroyNotify) g_free);

	g_object_unref (G_OBJECT (gui));

	return selector;
}

static gboolean
graph_guru_init (GraphGuruState *s)
{
	s->gui = go_libglade_new ("gog-guru.glade", NULL, NULL, s->cc);
        if (s->gui == NULL)
                return TRUE;

	s->dialog = glade_xml_get_widget (s->gui, "GraphGuru");
	s->steps  = GTK_NOTEBOOK (glade_xml_get_widget (s->gui, "notebook"));

	/* Buttons */
	s->button_cancel   = graph_guru_init_button (s, "button_cancel");
	s->button_navigate = graph_guru_init_button (s, "button_navigate");
	s->button_ok	   = graph_guru_init_ok_button (s);

//#warning FIXME move the docs down to libgoffice
	go_gtk_help_button_init	(glade_xml_get_widget (s->gui, "help_button"),
				 go_sys_data_dir (), "gnumeric",
				 "sect-graphics-plots");

	return FALSE;
}

/**
 * dialog_graph_guru
 * @wb : The workbook to use as a parent window.
 * @graph : the graph to edit
 * @page : the page to start on.
 *
 * Pop up a graph guru.
 */
GtkWidget *
gog_guru (GogGraph *graph, GogDataAllocator *dalloc,
	  GOCmdContext *cc, GtkWindow *toplevel,
	  GClosure *closure)
{
	int page = (graph != NULL) ? 1 : 0;
	GraphGuruState *state;

	state = g_new0 (GraphGuruState, 1);
	state->valid	= FALSE;
	state->updating = FALSE;
	state->fmt_page_initialized = FALSE;
	state->editing  = (graph != NULL);
	state->gui	= NULL;
	state->cc       = cc;
	state->dalloc   = dalloc;
	state->current_page	= -1;
	state->register_closure	= closure;
	g_closure_ref (closure);

	if (graph != NULL) {
		g_return_val_if_fail (IS_GOG_GRAPH (graph), NULL);

		state->graph = gog_graph_dup (graph);
		state->chart = NULL;
		state->plot  = NULL;
	} else {
		state->plot = NULL;
		state->graph = g_object_new (GOG_GRAPH_TYPE, NULL);
		state->chart = GOG_CHART (gog_object_add_by_name (
				GOG_OBJECT (state->graph), "Chart", NULL));
	}

	if (state->graph == NULL || graph_guru_init (state)) {
		graph_guru_state_destroy (state);
		return NULL;
	}

	/* Ok everything is hooked up. Let-er rip */
	state->valid = TRUE;

	state->initial_page = page;
	if (page == 0) {
		GtkWidget *w = graph_guru_type_selector_new (state);
		gtk_notebook_prepend_page (state->steps, w, NULL);
		gtk_widget_show_all (w);
	}

	graph_guru_set_page (state, page);

	/* a candidate for merging into attach guru */
	g_object_set_data_full (G_OBJECT (state->dialog),
		"state", state, (GDestroyNotify) graph_guru_state_destroy);
	go_gtk_nonmodal_dialog (toplevel, GTK_WINDOW (state->dialog));
	gtk_widget_show (GTK_WIDGET (state->dialog));

	return state->dialog;
}

#if 0
gtk_tree_sortable_set_sort_func (store, column, compare_rows,
				 GUINT_TO_POINTER (column), NULL);

for each column of your database table.

int
compare_rows (GtkTreeModel *model,
	      GtkTreeIter  *a,
	      GtkTreeIter  *b,
	      gpointer      user_data)
{
	int column = GPOINTER_TO_UINT (user_data);
	MyRow *row_a, row_b;

	gtk_tree_model_get (model, DATA_COLUMN, a, &row_a, -1);
	gtk_tree_model_get (model, DATA_COLUMN, b, &row_b, -1);

	return compare_cells (row_a->cells[column], row_b->cells[column]);
}


#endif
