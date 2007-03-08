#ifndef __PLANNER_CELL_RENDERER_POPUP_H__
#define __PLANNER_CELL_RENDERER_POPUP_H__

#include <pango/pango.h>
#include <gtk/gtkcellrenderertext.h>

#define PLANNER_TYPE_CELL_RENDERER_POPUP		(planner_cell_renderer_popup_get_type ())
#define PLANNER_CELL_RENDERER_POPUP(obj)		(GTK_CHECK_CAST ((obj), PLANNER_TYPE_CELL_RENDERER_POPUP, PlannerCellRendererPopup))
#define PLANNER_CELL_RENDERER_POPUP_CLASS(klass)	(GTK_CHECK_CLASS_CAST ((klass), PLANNER_TYPE_CELL_RENDERER_POPUP, PlannerCellRendererPopupClass))
#define PLANNER_IS_CELL_RENDERER_POPUP(obj)		(GTK_CHECK_TYPE ((obj), PLANNER_TYPE_CELL_RENDERER_POPUP))
#define PLANNER_IS_CELL_RENDERER_POPUP_CLASS(klass)	(GTK_CHECK_CLASS_TYPE ((obj), PLANNER_TYPE_CELL_RENDERER_POPUP))
#define PLANNER_CELL_RENDERER_POPUP_GET_CLASS(obj)   (GTK_CHECK_GET_CLASS ((obj), PLANNER_TYPE_CELL_RENDERER_POPUP, PlannerCellRendererPopupClass))

typedef struct _PlannerCellRendererPopup      PlannerCellRendererPopup;
typedef struct _PlannerCellRendererPopupClass PlannerCellRendererPopupClass;

struct _PlannerCellRendererPopup
{
	GtkCellRendererText  parent;

	/* Cached width of the popup button. */
	gint                 button_width;
	
	/* The popup window. */
	GtkWidget           *popup_window;

	/* The widget that should grab focus on popup. */
	GtkWidget           *focus_window;

	/* The editable entry. */
	GtkWidget           *editable;

	gboolean             shown;
	gboolean             editing_canceled;
};

struct _PlannerCellRendererPopupClass
{
	GtkCellRendererTextClass parent_class;
	
	void   (* show_popup) (PlannerCellRendererPopup *cell,
			       const gchar         *path,
			       gint                 x1,
			       gint                 y1,
			       gint                 x2,
			       gint                 y2);
	
	void   (* hide_popup) (PlannerCellRendererPopup *cell);
};

GtkType          planner_cell_renderer_popup_get_type (void) G_GNUC_CONST;

GtkCellRenderer *planner_cell_renderer_popup_new      (void);

void             planner_cell_renderer_popup_show     (PlannerCellRendererPopup *cell,
						  const gchar         *path,
						  gint                 x1,
						  gint                 y1,
						  gint                 x2,
						  gint                 y2);

void             planner_cell_renderer_popup_hide     (PlannerCellRendererPopup *cell);

#endif /* __PLANNER_CELL_RENDERER_POPUP_H__ */
