#ifndef GNUMERIC_WORKBOOK_CONTROL_GUI_H
#define GNUMERIC_WORKBOOK_CONTROL_GUI_H

//#include "workbook-control.h"
#include "gnumeric.h"

#include "gui-gnumeric.h"
#include <gtk/gtkwindow.h>
#include <gtk/gtktoggleaction.h>

#define WORKBOOK_CONTROL_GUI_TYPE     (workbook_control_gui_get_type ())
#define WORKBOOK_CONTROL_GUI(obj)     (G_TYPE_CHECK_INSTANCE_CAST ((obj), WORKBOOK_CONTROL_GUI_TYPE, WorkbookControlGUI))
#define IS_WORKBOOK_CONTROL_GUI(o)    (G_TYPE_CHECK_INSTANCE_TYPE ((o), WORKBOOK_CONTROL_GUI_TYPE))

GType    	 workbook_control_gui_get_type  (void);
WorkbookControl *workbook_control_gui_new	(WorkbookView *optional_view,
						 Workbook *optional_wb,
						 GdkScreen *optional_screen);

int      wbcg_sheet_to_page_index (WorkbookControlGUI *wbcg, Sheet *sheet,
				   SheetControlGUI **res);
GtkWindow	*wbcg_toplevel	  (WorkbookControlGUI *wbcg);
void	         wbcg_set_transient (WorkbookControlGUI *wbcg,
				     GtkWindow *window);
SheetControlGUI *wbcg_cur_scg	  (WorkbookControlGUI *wbcg);
Sheet		*wbcg_cur_sheet	  (WorkbookControlGUI *wbcg);
Sheet		*wbcg_focus_cur_scg (WorkbookControlGUI *wbcg);

gboolean   wbcg_ui_update_begin	  (WorkbookControlGUI *wbcg);
void	   wbcg_ui_update_end	  (WorkbookControlGUI *wbcg);

gboolean   wbcg_rangesel_possible (WorkbookControlGUI const *wbcg);
gboolean   wbcg_is_editing	  (WorkbookControlGUI const *wbcg);
void       wbcg_autosave_cancel	  (WorkbookControlGUI *wbcg);
void       wbcg_autosave_set      (WorkbookControlGUI *wbcg,
				   int minutes, gboolean prompt);
void	   wbcg_set_status_text	  (WorkbookControlGUI *wbcg,
				   char const *text);
void       wbcg_toggle_visibility (WorkbookControlGUI *wbcg,
				   GtkToggleAction *action);
void       wbcg_copy_toolbar_visibility (WorkbookControlGUI *new_wbcg,
					 WorkbookControlGUI *wbcg);

void       wbcg_toggle_end_mode   (WorkbookControlGUI *wbcg);
void       wbcg_set_end_mode      (WorkbookControlGUI *wbcg, gboolean flag);

PangoFontDescription *wbcg_get_font_desc (WorkbookControlGUI *wbcg);

#endif /* GNUMERIC_WORKBOOK_CONTROL_GUI_H */
