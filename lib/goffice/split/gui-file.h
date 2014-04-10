#ifndef GNUMERIC_GUI_FILE_H
#define GNUMERIC_GUI_FILE_H

#include "gui-gnumeric.h"

typedef struct {
	char *name;
	char *desc;
	char *ext;
	gboolean has_pixbuf_saver;
} GnmImageFormat;

gboolean gui_file_save_as   (WorkbookControlGUI *wbcg, WorkbookView *);
gboolean gui_file_save      (WorkbookControlGUI *wbcg, WorkbookView *);
void     gui_file_open      (WorkbookControlGUI *wbcg, 
			     char const *default_format);
void     gui_wb_view_show   (WorkbookControlGUI *wbcg, WorkbookView *wbv);
gboolean gui_file_read	    (WorkbookControlGUI *wbcg, char const *file_name,
			     GnmFileOpener const *optional_format,
			     gchar const *optional_encoding);
char *   gui_image_file_select (WorkbookControlGUI *wbcg, const char *initial);
char *   gui_get_image_save_info (WorkbookControlGUI *wbcg, GSList *formats, 
				  GnmImageFormat **ret_format);

#endif /* GNUMERIC_GUI_FILE_H */
