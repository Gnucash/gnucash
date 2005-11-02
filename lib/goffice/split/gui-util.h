#ifndef GNUMERIC_GUI_UTIL_H
#define GNUMERIC_GUI_UTIL_H

#include "workbook-control-gui.h"
#include "error-info.h"
#include "command-context.h"
#include "gnumeric.h"
#include "gutils.h"
#include <gtk/gtkbutton.h>
#include <gtk/gtkcombo.h>
#include <gtk/gtkfilesel.h>
#include <gtk/gtkmessagedialog.h>
#include <gtk/gtkmenu.h>
#include <gtk/gtktoolbar.h>
#include <gtk/gtktextview.h>
#include <gtk/gtkentry.h>
#include <glade/glade-xml.h>

#define GNM_ACTION_DEF(name)			\
	void name (GtkAction *a, WorkbookControlGUI *wbcg)
gboolean   gnumeric_dialog_question_yes_no (GtkWindow *toplevel,
                                            char const *message,
                                            gboolean default_answer);
gboolean   gnumeric_dialog_file_selection (WorkbookControlGUI *wbcg, 
					   GtkWidget *w);
void       gnumeric_notice (GtkWindow *parent, GtkMessageType type, 
			    char const *str);
void       gnumeric_notice_nonmodal (GtkWindow *parent, GtkWidget **ref,
				     GtkMessageType type, char const *str);

void       gnumeric_non_modal_dialog (GtkWindow *toplevel, GtkWindow *dialog);
gint       gnumeric_dialog_run  (GtkWindow *parent, GtkDialog *dialog);
GtkWidget* gnumeric_error_info_dialog_new (ErrorInfo *error);
void       gnumeric_error_info_dialog_show (GtkWindow *parent,
                                            ErrorInfo *error);
void       gnumeric_set_transient (GtkWindow *parent, GtkWindow *window);
void       gnumeric_keyed_dialog (WorkbookControlGUI *wbcg,
				  GtkWindow *dialog,
				  char const *key);
gpointer   gnumeric_dialog_raise_if_exists (WorkbookControlGUI *wbcg,
					    char const *key);
void       gnumeric_editable_enters	(GtkWindow *window, GtkWidget *w);

/* Utility routine as Gtk does not have any decent routine to do this */
int gtk_radio_group_get_selected (GSList *radio_group);
/* Utility routine as libglade does not have any decent routine to do this */
int gnumeric_glade_group_value (GladeXML *gui, char const *group[]);

/* Use this on menus that are popped up */
void gnumeric_popup_menu (GtkMenu *menu, GdkEventButton *event);

/*
 * Pseudo-tool-tip support code.
 */
void        gnumeric_position_tooltip (GtkWidget *tip, int horizontal);
GtkWidget  *gnumeric_create_tooltip (void);

GladeXML   *gnm_glade_xml_new (GnmCmdContext *cc, char const *gladefile,
			       char const *root, char const *domain);

typedef struct {
	char const *name;
	char const *pixmap;
	int display_filter;
	int sensitive_filter;

	int index;
} GnumericPopupMenuElement;

typedef gboolean (*GnumericPopupMenuHandler) (GnumericPopupMenuElement const *e,
					      gpointer user_data);

void gnumeric_create_popup_menu (GnumericPopupMenuElement const *elements,
				 GnumericPopupMenuHandler handler,
				 gpointer user_data,
				 int display_filter,
				 int sensitive_filter,
				 GdkEventButton *event);

#define gnumeric_filter_modifiers(a) ((a) &(~(GDK_LOCK_MASK|GDK_MOD2_MASK|GDK_MOD5_MASK)))

GnmColor *go_combo_color_get_style_color (GtkWidget *color_combo);

void gnumeric_help_display	(char const *link);
void gnumeric_init_help_button	(GtkWidget *w, char const *link);
void gnumeric_pbox_init_help	(GtkWidget *dialog, char const *link);

char *gnumeric_textview_get_text (GtkTextView *text_view);
void  gnumeric_textview_set_text (GtkTextView *text_view, char const *txt);

void focus_on_entry (GtkEntry *entry);

/* WARNING : These do not handle dates correctly
 * We should be passing in a DateConvention */
#define entry_to_float(entry, the_float, update)	\
	entry_to_float_with_format (entry, the_float, update, NULL)
gboolean entry_to_float_with_format (GtkEntry *entry, gnm_float *the_float, gboolean update,
				     GnmFormat *format);
gboolean entry_to_float_with_format_default (GtkEntry *entry, gnm_float *the_float, gboolean update,
					     GnmFormat *format, gnm_float num);
gboolean entry_to_int	(GtkEntry *entry, gint *the_int, gboolean update);
void	 float_to_entry	(GtkEntry *entry, gnm_float the_float);
void	 int_to_entry	(GtkEntry *entry, gint the_int);

GtkWidget *gnumeric_load_image  (char const *name);
GdkPixbuf *gnumeric_load_pixbuf (char const *name);
char	  *gnumeric_icondir     (char const *subdir);

GdkPixbuf *gnm_pixbuf_tile (GdkPixbuf const *src, int w, int h);

void gnm_setup_label_atk (GtkWidget *label, GtkWidget *target);

int gnm_measure_string (PangoContext *context, PangoFontDescription const *font_desc, char const *str);

void gnm_link_button_and_entry (GtkWidget *button, GtkWidget *entry);

void gnm_widget_set_cursor_type (GtkWidget *w, GdkCursorType ct);
void gnm_widget_set_cursor (GtkWidget *w, GdkCursor *ct);
GdkCursor *gnm_fat_cross_cursor (GdkDisplay *display);

GtkWidget * gnumeric_button_new_with_stock_image (char const *text, char const *stock_id);
GtkWidget * gnumeric_dialog_add_button (GtkDialog *dialog, char const *text, char const *stock_id,
					gint response_id);
GtkWidget * gnumeric_message_dialog_new (GtkWindow * parent,
					 GtkDialogFlags flags,
					 GtkMessageType type,
					 char const *primary_message,
					 char const *secondary_message);

GdkPixbuf* gnm_pixbuf_intelligent_scale (GdkPixbuf *pixbuf, 
					 guint width, guint height);
void	   gnm_widget_disable_focus (GtkWidget *w);

typedef gboolean gnm_iter_search_t (GtkTreeModel *model, GtkTreeIter* iter);
#define gnm_tree_model_iter_next gtk_tree_model_iter_next
gboolean gnm_tree_model_iter_prev (GtkTreeModel *model, GtkTreeIter* iter);


#endif /* GNUMERIC_GUI_UTIL_H */
