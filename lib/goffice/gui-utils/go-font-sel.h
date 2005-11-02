#ifndef GO_FONT_SEL_H
#define GO_FONT_SEL_H

#include <gtk/gtkwindow.h>
#include <goffice/utils/goffice-utils.h>

#define GO_FONT_SEL_TYPE	(go_font_sel_get_type ())
#define GO_FONT_SEL(obj)	(G_TYPE_CHECK_INSTANCE_CAST((obj), GO_FONT_SEL_TYPE, GOFontSel))
#define IS_GO_FONT_SEL(obj)	(G_TYPE_CHECK_INSTANCE_TYPE((obj), GO_FONT_SEL_TYPE))

typedef struct _GOFontSel GOFontSel;

GType         go_font_sel_get_type (void);
GtkWidget    *go_font_sel_new      (void);
void	      go_font_sel_set_font (GOFontSel *fs, GOFont const *font);
GOFont const *go_font_sel_get_font (GOFontSel const *fs);
void go_font_sel_editable_enters   (GOFontSel *fs, GtkWindow *dialog);
void go_font_sel_set_sample_text   (GOFontSel *fs, char const *text);

#endif /* GO_FONT_SEL_H */
