#ifndef GNUMERIC_WIDGET_FONT_SELECTOR_H
#define GNUMERIC_WIDGET_FONT_SELECTOR_H

#include <gui-gnumeric.h>
#include <style.h>
///#include <libfoocanvas/foo-canvas.h>
#include <gtk/gtkwindow.h>

#define FONT_SELECTOR_TYPE        (font_selector_get_type ())
#define FONT_SELECTOR(obj)        (G_TYPE_CHECK_INSTANCE_CAST((obj), FONT_SELECTOR_TYPE, FontSelector))
#define IS_FONT_SELECTOR(obj)     (G_TYPE_CHECK_INSTANCE_TYPE((obj), FONT_SELECTOR_TYPE))

typedef struct _FontSelector FontSelector;

GType    font_selector_get_type (void);
GtkWidget *font_selector_new      (void);

void font_selector_set_value       (FontSelector *fs, GnmValue const *v);
void font_selector_set_name        (FontSelector *fs, char const *font_name);
void font_selector_set_style       (FontSelector *fs,
				    gboolean is_bold, gboolean is_italic);
void font_selector_set_underline   (FontSelector *fs, StyleUnderlineType sut);
void font_selector_set_strike      (FontSelector *fs, gboolean strikethrough);
void font_selector_set_color       (FontSelector *fs, GnmColor *color);
void font_selector_set_points      (FontSelector *fs, double point_size);
void font_selector_editable_enters (FontSelector *fs, GtkWindow *dialog);

void font_selector_set_from_pango  (FontSelector *fs, PangoFontDescription const *desc);
void font_selector_get_pango  	   (FontSelector *fs, PangoFontDescription *desc);

#endif /* GNUMERIC_WIDGET_FONT_SELECTOR_H */

