/**
 * go-format-sel.h: A widget to select a format
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 **/

#ifndef _GO_FORMAT_SEL_H_
#define _GO_FORMAT_SEL_H_

#include <gtk/gtkwindow.h>
#include <goffice/utils/goffice-utils.h>

G_BEGIN_DECLS

#define GO_FORMAT_SEL_TYPE	(go_format_sel_get_type ())
#define GO_FORMAT_SEL(o)	(G_TYPE_CHECK_INSTANCE_CAST((o), GO_FORMAT_SEL_TYPE, GOFormatSel))
#define IS_GO_FORMAT_SEL(o)	(G_TYPE_CHECK_INSTANCE_TYPE((o), GO_FORMAT_SEL_TYPE))
typedef struct _GOFormatSel	GOFormatSel;

GType		go_format_sel_get_type	(void);
GtkWidget * 	go_format_sel_new  	(void);

void		go_format_sel_set_focus (GOFormatSel *gfs);
void		go_format_sel_set_style_format (GOFormatSel *gfs,
						GOFormat *style_format);
void		go_format_sel_set_locale (GOFormatSel *gfs, 
					  char const *locale);

GOFormat *go_format_sel_get_fmt		(GOFormatSel *gfs);

GODateConventions const *
	  go_format_sel_get_dateconv	(GOFormatSel *gfs);
void	  go_format_sel_set_dateconv	(GOFormatSel *gfs,
					 GODateConventions const *date_conv);

void	  go_format_sel_show_preview	(GOFormatSel *gfs);
void	  go_format_sel_hide_preview	(GOFormatSel *gfs);
void	  go_format_sel_editable_enters	(GOFormatSel *gfs,
					 GtkWindow *window);

/* FIXME FIXME FIXME does not belong here */
char const *go_format_sel_format_classification (GOFormat const *style_format);

G_END_DECLS

#endif /* _GO_FORMAT_SEL_H_ */
