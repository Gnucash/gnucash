/**
 * widget-number-format-selector.h:  Implements a widget to select number format.
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

#ifndef __WIDGET_FORMAT_SELECTOR_H__
#define __WIDGET_FORMAT_SELECTOR_H__

//#include <gui-gnumeric.h>
#include <gui-util.h>

#define NUMBER_FORMAT_SELECTOR_TYPE        (number_format_selector_get_type ())
#define NUMBER_FORMAT_SELECTOR(obj)        (G_TYPE_CHECK_INSTANCE_CAST((obj), NUMBER_FORMAT_SELECTOR_TYPE, NumberFormatSelector))
#define IS_NUMBER_FORMAT_SELECTOR(obj)     (G_TYPE_CHECK_INSTANCE_TYPE((obj), NUMBER_FORMAT_SELECTOR_TYPE))

typedef struct _NumberFormatSelector NumberFormatSelector;


GType		number_format_selector_get_type	(void);
GtkWidget * 	number_format_selector_new  	(void);

void		number_format_selector_set_focus (NumberFormatSelector *nfs);
void		number_format_selector_set_style_format (NumberFormatSelector *nfs,
							 GnmFormat *style_format);
void		number_format_selector_set_value (NumberFormatSelector *nfs,
						  GnmValue const *value);
void		number_format_selector_set_date_conv (NumberFormatSelector *nfs,
						      GnmDateConventions const *date_conv);
void		number_format_selector_editable_enters (NumberFormatSelector *nfs,
							GtkWindow *window);
void		number_format_selector_set_locale (NumberFormatSelector *nfs, 
						   char const *locale);

/* Number Format Selector Utilities */

char const *    number_format_selector_format_classification (GnmFormat const *style_format);

#endif /*__WIDGET_FORMAT_SELECTOR_H__*/

