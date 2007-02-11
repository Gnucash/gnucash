/********************************************************************\
 * gnc-frequency.h -- GnuCash widget for frequency editing.         *
 * Copyright (C) 2001,2002 Joshua Sled <jsled@asynchronous.org>     *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_FREQUENCY_H
#define GNC_FREQUENCY_H

#include "gnc-date-edit.h"
#include "FreqSpec.h"

#define GNC_TYPE_FREQUENCY	  (gnc_frequency_get_type())
#define GNC_FREQUENCY(obj)	  G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_FREQUENCY, GncFrequency)
#define GNC_FREQENCY_CLASS(klass) G_TYPE_CHECK_CLASS_CAST (klass, GNC_TYPE_FREQUENCY, GncFrequency)
#define GNC_IS_FREQUENCY(obj)     G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_FREQUENCY)

/**
 * A GncFrequency is a VBox containing a scrollable GtkNotebook [and other
 * widgets] which allows the user to specify the frequency [of a scheduled
 * transaction or budgeting category, for instance], manipulating a FreqSpec
 * object in the process.
 **/
typedef struct _GncFrequency 
{
	GtkVBox	        widget;

  	GtkVBox         *vb;
	GtkNotebook     *nb;
	GtkComboBox     *freqComboBox;
	GNCDateEdit     *startDate;
	GladeXML        *gxml;
} GncFrequency;

typedef struct _GncFrequencyClass
{
	GtkVBoxClass parent_class;

	void (*changed) (GncFrequency *gf);
} GncFrequencyClass;

struct pageDataTuple 
{
	int		idx;
	UIFreqType	uiFTVal;
	char		*name;
};

GType gnc_frequency_get_type( void );

/**
 * Create a new GncFrequencey widget.
 * The menus and panels will be initialized to correspond to the
 * settings in the FreqSpec, and the date window will show the 
 * indicated date.  Either or both may be NULL.
 * For the default freq spec widget, use 'NULL'.
 **/
GtkWidget* gnc_frequency_new(FreqSpec *fs, GDate *start_date);
void gnc_frequency_init( GncFrequency *gf );

/**
 * Change the given GncFrequency with the given FreqSpec and GDate.  
 * If the FreqSpec is NULL, then no change is made to the widget menus.
 * If the date is NULL, then no change is made to the widget date field.
 **/
void gnc_frequency_setup( GncFrequency *gf, FreqSpec *fs, GDate *date );

/**
 * Saves the state of the GNCFrequenecy widget.
 * Updates the given FreqSpec if it's not NULL.
 * Places the date in outDate, if it's not NULL.
 **/
void gnc_frequency_save_state( GncFrequency *gf, FreqSpec *fs, GDate *outDate);

/**
 * Set the label text for the frequency option menu.  In the current
 * implementation, the default label text is "Frequency:"
 */
void gnc_frequency_set_frequency_label_text (GncFrequency *gf, const gchar *txt);

/**
 * Set the label text for the date entry widget. In the current
 * impelmentation, the default label text is "Start Date:"
 */
void gnc_frequency_set_date_label_text (GncFrequency *gf, const gchar *txt);

void gnc_frequency_setup_default( GncFrequency *gf, FreqSpec *fs, GDate *date );

#endif /* !defined( GNC_FREQUENCY_H ) */
