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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_FREQUENCY_H
#define GNC_FREQUENCY_H

#include <gnome.h>
#include "gnc-date-edit.h"
#include "FreqSpec.h"
#include "dialog-utils.h"

BEGIN_GNOME_DECLS

#define GNC_FREQUENCY(obj)	  GTK_CHECK_CAST(obj, gnc_frequency_get_type(), GNCFrequency)
#define GNC_FREQENCY_CLASS(klass) GTK_CHECK_CLASS_CAST(klass, gnc_frequency_get_type(), GNCFrequency)
#define GNC_IS_FREQUENCY(obj)     GTK_CHECK_TYPE(obj, gnc_frequency_get_type())

/**
 * A GNCFrequency is a VBox containing a scrollable GtkNotebook [and other
 * widgets] which allows the user to specify the frequency [of a scheduled
 * transaction or budgeting category, for instance], manipulating a FreqSpec
 * object in the process.
 **/
typedef struct _GNCFrequency 
{
	GtkVBox	        widget;

  	GtkVBox         *vb;
	GtkNotebook     *nb;
	GtkOptionMenu   *freqOpt;
	GNCDateEdit     *startDate;
	GladeXML        *gxml;
} GNCFrequency;

typedef struct _GNCFrequencyClass 
{
  GtkVBoxClass parent_class;
  void (*changed) (GNCFrequency *gf);
} GNCFrequencyClass;

struct pageDataTuple 
{
	int		idx;
	UIFreqType	uiFTVal;
	char		*name;
};

guint gnc_frequency_get_type( void );

/**
 * For the default freq spec widget, use 'NULL'.
 **/
GtkWidget * gnc_frequency_new( FreqSpec *fs, GDate *startDate );
void gnc_frequency_init( GNCFrequency *gf );

/**
 * Sets up the given GNCFrequency with the given FreqSpec and
 * UIFreqSpec.  If the FreqSpec is NULL, then the default value is
 * Daily; if the UIFreqSpec is not NONE, then that value is the
 * default.  If the FreqSpec is non-NULL, then it really should agree
 * with the UIFreqSpec; this is considered a 'critical' error.
 **/
void gnc_frequency_setup( GNCFrequency *gf, FreqSpec *fs, GDate *startDate );

/**
 * Saves the state of the GNCFrequenecy widget.
 * Updates the given FreqSpec if it's not NULL.
 * Places the date in outDate, if it's not NULL.
 **/
void gnc_frequency_save_state( GNCFrequency *gf, FreqSpec *fs, GDate *outDate);

/**
 * Set the label text for the frequency option menu.  In the current
 * implementation, the default label text is "Frequency:"
 */
void gnc_frequency_set_frequency_label_text (GNCFrequency *gf, const gchar *txt);
/**
 * Set the label text for the date entry widget. In the current
 * impelmentation, the default label text is "Start Date:"
 */
void gnc_frequency_set_date_label_text (GNCFrequency *gf, const gchar *txt);

END_GNOME_DECLS

#endif /* !defined( GNC_FREQUENCY_H ) */
