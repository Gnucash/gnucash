/********************************************************************\
 * gnc-combott.h -- Basic simulation of ComboBox with tooltips for  *
 *                  each item.                                      *
 * Copyright (c) 2012 Robert Fewell                                 *
 *                                                                  *
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
 * This widget requires external ListStore which has two columns.   *
 * By default, column 0 holds the text to display and column 1 the  *
 * per item tooltip but these can be specified if the liststore has *
 * a different format.                                              *
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
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
\********************************************************************/

#ifndef __GNC_COMBOTT_H__
#define __GNC_COMBOTT_H__

/* type macros */
#define GNC_TYPE_COMBOTT            (gnc_combott_get_type ())
#define GNC_COMBOTT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_COMBOTT, GncCombott))
#define GNC_COMBOTT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_COMBOTT, GncCombottClass))
#define GNC_IS_COMBOTT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_COMBOTT))
#define GNC_IS_COMBOTT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_COMBOTT))
#define GNC_COMBOTT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_COMBOTT, GncCombottClass))
#define GNC_COMBOTT_NAME            "GncCombott"

/* typedefs & structures */
typedef struct _GncCombott      GncCombott;
typedef struct _GncCombottClass GncCombottClass;

struct _GncCombott
{
    GtkBox hbox;
};

struct _GncCombottClass
{
    GtkButtonClass parent;
    void   (* changed) (GncCombott *combott);
};

/* Standard g_object type */
GType            gnc_combott_get_type (void);
GncCombott	*gnc_combott_new (void);

gint             gnc_combott_get_active       (GncCombott *combott);
void             gnc_combott_set_active       (GncCombott *combott, gint index);

#endif /* __GNC_COMBOTT_H__ */

