/*
 * gnc-general-search.h -- Widget to pop-up a search dialog and show
 *			the selected item.
 *
 * Copyright (C) 2001 Free Software Foundation
 * All rights reserved.
 *
 * Derek Atkins <warlord@MIT.EDU>
 *
 * GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#ifndef GNC_GENERAL_SEARCH_H
#define GNC_GENERAL_SEARCH_H

#include "dialog-search.h"
#include "gnc-book.h"

#define GNC_TYPE_GENERAL_SEARCH \
	(gnc_general_search_get_type ())

#define GNC_GENERAL_SEARCH(obj) \
	G_TYPE_CHECK_INSTANCE_CAST (obj, GNC_TYPE_GENERAL_SEARCH, GNCGeneralSearch)

#define GNC_GENERAL_SEARCH_CLASS(klass) \
	G_TYPE_CLASS_CAST (klass, GNC_TYPE_GENERAL_SEARCH, \
				GNCGeneralSearchClass)
#define GNC_IS_GENERAL_SEARCH(obj) \
	G_TYPE_CHECK_INSTANCE_TYPE (obj, GNC_TYPE_GENERAL_SEARCH)

/*
 * If this returns NULL, then do nothing (probably an edit window).  If
 * it actually returns a search-window, then this widget will set up the
 * appropriate callbacks to the search window to obtain selections.
 */
typedef GNCSearchWindow *(*GNCSearchCB) (gpointer start, gpointer user_data);

typedef struct
{
    GtkHBox hbox;

    GtkWidget *	entry;  /* display of selection name */
    GtkWidget *	button; /* button for popping up search window */

    gpointer	selected_item;

    gboolean		allow_clear;
} GNCGeneralSearch;

typedef struct
{
    GtkHBoxClass parent_class;

    void 		(*changed) (GNCGeneralSearch *edit);
} GNCGeneralSearchClass;


GtkWidget *gnc_general_search_new            (GNCIdTypeConst type,
        const char *label,
        gboolean text_editable,
        GNCSearchCB search_cb,
        gpointer user_data,
        QofBook *book);

void	   gnc_general_search_allow_clear    (GNCGeneralSearch *gsl,
        gboolean allow_clear);
void       gnc_general_search_set_selected   (GNCGeneralSearch *gsl,
        gpointer searched);
gpointer   gnc_general_search_get_selected   (GNCGeneralSearch *gsl);

GType      gnc_general_search_get_type       (void);

#endif

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
