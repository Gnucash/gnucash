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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

#ifndef GNC_GENERAL_SEARCH_H
#define GNC_GENERAL_SEARCH_H

#include <gnome.h>
#include "dialog-search.h"
#include "gnc-book.h"

BEGIN_GNOME_DECLS


#define GNC_GENERAL_SEARCH(obj) \
	GTK_CHECK_CAST (obj, gnc_general_search_get_type(), GNCGeneralSearch)

#define GNC_GENERAL_SEARCH_CLASS(klass) \
	GTK_CHECK_CLASS_CAST (klass, gnc_general_search_get_type(), \
				GNCGeneralSearchClass)
#define GNC_IS_GENERAL_SEARCH(obj) \
	GTK_CHECK_TYPE (obj, gnc_general_search_get_type ())

/*
 * If this returns NULL, then do nothing (probably an edit window).  If
 * it actually returns a search-window, then this widget will set up the
 * appropriate callbacks to the search window to obtain selections.
 */
typedef GNCSearchWindow *(*GNCSearchCB) (gpointer start, gpointer user_data);

typedef struct {
  GtkHBox hbox;
  struct _GNCGeneralSearchPrivate	*priv;

  GtkWidget *	entry;  /* display of selection name */
  GtkWidget *	button; /* button for popping up search window */

  gpointer	selected_item;

  gboolean		allow_clear;
} GNCGeneralSearch;

typedef struct {
  GtkHBoxClass parent_class;

  void 		(*changed) (GNCGeneralSearch *edit);
} GNCGeneralSearchClass;


GtkWidget *gnc_general_search_new            (GNCIdTypeConst type,
					      const char *label,
					      GNCSearchCB search_cb,
					      gpointer user_data);

void	   gnc_general_search_allow_clear    (GNCGeneralSearch *gsl,
					      gboolean allow_clear);
void       gnc_general_search_set_selected   (GNCGeneralSearch *gsl,
					      gpointer searched);
gpointer   gnc_general_search_get_selected   (GNCGeneralSearch *gsl);

guint      gnc_general_search_get_type       (void);


END_GNOME_DECLS

#endif

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
