/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifndef _GNCSEARCH_CORE_TYPE_H
#define _GNCSEARCH_CORE_TYPE_H

#include "qof.h"
#include "search-param.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_TYPE_SEARCH_CORE_TYPE		(gnc_search_core_type_get_type ())
G_DECLARE_DERIVABLE_TYPE (GNCSearchCoreType, gnc_search_core_type, GNC, SEARCH_CORE_TYPE, GObject)

struct _GNCSearchCoreTypeClass
{
    GObjectClass parent_class;

    /* virtual methods */
    void			(*grab_focus) (GNCSearchCoreType *fe);
    void			(*editable_enters) (GNCSearchCoreType *fe);
    void			(*pass_parent) (GNCSearchCoreType *fe, gpointer parent);
    gboolean		(*validate) (GNCSearchCoreType *fe);
    GNCSearchCoreType *	(*clone) (GNCSearchCoreType *fe);
    GtkWidget *		(*get_widget) (GNCSearchCoreType *);
    QofQueryPredData*	(*get_predicate) (GNCSearchCoreType *);
};

/* These are internal functions */
GNCSearchCoreType *	gnc_search_core_type_new (void);

/* Create a new search core_type */
GNCSearchCoreType *	gnc_search_core_type_new_type_name (const char *type);

/* methods */
void			gnc_search_core_type_grab_focus (GNCSearchCoreType *fe);
void			gnc_search_core_type_editable_enters (GNCSearchCoreType *fe);
void			gnc_search_core_type_pass_parent (GNCSearchCoreType *fe, gpointer parent);
gboolean        	gnc_search_core_type_validate (GNCSearchCoreType *fe);
GNCSearchCoreType *	gnc_search_core_type_clone (GNCSearchCoreType *fe);
GtkWidget *		gnc_search_core_type_get_widget (GNCSearchCoreType *fe);
QofQueryPredData*		gnc_search_core_type_get_predicate (GNCSearchCoreType *fe);

/* Register a new type in the Core Type Database */
typedef GNCSearchCoreType * (*GNCSearchCoreNew) (void);
void gnc_search_core_register_type (const char *type_name,
                                    GNCSearchCoreNew fcn);


/* Initialize and Finalize the Core Type Database */
void gnc_search_core_initialize (void);
void gnc_search_core_finalize (void);

#ifdef __cplusplus
}
#endif

#endif /* ! _GNCSEARCH_CORE_TYPE_H */
