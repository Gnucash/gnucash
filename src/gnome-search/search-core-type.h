/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef _GNCSEARCH_CORE_TYPE_H
#define _GNCSEARCH_CORE_TYPE_H

#include <gnome.h>
#include "QueryNew.h"
#include "search-param.h"

#define GNCSEARCH_CORE_TYPE(obj)	GTK_CHECK_CAST (obj, gnc_search_core_type_get_type (), GNCSearchCoreType)
#define GNCSEARCH_CORE_TYPE_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gnc_search_core_type_get_type (), GNCSearchCoreTypeClass)
#define IS_GNCSEARCH_CORE_TYPE(obj)      GTK_CHECK_TYPE (obj, gnc_search_core_type_get_type ())

typedef struct _GNCSearchCoreType	GNCSearchCoreType;
typedef struct _GNCSearchCoreTypeClass	GNCSearchCoreTypeClass;

struct _GNCSearchCoreType {
  GtkObject parent;
  struct _GNCSearchCoreTypePrivate *priv;

  GNCSearchParam *	param;
};

struct _GNCSearchCoreTypeClass {
  GtkObjectClass parent_class;

  /* virtual methods */
  void			(*grab_focus) (GNCSearchCoreType *fe);
  void			(*editable_enters) (GNCSearchCoreType *fe,
					    GnomeDialog *dialog);
  gboolean		(*validate) (GNCSearchCoreType *fe);
  GNCSearchCoreType *	(*clone) (GNCSearchCoreType *fe);
  GtkWidget *		(*get_widget) (GNCSearchCoreType *);
  QueryPredData_t	(*get_predicate) (GNCSearchCoreType *);

  /* signals */
};

/* These are internal functions */
guint			gnc_search_core_type_get_type (void);
GNCSearchCoreType *	gnc_search_core_type_new (void);

/* Create a new search core_type */
GNCSearchCoreType *	gnc_search_core_type_new_type_name (const char *type);

/* methods */
void			gnc_search_core_type_grab_focus (GNCSearchCoreType *fe);
void			gnc_search_core_type_editable_enters (GNCSearchCoreType *fe,
							      GnomeDialog *dialog);
gboolean        	gnc_search_core_type_validate (GNCSearchCoreType *fe);
GNCSearchCoreType *	gnc_search_core_type_clone (GNCSearchCoreType *fe);
GtkWidget *		gnc_search_core_type_get_widget (GNCSearchCoreType *fe);
QueryPredData_t		gnc_search_core_type_get_predicate (GNCSearchCoreType *fe);

/* Register a new type in the Core Type Database */
typedef GNCSearchCoreType * (*GNCSearchCoreNew) (void);
void gnc_search_core_register_type (const char *type_name,
				    GNCSearchCoreNew fcn);


/* Initialize and Finalize the Core Type Database */
void gnc_search_core_initialize (void);
void gnc_search_core_finalize (void);


#endif /* ! _GNCSEARCH_CORE_TYPE_H */
