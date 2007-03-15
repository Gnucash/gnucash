/********************************************************************\
 * SX-book.h -- scheduled transaction dataset access                *
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

/**
 * @addtogroup Engine
 * @{ */
/**
 * @addtogroup SchedXaction
 * @{ */
/**
 * @file SX-book.h
 * @brief Anchor Scheduled Transaction info in a book.
 *        See src/doc/books.txt for design overview.
 * @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2006 Joshua Sled <jsled@asynchronous.org>
 * 
 * XXX currently, this is crufty, it should be modified to use
 * entities a bit more whole-heartedly than it does.
 **/

#ifndef GNC_SX_BOOK_H
#define GNC_SX_BOOK_H

#include <glib.h>
#include "SchedXaction.h"
#include "qof.h"

/* GObject declarations */

#define GNC_TYPE_SCHEDULE_ACTIONS            (gnc_schedule_actions_get_type ())
#define GNC_SCHEDULE_ACTIONS(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_SCHEDULE_ACTIONS, GncScheduleActions))
#define GNC_SCHEDULE_ACTIONS_CLASS(k)        (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_SCHEDULE_ACTIONS, GncScheduleActionsClass))
#define GNC_IS_SCHEDULE_ACTIONS(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_SCHEDULE_ACTIONS))
#define GNC_IS_SCHEDULE_ACTIONS_CLASS(k)     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_SCHEDULE_ACTIONS))
#define GNC_SCHEDULE_ACTIONS_GET_CLASS(o)    (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_SCHEDULE_ACTIONS, GncScheduleActionsClass))


typedef struct _GncScheduleActionsClass GncScheduleActionsClass;
typedef struct _GncScheduleActionsPrivate GncScheduleActionsPrivate;
typedef struct _GncScheduleActions GncScheduleActions;
typedef GncScheduleActions SchedXactions ; /* Backward compatibility*/

struct _GncScheduleActions {
	QofInstance inst;
  GList* sx_list;
  gboolean sx_notsaved;
};

struct _GncScheduleActionsClass {
	QofInstanceClass parent_class;
	/* virtual table */
	 
	/* Add Signal Functions Here */
};

GType   gnc_schedule_actions_get_type (void);


#define GNC_IS_SXES(obj)  GNC_IS_SCHEDULE_ACTIONS(o)
#define GNC_SXES(obj)     GNC_SCHEDULE_ACTIONS(o)

SchedXactions* gnc_book_get_schedxactions(QofBook* book);

void gnc_sxes_add_sx(SchedXactions* sxes, SchedXaction* sx);
void gnc_sxes_del_sx(SchedXactions* sxes, SchedXaction* sx);

/** Returns the template group from the book. **/
AccountGroup* gnc_book_get_template_group(QofBook* book);
AccountGroup* gnc_collection_get_template_group(const QofCollection *col);

/** @return The list of SXes which reference the given Account. Caller should free this list. **/
GList* gnc_sx_get_sxes_referencing_account(QofBook *book, Account *acct);

#endif /* GNC_SX_BOOK_H */
/** @} */
/** @} */
