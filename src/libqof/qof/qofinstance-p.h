/********************************************************************\
 * qofinstance-p.h -- private fields common to all object instances *
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
/*
 * Object instance holds many common fields that most
 * gnucash objects use.
 *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2007 David Hampton <hampton@employees.org>
 */

#ifndef QOF_INSTANCE_P_H
#define QOF_INSTANCE_P_H

#include "qofinstance.h"

#ifdef __cplusplus
extern "C"
{
#endif

/** Set the collection this instance belongs to.  This function should never
 *  be called by user code. Instead call the qof_collection_insert_entity()
 *  function. */
void qof_instance_set_collection (gconstpointer ptr, QofCollection *col);

void qof_instance_set_slots (QofInstance *, KvpFrame *);

/*  Set the last_update time. Reserved for use by the SQL backend;
 *  used for comparing version in local memory to that in remote
 *  server.
 */
void qof_instance_set_last_update (QofInstance *inst, Timespec ts);

/** Set the dirty flag of just the instance. Don't modify the
 *  collection flag at all. */
void qof_instance_set_dirty_flag (gconstpointer inst, gboolean flag);

/** Set the GncGUID of this instance */
void qof_instance_set_guid (gpointer inst, const GncGUID *guid);

/** Copy the GncGUID from one instance to another.  This routine should
 *  be used with extreme caution, since GncGUID values are everywhere
 *  assumed to be unique. */
void qof_instance_copy_guid (gpointer to, gconstpointer from);

//QofIdType qof_instance_get_e_type (const QofInstance *inst);
//void qof_instance_set_e_type (QofInstance *ent, QofIdType e_type);

/** Return the pointer to the kvp_data */
/*@ dependent @*/
KvpFrame* qof_instance_get_slots (const QofInstance *);
void qof_instance_set_editlevel(gpointer inst, gint level);
void qof_instance_increase_editlevel (gpointer ptr);
void qof_instance_decrease_editlevel (gpointer ptr);
void qof_instance_reset_editlevel (gpointer ptr);
/** Set the flag that indicates whether or not this object is about to
 *  be destroyed.
 *
 *  @param ptr The object whose flag should be set.
 *
 *  @param value The new value to be set for this object. */
void qof_instance_set_destroying (gpointer ptr, gboolean value);

/** \brief Set the dirty flag
Sets this instance AND the collection as dirty.
*/
void qof_instance_set_dirty(QofInstance* inst);

/* reset the dirty flag */
void qof_instance_mark_clean (QofInstance *);
/** Get the version number on this instance.  The version number is
 *  used to manage multi-user updates. */
gint32 qof_instance_get_version (gconstpointer inst);

/** Set the version number on this instance.  The version number is
 *  used to manage multi-user updates. */
void qof_instance_set_version (gpointer inst, gint32 value);
/** Copy the version number on this instance.  The version number is
 *  used to manage multi-user updates. */
void qof_instance_copy_version (gpointer to, gconstpointer from);

/** Get the instance version_check number */
guint32 qof_instance_get_version_check (gconstpointer inst);
/** Set the instance version_check number */
void qof_instance_set_version_check (gpointer inst, guint32 value);
/** copy the instance version_check number */
void qof_instance_copy_version_check (gpointer to, gconstpointer from);
void qof_instance_set_idata(gpointer inst, guint32 idata);
/* Convenience functions to save some typing in property handlers */
void qof_instance_set_kvp (QofInstance *inst, const gchar *key, const GValue *value);
void qof_instance_get_kvp (QofInstance *inst, const gchar *key, GValue *value);

#ifdef __cplusplus
}
#endif


#endif /* QOF_INSTANCE_P_H */
