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
#include "kvp-frame.hpp"
#include <string>
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
/** @ingroup KVP
 * @{ */
/** Report whether a QofInstance has anything stored in KVP
 * @param inst The QofInstance
 * @return TRUE if Kvp isn't empty.
 */
gboolean qof_instance_has_kvp (QofInstance *inst);
/** Sets a KVP slot to a value from a GValue. The key can be a '/'-delimited
 * path, and intermediate container frames will be created if necessary.
 * Commits the change to the QofInstance.
 * @param inst: The QofInstance on which to set the value.
 * @param key: The key for the slot or '/'-delimited path
 * @param value: A GValue containing an item of a type which KvpValue knows
 * how to store.
 */
void qof_instance_set_kvp (QofInstance *inst, const gchar *key, const GValue *value);
/** Retrieves the contents of a KVP slot into a provided GValue.
 * @param inst: The QofInstance
 * @param key: The key of or '/'-delimited path to the slot.
 * @param value: A GValue into which to store the value of the slot. It will be
 *               set to the correct type.
 */
void qof_instance_get_kvp (const QofInstance *inst, const gchar *key, GValue
*value);
/** @} Close out the DOxygen ingroup */
/* Functions to isolate the KVP mechanism inside QOF for cases where
GValue * operations won't work.
 */
void qof_instance_copy_kvp (QofInstance *to, const QofInstance *from);
void qof_instance_swap_kvp (QofInstance *a, QofInstance *b);
int qof_instance_compare_kvp (const QofInstance *a, const QofInstance *b);
/** Returns a g_strdup'd string which must be g_freed. */
char* qof_instance_kvp_as_string (const QofInstance *inst);
void qof_instance_kvp_add_guid (const QofInstance *inst, const char* path,
                                const Timespec time, const char* key,
                                const GncGUID *guid);
void qof_instance_kvp_remove_guid (const QofInstance *inst, const char *path,
                                   const char* key, const GncGUID *guid);
gboolean qof_instance_kvp_has_guid (const QofInstance *inst, const char *path,
                                    const char* key, const GncGUID *guid);
void qof_instance_kvp_merge_guids (const QofInstance *target,
                                   const QofInstance *donor, const char* path);
gboolean qof_instance_has_slot (const QofInstance *inst, const char *path);
void qof_instance_slot_delete (const QofInstance *inst, const char *path);
void qof_instance_slot_delete_if_empty (const QofInstance *inst,
                                        const char *path);
void qof_instance_foreach_slot (const QofInstance *inst, const char *path,
                                void(*proc)(const char*, const GValue*, void*),
                                void* data);
#ifdef __cplusplus
} /* extern "C" */

/** Returns all keys that match the given prefix and their corresponding values.*/
std::map<std::string, KvpValue*>
qof_instance_get_slots_prefix (QofInstance const *, std::string const & prefix);

/* Don't pass nullptr as the function */
template<typename func_type, typename data_type>
void qof_instance_foreach_slot_temp (QofInstance const * inst, std::string const & path,
        func_type const & func, data_type & data)
{
    auto slot = inst->kvp_data->get_slot(path.c_str());
    if (slot == nullptr || slot->get_type() != KvpValue::Type::FRAME)
        return;
    auto frame = slot->get<KvpFrame*>();
    frame->for_each_slot(func, data);
}

/**
 * Similar to qof_instance_foreach_slot, but we don't traverse the depth of the key value frame,
 * we only check the root level for keys that match the specified prefix.
 */
template<typename func_type, typename data_type>
void qof_instance_foreach_slot_prefix(QofInstance const * inst, std::string const & path_prefix,
        func_type const & func, data_type & data)
{
    inst->kvp_data->for_each_slot_prefix(path_prefix, func, data);
}

#endif

#endif /* QOF_INSTANCE_P_H */
