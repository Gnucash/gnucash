/*
 * gnc-gobject-utils.h -- utility functions for working
 *                        with GObjects
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup GLib
    @{ */
/** @addtogroup Gobject
    @{ */

/** @file gnc-gobject-utils.h
 *  @brief Gobject helper routines.
 *  @author Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 *  The APIs in this file are designed to provide additional
 *  functionality to GObjects, or to make it easier to use the Gobject
 *  system from within Gnucash.
 */

#ifndef GNC_GOBJECT_UTILS_H
#define GNC_GOBJECT_UTILS_H

#include <glib.h>
#include <glib-object.h>

/** @name Gobject Tracking Functions
 *  @{
 *
 *  This set of functions is used to maintain a "database" of objects
 *  that are built on top of a GObject (any level of nesting).  This
 *  database is simply a hash table of lists.  The hash table takes
 *  the object name as its key and returns a list of all objects of
 *  that type.  The object is then added to, deleted from, or looked
 *  up in the list.  The database can also be queried for a list of
 *  all objects of a specified type.  This can be used to find
 *  pre-existing GncTreeModels, etc.  (In this case performing a
 *  search for a specific object wouldn't help because the information
 *  being inspected is private to the object.)
 *
 *  Any object added to this databasse during the execution of gnucash
 *  should be deleted from it before completion of the program.  WHen
 *  the program shuts down, a list of all abjects still in the
 *  database will be dumped out to the logfile.  This should help
 *  developers find memoty leaks in their code where an object is
 *  lost, or is not release because it gained an extra reference at
 *  some point during its lifetime.
 */


/** Tell gnucash to remember this object in the database.
 *
 *  @param object The object to be tracked.  This can be a fully or
 *  partially instantiated object.
 *
 *  @param klass The class structure for the object.  This argument
 *  may be NULL if a fully instantiated object is passed in as the
 *  first argument.  If a partially instantiated object is provided
 *  (I.E. a parent class called this function) then this argument is
 *  required.  This is necessary because the class of the object
 *  changes as each of the parent class is instantiated.  The class
 *  structure, however, status constant and always reflects the fully
 *  instantiated object.
 */
void gnc_gobject_tracking_remember (GObject *object, GObjectClass *klass);

/** Tell gnucash to drop this object from the database.
 *
 *  @param object The object to be dropped.
 */
void gnc_gobject_tracking_forget (GObject *object);

/** Get a list of all known objects of a specified type.
 *
 *  @param name The type name of the objects to be found.  This is the
 *  name used when the object type was initialized.  If unknown, it
 *  can be found by calling G_OBJECT_TYPE_NAME(object).
 *
 *  @return A GList of objects of the specified type.  This list is
 *  owned by the tracking code and must not be modified by the caller.
 */
const GList *gnc_gobject_tracking_get_list (const gchar *name);


/** Dump the entire object tracking database via the g_log() family of
 *  functions.  This function is only called when gnucash exits, and
 *  at that point all of the objects should have been removed from the
 *  database and freed.  Any object remaining is the result of a
 *  memory/object leakage.
 */
void gnc_gobject_tracking_dump (void);

/** @} */


#endif /* GNC_GOBJECT_UTILS_H */
/** @} */
/** @} */
