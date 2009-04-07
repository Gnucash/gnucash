/********************************************************************\
 * qofgobj.h -- QOF to GLib GObject mapping                         *
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

#ifndef QOF_GOBJ_H
#define QOF_GOBJ_H

/** @addtogroup Object
    @{ */
/** @addtogroup GObject GLib GObjects
    The API defined in this file allows a user to register any
    GLib GObject (and any object derived from one, e.g. GTK/Gnome)
    with the QOF system, as a QOF Object Class.  This allows
    the QOF Query routines to be used to search over collections
    of GObjects.
 
    XXX Only GObject properties are searchable, data and other 
    hanging off the GObject is not.  Fix this. This needs fixing.

@{ */
/** @file qofgobj.h
    @brief QOF to GLib GObject mapping
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/


#include <glib-object.h>
#include "qofbook.h"
#include "qofclass.h"

/** Initalize and shut down this subsystem. */
void qof_gobject_init(void);
void qof_gobject_shutdown (void);

/** Register a GObject class with the QOF subsystem.
 *  Doing this will make the properties associated with
 *  this GObject searchable using the QOF subsystem.
 *
 *  The QofType can be any string you desire, although typically
 *  you might want to set it to G_OBJECT_CLASS_NAME() of the 
 *  object class.  Note that this type will become the name of
 *  the "table" that is searched by SQL queries:
 *  e.g. in order to be able to say "SELECT * FROM MyStuff;"
 *  you must first say:
 *   qof_gobject_register ("MyStuff", gobj_class);
 */
void qof_gobject_register (QofType type, GObjectClass *obclass);

/** Register an instance of a GObject with the QOF subsystem.
 *
 *  The QofType can be any string you desire, although typically
 *  you might want to set it to G_OBJECT_CLASS_NAME() of the 
 *  object class.  Note that this type will become the name of
 *  the "table" that is searched by SQL queries:
 *  e.g. in order to be able to say "SELECT * FROM MyStuff;"
 *  you must first say:
 *   qof_gobject_register_instance (book, "MyStuff", obj);
 *
 *  The 'book' argument specifies an anchor point for the collection
 *  of all of the registered instances.  By working with disjoint books,
 *  you can have multiple disjoint searchable sets of objects.
 */

void qof_gobject_register_instance (QofBook *book, QofType, GObject *);

#endif /* QOF_GOBJ_H */
/** @} */
/** @} */
