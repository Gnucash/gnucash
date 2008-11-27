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

#include "config.h"

#include <stdio.h>
#include "gnc-gobject-utils.h"

#include <gtk/gtk.h>	// For gtk_main_quit(). Can't get this to work with
			// a g_source attached to the main glib context.


static void gnc_gobject_weak_cb (gpointer user_data, GObject *object);

/************************************************************/
/*                      Gconf Utilities                     */
/************************************************************/


/** Get a pointer to the hash table used by the tracking database.  If
 *  the hash table doesn't exist, it will be created.  If gnucash was
 *  compiled with --enable-ref-count-dumps, this funtion is also the
 *  point where the gnc_gobject_tracking_dump() function is registered
 *  to be called the GTK exits.
 *
 *  @internal.
 */
static GHashTable*
gnc_gobject_tracking_table (void)
{
  static GHashTable *singleton = NULL;

  if (!singleton) {
    singleton = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, NULL);
#if DEBUG_REFERENCE_COUNTING
    gtk_quit_add (0, (GtkFunction)gnc_gobject_tracking_dump, NULL);
#endif
  }
  return singleton;
}


/** Dump a single object from the tracking database.  This is an
 *  auxiliary function used by the gnc_gobject_tracking_dump()
 *  function.  This function currently only dumps the object pointer
 *  and the reference count.  It can be extended, if necessary, via
 *  object introspection.
 *
 *  @internal.
 */
static void
gnc_gobject_dump_gobject (GObject *object, const gchar *name)
{
  //printf("Enter %s: object %p, name %s\n", G_STRFUNC, object, name);
  g_message("    object %p, ref count %d", object, object->ref_count);
  //printf("Leave %s:\n", G_STRFUNC);
}


/** Dump a single list of objects from the tracking database.  This is
 *  an auxiliary function used by the gnc_gobject_tracking_dump()
 *  function.
 *
 *  @internal.
 */
static gboolean
gnc_gobject_dump_list (const gchar *name, GList *list, gpointer user_data)
{
  //printf("Enter %s: name %s, list %p\n", G_STRFUNC, name, list);
  g_message("  %d %s", g_list_length(list), name);
  g_list_foreach(list, (GFunc)gnc_gobject_dump_gobject, (gpointer)name);
  //printf("Leave %s:\n", G_STRFUNC);
  return TRUE;
}


/** Dump the entire object tracking database via the g_log() family of
 *  functions.  This function is only called when gnucash exits, and
 *  at that point all of the objects should have been removed from the
 *  database and freed.  Any object remaining is the result of a
 *  memory/object leakage.
 */
void
gnc_gobject_tracking_dump (void)
{
  GHashTable *table;

  //printf("Enter %s:\n", G_STRFUNC);
  table = gnc_gobject_tracking_table();

  if (g_hash_table_size(table) > 0) {
    g_message("The following objects remain alive:");
    g_hash_table_foreach_remove(table, (GHRFunc)gnc_gobject_dump_list, NULL);
  }
  //printf("Leave %s:\n", G_STRFUNC);
}


/** Tell gnucash to remember this object in the database.
 */
void
gnc_gobject_tracking_remember (GObject *object, GObjectClass *klass)
{
  GHashTable *table;
  GList *list;
  const gchar *name;

  g_return_if_fail(G_IS_OBJECT(object));

  /* Little dance here to handle startup conditions. During object
   * initialization the object type changes as each each parent class
   * is initialized.  The class passed to the initialization function
   * is always the ultimate class of the object. */
  if (klass == NULL)
    klass = G_OBJECT_GET_CLASS(object);
  name = g_type_name(G_TYPE_FROM_CLASS(klass));

  //printf("Enter %s: object %p of type %s\n", G_STRFUNC, object, name);
  table = gnc_gobject_tracking_table();
  list = g_hash_table_lookup(table, name);

  if (g_list_index(list, object) != -1) {
    g_critical("Object %p is already in list of %s", object, name);
    //printf("Leave %s: already in list\n", G_STRFUNC);
    return;
  }

  list = g_list_append(list, object);
  g_hash_table_insert(table, g_strdup(name), list);

  g_object_weak_ref(object, gnc_gobject_weak_cb, NULL);
  //printf("Leave %s:\n", G_STRFUNC);
}


static gboolean
gnc_gobject_tracking_forget_internal (GObject *object)
{
  GHashTable *table;
  GList *list, *item;
  const gchar *name;

  g_return_val_if_fail(G_IS_OBJECT(object), FALSE);

  name = G_OBJECT_TYPE_NAME(object);
  //printf("Enter %s: object %p of type %s\n", G_STRFUNC, object, name);
  table = gnc_gobject_tracking_table();
  list = g_hash_table_lookup(table, name);
  if (!list) {
    //printf("Leave %s: list for %s objects not found.\n", G_STRFUNC, name);
    return FALSE;
  }

  item = g_list_find(list, object);
  if (!item) {
    //printf("Leave %s: object %p not in %s object list.\n", G_STRFUNC,
    //       object, name);
    return FALSE;
  }

  list = g_list_remove_link(list, item);
  if (list) {
    g_hash_table_replace(table, g_strdup(name), list);
    //printf("Leave %s: object removed.\n", G_STRFUNC);
  } else {
    g_hash_table_remove(table, name);
    //printf("Leave %s: object and list removed.\n", G_STRFUNC);
  }
  return TRUE;
}


/** Tell gnucash to remember this object in the database.
 */
void
gnc_gobject_tracking_forget (GObject *object)
{
  if (gnc_gobject_tracking_forget_internal(object))
    g_object_weak_unref(object, gnc_gobject_weak_cb, NULL);
}


/** This callback function is called when a remembered object is
 *  destroyed.  It is used to catch the destruction an object when the
 *  caller didn't first tell this code to forget that object.  No need
 *  for an error message, just forget the object.
 *
 *  @internal
 *
 *  @param object The object being destroyed.
 */
static void
gnc_gobject_weak_cb (gpointer user_data, GObject *object)
{
  gnc_gobject_tracking_forget_internal(object);
}


/** Get a list of all known objects of a specified type.
 */
const GList *
gnc_gobject_tracking_get_list (const gchar *name)
{
  GHashTable *table;
  GList *list;

  //printf("Enter %s: name %s\n", G_STRFUNC, name);
  table = gnc_gobject_tracking_table();
  list = g_hash_table_lookup(table, name);
  //printf("Leave %s: list %p\n", G_STRFUNC, list);
  return list;
}
