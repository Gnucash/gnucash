/********************************************************************\
 * gnc-gsettings.h -- utility functions for storing/retrieving      *
 *              data in the GSettings database for GnuCash          *
 * Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>           *
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

/** @addtogroup GLib
    @{ */
/** @addtogroup GSettings GSettings Utilities

    The API in this file is designed to make it easy to use the GSettings
    system from within Gnucash.  GSettings is a shared key/value storage
    system.

    The main benefits of these routines are that they
    -# maintain a list of GSettings objects (one per schema),
    -# convert gnucash internal schema names into full gsettings schema id's, and
    -# optionally take care of error checking on return values.

    Note that this api should not be called directly. Instead use
    the gnc_gsettings_load_backend function to configure gsettings
    as backend for the gnucash preferences api and then use
    the gnc_prefs_* functions instead to work with preferences.

    @{ */
/** @file gnc-gsettings.h
 *  @brief GSettings helper routines.
 *  @author Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 */


#ifndef GNC_GSETIINGS_H
#define GNC_GSETTINGS_H

#include <gio/gio.h>

#define GSET_SCHEMA_PREFIX            "org.gnucash"

/** Convert a partial schema name into a complete gsettings schema name.
 *
 *  This function takes a partial gsettings schema name and converts
 *  it into a fully qualified gsettings schema name.  It does this
 *  by prepending the standard prefix for all gnucash schemas.
 *  If the schema is already fully qualified (i.e. begins with the
 *  default schema prefix, this routine will not change it.
 *
 *  @param name A partial schema name.  The default prefix is
 *  prepended to this name to produce a fully qualified schema
 *  name.
 *
 *  @return This function returns a string pointer to the fully
 *  qualified schema name.  It is the caller's responsibility to
 *  free this string.
 */
gchar *gnc_gsettings_normalize_schema_name (const gchar *name);


/** Set the default gsettings schema prefix. This is
 *  used to generate complete schema id's if only
 *  partial id's are passed.
 */
void gnc_gsettings_set_prefix (const gchar *prefix);

/** Get the default gsettings schema prefix.
 *  If none was set explicitly, this defaults to
 *  "org.gnucash"
 */
const gchar *gnc_gsettings_get_prefix (void);


/** @name Listening for changes
 @{
*/


/** Register a callback for when a specific key in the settings
 *  schema is changed.  Any time the key's value changes, the routine
 *  will be invoked and will be passed both the changed gsettings entry
 *  and the user data passed to this function.
 *
 *  @param schema This value contains the schema name of the key
 *  to watch.
 *
 *  @param key This value contains the name of the key to watch.
 *
 *  @param func This is a pointer to the function to call when the key
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 *
 *  @return This function returns the handler id for the registered
 *  callback.
 */
gulong gnc_gsettings_register_cb (const char *schema,
                                  const gchar *key,
                                  gpointer func,
                                  gpointer user_data);


/** Remove a function that was registered for a callback when a
 *  specific key in the settings schema changed.  Both the func and
 *  user_data arguments are used to match up the callback to remove.
 *  If no matching func and user_data are found to be registered
 *  for the given key, nothing will happen.
 *
 *  @param schema This value contains the schema name of the key
 *  that is being watched.
 *
 *  @param key This value contains the name of the key being watched.
 *
 *  @param func This is a pointer to the function that was registered
 *  earlier.
 *
 *  @param user_data This pointer was passed to the callback
 *  function when it was registered.
 */
void gnc_gsettings_remove_cb_by_func (const gchar *schema,
                                      const gchar *key,
                                      gpointer func,
                                      gpointer user_data);


/** Remove a function that was registered for a callback when a
 *  specific key in the settings schema changed.  The handler id
 *  that was generated when the callback was registered is
 *  use to find the callback to remove.
 *  If no handler id is found nothing will happen.
 *
 *  @param schema This value contains the schema name of the key
 *  that is being watched.
 *
 *  @param id The handler id of the callback to be removed.
 */
void gnc_gsettings_remove_cb_by_id (const gchar *schema,
                                    guint id);


/** Register a callback for when any key in the settings schema
 *  is changed.  Any time the value of a key in this schema changes,
 *  the routine will be invoked and will be passed the specified
 *  user data.
 *
 *  @param schema This value contains the name of the schema
 *  that is being watched.
 *
 *  @param func This is a pointer to the function to call when a key
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 */
guint gnc_gsettings_register_any_cb (const gchar *schema,
                                     gpointer func,
                                     gpointer user_data);


/** Remove a function that was registered for a callback when any key
 *  in the given settings schema changed.  Both the func and user_data
 *  arguments are used to match up the callback to remove.
 *  If no matching func and user_data are found to be registered
 *  for the given key, nothing will happen.
 *
 *  @param schema This value contains the name of the schema
 *  that is being watched.
 *
 *  @param func This is a pointer to the function that was registered
 *  earlier.
 *
 *  @param user_data This pointer was passed to the callback
 *  function when it was registered.
 *
 *  @note there is no gnc_settings_remove_any_cb_by_id. Use
 *  gnc_settings_remove_cb_by_id instead if you want to
 *  remove a callback set with gnc_settings_register_any_cb
 *  by its handler id.
 */
void gnc_gsettings_remove_any_cb_by_func (const gchar *schema,
        gpointer func,
        gpointer user_data);


/** Bind a setting to a g_object property. When this succeeds a change
 *  of the setting will automatically update the bound object property
 *  and vice versa.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param object The object to be bound.
 *
 *  @param property The property of the object to bind to.
 */
void gnc_gsettings_bind (const gchar *schema,
                         /*@ null @*/ const gchar *key,
                         gpointer object,
                         const gchar *property);


/** @name GSettings Get Functions
 @{
*/

/** Get a boolean value from GSettings.
 *
 *  Retrieve a TRUE/FALSE value from GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @return This function returns the TRUE or FALSE value stored at
 *  the requested key in the gsettings database.  If the key has never
 *  been set, this function passes on the default value returned by
 *  GSettings as specified in the schema for this key.
 */
gboolean gnc_gsettings_get_bool (const gchar *schema,
                                 /*@ null @*/ const gchar *key);

/** Get an integer value from GSettings.
 *
 *  Retrieve an integer value from GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @return This function returns the integer value stored at the
 *  requested key in the gsettings database.  If the key has never been
 *  set, this function passes on the default value returned by GSettings
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the value of zero as returned
 *  by GSettings.
 */
gint gnc_gsettings_get_int (const gchar *schema,
                            const gchar *key);

/** Get an float value from GSettings.
 *
 *  Retrieve an float value from GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @return This function returns the float value stored at the
 *  requested key in the gsettings database.  If the key has never been
 *  set, this function passes on the default value returned by GSettings
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the value of zero as returned
 *  by GSettings.
 */
gdouble gnc_gsettings_get_float (const gchar *schema,
                                 const gchar *key);

/** Get a string value from GSettings.
 *
 *  Retrieve an string value from GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @return This function returns the string value stored at the
 *  requested key in the gsettings database.  If the key has never been
 *  set, this function passes on the default value returned by GSettings
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the NULL value as returned by
 *  GSettings.  It is the callers responsibility to free any string
 *  returned by this function.
 */
gchar *gnc_gsettings_get_string (const gchar *schema,
                                 const gchar *key);

/** Get an enum value from GSettings.
 *
 *  Retrieve an enum value from GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @return This function returns the enum value stored at the
 *  requested key in the gsettings database.  If the key has never been
 *  set, this function passes on the default value returned by GSettings
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the value of zero as returned
 *  by GSettings.
 */
gint gnc_gsettings_get_enum (const gchar *schema,
                             const gchar *key);

/** Get an arbitrary combination of values from GSettings.
 *
 *  Retrieve an arbitrary combination of values from GSettings.   This
 *  combination of values can be anything that can be encapsulated
 *  in a GVariant structure.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @return This function returns the a GVariant encapsulating the combination
 *  of values stored at the requested key in the gsettings database.
 *  If the key has never been set, this function passes on the default
 *  value returned by GSettings as specified in the schema for this key.
 *  If there is an error in processing, this function passed on the NULL
 *  value as returned by GSettings.
 *  It is the callers responsibility to free any GVariant data returned
 *  by this function.
 */
GVariant *gnc_gsettings_get_value (const gchar *schema,
                                   const gchar *key);

/** @} */

/** @name GSettings Set/Unset Functions
 @{
*/


/** Store a boolean value into GSettings.
 *
 *  Store a boolean value into GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param value The boolean value to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the key or false if not.
 */
gboolean gnc_gsettings_set_bool (const gchar *schema,
                                 const gchar *key,
                                 gboolean value);

/** Store an integer value into GSettings.
 *
 *  Store an integer into GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param value The integer number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the key or false if not.
 */
gboolean gnc_gsettings_set_int (const gchar *schema,
                                const gchar *key,
                                gint value);

/** Store a float value into GSettings.
 *
 *  Store a float into GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param value The floating point number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the key or false if not.
 */
gboolean gnc_gsettings_set_float (const gchar *schema,
                                  const gchar *key,
                                  gdouble value);


/** Store a string into GSettings.
 *
 *  Store a single string into GSettings. The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param value The string to be stored.  GSettings will make a copy of this
 *  string, so it is the callers responsibility to free the space used
 *  by this string (if necessary).
 *
 *  @return This function returns true if the value was set successfully
 *  on the key or false if not.
 */
gboolean gnc_gsettings_set_string (const gchar *schema,
                                   const gchar *key,
                                   const gchar *value);

/** Store an enum value into GSettings.
 *
 *  Store an enum into GSettings.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param value The enum number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the key or false if not.
 */
gboolean gnc_gsettings_set_enum (const gchar *schema,
                                 const gchar *key,
                                 gint value);

/** Store an arbitrary combination of values into GSettings.
 *
 *  Store an arbitrary combination of values into GSettings.  This
 *  combination of values can be anything that can be encapsulated
 *  in a GVariant structure.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 *
 *  @param value The combination of values encapsulated in a GVariant
 *  to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the key or false if not.
 */
gboolean gnc_gsettings_set_value (const gchar *schema,
                                  const gchar *key,
                                  GVariant *value);

/** Reset a key to its default value in GSettings.
 *
 *  Reset a key to its default value in GSettings.  Internally this
 *  is done by removing the value from the database.  The next attempt
 *  to read this value will return the default as specified in the
 *  GSettings schema for this key.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param key This string is the name of the particular key within
 *  the named schema of gsettings.
 */
void gnc_gsettings_reset (const gchar *schema,
                          const gchar *key);

/** Reset all keys in a schema to their default values in GSettings.
 *
 *  Reset a keys in schema to their default values in GSettings.  Internally
 *  this is done by removing the values from the database.  The next attempt
 *  to read a keys will return its default as specified in the
 *  GSettings schema for this key.  The schema name
 *  provided as argument is combined with the default gnucash schema
 *  prefix to produce a fully qualified schema name.
 *
 *  @param schema This string provides a grouping of keys within the
 *  GnuCash schema of the gsettings database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs.business.invoice" for setting that only apply to one
 *  specific area of the program.
 */
void gnc_gsettings_reset_schema (const gchar *schema);

/** @} */


/** Configure gsettings as the backend for the gnucash preferences api.
 */
void gnc_gsettings_load_backend (void);


/* Attempt to migrate preferences from gconf files
    to gsettings if not already done so */
void gnc_gsettings_migrate_from_gconf (void);

#endif /* GNC_GSETTINGS_H */
/** @} */
/** @} */
