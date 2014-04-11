/********************************************************************\
 * gnc-gconf-utils.h -- utility functions for storing/retrieving    *
 *              data in the GConf database for GnuCash              *
 * Copyright (C) 2005,2006 David Hampton <hampton@employees.org>    *
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
/** @addtogroup GConf GConf Utilities

    The API in this file is designed to make it easy to use the GConf
    system from within Gnucash.  GConf is a shared key/value storage
    system.

    The main benefits of these routines are that they 
    -# maintain a GConfClient object, 
    -# convert gnucash internal section names into full gconf pathnames, and 
    -# optionally take care of error checking on return values.

    @{ */
/** @file gnc-gconf-utils.h
 *  @brief GConf helper routines.
 *  @author Copyright (C) 2005,2006 David Hampton <hampton@employees.org>
 */


#ifndef GNC_GCONF_UTILS_H
#define GNC_GCONF_UTILS_H

#include <gconf/gconf-client.h>

/* Section names used across multiple modules */
#define GCONF_GENERAL		"general"
#define GCONF_GENERAL_REGISTER	"general/register"
#define GCONF_GENERAL_REPORT	"general/report"
#define GCONF_WARNINGS		"general/warnings"
#define GCONF_WARNINGS_TEMP	"general/warnings/temporary"
#define GCONF_WARNINGS_PERM	"general/warnings/permanent"

/* Keys used across multiple modules */
#define DESKTOP_GNOME_INTERFACE "/desktop/gnome/interface"
#define KEY_TOOLBAR_STYLE	"toolbar_style"
#define KEY_SAVE_GEOMETRY	"save_window_geometry"
#define KEY_LAST_PATH		"last_path"
#define KEY_USE_NEW   		"use_new_window"
#define KEY_ACCOUNTING_LABELS	"use_accounting_labels"
#define KEY_ACCOUNT_SEPARATOR	"account_separator"
#define KEY_NEGATIVE_IN_RED	"negative_in_red"
#define KEY_NUMBER_OF_ROWS	"number_of_rows"
#define KEY_ENABLE_EURO		"enable_euro"
#define KEY_DATE_FORMAT 	"date_format"
#define KEY_SHOW_LEAF_ACCOUNT_NAMES "show_leaf_account_names"

typedef void (*GncGconfGeneralCb)    (GConfEntry *entry, gpointer user_data);
typedef void (*GncGconfGeneralAnyCb) (gpointer user_data);


/** @name GConf Miscellaneous Functions
 @{ 
*/

/** This function takes an enum value and returns its nickname.
 *
 *  @param type The value defining the enum class.  For example,
 *  GTK_TYPE_SORT_TYPE.
 *
 *  @param value A value contained in the enum.  For example,
 *  GTK_SORT_ASCENDING.
 *
 *  @return A pointer to the textual "nickname" for this enum.  Tor
 *  example, "ascending".
 */
const gchar * gnc_enum_to_nick(GType type, gint value);

/** This function takes an enum nickname and returns its value.
 *
 *  @param type The value defining the enum class.  For example,
 *  GTK_TYPE_SORT_TYPE or GTK_TYPE_ARROW_TYPE.
 *
 *  @param name The textual name for one of the items in the enum.
 *  For example, "ascending".
 *
 *  @param default_value A value contained in the enum.  This value
 *  will be returned if the supplied nickname is invalid.  For
 *  example, GTK_SORT_ASCENDING.
 *
 *  @return A pointer to the textual "nickname" for this enum.
 */
gint gnc_enum_from_nick(GType type,
			const gchar *name,
			gint default_value);

/** Convert a local key name to a full gconf path name.
 *
 *  This function takes a gconf key name and converts it into a fully
 *  qualified gconf path name.  It does this by prepending the
 *  standard path for all gnucash keys.  It the key is already fully
 *  qualified (i.e. begins with a '/' character), this routine does
 *  not change the key.
 *
 *  @param name A partial gconf key or section name.  This name is
 *  added to the standard prefix to produce a fully qualified key
 *  name.
 *
 *  @return This function returns a string pointer to the fully
 *  qualified path name of the gconf key.  It is the caller's
 *  responsibility to free this string.
 */
char *gnc_gconf_section_name (const char *name);


/** Convert a local schema key name to a full gconf schemapath name.
 *
 *  This function takes a gconf schema key name and converts it into a
 *  fully qualified gconf schema path name.  It does this by
 *  prepending the standard path for all gnucash schema keys.  It the
 *  key is already fully qualified (i.e. begins with the string
 *  "/schemas), this routine does not change the key.
 *
 *  @param name A partial gconf schema key or section name.  This name
 *  is added to the standard schema prefix to produce a fully
 *  qualified schema key name.
 *
 *  @return This function returns a string pointer to the fully
 *  qualified path name of the gconf schema key.  It is the caller's
 *  responsibility to free this string.
 */
char *gnc_gconf_schema_section_name (const char *name);


/** Tell GConf to propagate changes.
 *
 *  This function tells gconf that changes have been made and that is
 *  should propagate its internal state to permanent storage and any
 *  other clients.  This function is a suggestion to gconf, not a
 *  directive, and is therefore should be considered optional.  Doesn't
 *  hurt to call it though if you've made numerous changes to gconf in
 *  a short period of time.
 */
void gnc_gconf_suggest_sync (void);

/** @} */



/** @name GConf "General" Section Convenience Functions 
 @{ 
*/


/** Register a callback for when a specific key in the general section
 *  of Gnucash's gconf data is changed.  Any time the key's value
 *  changes, the routine will be invoked and will be passed both the
 *  changes gconf entry and the user data passed to this function.
 *
 *  @param key This value contains the name of the key within the
 *  "general" section to watch.
 *
 *  @param func This is a pointer to the function to call when the key
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 */
void gnc_gconf_general_register_cb (const gchar *key,
				    GncGconfGeneralCb func,
				    gpointer user_data);


/** Remove a function that was registered for a callback when a
 *  specific key in the general section of Gnucash's gconf data
 *  changed.  Both the func and user_data arguments are used to match
 *  up the callback to remove.
 *
 *  @param key This value contains the name of the key within the
 *  "general" section to watch.
 *
 *  @param func This is a pointer to the function to call when the key
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 */
void gnc_gconf_general_remove_cb (const gchar *key,
				  GncGconfGeneralCb func,
				  gpointer user_data);


/** Register a callback for when any key in the general section of
 *  Gnucash's gconf data is changed.  Any time the value of a key in
 *  this section chagnes, the routine will be invoked and will be
 *  passed the specified user data.
 *
 *  @param func This is a pointer to the function to call when the key
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 */
void gnc_gconf_general_register_any_cb (GncGconfGeneralAnyCb func,
					gpointer user_data);


/** Remove a function that was registered for a callback when any key
 *  in the general section of Gnucash's gconf data changed.  Both the
 *  func and user_data arguments are used to match up the callback to
 *  remove.
 *
 *  @param func This is a pointer to the function to call when a key
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 */
void gnc_gconf_general_remove_any_cb (GncGconfGeneralAnyCb func,
				      gpointer user_data);

/** @} */



/** @name GConf Get Functions 
 @{ 
*/

/** Get a boolean value from GConf.
 *
 *  Retrieve a TRUE/FALSE value from GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 *
 *  @return This function returns the TRUE or FALSE value stored at
 *  the requested key in the gconf database.  If the key has never
 *  been set, this function passes on the default value returned by
 *  GConf as specified in the schema for this key.  If there is an
 *  error in processing, this function passed on the value of FALSE as
 *  returned by GConf.
 */
gboolean gnc_gconf_get_bool (const gchar *section,
			     /*@ null @*/ const gchar *name,
			     /*@ null @*/ GError **error);

/** Get a boolean value from GConf with no error argument.
 *
 *  Retrieve a TRUE/FALSE value from GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @return This function returns the TRUE or FALSE value stored at
 *  the requested key in the gconf database.  If the key has never
 *  been set, this function passes on the default value returned by
 *  GConf as specified in the schema for this key.  If there is an
 *  error in processing, this function passed on the value of FALSE as
 *  returned by GConf.
 *
 * @note This function was intended for use only by the guile wrapper
 * functions.  It should not be called from C code.
 */
gboolean gnc_gconf_get_bool_no_error (const gchar *section,
				      const gchar *name);

/** Get an integer value from GConf.
 *
 *  Retrieve an integer value from GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 *
 *  @return This function returns the float value stored at the
 *  requested key in the gconf database.  If the key has never been
 *  set, this function passes on the default value returned by GConf
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the value of zero as returned
 *  by GConf.
 */
gint gnc_gconf_get_int (const gchar *section,
			const gchar *name,
			GError **error);

/** Get an float value from GConf.
 *
 *  Retrieve an float value from GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 *
 *  @return This function returns the integer value stored at the
 *  requested key in the gconf database.  If the key has never been
 *  set, this function passes on the default value returned by GConf
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the value of zero as returned
 *  by GConf.
 */
gdouble gnc_gconf_get_float (const gchar *section,
			     const gchar *name,
			     GError **error);

/** Get a string value from GConf.
 *
 *  Retrieve an string value from GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 *
 *  @return This function returns the string value stored at the
 *  requested key in the gconf database.  If the key has never been
 *  set, this function passes on the default value returned by GConf
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the NULL value as returned by
 *  GConf.  It is the callers responsibility to free any string
 *  returned by this function.
 */
char *gnc_gconf_get_string (const gchar *section,
			    const gchar *name,
			    GError **error);

/** Get a list of values from GConf.
 *
 *  Retrieve a list of values from GConf.  This list may be of any
 *  kind of value understood by GConf, but all values in the list will
 *  be of the same type.  The section and key names provided as
 *  arguments are combined with the standard gnucash key prefix to
 *  produce a fully qualified key name.  Either name (but not both)
 *  may be a fully qualified key path name, in which case it is used
 *  as is, without the standard gnucash prefix.  This allows the
 *  program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param list_type This enum indicates the type of each item in the
 *  returned list.  This type must match the type off the stored
 *  items.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 *
 *  @return This function returns a list of value stored at the
 *  requested key in the gconf database.  If the key has never been
 *  set, this function passes on the default value returned by GConf
 *  as specified in the schema for this key.  If there is an error in
 *  processing, this function passed on the NULL value as returned by
 *  GConf.  It is the callers responsibility to free any memory
 *  returned by this function.  This include the list itself, and any
 *  list data that are string values.
 */
GSList *gnc_gconf_get_list (const gchar *section,
			    const gchar *name,
			    GConfValueType list_type,
			    GError **error);


/** Get a schema value from GConf.
 *
 *  Retrieve a schema value from GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param caller_error An optional pointer to a GError structure.  If
 *  NULL, this function will check for any errors returned by GConf
 *  and will display an error message via stdout.  If present, this
 *  function will pass any error back to the calling function for it
 *  to handle.
 *
 *  @return This function returns the schema stored at the requested
 *  key in the gconf database.  If there is an error in processing,
 *  this function passed on the NULL value as returned by GConf.  It
 *  is the callers responsibility to free any returned schema by
 *  calling the gconf_schema_free() function.
 */
GConfSchema *gnc_gconf_get_schema (const gchar *section,
				   const gchar *name,
				   GError **caller_error);

/** @} */

/** @name GConf Set/Unset Functions 
 @{ 
*/


/** Store a boolean value into GConf.
 *
 *  Store a boolean value into GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param value The TRUE/FALSE value to be stored.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_set_bool (const gchar *section,
			 const gchar *name,
			 const gboolean value,
			 GError **error);

/** Store an integer value into GConf.
 *
 *  Store an integer into GConf.  The section and key names provided
 *  as arguments are combined with the standard gnucash key prefix to
 *  produce a fully qualified key name.  Either name (but not both)
 *  may be a fully qualified key path name, in which case it is used
 *  as is, without the standard gnucash prefix.  This allows the
 *  program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param value The number to be stored.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_set_int (const gchar *section,
			const gchar *name,
			const gint value,
			GError **error);

/** Store an float value into GConf.
 *
 *  Store an float into GConf.  The section and key names provided
 *  as arguments are combined with the standard gnucash key prefix to
 *  produce a fully qualified key name.  Either name (but not both)
 *  may be a fully qualified key path name, in which case it is used
 *  as is, without the standard gnucash prefix.  This allows the
 *  program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param value The number to be stored.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_set_float (const gchar *section,
			  const gchar *name,
			  const gdouble value,
			  GError **error);

/** Store a string into GConf.
 *
 *  Store a single string into GConf.  The section and key names
 *  provided as arguments are combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  Either name (but
 *  not both) may be a fully qualified key path name, in which case it
 *  is used as is, without the standard gnucash prefix.  This allows
 *  the program to access keys like standard desktop settings.  Either
 *  name (but not both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param value The string to be stored.  GConf will make a copy of this
 *  string, so it is the callers responsibility to free the space used
 *  by this string (if necessary).
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_set_string (const gchar *section,
			   const gchar *name,
			   const gchar *value,
			   GError **error);

/** Store a list of values into GConf.
 *
 *  Store a list of values into GConf.  This list may be of any kind
 *  of value understood by GConf, but all values in the list must be
 *  of the same type.  The section and key names provided as arguments
 *  are combined with the standard gnucash key prefix to produce a
 *  fully qualified key name.  Either name (but not both) may be a
 *  fully qualified key path name, in which case it is used as is,
 *  without the standard gnucash prefix.  This allows the program to
 *  access keys like standard desktop settings.  Either name (but not
 *  both) may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param list_type This enum indicates the type of each item in the
 *  list to be stored.
 *
 *  @param value The list of items to be stored.  Each item in the list must
 *  be of the type specified.  E.G. If the list_type is
 *  GCONF_VALUE_STRING, then the data field of each element in the
 *  list must be a string pointer.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_set_list (const gchar *section,
			 const gchar *name,
			 GConfValueType list_type,
			 GSList *value,
			 GError **error);

/** Delete a value from GConf.
 *
 *  Completely remove a value from GConf.  The next attempt to read this
 *  value will return the default as specified in the GConf schema for
 *  this key.  The section and key names provided as arguments are
 *  combined with the standard gnucash key prefix to produce a fully
 *  qualified key name.  Either name (but not both) may be a fully
 *  qualified key path name, in which case it is used as is, without
 *  the standard gnucash prefix.  This allows the program to access
 *  keys like standard desktop settings.  Either name (but not both)
 *  may be NULL.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param name This string is the name of the particular key within
 *  the named section of gconf.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_unset (const gchar *section,
		      const gchar *name,
		      GError **error);


/** Delete a directory of values from GConf.
 *
 *  Completely remove a directory of values from GConf.  The next
 *  attempt to read any of these values will return the default as
 *  specified in the GConf schema for the key.  The section names
 *  provided as an arguments is combined with the standard gnucash key
 *  prefix to produce a fully qualified directory name.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @param error An optional pointer to a GError structure.  If NULL,
 *  this function will check for any errors returned by GConf and will
 *  display an error message via stdout.  If present, this function
 *  will pass any error back to the calling function for it to handle.
 */
void gnc_gconf_unset_dir (const gchar *section,
			  GError **error);

/** @} */

/** @name GConf Notification Functions 
 @{ 
*/

/** Add a notification callback to GConf.
 *
 *  Add a function that will be called whenever a value within the
 *  specified section of the GConf tree changes.  The section name
 *  provided as an argument is combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  This name may be a
 *  fully qualified key path name, in which case it is used as is,
 *  without the standard gnucash prefix.  This allows the object to
 *  respond to keys like standard desktop settings.
 *
 *  @param object This is a pointer to a GObject derivative.  This
 *  object will be provided to the callback function when it is
 *  invoked.  Several values will also be attached to this object that
 *  are used by the gnc_gconf_remove_notification() function.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.  Any key changes within this section
 *  will invoke the notification function.
 *
 *  @param callback The function to call when a value changes.  This
 *  function will receive the key/value pair as one argument, and the
 *  'object' argument to this function as another of its arguments.
 *
 *  @param whoami A magic value that must match up this call to the
 *  corresponding call to gnc_gconf_remove_notification().  The pair of
 *  section and whoami should be unique across all callers.
 */
void gnc_gconf_add_notification (GObject *object,
				 const gchar *section,
				 GConfClientNotifyFunc callback,
				 const gchar *whoami);


/** An alternative function for adding a notification callback to
 *  GConf.
 *
 *  Add a function that will be called whenever a value within the
 *  specified section of the GConf tree changes.  The section name
 *  provided as an argument is combined with the standard gnucash key
 *  prefix to produce a fully qualified key name.  This name may be a
 *  fully qualified key path name, in which case it is used as is,
 *  without the standard gnucash prefix.  This allows the object to
 *  respond to keys like standard desktop settings.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.  Any key changes within this section
 *  will invoke the notification function.
 *
 *  @param callback The function to call when a value changes.  This
 *  function will receive the key/value pair as one argument, and the
 *  'object' argument to this function as another of its arguments.
 *
 *  @param data This pointer will be provided to the callback function
 *  when it is invoked.
 *
 *  @return This function returns an identification number that must
 *  be passed to the gnc_gconf_remove_anon_notification() function to
 *  reverse the actions of this function.
 */
guint gnc_gconf_add_anon_notification (const gchar *section,
				       GConfClientNotifyFunc callback,
				       gpointer data);


/** Remove a callback from GConf.
 *
 *  Remove a GConf callback function previously added with the
 *  gnc_gconf_add_notification function.  The section name must be the
 *  same string provided when the callback function was added.  This
 *  name is used to find/remove the callback.
 *
 *  @param object This is a pointer to a GObject derivative.  This
 *  must be the same object originally passed to the
 *  gnc_gconf_add_notification() function, as that function attached
 *  several values to the object that are needed by this function.
 *
 *  @param section This string is used to find the correct
 *  notification function to remove from GConf.
 *
 *  @param whoami A magic value that must match up this call to the
 *  corresponding call to gnc_gconf_add_notification().  The pair of
 *  section and whoami should be unique across all callers.
 */
void gnc_gconf_remove_notification (GObject *object,
				    const gchar *section,
				    const gchar *whoami);



/** An alternative method for remove a callback from GConf; paired
 *  with gnc_gconf_add_anon_notification().
 *
 *  Remove a GConf callback function previously added with the
 *  gnc_gconf_add_notification function.  The section name must be the
 *  same string provided when the callback function was added.  This
 *  name is used to find/remove the callback.
 *
 *  @param section This string is used to find the correct
 *  notification function to remove from GConf.
 *
 *  @param cnxn_id An identification number returned by the
 *  gnc_gconf_add_anon_notification() function.
 */
void gnc_gconf_remove_anon_notification (const gchar *section,
					 guint cnxn_id);


/** Retrieve a list of all key/value pairs in the specified GConf
 *  section.  The section name provided as an argument is combined
 *  with the standard gnucash key prefix to produce a fully qualified
 *  section name.
 *
 *  @param section This string provides a grouping of keys within the
 *  GnuCash section of the gconf database.  It can be a simple string
 *  as in "history" for settings that are common to many areas of
 *  gnucash, or it can be a partial path name as in
 *  "dialogs/business/invoice" for setting that only apply to one
 *  specific area of the program.
 *
 *  @return This function returns a list of all key/value pairs stored
 *  in this section of the gconf database.  These are GConfEntry
 *  objects.  It is the callers responsibility to free any memory
 *  returned by this function.  This include the list itself, and any
 *  entries contained in the list.  See gconf_client_all_entries in
 *  the gconf documentation.
 */
GSList *gnc_gconf_client_all_entries (const gchar *section);


/** Check gconf to see if the schema for one of the gnucash keys can
 *  be found.  This function is called to determine whether or not to
 *  launch a druid to help the user properly set up GConf for Gnucash.
 *
 *  @return This function returns TRUE if it was able to find a
 *  schema.
 */
gboolean gnc_gconf_schemas_found (void);

/** @} */

/** @name GConf One Liners 
 @{ 
*/

#define DESTKOP_TEAROFF_MENUS "/desktop/gnome/interface/menus_have_tearoff"
#define DESTKOP_MENUBAR_DETACHABLE "/desktop/gnome/interface/menubar_detachable"
#define DESTKOP_TOOLBAR_DETACHABLE "/desktop/gnome/interface/toolbar_detachable"

static inline gboolean
gnc_gconf_menus_have_tearoff (void)
{
  return gnc_gconf_get_bool(DESTKOP_TEAROFF_MENUS, NULL, NULL);
}

static inline gboolean
gnc_gconf_menubar_detachable (void)
{
  return gnc_gconf_get_bool(DESTKOP_MENUBAR_DETACHABLE, NULL, NULL);
}

static inline gboolean
gnc_gconf_toolbar_detachable (void)
{
  return gnc_gconf_get_bool(DESTKOP_TOOLBAR_DETACHABLE, NULL, NULL);
}

/** @} */

#endif /* GNC_GCONF_UTILS_H */
/** @} */
/** @} */
