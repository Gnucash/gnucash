/********************************************************************\
 * gnc-prefs.h -- Api to load and store preferences to a            *
 *                configurable backend                              *
 *                                                                  *
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
 ********************************************************************/

/** @addtogroup Engine
    @{ */
/** @addtogroup Preferences Preferences

    The API in this file is used to read and store preferences used
    by gnucash. This api is a generic one. To actually store and
    load preferences, a preferences backend should be configured.
    Currently only one backend is defined: the preferences backend.

    Note that preferences are organized in groups. Most functions
    will require both a group and a preference name to find the
    exact preference to work with.

    @{ */
/** @file gnc-prefs.h
 *  @brief Generic api to store and retrieve preferences.
 *  @author Copyright (C) 2013 Geert Janssens <geert@kobaltwit.be>
 */

#ifndef GNC_PREFS_H
#define GNC_PREFS_H

#include <glib.h>

/* Preference groups used across multiple modules */
#define GNC_PREFS_GROUP_GENERAL           "general"
#define GNC_PREFS_GROUP_GENERAL_REGISTER  "general.register"
#define GNC_PREFS_GROUP_GENERAL_REPORT    "general.report"
#define GNC_PREFS_GROUP_WARNINGS          "general.warnings"
#define GNC_PREFS_GROUP_WARNINGS_TEMP     "warnings.temporary"
#define GNC_PREFS_GROUP_WARNINGS_PERM     "warnings.permanent"
#define GNC_PREFS_GROUP_ACCT_SUMMARY      "window.pages.account-tree.summary"

/* Preference names used across multiple modules */
#define GNC_PREF_SAVE_GEOMETRY       "save-window-geometry"
#define GNC_PREF_LAST_PATH           "last-path"
#define GNC_PREF_USE_NEW             "use-new-window"
#define GNC_PREF_ACCOUNTING_LABELS   "use-accounting-labels"
#define GNC_PREF_ACCOUNT_SEPARATOR   "account-separator"
#define GNC_PREF_NEGATIVE_IN_RED     "negative-in-red"
#define GNC_PREF_NUM_SOURCE          "num-source"
#define GNC_PREF_DATE_FORMAT         "date-format"
#define GNC_PREF_DATE_COMPL_THISYEAR "date-completion-thisyear"
#define GNC_PREF_DATE_COMPL_SLIDING  "date-completion-sliding"
#define GNC_PREF_DATE_BACKMONTHS     "date-backmonths"
#define GNC_PREF_SHOW_LEAF_ACCT_NAMES "show-leaf-account-names"
#define GNC_PREF_ENTER_MOVES_TO_END  "enter-moves-to-end"
/* Register preferences */
#define GNC_PREF_DRAW_HOR_LINES      "draw-horizontal-lines"
#define GNC_PREF_DRAW_VERT_LINES     "draw-vertical-lines"
#define GNC_PREF_ALT_COLOR_BY_TRANS  "alternate-color-by-transaction"
#define GNC_PREF_USE_THEME_COLORS    "use-theme-colors"
#define GNC_PREF_TAB_TRANS_MEMORISED "tab-to-transfer-on-memorised"
#define GNC_PREF_FUTURE_AFTER_BLANK  "future-after-blank-transaction"
/* Date preferences */
#define GNC_PREF_START_CHOICE_ABS    "start-choice-absolute"
#define GNC_PREF_START_CHOICE_REL    "start-choice-relative"
#define GNC_PREF_START_DATE          "start-date"
#define GNC_PREF_START_PERIOD        "start-period"
#define GNC_PREF_END_CHOICE_ABS      "end-choice-absolute"
#define GNC_PREF_END_CHOICE_REL      "end-choice-relative"
#define GNC_PREF_END_DATE            "end-date"
#define GNC_PREF_END_PERIOD          "end-period"
/* Currency preferences */
#define GNC_PREF_CURRENCY_OTHER      "currency-other"
#define GNC_PREF_CURRENCY_CHOICE_LOCALE "currency-choice-locale"
#define GNC_PREF_CURRENCY_CHOICE_OTHER  "currency-choice-other"

/** @name Early bird functions, needed before any backend has been set up
 @{
*/

const gchar *gnc_prefs_get_namespace_regexp(void);
void gnc_prefs_set_namespace_regexp(const gchar *str);

gboolean gnc_prefs_is_debugging_enabled(void);
void gnc_prefs_set_debugging(gboolean d);

gboolean gnc_prefs_is_extra_enabled(void);
void gnc_prefs_set_extra(gboolean enabled);

gboolean gnc_prefs_get_file_save_compressed(void);
void gnc_prefs_set_file_save_compressed(gboolean compressed);

gint gnc_prefs_get_file_retention_policy(void);
void gnc_prefs_set_file_retention_policy(gint policy);

gint gnc_prefs_get_file_retention_days(void);
void gnc_prefs_set_file_retention_days(gint days);

guint gnc_prefs_get_long_version( void );

/** @} */


/** Test if preferences backend is set up
*/
gboolean gnc_prefs_is_set_up (void);

/** @name Listening for changes
 @{
*/


/** Register a callback that gets triggered when the given preference changes.
 *  Any time the preference's value changes, the routine
 *  will be invoked and will be passed both the changed preference
 *  and the user data passed to this function.
 *
 *  @param group This string contains the group name of the preference
 *  to watch.
 *
 *  @param preference This string contains the name of the preference to watch.
 *
 *  @param func This is a pointer to the function to call when the preference
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 *
 *  @return This function returns the handler id for the registered
 *  callback.
 */
gulong gnc_prefs_register_cb (const char *group,
                              const gchar *pref_name,
                              gpointer func,
                              gpointer user_data);


/** Remove a function that was registered for a callback when the
 *  given preference changed.  Both the func and user_data
 *  arguments are used to match up the callback to remove.
 *  If no matching func and user_data are found to be registered
 *  for the given preference, nothing will happen.
 *
 *  @param group This string contains the group name of the preference
 *  that is being watched.
 *
 *  @param preference This string contains the name of the preference being watched.
 *
 *  @param func This is a pointer to the function that was registered
 *  earlier.
 *
 *  @param user_data This pointer was passed to the callback
 *  function when it was registered.
 */
void gnc_prefs_remove_cb_by_func (const gchar *group,
                                  const gchar *pref_name,
                                  gpointer func,
                                  gpointer user_data);


/** Remove a function that was registered for a callback when a
 *  specific preference in the settings group changed.  The handler id
 *  that was generated when the callback was registered is
 *  use to find the callback to remove.
 *  If no handler id is found nothing will happen.
 *
 *  @param group This string contains the group name of the preference
 *  that is being watched.
 *
 *  @param id The handler id of the callback to be removed.
 */
void gnc_prefs_remove_cb_by_id (const gchar *group,
                                guint id);


/** Register a callback for when any preference in the settings group
 *  is changed.  Any time the value of a preference in this group changes,
 *  the routine will be invoked and will be passed the specified
 *  user data.
 *
 *  @param group This string contains the name of the group
 *  that is being watched.
 *
 *  @param func This is a pointer to the function to call when a preference
 *  changes.
 *
 *  @param user_data This pointer will be passed to the callback
 *  function.
 */
guint gnc_prefs_register_group_cb (const gchar *group,
                                   gpointer func,
                                   gpointer user_data);


/** Remove a function that was registered for a callback when any preference
 *  in the given settings group changed.  Both the func and user_data
 *  arguments are used to match up the callback to remove.
 *  If no matching func and user_data are found to be registered
 *  for the given preference, nothing will happen.
 *
 *  @param group This string contains the name of the group
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
void gnc_prefs_remove_group_cb_by_func (const gchar *group,
                                        gpointer func,
                                        gpointer user_data);


/** Bind a setting to a g_object property. When this succeeds a change
 *  of the setting will automatically update the bound object property
 *  and vice versa.
 *
 *  @param group This string contains the group name of the preference to bind to.
 *
 *  @param preference This string is the name of the particular preference to
 *  bind to.
 *
 *  @param object The object to be bound.
 *
 *  @param property The property of the object to bind to.
 */
void gnc_prefs_bind (const gchar *group,
                     /*@ null @*/ const gchar *pref_name,
                     gpointer object,
                     const gchar *property);

/** @} */

/** @name Preference Getters
 @{
*/

/** Get a boolean value from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the TRUE or FALSE value stored at
 *  the requested preference in the preferences backend.  If the preference has never
 *  been set, this function passes on the default value returned by
 *  the preferences backend.
 */
gboolean gnc_prefs_get_bool (const gchar *group,
                             /*@ null @*/ const gchar *pref_name);

/** Get an integer value from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the integer value stored at the
 *  requested preference in the preferences backend.  If the preference has never been
 *  set, this function passes on the default value returned by the preferences backend.
 *  If there is an error in processing, zero will be returned.
 */
gint gnc_prefs_get_int (const gchar *group,
                        const gchar *pref_name);

/** Get an 64 bit integer value from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the 64 bit integer value stored at the
 *  requested preference in the preferences backend.  If the preference has never been
 *  set, this function passes on the default value returned by the preferences backend.
 *  If there is an error in processing, zero will be returned.
 */
gint64 gnc_prefs_get_int64 (const gchar *group,
                            const gchar *pref_name);

/** Get an float value from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the float value stored at the
 *  requested preference in the preferences backend.  If the preference has never been
 *  set, this function passes on the default value returned by the preferences backend.
 *  If there is an error in processing, zero will be returned.
 */
gdouble gnc_prefs_get_float (const gchar *group,
                             const gchar *pref_name);

/** Get a string value from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the string value stored at the
 *  requested preference in the preferences backend.  If the preference has never been
 *  set, this function passes on the default value returned by the preferences backend
 *  If there is an error in processing, NULL will be returned.
 */
gchar *gnc_prefs_get_string (const gchar *group,
                             const gchar *pref_name);

/** Get an enum value from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the enum value stored at the
 *  requested preference in the preferences backend.  If the preference has never been
 *  set, this function passes on the default value returned by the preferences backend.
 *  If there is an error in processing, zero will be returned.
 */
gint gnc_prefs_get_enum (const gchar *group,
                         const gchar *pref_name);

/** Get a pair of coordinates from the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param x The x coordinate to retrieve.
 *
 *  @param y The y coordinate to retrieve.
 *
 *  If there is an error in processing, both coordinates will be set to zero.
 */
void gnc_prefs_get_coords (const gchar *group,
                           const gchar *pref_name,
                           gdouble *x, gdouble *y);

/** Get an arbitrary combination of values from the preferences backend.  This
 *  combination of values can be anything that can be encapsulated
 *  in a GVariant structure.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @return This function returns the a GVariant encapsulating the combination
 *  of values stored at the requested preference in the preferences backend.
 *  If the preference has never been set, this function passes on the default
 *  value returned by the preferences backend.
 *  If there is an error in processing, NULL will be returned.
 *  It is the callers responsibility to free any GVariant data returned
 *  by this function.
 */
GVariant *gnc_prefs_get_value (const gchar *group,
                               const gchar *pref_name);

/** @} */

/** @name Preference Setters and Unset Functions
 @{
*/


/** Store a boolean value into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The boolean value to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_bool (const gchar *group,
                             const gchar *pref_name,
                             gboolean value);

/** Store an integer value into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The integer number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_int (const gchar *group,
                            const gchar *pref_name,
                            gint value);

/** Store a 64 bit integer value into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The 64 bit integer number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_int64 (const gchar *group,
                              const gchar *pref_name,
                              gint64 value);

/** Store a float value into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The floating point number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_float (const gchar *group,
                              const gchar *pref_name,
                              gdouble value);


/** Store a string into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The string to be stored.  the preferences backend will make a copy of this
 *  string, so it is the callers responsibility to free the space used
 *  by this string (if necessary).
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_string (const gchar *group,
                               const gchar *pref_name,
                               const gchar *value);

/** Store an enum value into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The enum number to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_enum (const gchar *group,
                             const gchar *pref_name,
                             gint value);

/** Store coordinates into the preferences backend. Coordinates consist of
 *  a pair of floating point values (x and y).
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param x The x coordinate to be stored.
 *
 *  @param y The y coordinate to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_coords (const gchar *group,
                               const gchar *pref_name,
                               gdouble x, gdouble y);

/** Store an arbitrary combination of values into the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 *
 *  @param value The combination of values encapsulated in a GVariant
 *  to be stored.
 *
 *  @return This function returns true if the value was set successfully
 *  on the preference or false if not.
 */
gboolean gnc_prefs_set_value (const gchar *group,
                              const gchar *pref_name,
                              GVariant *value);

/** Reset a preference to its default value in the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 *
 *  @param preference This string is the name of the particular preference within
 *  the named group of the preferences backend.
 */
void gnc_prefs_reset (const gchar *group,
                      const gchar *pref_name);

/** Reset all preferences in a group to their default values in the preferences backend.
 *
 *  @param group This string specifies the group to which the preference belongs
 */
void gnc_prefs_reset_group (const gchar *group);

/** @} */


#endif /* GNC_PREFS_H */

/** @} */
/** @} */
