/********************************************************************\
 * global-options.c -- GNOME global option handling                 *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "top-level.h"

#include <gnome.h>

#include "global-options.h"
#include "dialog-options.h"
#include "dialog-utils.h"
#include "option-util.h"
#include "query-user.h"
#include "guile-util.h"
#include "window-help.h"
#include "messages.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GNCOptionDB *global_options = NULL;


/********************************************************************\
 * gnc_options_init                                                 *
 *   initialize the options structures from the guile side          *
 *                                                                  *
 * Args: none                                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_options_init()
{
  SCM func = gh_eval_str("gnc:send-global-options");
  SCM options;

  if (gh_procedure_p(func))
    options = gh_call0(func);
  else
  {
    PERR("gnc_options_init: no guile options!");
    return;
  }

  global_options = gnc_option_db_new(options);
}


/********************************************************************\
 * gnc_options_shutdown                                             *
 *   unregister the scheme options and free the structure memory    *
 *                                                                  *
 * Args: none                                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_options_shutdown()
{
  gnc_option_db_destroy(global_options);
  global_options = NULL;
}


/********************************************************************\
 * gnc_register_option_change_callback                              *
 *   register a callback to be called whenever an option changes    *
 *                                                                  *
 * Args: callback  - the callback function                          *
 *       user_data - the user data for the callback                 *
 *       section   - the section to get callbacks for.              *
 *                   If NULL, get callbacks for any section changes.*
 *       name      - the option name to get callbacks for.          *
 *                   If NULL, get callbacks for any option in the   *
 *                   section. Only used if section is non-NULL.     *
 * Returns: SCM handle for unregistering                            *
\********************************************************************/
SCM
gnc_register_option_change_callback(OptionChangeCallback callback,
                                    void *user_data,
                                    char *section,
                                    char *name)
{
  return gnc_option_db_register_change_callback(global_options, callback,
                                                user_data, section, name);
}


/********************************************************************\
 * gnc_unregister_option_change_callback_id                         *
 *   unregister the change callback associated with the given id    *
 *                                                                  *
 * Args: callback_id - the callback function id                     *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_unregister_option_change_callback_id(SCM callback_id)
{
  gnc_option_db_unregister_change_callback_id(global_options, callback_id);
}


/********************************************************************\
 * gnc_get_option_by_name                                           *
 *   returns an option given section name and name                  *
 *                                                                  *
 * Args: section_name - name of section to search for               *
 *       name         - name to search for                          *
 * Returns: given option, or NULL if none                           *
\********************************************************************/
GNCOption *
gnc_get_option_by_name(const char *section_name, const char *name)
{
  return gnc_option_db_get_option_by_name(global_options,
                                          section_name, name);
}


/********************************************************************\
 * gnc_get_option_by_SCM                                            *
 *   returns an option given SCM handle. Uses section and name.     *
 *                                                                  *
 * Args: guile_option - SCM handle of option                        *
 * Returns: given option, or NULL if none                           *
\********************************************************************/
GNCOption *
gnc_get_option_by_SCM(SCM guile_option)
{
  return gnc_option_db_get_option_by_SCM(global_options, guile_option);
}


/********************************************************************\
 * gnc_lookup_boolean_option                                        *
 *   looks up a boolean option. If present, returns its value,      *
 *   otherwise returns the default.                                 *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: gboolean option value                                    *
\********************************************************************/
gboolean
gnc_lookup_boolean_option(const char *section, const char *name,
                          gboolean default_value)
{
  return gnc_option_db_lookup_boolean_option(global_options, section,
                                             name, default_value);
}


/********************************************************************\
 * gnc_lookup_string_option                                         *
 *   looks up a string option. If present, returns its malloc'ed    *
 *   value, otherwise returns the strdup'ed default, or NULL if     *
 *   default was NULL.                                              *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
char *
gnc_lookup_string_option(const char *section, const char *name,
                         char *default_value)
{
  return gnc_option_db_lookup_string_option(global_options, section,
                                            name, default_value);
}


/********************************************************************\
 * gnc_lookup_multichoice_option                                    *
 *   looks up a multichoice option. If present, returns its         *
 *   name as a malloc'ed string                                     *
 *   value, otherwise returns the strdup'ed default, or NULL if     *
 *   default was NULL.                                              *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
char *
gnc_lookup_multichoice_option(const char *section, const char *name,
                              char *default_value)
{
  return gnc_option_db_lookup_multichoice_option(global_options, section,
                                                 name, default_value);
}


/********************************************************************\
 * gnc_lookup_number_range_option                                   *
 *   looks up a number range option. If present, return its value   *
 *   as a gdouble, otherwise returns default_value.                 *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 *       default - default value if not found                       *
 * Return: char * option value                                      *
\********************************************************************/
gdouble
gnc_lookup_number_range_option(const char *section, const char *name,
                               gdouble default_value)
{
  return gnc_option_db_lookup_number_range_option(global_options, section,
                                                  name, default_value);
}


/********************************************************************\
 * gnc_lookup_color_option                                          *
 *   looks up a color option. If present, returns its value in the  *
 *   color variable, otherwise leaves the color variable alone.     *
 *                                                                  *
 * Args: section   - section name of option                         *
 *       name      - name of option                                 *
 *       red       - where to store the red value                   *
 *       blue      - where to store the blue value                  *
 *       green     - where to store the green value                 *
 *       alpha     - where to store the alpha value                 *
 * Return: true if option was found                                 *
\********************************************************************/
gboolean gnc_lookup_color_option(const char *section, const char *name,
                                 gdouble *red, gdouble *green,
                                 gdouble *blue, gdouble *alpha)
{
  return gnc_option_db_lookup_color_option(global_options, section, name,
                                           red, green, blue, alpha);
}


/********************************************************************\
 * gnc_lookup_color_option_argb                                     *
 *   looks up a color option. If present, returns its argb value,   *
 *   otherwise returns the given default value.                     *
 *                                                                  *
 * Args: odb       - option database to search in                   *
 *       section   - section name of option                         *
 *       name      - name of option                                 *
 *       default_value - default value to return if problem         *
 * Return: argb value                                               *
\********************************************************************/
uint32
gnc_lookup_color_option_argb(const char *section, const char *name,
                             uint32 default_value)
{
  return gnc_option_db_lookup_color_option_argb(global_options, section, name,
                                                default_value);
}


/********************************************************************\
 * gnc_lookup_list_option                                           *
 *   looks up a list option. If present, returns its value as a     *
 *   list of strings representing the symbols.                      *
 *                                                                  *
 * Args: section   - section name of option                         *
 *       name      - name of option                                 *
 *       default_value - default value to return if problem         *
 * Return: list of values                                           *
\********************************************************************/
GSList *
gnc_lookup_list_option(const char *section, const char *name,
                       GSList *default_value)
{
  return gnc_option_db_lookup_list_option(global_options, section, name,
                                          default_value);
}


/********************************************************************\
 * gnc_set_option_default                                           *
 *   set the option to its default value                            *
 *                                                                  *
 * Args: section - section name of option                           *
 *       name    - name of option                                   *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_set_option_default(const char *section, const char *name)
{
  gnc_option_db_set_option_default(global_options, section, name);
}


/********************************************************************\
 * gnc_set_number_range_option                                      *
 *   sets the number range option to the given value. If successful *
 *   returns TRUE, otherwise FALSE.                                 *
 *                                                                  *
 * Args: section   - section name of option                         *
 *       name      - name of option                                 *
 *       value     - value to set to                                *
 * Return: success indicator                                        *
\********************************************************************/
gboolean
gnc_set_number_range_option(const char *section, const char *name,
                            gdouble value)
{
  return gnc_option_db_set_number_range_option(global_options,
                                               section, name,
                                               value);
}


/********************************************************************\
 * _gnc_option_refresh_ui                                           *
 *   sets the GUI representation of an option with its current      *
 *   current guile value. this is intended for use by guile only    *
 *                                                                  *
 * Args: option - SCM handle to option                              *
 * Return: nothing                                                  *
\********************************************************************/
void
_gnc_option_refresh_ui(SCM guile_option)
{
  GNCOption *option;

  option = gnc_option_db_get_option_by_SCM(global_options, guile_option);
  gnc_option_set_ui_value(option, FALSE);
}


static void
gnc_options_dialog_apply_cb(GnomePropertyBox *propertybox,
			    gint arg1, gpointer user_data)
{
  if (arg1 == -1)
    gnc_option_db_commit(global_options);
}

static void
gnc_options_dialog_help_cb(GnomePropertyBox *propertybox,
			   gint arg1, gpointer user_data)
{
  helpWindow(NULL, HELP_STR, HH_GLOBPREFS);
}

/* Options dialog... this should house all of the config options     */
/* like where the docs reside, and whatever else is deemed necessary */
void
gnc_show_options_dialog()
{
  static GnomePropertyBox *options_dialog = NULL;

  if (gnc_option_db_num_sections(global_options) == 0)
  {
    gnc_warning_dialog("No options!");
    return;
  }

  if (gnc_option_db_dirty(global_options))
  {
    if (options_dialog != NULL)
      gtk_widget_destroy(GTK_WIDGET(options_dialog));

    options_dialog = NULL;
  }

  if (options_dialog == NULL)
  {
    options_dialog = GNOME_PROPERTY_BOX(gnome_property_box_new());
    gnome_dialog_close_hides(GNOME_DIALOG(options_dialog), TRUE);

    gnc_build_options_dialog_contents(options_dialog, global_options);
    gnc_option_db_clean(global_options);

    gtk_window_set_title(GTK_WINDOW(options_dialog), GNC_PREFS);

    gtk_signal_connect(GTK_OBJECT(options_dialog), "apply",
		       GTK_SIGNAL_FUNC(gnc_options_dialog_apply_cb),
		       NULL);

    gtk_signal_connect(GTK_OBJECT(options_dialog), "help",
		       GTK_SIGNAL_FUNC(gnc_options_dialog_help_cb),
		       NULL);
  }

  gtk_widget_show(GTK_WIDGET(options_dialog));  
  gdk_window_raise(GTK_WIDGET(options_dialog)->window);
}
