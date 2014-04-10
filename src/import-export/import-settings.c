/********************************************************************\
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
/** @addtogroup Import_Export
    @{ */
/**@internal
   @file import-settings.c
   @brief User preference handling for 
   transaction matching (for both the gui and the backend)
   @author Copyright (C) 2002 Benoit Grégoire
*/
#define _GNU_SOURCE

#include "config.h"

#include <glib.h>
#include "dialog-utils.h"
#include "global-options.h"
#include "import-settings.h"

/********************************************************************\
 * Default values for user prefs (or values for unimplemented prefs.*
 * If you modify the value of any of these, you must do the same in *
 * generic-import.scm                                               *
\********************************************************************/
#define MATCHER_PREF_PAGE _("Online Banking & Importing")

/** Transaction who's best match probability is equal or higher than
   this will reconcile their best match by default */
#define DEFAULT_CLEAR_THRESHOLD 6
/** Transaction who's best match probability is below or equal to 
   this will be added as new by default */
#define DEFAULT_ADD_THRESHOLD 3
/** Transaction's match probability must be at least this much to be
   displayed in the match list.  Dont set this to 0 except for 
   debugging purposes, otherwise all transactions of every accounts 
   will be shown in the list */
#define DEFAULT_DISPLAY_THRESHOLD 1

#define DEFAULT_ATM_FEE_THRESHOLD 2.00

static const int DEFAULT_ACTION_SKIP_ENABLED = TRUE;
static const int DEFAULT_ACTION_EDIT_ENABLED = FALSE;
static const int DEFAULT_ACTION_ADD_ENABLED = TRUE;
static const int DEFAULT_ACTION_CLEAR_ENABLED = TRUE;

/********************************************************************\
 *               Structures passed between the functions             *
\********************************************************************/

struct _genimportsettings {

  gboolean action_skip_enabled;
  gboolean action_edit_enabled;
  gboolean action_add_enabled;
  gboolean action_clear_enabled;
  
  /**Transaction who's best match probability is equal or higher than
     this will reconcile their best match by default */
  int clear_threshold;
  /**Transaction who's best match probability is below or equal to 
     this will be added as new by default */
  int add_threshold;
  /**Transaction's match probability must be at least this much to be 
     displayed in the match list.  Dont set this to 0 except for 
     debugging purposes, otherwise all transactions of every accounts 
     will be shown in the list */
  int display_threshold;
  /** The allowed amount range for fuzzy amount matching, 
      in the users default commodity.*/
  double fuzzy_amount;
};


/************************************************************************
 *   Getter/Setter Functions for the Data Types. 
 ************************************************************************/
/* Constructor */
GNCImportSettings *
gnc_import_Settings_new (void)
{
  GNCImportSettings * settings;
  
  settings = g_new0 ( GNCImportSettings, 1);

  
  settings->action_skip_enabled = 
    gnc_lookup_boolean_option(MATCHER_PREF_PAGE,
			      "Enable SKIP transaction action",
			      DEFAULT_ACTION_SKIP_ENABLED);
  settings->action_edit_enabled =
    gnc_lookup_boolean_option(MATCHER_PREF_PAGE,
			      "Enable EDIT match action",
			      DEFAULT_ACTION_EDIT_ENABLED);
  settings->action_add_enabled=DEFAULT_ACTION_ADD_ENABLED;
  settings->action_clear_enabled=DEFAULT_ACTION_CLEAR_ENABLED;
  settings->clear_threshold=gnc_lookup_number_option(MATCHER_PREF_PAGE,
						     "Auto-CLEAR threshold",
						     DEFAULT_CLEAR_THRESHOLD);
  settings->add_threshold=gnc_lookup_number_option(MATCHER_PREF_PAGE,
						   "Auto-ADD threshold",
						   DEFAULT_ADD_THRESHOLD);
  settings->display_threshold =
    gnc_lookup_number_option(MATCHER_PREF_PAGE,"Match display threshold",
			     DEFAULT_DISPLAY_THRESHOLD);

  settings->fuzzy_amount =
    gnc_lookup_number_option(MATCHER_PREF_PAGE,"Commercial ATM fees threshold",
			     DEFAULT_ATM_FEE_THRESHOLD);
  
  return settings;
}

/* Destructor */
void gnc_import_Settings_delete (GNCImportSettings *settings)
{
  if (settings) {
    g_free(settings);
  }
}

double 
gnc_import_Settings_get_fuzzy_amount (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->fuzzy_amount;
};

gboolean gnc_import_Settings_get_action_skip_enabled (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->action_skip_enabled;
};

gboolean gnc_import_Settings_get_action_add_enabled (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->action_add_enabled;
};

gboolean gnc_import_Settings_get_action_edit_enabled (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->action_edit_enabled;
};

gboolean gnc_import_Settings_get_action_clear_enabled (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->action_clear_enabled;
};

gint gnc_import_Settings_get_clear_threshold (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->clear_threshold;
};

gint gnc_import_Settings_get_add_threshold (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->add_threshold;
};

gint gnc_import_Settings_get_display_threshold (GNCImportSettings *settings)
{
  g_assert (settings);
  return settings->display_threshold;
};

/**@}*/
