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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/**@internal
   @file import-settings.c
   @brief User preference handling for
   transaction matching (for both the gui and the backend)
   @author Copyright (C) 2002 Benoit Grégoire
*/
#include "config.h"

#include <glib.h>

#include "import-settings.h"
#include "gnc-gconf-utils.h"

/********************************************************************\
 * Default values for user prefs (or values for unimplemented prefs.*
 * If you modify the value of any of these, you must do the same in *
 * generic-import.scm                                               *
\********************************************************************/
#define GCONF_SECTION "dialogs/import/generic_matcher"

static const int DEFAULT_ACTION_ADD_ENABLED = TRUE;
static const int DEFAULT_ACTION_CLEAR_ENABLED = TRUE;

/********************************************************************\
 *               Structures passed between the functions             *
\********************************************************************/

struct _genimportsettings
{

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
    gint match_date_hardlimit;
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
        gnc_gconf_get_bool(GCONF_SECTION, "enable_skip", NULL);
    settings->action_edit_enabled =
        gnc_gconf_get_bool(GCONF_SECTION, "enable_edit", NULL);
    settings->action_add_enabled = DEFAULT_ACTION_ADD_ENABLED;
    settings->action_clear_enabled = DEFAULT_ACTION_CLEAR_ENABLED;
    settings->clear_threshold =
        gnc_gconf_get_float(GCONF_SECTION, "auto_clear_threshold", NULL);
    settings->add_threshold =
        gnc_gconf_get_float(GCONF_SECTION, "auto_add_threshold", NULL);
    settings->display_threshold =
        gnc_gconf_get_float(GCONF_SECTION, "match_threshold", NULL);

    settings->fuzzy_amount =
        gnc_gconf_get_float(GCONF_SECTION, "atm_fee_threshold", NULL);

    settings->match_date_hardlimit = 42; /* 6 weeks */
    return settings;
}

/* Destructor */
void gnc_import_Settings_delete (GNCImportSettings *settings)
{
    if (settings)
    {
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

void gnc_import_Settings_set_match_date_hardlimit (GNCImportSettings *s, gint m)
{
    g_assert(s);
    s->match_date_hardlimit = m;
}
gint gnc_import_Settings_get_match_date_hardlimit (const GNCImportSettings *s)
{
    g_assert(s);
    return s->match_date_hardlimit;
}

/**@}*/
