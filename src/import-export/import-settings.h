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
/**@file import-settings.h
   \brief Import preference handling.
   *User preference interface for 
   transaction matching (for both the gui and the backend)
   \author Copyright (C) 2002 Benoit Grégoire
*/
 
#ifndef GNC_GEN_SETTINGS_H
#define GNC_GEN_SETTINGS_H

typedef struct _genimportsettings GNCImportSettings;

/************************************************************************
 *   Getter/Setter Functions for the Data Types. 
 ************************************************************************/



/** Allocates a new GNCImportSettings object, and initialize it with the
    appropriate user prefs.
*/
GNCImportSettings * 
gnc_import_Settings_new (void);

/** Destructor */
void gnc_import_Settings_delete (GNCImportSettings *settings);


/** @name Getters/Setters for GNCImportSettings */
/*@{*/

/** Return the allowed amount range for fuzzy amount matching.
    @return The allowed amount range for fuzzy amount matching, 
    in the users default commodity.
*/
double 
gnc_import_Settings_get_fuzzy_amount (GNCImportSettings *settings);

/** Return the selected action is enable state.
*/
gboolean gnc_import_Settings_get_action_skip_enabled (GNCImportSettings *settings);

/** Return the selected action is enable state.
*/
gboolean gnc_import_Settings_get_action_add_enabled (GNCImportSettings *settings);

/** Return the selected action is enable state.
*/
gboolean gnc_import_Settings_get_action_edit_enabled (GNCImportSettings *settings);

/** Return the selected action is enable state.
*/
gboolean gnc_import_Settings_get_action_clear_enabled (GNCImportSettings *settings);

/** Return the selected threshold.
*/
gint gnc_import_Settings_get_clear_threshold (GNCImportSettings *settings);

/** Return the selected threshold.
*/
gint gnc_import_Settings_get_add_threshold (GNCImportSettings *settings);

/** Return the selected threshold.
*/
gint gnc_import_Settings_get_display_threshold (GNCImportSettings *settings);
/**@}*/
/**@}*/

#endif
