/********************************************************************
 * dialog-commodity.h -- "select" and "new" commodity windows       *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 ********************************************************************/

/** @addtogroup UI
    @{ */
/** @file dialog-commodity.h
    @brief "select" and "new" commodity windows
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
*/

#ifndef GNC_DIALOG_COMMODITY_H
#define GNC_DIALOG_COMMODITY_H

#include <gnome.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"

typedef void (* gnc_commodity_help_callback)(void);

/** This function is used to set the action routine for the help
 *  button in the commodity dialog windows.  If the action routine is
 *  unset, the help button will not be visible to the user.
 *  
 *  @param cb The function to be called when the user clicks the help
 *  button. */
void gnc_ui_commodity_set_help_callback (gnc_commodity_help_callback cb);


/** @name Commodity Selection */
/** @{ */

/** Ask the user to select a commodity from the existing set of
 *  commodities.  Arguments to this function determine the message
 *  placed at the top of the dialog but force no restriction on the
 *  commodities that may be chosen.  The user will also have the
 *  option of creating a new commodity from this dialog box..  If the
 *  user decides to create a new one, those provided values are used
 *  as default values for the new commodity.
 *
 *  @param orig_sel A pointer to a commodity that should initially be
 *  selected in the dialog box.
 *
 *  @param parent The parent window of the new dialog.
 *
 *  @param user_message A string that will be installed in the top of
 *  the dialog box as an instruction to the user.  If NULL, a generic
 *  instruction will be used.
 *
 *  @param exchange_code If present, a note will be added to the user
 *  instruction providing this exchange specific code, and this will
 *  be the default exchange code for any newly created commodities.
 *
 *  @param fullname If present, a note will be added to the user
 *  instruction providing this commodity's full name, and this will be
 *  the default fullname for any newly created commodities.
 *
 *  @param mnemonic If present, a note will be added to the user
 *  instruction providing this commodity's mnemonic, and this will be
 *  the default mnemonic for any newly created commodities.
 *
 *  @return The commodity selected.  May or may not be a newly created
 *  commodity.
 */
gnc_commodity * 
gnc_ui_select_commodity_modal_full(gnc_commodity * orig_sel, 
				   GtkWidget * parent,
				   const char * user_message,
				   const char * exchange_code,
				   const char * fullname,
				   const char * mnemonic);


/** Ask the user to select a commodity from the existing set of
 *  commodities.  The user will also have the
 *  option of creating a new commodity from this dialog box..  If the
 *  user decides to create a new one, those provided values are used
 *  as default values for the new commodity.
 *
 *  @param orig_sel A pointer to a commodity that should initially be
 *  selected in the dialog box.
 *
 *  @return The commodity selected.  May or may not be a newly created
 *  commodity.
 */
gnc_commodity * 
gnc_ui_select_commodity_modal(gnc_commodity * orig_sel, 
                              GtkWidget * parent);
/** @} */


/** @name Commodity Creation or Modification */
/** @{ */

/** Ask the user to provide the information necessary to create a new
 *  commodity.
 *
 *  @param namespace If present, this will be the default namespace
 *  for the new commodity.  This value will be ignored if it is the
 *  namespace for ISO 4217 currencies.
 *
 *  @param parent The parent window of the new dialog.
 *
 *  @param user_message A string that will be installed in the top of
 *  the dialog box as an instruction to the user.  If NULL, a generic
 *  instruction will be used.
 *
 *  @param exchange_code If present, this will be the default exchange
 *  code for the new commodity.
 *
 *  @param fullname If present, this will be the default fullname for
 *  the new commodity.
 *
 *  @param mnemonic If present, this will be the default mnemonic for
 *  the new commodity.
 *
 *  @param fraction If present, this will be the default fraction for
 *  the new commodity.  If absent, a default of 1000 will be used.
 *  
 *  @return The newly created commodity, or NULL if the user cancelled.
 */
gnc_commodity * 
gnc_ui_new_commodity_modal_full(const char * namespace, 
				GtkWidget * parent,
				const char * exchange_code,
				const char * fullname,
				const char * mnemonic,
				int fraction);

/** Ask the user to provide the information necessary to create a new
 *  commodity.
 *
 *  @param namespace If present, this will be the default namespace
 *  for the new commodity.  This value will be ignored if it is the
 *  namespace for ISO 4217 currencies.
 *
 *  @param parent The parent window of the new dialog.
 *  
 *  @return The newly created commodity, or NULL if the user cancelled.
 */
gnc_commodity * 
gnc_ui_new_commodity_modal(const char * default_namespace, 
                           GtkWidget * parent
			   );

/** Allow the user to edit the information about a commodity.  For
 *  currencies, only the price quote information may be changed.  For
 *  any other commodity, all aspects of the commodity information may
 *  be changed except that the namespace may not be changed to
 *  indicate a currency.  The new information overwrites any old
 *  information, so this routine may not be used to create new
 *  commodities.
 *
 *  @param commodity The commodity to edit.
 *
 *  @param parent The parent window of the new dialog.
 *  
 *  @return The newly created commodity, or NULL if the user cancelled.
 */
gboolean
gnc_ui_edit_commodity_modal(gnc_commodity *commodity,
                            GtkWidget * parent);
/** @} */


/** @name Auxiliary Dialog Functions */
/** @{ */

/** Given a combo box, fill in the known commodity namespaces and then
 *  select one.
 *
 *  @param combobox The combo box to populate with information.
 *
 *  @param sel The namespace that should be initially selected when
 *  the combo box appears.
 *
 *  @param include_iso Set to TRUE if the combo box should inlude the
 *  ISO4217 namespace for currencies.  FALSE if the currency namespace
 *  should not be included.  This flag has precedence over the
 *  following flag.
 *
 *  @param include_all Set to TRUE if the combo box should include all
 *  known namespaces, both application and user defined.  FALSE if
 *  only the default application namespaces should be included.
 *
 *  @return The currently selected namespace.
 *
 *  @note The returned string must be freed by the caller.
 */
char * gnc_ui_update_namespace_picker(GtkWidget * combobox,
                                      const char * sel,
                                      gboolean include_iso,
                                      gboolean include_all);

/** Given a combo box, return the currently selected namespaces.
 *
 *  @param combobox The combo box of namespaces.
 *
 *  @return The currently selected namespace.
 *
 *  @note This string is owned by the engine and must not be freed by
 *  the caller.
 */
const char * gnc_ui_namespace_picker_ns (GtkWidget *combobox);

/** Given a combo box, fill in all the known commodities for the
 *  specified namespace, and then select one.
 *
 *  @param combobox The combo box to populate with information.
 *
 *  @param namespace All commodities with this namespace will be added
 *  to the combo box.
 *
 *  @param sel The commodity that should be initially selected when
 *  the combo box appears.
 */
void gnc_ui_update_commodity_picker(GtkWidget * combobox, 
                                    const char * namespace,
                                    const char * sel);
/** @} */

#endif
/** @} */
