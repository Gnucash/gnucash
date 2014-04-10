/********************************************************************\
 * ui-callbacks.h                                                   *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1999, 2000 Rob Browning <rlb@cs.utexas.edu>        *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __UI_CALLBACKS_H__
#define __UI_CALLBACKS_H__

#include <glib.h>

#include "top-level.h"


/* Dialog windows ***************************************************/
typedef enum
{
  GNC_VERIFY_NO,
  GNC_VERIFY_YES,
  GNC_VERIFY_CANCEL,
  GNC_VERIFY_OK
} GNCVerifyResult;

GNCVerifyResult
gnc_verify_cancel_dialog_parented(gncUIWidget parent,
                                  const char *message,
                                  GNCVerifyResult default_result);

GNCVerifyResult
gnc_ok_cancel_dialog_parented(gncUIWidget parent,
                              const char *message,
                              GNCVerifyResult default_result);

void gnc_warning_dialog_parented(gncUIWidget parent, const char *message);

gncBoolean gnc_verify_dialog( const char *message,
			      gncBoolean yes_is_default );
void       gnc_error_dialog( const char *message );

int gnc_choose_radio_option_dialog_parented(gncUIWidget parent,
                                            const char *title,
                                            const char *msg,
                                            int default_value,
                                            char **radio_list);


/* Managing the GUI Windows *****************************************/
void gnc_refresh_main_window( void );
void gnc_ui_destroy_all_subwindows( void );


/* Changing the GUI Cursor ******************************************/
void gnc_set_busy_cursor( gncUIWidget w );
void gnc_unset_busy_cursor( gncUIWidget w );

/* Getting main window information **********************************/
Account * gnc_get_current_account();
GList   * gnc_get_current_accounts();

/* QIF Import Windows ***********************************************/
typedef struct _qifimportwindow QIFImportWindow;

QIFImportWindow * gnc_ui_qif_import_dialog_make();
void gnc_ui_qif_import_dialog_destroy(QIFImportWindow * window);


#endif
