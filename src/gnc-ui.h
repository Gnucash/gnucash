/********************************************************************\
 * gnc-ui.h - High level UI functions for GnuCash                   *
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
\********************************************************************/

#ifndef __GNC_UI_H__
#define __GNC_UI_H__

#include "config.h"

#include <glib.h>

#include "gnc-ui-common.h"
#include "Account.h"


/** Help Files ******************************************************/
#define HH_ABOUT             "xacc-about.html"
#define HH_ACC               "xacc-newacctwin.html"
#define HH_ACCEDIT           "xacc-accountedit.html"
#define HH_ADJBWIN           "xacc-adjbwin.html"
#define HH_COMMODITY         "xacc-commodity.html"
#define HH_FIND_TRANSACTIONS "xacc-locatingtxns.html"
#define HH_GLOBPREFS         "xacc-preferences.html"
#define HH_GPL               "xacc-gpl.html"
#define HH_MAIN              "index.html"
#define HH_PRINT             "xacc-print.html"
#define HH_PRINTCHECK        "xacc-print-check.html"
#define HH_QIFIMPORT         "xacc-qif-import.html"
#define HH_QUICKSTART        "xacc-quickstart.html"
#define HH_RECNWIN           "xacc-recnwin.html"
#define HH_REGWIN            "xacc-regwin.html"

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

GNCVerifyResult gnc_verify_cancel_dialog(const char *message, 
                                         GNCVerifyResult default_result);

gboolean gnc_verify_dialog_parented(gncUIWidget parent,
                                    const char *message,
                                    gboolean yes_is_default);

GNCVerifyResult gnc_ok_cancel_dialog_parented(gncUIWidget parent,
                                              const char *message,
                                              GNCVerifyResult default_result);

void     gnc_warning_dialog_parented(gncUIWidget parent, const char *message);

gboolean gnc_verify_dialog(const char *message, gboolean yes_is_default);
void     gnc_error_dialog(const char *message);

int      gnc_choose_radio_option_dialog_parented(gncUIWidget parent,
                                                 const char *title,
                                                 const char *msg,
                                                 int default_value,
                                                 GList *radio_list);

gboolean gnc_dup_trans_dialog (gncUIWidget parent, time_t *date_p,
                               const char *num, char **out_num);
void     gnc_tax_info_dialog (gncUIWidget parent);
void     gnc_stock_split_dialog (Account * initial);
void     gnc_prices_dialog (gncUIWidget parent);
void     gnc_commodities_dialog (gncUIWidget parent);

/* Open a dialog asking for username and password. The heading and
 * either 'initial_*' arguments may be NULL. If the dialog returns
 * TRUE, the user pressed OK and the entered strings are stored in the
 * output variables. They should be g_freed when no longer needed. If
 * the dialog returns FALSE, the user pressed CANCEL and NULL was
 * stored in username and password. */
gboolean gnc_get_username_password (gncUIWidget parent,
                                    const char *heading,
                                    const char *initial_username,
                                    const char *initial_password,
                                    char **username,
                                    char **password);

void gnc_ui_new_user_dialog (void);
void gnc_ui_hierarchy_druid (void);

/* Managing the GUI Windows *****************************************/

void        gnc_ui_shutdown (void);
void        gnc_ui_destroy_all_subwindows (void);
gncUIWidget gnc_ui_get_toplevel(void);

/* Changing the GUI Cursor ******************************************/

void gnc_set_busy_cursor(gncUIWidget w, gboolean update_now);
void gnc_unset_busy_cursor(gncUIWidget w);

/* QIF Import Windows ***********************************************/

typedef struct _qifimportwindow QIFImportWindow;

QIFImportWindow * gnc_ui_qif_import_dialog_make(void);
void              gnc_ui_qif_import_dialog_destroy(QIFImportWindow * window);


#endif
