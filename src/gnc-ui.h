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


/** @addtogroup GUI
    @{ */

/** @defgroup Dialogs Dialogs */
/** @defgroup Druids Druids */

/** @} */


#ifndef GNC_UI_H
#define GNC_UI_H

#include <glib.h>

#include "gnc-ui-common.h"
#include "Account.h"
#include "gnc-pricedb.h"
#include "guile-mappings.h"


/** Help Files ******************************************************/
#ifdef G_OS_WIN32
#    define HF_GUIDE         "gnucash-guide.chm"
#    define HF_HELP          "gnucash-help.chm"
#else
#    define HF_GUIDE         "gnucash-guide.xml"
#    define HF_HELP          "gnucash-help.xml"
#endif

/** Links in the Help Files *****************************************/
#define HL_USAGE             "usage"
#define HL_ACC               "acct-create"
#define HL_ACCEDIT           "acct-edit"
#define HL_COMMODITY         "tool-commodity"
#define HL_FIND_TRANSACTIONS "tool-find"
#define HL_GLOBPREFS         "set-prefs"
#define HL_PRINTCHECK        "print-check"
#define HL_RECNWIN           "acct-reconcile"
#define HL_SXEDITOR          "tool-sched"
#define HL_GCONF             "gconf"

/* GTK Windows - Common Response Codes */

#define GNC_RESPONSE_NEW    1
#define GNC_RESPONSE_DELETE 2
#define GNC_RESPONSE_EDIT   3

/* Dialog windows ***************************************************/

extern gboolean
gnc_verify_dialog(gncUIWidget parent,
		  gboolean yes_is_default,
		  const char *format, ...) G_GNUC_PRINTF (3, 4);

extern gint
gnc_ok_cancel_dialog(gncUIWidget parent,
		     gint default_result,
		     const char *format, ...) G_GNUC_PRINTF (3,4);



extern void
gnc_warning_dialog(gncUIWidget parent,
		   const char *format, ...) G_GNUC_PRINTF (2, 3);



extern void
gnc_error_dialog(GtkWidget *parent,
		 const char *format, ...) G_GNUC_PRINTF (2, 3);


extern void
gnc_gnome_help (const char *file_name, const char *target_link);

int      gnc_choose_radio_option_dialog (gncUIWidget parent,
					 const char *title,
					 const char *msg,
					 const char *button_name,
					 int default_value,
					 GList *radio_list);

gboolean gnc_dup_trans_dialog (gncUIWidget parent, time_t *date_p,
                               const char *num, char **out_num);
void     gnc_tax_info_dialog (gncUIWidget parent);
void     gnc_stock_split_dialog (gncUIWidget parent, Account * initial);

typedef enum
{
  GNC_PRICE_EDIT,
  GNC_PRICE_NEW,
} GNCPriceEditType;

void gnc_price_edit_dialog (gncUIWidget parent, QofSession *session,
				 GNCPrice *price, GNCPriceEditType type);
GNCPrice* gnc_price_edit_by_guid (GtkWidget * parent, const GUID * guid);
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

/* Managing the GUI Windows *****************************************/

gncUIWidget gnc_ui_get_toplevel (void);

/* Changing the GUI Cursor ******************************************/

void gnc_set_busy_cursor(gncUIWidget w, gboolean update_now);
void gnc_unset_busy_cursor(gncUIWidget w);

/* QIF Import Windows ***********************************************/

typedef struct _qifimportwindow QIFImportWindow;

QIFImportWindow * gnc_ui_qif_import_dialog_make(void);
void              gnc_ui_qif_import_dialog_destroy(QIFImportWindow * window);


#endif
