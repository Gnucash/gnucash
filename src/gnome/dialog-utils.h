/********************************************************************\
 * dialog-utils.h -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999-2000 Linas Vepstas                            *
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
 *                                                                  *
\********************************************************************/

#ifndef __DIALOG_UTILS_H__
#define __DIALOG_UTILS_H__

#include <gnome.h>

#include "Account.h"


/* Information about the account edit frame and entry widgets */
typedef struct _AccountEditInfo AccountEditInfo;
struct _AccountEditInfo
{
  GtkEditable * name_entry;
  GtkEditable * type_entry;
  GtkEditable * description_entry;
  GtkEditable * currency_entry;
  GtkEditable * security_entry;
  GtkEditable * code_entry;
  GtkEditable * notes_entry;

  GtkOptionMenu * source_menu;
  gint source;
};


typedef struct _AccountFieldStrings AccountFieldStrings;
struct _AccountFieldStrings
{
  gchar * name;
  gchar * description;
  gchar * currency;
  gchar * security;
  gchar * code;
  gchar * notes;
  gchar * source;
};


typedef enum
{
  SOURCE_NONE = 0,
  SOURCE_YAHOO,
  SOURCE_YAHOO_EUROPE,
  SOURCE_FIDELITY,
  SOURCE_TROWEPRICE,
  SOURCE_VANGUARD,
  SOURCE_ASX,
  SOURCE_TIAA_CREF,
  NUM_SOURCES
} PriceSourceCode;


typedef enum
{
  ACCOUNT_TYPE = 0,
  ACCOUNT_NAME,
  ACCOUNT_CODE,
  ACCOUNT_DESCRIPTION,
  ACCOUNT_NOTES,
  ACCOUNT_CURRENCY,
  ACCOUNT_SECURITY,
  ACCOUNT_BALANCE, /* with sign reversal */
  ACCOUNT_TOTAL,   /* balance + children's balance with sign reversal */
  NUM_ACCOUNT_FIELDS
} AccountFieldCode;


/* option button callback function */
typedef void (*GNCOptionCallback) (GtkWidget *, gint index,
                                   gpointer user_data);

/* Structure for building option buttons */
typedef struct _GNCOptionInfo GNCOptionInfo;
struct _GNCOptionInfo
{
  char *name;
  char *tip;
  GNCOptionCallback callback;
  gpointer user_data;
};


/**** PROTOTYPES *************************************************/
char * gnc_ui_get_account_field_name(int field);

char * gnc_ui_get_account_field_value_string(Account *account, int field);

double gnc_ui_account_get_balance(Account *account, gboolean include_children);

GtkWidget * gnc_ui_notes_frame_create(GtkEditable **notes_entry);

GtkWidget * gnc_ui_account_field_box_create(AccountEditInfo * info,
					    gboolean include_type);

GtkWidget *
gnc_ui_account_field_box_create_from_account(Account * account,
					     AccountEditInfo * info);

GtkWidget * gnc_ui_create_account_label(int field_type);

void gnc_ui_extract_field_strings(AccountFieldStrings *strings,
				  AccountEditInfo *edit_info);

void gnc_ui_free_field_strings(AccountFieldStrings *strings);

void gnc_ui_install_field_strings(Account * account,
				  AccountFieldStrings *strings,
				  gboolean new_code);

GtkWidget *
gnc_ui_account_source_box_create_from_account(Account * account,
					      AccountEditInfo * info);

GtkWidget * gnc_ui_account_source_box_create(AccountEditInfo * info);

GtkWidget * gnc_ui_account_menu_create(AccountEditInfo * info);

gchar * gnc_get_source_name(gint source);
gchar * gnc_get_source_code_name(gint source);
gint    gnc_get_source_code(gchar * codename);

GtkWidget * gnc_build_option_menu(GNCOptionInfo *option_info,
				  gint num_options);

GtkToolbarStyle gnc_get_toolbar_style();

void gnc_get_deficit_color(GdkColor *color);
void gnc_set_label_color(GtkWidget *label, double value);

char gnc_get_account_separator();

void gnc_get_window_size(const char *prefix, int *width, int *height);
void gnc_save_window_size(const char *prefix, int width, int height);

void gnc_fill_menu_with_data(GnomeUIInfo *info, gpointer data);

#endif
