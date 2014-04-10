/********************************************************************\
 * dialog-utils.c -- utility functions for creating dialogs         *
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

#include "top-level.h"

#include "gnome-top-level.h"
#include "account-tree.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-currency-edit.h"
#include "messages.h"
#include "EuroUtils.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_GUI;


/********************************************************************\
 * gnc_ui_notes_frame_create                                        *
 *   create the frame holding the 'notes' entry                     *
 *                                                                  *
 * Args: notes_entry - pointer to notes widget filled in by function*
 * Returns: notes in a box                                          *
 \*******************************************************************/
GtkWidget *
gnc_ui_notes_frame_create(GtkEditable **notes_entry)
{
  GtkWidget *frame, *text, *table, *vscr;
    
  frame = gtk_frame_new(NOTES_STR);
  gtk_container_border_width(GTK_CONTAINER(frame), 5);
  gtk_widget_show(frame);

  table = gtk_table_new(1, 2, FALSE);
  gtk_widget_show(table);
  gtk_container_add(GTK_CONTAINER(frame), table);
  gtk_container_border_width (GTK_CONTAINER (table), 3);

  text = gtk_text_new(NULL, NULL);
  *notes_entry = GTK_EDITABLE(text);
  gtk_widget_set_usize(text, 300, 50);
  gtk_widget_show(text);
  gtk_text_set_editable(GTK_TEXT(text), TRUE);

  gtk_table_attach (GTK_TABLE (table), text, 0, 1, 0, 1,
		    GTK_FILL | GTK_EXPAND,
 		    GTK_FILL | GTK_EXPAND | GTK_SHRINK, 0, 0);

  vscr = gtk_vscrollbar_new(GTK_TEXT(text)->vadj);
  gtk_table_attach(GTK_TABLE(table), vscr, 1, 2, 0, 1,
		   GTK_FILL, GTK_EXPAND | GTK_FILL | GTK_SHRINK, 0, 0);
  gtk_widget_show(vscr);

  return frame;
}


/********************************************************************\
 * gnc_ui_create_account_label                                      *
 *   creates an account label from an account field                 *
 *                                                                  *
 * Args: type - the field type to create the label for              *
 * Return: the label widget                                         *
\********************************************************************/
GtkWidget *
gnc_ui_create_account_label(int field_type)
{
  GtkWidget *label;
  gchar *label_string;
  const gchar *l = gnc_ui_get_account_field_name(field_type);

  label_string = g_strconcat(l, ":", NULL);

  label = gtk_label_new (label_string);
  gtk_misc_set_alignment (GTK_MISC(label), 1.0, 0.5);
  gtk_widget_show(label);

  g_free(label_string);

  return label;
}


/********************************************************************\
 * gnc_ui_account_field_box_create                                  *
 *   create the frame holding the edit fields for an account        *
 *                                                                  *
 * Args: info - info structure that is filled in by the function    *
 *       include_type - whether to include an account type entry    *
 * Returns: the box                                                 *
 \*******************************************************************/
GtkWidget *
gnc_ui_account_field_box_create(AccountEditInfo * info,
				gboolean include_type)
{
  GtkWidget *frame, *vbox, *hbox, *widget;

  frame = gtk_frame_new(ACC_INFO_STR);
  gtk_container_border_width(GTK_CONTAINER(frame), 5);
  gtk_widget_show(frame);

  hbox = gtk_hbox_new(FALSE, 5);
  gtk_container_border_width(GTK_CONTAINER(hbox), 5);
  gtk_container_add(GTK_CONTAINER(frame), hbox);
  gtk_widget_show(hbox);

  vbox = gtk_vbox_new(TRUE, 3);
  widget = gnc_ui_create_account_label(ACCOUNT_NAME);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  if (include_type)
  {
    widget = gnc_ui_create_account_label(ACCOUNT_TYPE);
    gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  }
  widget = gnc_ui_create_account_label(ACCOUNT_DESCRIPTION);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  widget = gnc_ui_create_account_label(ACCOUNT_CURRENCY);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  widget = gnc_ui_create_account_label(ACCOUNT_SECURITY);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  widget = gnc_ui_create_account_label(ACCOUNT_CODE);
  gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);
  gtk_widget_show(vbox);

  vbox = gtk_vbox_new(TRUE, 3);

  widget = gtk_entry_new();
  info->name_entry = GTK_EDITABLE(widget);
  gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
  gtk_widget_show(widget);

  if (include_type)
  {
    widget = gtk_entry_new();
    info->type_entry = GTK_EDITABLE(widget);
    /* Set the type entry box to insensitive by default */
    gtk_widget_set_sensitive(GTK_WIDGET(widget), FALSE);
    gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
    gtk_widget_show(widget);
  }

  widget = gtk_entry_new();
  info->description_entry = GTK_EDITABLE(widget);
  gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
  gtk_widget_show(widget);

  widget = gnc_currency_edit_new();
  info->currency_entry = GTK_EDITABLE(GTK_COMBO(widget)->entry);
  gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
  gtk_widget_show(widget);
    
  widget = gtk_entry_new();
  info->security_entry = GTK_EDITABLE(widget);
  gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
  gtk_widget_show(widget);

  widget = gtk_entry_new();
  info->code_entry = GTK_EDITABLE(widget);
  gtk_box_pack_start(GTK_BOX(vbox), widget, TRUE, TRUE, 0);
  gtk_widget_show(widget);

  gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
  gtk_widget_show(vbox);

  return frame;
}


/* Get the full name of a source */
gchar *
gnc_get_source_name(gint source)
{
  switch (source)
  {
    case SOURCE_NONE :
      return NONE_STR;
    case SOURCE_YAHOO :
      return "Yahoo";
    case SOURCE_YAHOO_EUROPE :
      return "Yahoo Europe";
    case SOURCE_FIDELITY :
      return "Fidelity";
    case SOURCE_TROWEPRICE :
      return "T. Rowe Price";
    case SOURCE_VANGUARD :
      return "Vanguard";
    case SOURCE_ASX :
      return "ASX";
    case SOURCE_TIAA_CREF :
      return "TIAA-CREF";
  }

  PWARN("Unknown source");
  return NULL;
}

/* Get the codename string of a source */
gchar *
gnc_get_source_code_name(gint source)
{
  switch (source)
  {
    case SOURCE_NONE :
      return NULL;
    case SOURCE_YAHOO :
      return "YAHOO";
    case SOURCE_YAHOO_EUROPE :
      return "YAHOO_EUROPE";
    case SOURCE_FIDELITY :
      return "FIDELITY";
    case SOURCE_TROWEPRICE :
      return "TRPRICE";
    case SOURCE_VANGUARD :
      return "VANGUARD";
    case SOURCE_ASX :
      return "ASX";
    case SOURCE_TIAA_CREF :
      return "TIAACREF";
  }

  PWARN("Unknown source");
  return NULL;
}

/* Get the codename string of a source */
gint
gnc_get_source_code(gchar * codename)
{
  gint i;

  if (codename == NULL)
    return SOURCE_NONE;

  if (safe_strcmp(codename, "") == 0)
    return SOURCE_NONE;

  for (i = 1; i < NUM_SOURCES; i++)
    if (safe_strcmp(codename, gnc_get_source_code_name(i)) == 0)
      return i;

  PWARN("Unknown source");
  return SOURCE_NONE;
}


static void
gnc_source_menu_cb(GtkMenuItem *item, gpointer data)
{
  AccountEditInfo *info = (AccountEditInfo *) data;

  info->source = GPOINTER_TO_INT(gtk_object_get_user_data(GTK_OBJECT(item)));
}

/********************************************************************\
 * gnc_ui_source_menu_create                                        *
 *   create the menu of stock quote sources                         *
 *                                                                  *
 * Args: info - the structure to fill in on callback                *
 * Returns: the menu                                                *
 \*******************************************************************/
GtkWidget *
gnc_ui_account_menu_create(AccountEditInfo * info)
{
  gint i;
  GtkMenu   *menu;
  GtkWidget *item;

  menu = GTK_MENU(gtk_menu_new());
  gtk_widget_show(GTK_WIDGET(menu));

  for (i = 0; i < NUM_SOURCES; i++)
  {
    item = gtk_menu_item_new_with_label(gnc_get_source_name(i));
    gtk_widget_show(item);
    gtk_object_set_user_data(GTK_OBJECT(item), GINT_TO_POINTER(i));
    gtk_signal_connect(GTK_OBJECT(item), "activate",
		       GTK_SIGNAL_FUNC(gnc_source_menu_cb), info);
    gtk_menu_append(menu, item);
  }

  return GTK_WIDGET(menu);
}

/********************************************************************\
 * gnc_ui_account_source_box_create                                 *
 *   create the frame holding the source menu picker                *
 *                                                                  *
 * Args: info - info structure that is filled in by the function    *
 * Returns: the box                                                 *
 \*******************************************************************/
GtkWidget *
gnc_ui_account_source_box_create(AccountEditInfo * info)
{
  GtkWidget *frame, *hbox, *widget, *omenu;

  frame = gtk_frame_new(QUOTE_SRC_STR);
  gtk_container_border_width(GTK_CONTAINER(frame), 5);
  gtk_widget_show(frame);

  hbox = gtk_hbox_new(FALSE, 5);
  gtk_container_border_width(GTK_CONTAINER(hbox), 5);
  gtk_widget_show(hbox);

  widget = gtk_label_new(QUOTE_SRC_MSG);
  gtk_misc_set_alignment (GTK_MISC(widget), 1.0, 0.5);
  gtk_widget_show(widget);

  gtk_box_pack_start(GTK_BOX(hbox), widget, FALSE, FALSE, 5);

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);
  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu),
			   gnc_ui_account_menu_create(info));
  gtk_option_menu_set_history(GTK_OPTION_MENU(omenu), 0);
  info->source_menu = GTK_OPTION_MENU(omenu);
  info->source = 0;

  gtk_box_pack_start(GTK_BOX(hbox), omenu, TRUE, TRUE, 5);

  gtk_container_add(GTK_CONTAINER(frame), hbox);

  return frame;
}


/********************************************************************\
 * gnc_ui_account_source_box_create_from_account                    *
 *   create the frame holding the source menu from an account       *
 *                                                                  *
 * Args: account - the account to use                               *
 *       info - info structure that is filled in by the function    *
 * Returns: void                                                    *
 \*******************************************************************/
GtkWidget *
gnc_ui_account_source_box_create_from_account(Account * account,
					      AccountEditInfo * info)
{
  GtkWidget * box = gnc_ui_account_source_box_create(info);
  gchar *codename;
  AccInfo *accinfo;
  InvAcct *invacct;
  int accType;

  accType = xaccAccountGetType(account);
  if ((STOCK != accType) && (MUTUAL != accType) && (CURRENCY != accType))
    return box;

  accinfo = xaccAccountGetAccInfo(account);
  invacct = xaccCastToInvAcct(accinfo);
  if (invacct == NULL)
    return box;

  codename = xaccInvAcctGetPriceSrc(invacct);

  info->source = gnc_get_source_code(codename);
  gtk_option_menu_set_history(info->source_menu, info->source);

  return box;
}


static void
gnc_extract_string(GtkEditable *e, gchar **s)
{
  gchar * string;

  /* check for null widget */
  if (e == NULL)
  {
    *s = NULL;
    return;
  }

  /* get the string, it is g_malloc'ed */
  string = gtk_editable_get_chars(e, 0, -1);

  /* strip off whitespace */
  g_strstrip(string);

  *s = string;
}


/********************************************************************\
 * gnc_ui_extract_field_strings                                     *
 *   extract the field strings from the various editing widgets     *
 *   the strings are stripped of leading and trailing whitespaces   *
 *                                                                  *
 * Args: strings    - the string structure                          *
 *       edit_info  - the name, currency, etc. structure            *
 *       notes_info - the notes structure                           *
 * Returns: void                                                    *
 \*******************************************************************/
void gnc_ui_extract_field_strings(AccountFieldStrings *strings,
				  AccountEditInfo *edit_info)
{
  gnc_extract_string(edit_info->name_entry, &strings->name);
  gnc_extract_string(edit_info->description_entry, &strings->description);
  gnc_extract_string(edit_info->currency_entry, &strings->currency);
  gnc_extract_string(edit_info->security_entry, &strings->security);
  gnc_extract_string(edit_info->code_entry, &strings->code);
  gnc_extract_string(edit_info->notes_entry, &strings->notes);

  if (edit_info->source_menu != NULL)
    strings->source = gnc_get_source_code_name(edit_info->source);
  else
    strings->source = NULL;
}


/********************************************************************\
 * gnc_ui_free_field_strings                                        *
 *   frees the fields strings in a string structure                 *
 *                                                                  *
 * Args: strings    - the string structure                          *
 * Returns: void                                                    *
 \*******************************************************************/
void
gnc_ui_free_field_strings(AccountFieldStrings *strings)
{
  if (strings->name != NULL)
  {
    g_free(strings->name);
    strings->name = NULL;
  }

  if (strings->description != NULL)
  {
    g_free(strings->description);
    strings->description = NULL;
  }

  if (strings->currency != NULL)
  {
    g_free(strings->currency);
    strings->currency = NULL;
  }

  if (strings->security != NULL)
  {
    g_free(strings->security);
    strings->security = NULL;
  }

  if (strings->code != NULL)
  {
    g_free(strings->code);
    strings->code = NULL;
  }

  if (strings->notes != NULL)
  {
    g_free(strings->notes);
    strings->notes = NULL;
  }
}


/********************************************************************\
 * gnc_ui_account_field_box_create_from_account                     *
 *   create the frame holding the edit fields for an account        *
 *                                                                  *
 * Args: account - the account to use                               *
 *       info - info structure that is filled in by the function    *
 * Returns: void                                                    *
 \*******************************************************************/
GtkWidget *
gnc_ui_account_field_box_create_from_account(Account * account,
					     AccountEditInfo * info)
{
  GtkWidget * box = gnc_ui_account_field_box_create(info, FALSE);
  gboolean sensitive;
  int accType;

  gtk_entry_set_text(GTK_ENTRY(info->name_entry),
		     xaccAccountGetName(account));
  gtk_entry_set_text(GTK_ENTRY(info->description_entry),
		     xaccAccountGetDescription(account));
  gtk_entry_set_text(GTK_ENTRY(info->currency_entry),
		     xaccAccountGetCurrency(account));
  gtk_entry_set_text(GTK_ENTRY(info->security_entry),
		     xaccAccountGetSecurity(account));
  gtk_entry_set_text(GTK_ENTRY(info->code_entry),
		     xaccAccountGetCode(account));

  accType = xaccAccountGetType(account);

  sensitive = (STOCK == accType) ||
              (MUTUAL == accType) ||
              (CURRENCY == accType);

  gtk_widget_set_sensitive(GTK_WIDGET(info->security_entry), sensitive);

 return box;
}


/********************************************************************\
 * gnc_ui_install_field_strings                                     *
 *   installs the field strings in an account                       *
 *   does not do a begin or end edit                                *
 *                                                                  *
 * Args: account - the account to install into                      *
 *       strings - the strings to use                               *
 *       new_code - if code is blank, make new one                  *
 * Returns: void                                                    *
 \*******************************************************************/
void
gnc_ui_install_field_strings(Account * account,
			     AccountFieldStrings *strings,
			     gboolean new_code)
{
  int accType;
  const char * old;

  xaccAccountSetName(account, strings->name);
  xaccAccountSetDescription(account, strings->description);

  old = xaccAccountGetCurrency(account);
  if (safe_strcmp(old, strings->currency) != 0)
    xaccAccountSetCurrency(account, strings->currency);

  xaccAccountSetCode(account, strings->code);

  xaccAccountSetNotes(account, strings->notes);

  accType = xaccAccountGetType(account);
  if ((STOCK == accType) || (MUTUAL == accType) || (CURRENCY == accType))
  {
    AccInfo *accinfo;
    InvAcct *invacct;

    old = xaccAccountGetSecurity(account);
    if (safe_strcmp(old, strings->security) != 0)
      xaccAccountSetSecurity(account, strings->security);

    accinfo = xaccAccountGetAccInfo(account);
    invacct = xaccCastToInvAcct(accinfo);
    if (invacct != NULL)
      xaccInvAcctSetPriceSrc(invacct, strings->source);
  }
}


/* =========================================================== */

const char *
gnc_ui_get_account_field_name(int field)
{
  assert((field >= 0) && (field < NUM_ACCOUNT_FIELDS));

  switch (field)
  {
    case ACCOUNT_TYPE :
      return TYPE_STR;
      break;
    case ACCOUNT_NAME :
      return ACC_NAME_STR;
      break;
    case ACCOUNT_CODE :
      return ACC_CODE_STR;
      break;
    case ACCOUNT_DESCRIPTION :
      return DESC_STR;
      break;
    case ACCOUNT_NOTES :
      return NOTES_STR;
      break;
    case ACCOUNT_CURRENCY :
      return CURRENCY_STR;
      break;
    case ACCOUNT_SECURITY :
      return SECURITY_STR;
      break;
    case ACCOUNT_BALANCE :
      return BALN_STR;
      break;
    case ACCOUNT_BALANCE_EURO :
      return BALN_EURO_STR;
      break;
    case ACCOUNT_TOTAL :
      return TOTAL_STR;
      break;
    case ACCOUNT_TOTAL_EURO :
      return TOTAL_EURO_STR;
      break;
  }

  assert(0);
  return NULL;
}


double
gnc_ui_account_get_balance(Account *account, gboolean include_children)
{
  double balance;

  if (account == NULL)
    return 0.0;

  balance = xaccAccountGetBalance (account);

  if (include_children)
  {
    AccountGroup *children;

    children = xaccAccountGetChildren (account);
    balance += xaccGroupGetBalance (children);
  }

  /* reverse sign if needed */
  if (gnc_reverse_balance (account))
    balance = -balance;

  return balance;
}


const char *
gnc_ui_get_account_field_value_string(Account *account, int field)
{
  if (account == NULL)
    return NULL;

  assert((field >= 0) && (field < NUM_ACCOUNT_FIELDS));

  switch (field)
  {
    case ACCOUNT_TYPE :
      return xaccAccountGetTypeStr(xaccAccountGetType(account));
      break;
    case ACCOUNT_NAME :
      return xaccAccountGetName(account);
      break;
    case ACCOUNT_CODE :
      return xaccAccountGetCode(account);
      break;
    case ACCOUNT_DESCRIPTION :
      return xaccAccountGetDescription(account);
      break;
    case ACCOUNT_NOTES :
      return xaccAccountGetNotes(account);
      break;
    case ACCOUNT_CURRENCY :
      return xaccAccountGetCurrency(account);
      break;
    case ACCOUNT_SECURITY :
      return xaccAccountGetSecurity(account);
      break;
    case ACCOUNT_BALANCE :
      {
        double balance = gnc_ui_account_get_balance(account, FALSE);

	return xaccPrintAmount(balance, PRTSYM | PRTSEP,
			       xaccAccountGetCurrency(account));
      }
      break;
    case ACCOUNT_BALANCE_EURO :
      {
	const char *account_currency = xaccAccountGetCurrency(account);
        double balance = gnc_ui_account_get_balance(account, FALSE);
	double euro_balance = gnc_convert_to_euro(account_currency, balance);

	return xaccPrintAmount(euro_balance, PRTSYM | PRTSEP | PRTEUR, NULL);
      }
      break;
    case ACCOUNT_TOTAL :
      {
	double balance = gnc_ui_account_get_balance(account, TRUE);

	return xaccPrintAmount(balance, PRTSYM | PRTSEP,
			       xaccAccountGetCurrency(account));
      }
      break;
    case ACCOUNT_TOTAL_EURO :
      {
	const char *account_currency = xaccAccountGetCurrency(account);
	double balance = gnc_ui_account_get_balance(account, TRUE);
	double euro_balance = gnc_convert_to_euro(account_currency, balance);

	return xaccPrintAmount(euro_balance, PRTSYM | PRTSEP | PRTEUR, NULL);
      }
      break;
  }

  assert(0);
  return NULL;
}


static void
gnc_option_menu_cb(GtkWidget *w, gpointer data)
{
  GNCOptionCallback cb;
  gpointer _index;
  gint index;

  cb = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_cb");

  _index = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_index");
  index = GPOINTER_TO_INT(_index);

  cb(w, index, data);
}


/********************************************************************\
 * gnc_ui_create_option_button                                      *
 *   create an option button given the option structure             *
 *                                                                  *
 * Args: option_info - the option structure to use                  *
 *       num_options - the number of options                        *
 * Returns: void                                                    *
 \*******************************************************************/
GtkWidget *
gnc_build_option_menu(GNCOptionInfo *option_info, gint num_options)
{
  GtkTooltips *tooltips;
  GtkWidget *omenu;
  GtkWidget *menu;
  GtkWidget *menu_item;
  gint i;

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);

  menu = gtk_menu_new();
  gtk_widget_show(menu);

  tooltips = gtk_tooltips_new();

  for (i = 0; i < num_options; i++)
  {
    menu_item = gtk_menu_item_new_with_label(option_info[i].name);
    gtk_tooltips_set_tip(tooltips, menu_item, option_info[i].tip, NULL);
    gtk_widget_show(menu_item);

    gtk_object_set_data(GTK_OBJECT(menu_item),
                        "gnc_option_cb",
                        option_info[i].callback);

    gtk_object_set_data(GTK_OBJECT(menu_item),
                        "gnc_option_index",
                        GINT_TO_POINTER(i));

    gtk_object_set_data(GTK_OBJECT(menu_item),
                        "gnc_option_menu",
                        omenu);

    if (option_info[i].callback != NULL)
      gtk_signal_connect(GTK_OBJECT(menu_item), "activate",
                         GTK_SIGNAL_FUNC(gnc_option_menu_cb),
                         option_info[i].user_data);

    gtk_menu_append(GTK_MENU(menu), menu_item);
  }

  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu), menu);

  return omenu;
}


/********************************************************************\
 * gnc_get_toolbar_style                                            *
 *   returns the current toolbar style for gnucash toolbars         *
 *                                                                  *
 * Args: none                                                       *
 * Returns: toolbar style                                           *
 \*******************************************************************/
GtkToolbarStyle
gnc_get_toolbar_style()
{
  GtkToolbarStyle tbstyle = GTK_TOOLBAR_BOTH;
  char *style_string;

  style_string = gnc_lookup_multichoice_option("General",
                                               "Toolbar Buttons",
                                               "icons_and_text");

  if (safe_strcmp(style_string, "icons_and_text") == 0)
    tbstyle = GTK_TOOLBAR_BOTH;
  else if (safe_strcmp(style_string, "icons_only") == 0)
    tbstyle = GTK_TOOLBAR_ICONS;
  else if (safe_strcmp(style_string, "text_only") == 0)
    tbstyle = GTK_TOOLBAR_TEXT;

  if (style_string != NULL)
    free(style_string);

  return tbstyle;
}


/********************************************************************\
 * gnc_get_deficit_color                                            *
 *   fill in the 3 color values for the color of deficit values     *
 *                                                                  *
 * Args: color - color structure                                    *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_get_deficit_color(GdkColor *color)
{
  color->red   = 50000;
  color->green = 0;
  color->blue  = 0;
}


/********************************************************************\
 * gnc_set_label_color                                              *
 *   sets the color of the label given the value                    *
 *                                                                  *
 * Args: label - gtk label widget                                   *
 *       value - value to use to set color                          *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_set_label_color(GtkWidget *label, double value)
{
#if !USE_NO_COLOR
  gboolean deficit;
  GdkColormap *cm = gtk_widget_get_colormap(GTK_WIDGET(label));
  GtkStyle *style = gtk_widget_get_style(GTK_WIDGET(label));

  style = gtk_style_copy(style);

  deficit = (value < 0) && !DEQ(value, 0);

  if (deficit)
  {
    gnc_get_deficit_color(&style->fg[GTK_STATE_NORMAL]);
    gdk_colormap_alloc_color(cm, &style->fg[GTK_STATE_NORMAL], FALSE, TRUE);
  }
  else
    style->fg[GTK_STATE_NORMAL] = style->black;

  gtk_widget_set_style(label, style);
#endif
}


/********************************************************************\
 * gnc_get_toolbar_style                                            *
 *   returns the current account separator character                *
 *                                                                  *
 * Args: none                                                       *
 * Returns: account separator character                             *
 \*******************************************************************/
char
gnc_get_account_separator()
{
  char separator = ':';
  char *string;

  string = gnc_lookup_multichoice_option("General",
                                         "Account Separator",
                                         "colon");

  if (safe_strcmp(string, "colon") == 0)
    separator = ':';
  else if (safe_strcmp(string, "slash") == 0)
    separator = '/';
  else if (safe_strcmp(string, "backslash") == 0)
    separator = '\\';
  else if (safe_strcmp(string, "dash") == 0)
    separator = '-';
  else if (safe_strcmp(string, "period") == 0)
    separator = '.';

  if (string != NULL)
    free(string);

  return separator;
}


/********************************************************************\
 * gnc_get_window_size                                              *
 *   returns the window size to use for the given option prefix,    *
 *   if window sizes are being saved, otherwise returns 0 for both. *
 *                                                                  *
 * Args: prefix - the option name prefix                            *
 *       width  - pointer to width                                  *
 *       height - pointer to height                                 *
 * Returns: nothing                                                 *
 \*******************************************************************/
void
gnc_get_window_size(const char *prefix, int *width, int *height)
{
  int w, h;
  char *name;

  if (gnc_lookup_boolean_option("General", "Save Window Geometry", FALSE))
  {
    name = g_strconcat(prefix, "_width", NULL);
    w = gnc_lookup_number_option("__gui", name, 0.0);
    g_free(name);

    name = g_strconcat(prefix, "_height", NULL);
    h = gnc_lookup_number_option("__gui", name, 0.0);
    g_free(name);
  }
  else
  {
    w = 0;
    h = 0;
  }

  if (width != NULL)
    *width = w;

  if (height != NULL)
    *height = h;
}

void
gnc_save_window_size(const char *prefix, int width, int height)
{
  char *name;
  gboolean save;

  save = gnc_lookup_boolean_option("General", "Save Window Geometry", FALSE);

  name = g_strconcat(prefix, "_width", NULL);
  if (save)
    gnc_set_number_option("__gui", name, width);
  else
    gnc_set_option_default("__gui", name);
  g_free(name);

  name = g_strconcat(prefix, "_height", NULL);
  if (save)
    gnc_set_number_option("__gui", name, height);
  else
    gnc_set_option_default("__gui", name);
  g_free(name);
}

void
gnc_fill_menu_with_data(GnomeUIInfo *info, gpointer data)
{
  if (info == NULL)
    return;

  while (1)
  {
    switch (info->type)
    {
      case GNOME_APP_UI_RADIOITEMS:
      case GNOME_APP_UI_SUBTREE:
      case GNOME_APP_UI_SUBTREE_STOCK:
        gnc_fill_menu_with_data((GnomeUIInfo *) info->moreinfo, data);
        break;
      case GNOME_APP_UI_ENDOFINFO:
        return;
      default:
        info->user_data = data;
        break;
    }

    info++;
  }
}

void
gnc_option_menu_init(GtkWidget * w)
{
  GtkWidget * menu;
  GtkWidget * active;
  int i;

  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(w));

  for(i = 0; i < g_list_length(GTK_MENU_SHELL(menu)->children); i++)
  {
    gtk_option_menu_set_history(GTK_OPTION_MENU(w), i);
    active = gtk_menu_get_active(GTK_MENU(menu));
    gtk_object_set_data(GTK_OBJECT(active), 
                        "option_index",
                        GINT_TO_POINTER(i));
  }

  gtk_option_menu_set_history(GTK_OPTION_MENU(w), 0);
}

int
gnc_option_menu_get_active(GtkWidget * w)
{
  GtkWidget * menu;
  GtkWidget * menuitem;

  menu     = gtk_option_menu_get_menu(GTK_OPTION_MENU(w));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  return GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(menuitem),
                                             "option_index"));
}
