/********************************************************************\
 * dialog-utils.c -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999 Linas Vepstas                                 *
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

#include "top-level.h"

#include "account-tree.h"
#include "dialog-utils.h"
#include "messages.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
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
    
  frame = gtk_frame_new("Notes");
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
  gchar     *label_string = gnc_ui_get_account_field_name(field_type);

  label_string = g_strconcat(label_string, ":", NULL);

  label = gtk_label_new (label_string);
  gtk_misc_set_alignment (GTK_MISC(label), 0.95, 0.5);
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
    
  frame = gtk_frame_new("Account Info");
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

  widget = gtk_entry_new();
  info->currency_entry = GTK_EDITABLE(widget);
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
      return "None";
    case SOURCE_YAHOO :
      return "Yahoo";
    case SOURCE_FIDELITY :
      return "Fidelity";
    case SOURCE_TROWEPRICE :
      return "T. Rowe Price";
    case SOURCE_VANGUARD :
      return "Vanguard";
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
    case SOURCE_FIDELITY :
      return "FIDELITY";
    case SOURCE_TROWEPRICE :
      return "TRPRICE";
    case SOURCE_VANGUARD :
      return "VANGUARD";
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

  frame = gtk_frame_new("Quote Source");
  gtk_container_border_width(GTK_CONTAINER(frame), 5);
  gtk_widget_show(frame);

  hbox = gtk_hbox_new(FALSE, 5);
  gtk_container_border_width(GTK_CONTAINER(hbox), 5);
  gtk_widget_show(hbox);

  widget = gtk_label_new("Source for stock quotes:");
  gtk_misc_set_alignment (GTK_MISC(widget), 0.95, 0.5);
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
  GtkWidget * box = gnc_ui_account_field_box_create(info, TRUE);
  gboolean sensitive;
  int accType;

  gtk_entry_set_text(GTK_ENTRY(info->name_entry),
		     xaccAccountGetName(account));
  gtk_entry_set_text(GTK_ENTRY(info->description_entry),
		     xaccAccountGetDescription(account));
  gtk_entry_set_text(GTK_ENTRY(info->type_entry),
		     xaccAccountGetTypeStr(xaccAccountGetType(account)));
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
  gchar * old;

  xaccAccountSetName(account, strings->name);
  xaccAccountSetDescription(account, strings->description);

  old = xaccAccountGetCurrency(account);
  if (safe_strcmp(old, strings->currency) != 0)
    xaccAccountSetCurrency(account, strings->currency);

  if (safe_strcmp(strings->code, "") != 0)
    xaccAccountSetCode(account, strings->code);
  else
    xaccAccountAutoCode(account, 4);

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

char * gnc_ui_get_account_field_name(int field)
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
  }

  assert(0);
  return NULL;
}


double
gnc_ui_get_account_full_balance(Account *account)
{
  AccountGroup *acc_children;
  double balance;
  int type;

  assert(account != NULL);

  acc_children = xaccAccountGetChildren (account);
  type = xaccAccountGetType(account);
  balance = xaccAccountGetBalance (account);

  /* if the account has children, add in their balance */
  if (acc_children)
    balance += xaccGroupGetBalance(acc_children);

  /* the meaning of "balance" for income and expense
   * accounts is reversed, since a deposit of a paycheck in a
   * bank account will appear as a debit of the corresponding
   * amount in the income account */
  if ((type == EXPENSE) || (type == INCOME))
    balance = -balance;

  return balance;
}


char * gnc_ui_get_account_field_value_string(Account *account, int field)
{
  assert(account != NULL);
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
	double balance = gnc_ui_get_account_full_balance(account);

	return xaccPrintAmount(balance, PRTSYM | PRTSEP);
      }
      break;
  }

  assert(0);
  return NULL;
}


void gnc_set_tooltip(GtkWidget *w, const gchar *tip)
{
  GtkTooltips *t = gtk_tooltips_new();

  gtk_tooltips_set_tip(t, w, tip, NULL);
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
  GtkWidget *omenu;
  GtkWidget *menu;
  GtkWidget *menu_item;
  gint i;

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);

  menu = gtk_menu_new();
  gtk_widget_show(menu);

  for (i = 0; i < num_options; i++)
  {
    menu_item = gtk_menu_item_new_with_label(option_info[i].name);
    gtk_widget_show(menu_item);

    gtk_signal_connect(GTK_OBJECT(menu_item), "activate",
                       GTK_SIGNAL_FUNC(option_info[i].callback),
		       option_info[i].user_data);

    gtk_menu_append(GTK_MENU(menu), menu_item);
  }

  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu), menu);

  return omenu;
}
