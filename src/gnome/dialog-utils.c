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

#include "config.h"

#include "account-tree.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-commodity.h"
#include "messages.h"
#include "EuroUtils.h"
#include "Group.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"


/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_GUI;


/* Get the full name of a quote source */
gchar *
gnc_get_source_name(gint source)
{
  switch (source)
  {
    case SOURCE_NONE :
      return _("(none)");
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

/* Get the codename string of a quote source */
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
gnc_get_source_code(const char * codename)
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


/********************************************************************\
 * gnc_ui_source_menu_create                                        *
 *   create the menu of stock quote sources                         *
 *                                                                  *
 * Args:    account - account to use to set default choice          *
 * Returns: the menu                                                *
 \*******************************************************************/
GtkWidget *
gnc_ui_source_menu_create(Account *account)
{
  gint i;
  GtkMenu   *menu;
  GtkWidget *item;
  GtkWidget *omenu;
  /* gchar *codename;      This didn't appear to be used anywhere... */
  GNCAccountType type;

  menu = GTK_MENU(gtk_menu_new());
  gtk_widget_show(GTK_WIDGET(menu));

  for (i = 0; i < NUM_SOURCES; i++)
  {
    item = gtk_menu_item_new_with_label(gnc_get_source_name(i));
    gtk_widget_show(item);
    gtk_menu_append(menu, item);
  }

  omenu = gtk_option_menu_new();
  gtk_widget_show(omenu);
  gtk_option_menu_set_menu(GTK_OPTION_MENU(omenu), GTK_WIDGET(menu));
  gnc_option_menu_init(omenu);

  if (account == NULL)
    return omenu;

  type = xaccAccountGetType(account);
  if ((STOCK != type) && (MUTUAL != type) && (CURRENCY != type))
    return omenu;

  if ((STOCK != type) && (MUTUAL != type))
    return omenu;
  
  /* 
     This didn't appear to be used anywhere...
     codename = xaccInvAcctGetPriceSrc(invacct); */

  return omenu;
}

/* =========================================================== */

const char *
gnc_ui_get_account_field_name(int field)
{
  assert((field >= 0) && (field < NUM_ACCOUNT_FIELDS));

  switch (field)
  {
    case ACCOUNT_TYPE :
      return _("Type");
      break;
    case ACCOUNT_NAME :
      return _("Account Name");
      break;
    case ACCOUNT_CODE :
      return _("Account Code");
      break;
    case ACCOUNT_DESCRIPTION :
      return _("Description");
      break;
    case ACCOUNT_NOTES :
      return _("Notes");
      break;
    case ACCOUNT_CURRENCY :
      return _("Currency");
      break;
    case ACCOUNT_SECURITY :
      return _("Security");
      break;
    case ACCOUNT_BALANCE :
      return _("Balance");
      break;
    case ACCOUNT_BALANCE_EURO :
      return _("Balance");
      break;
    case ACCOUNT_TOTAL :
      return _("Total");
      break;
    case ACCOUNT_TOTAL_EURO :
      return _("Total");
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

  balance = DxaccAccountGetBalance (account);

  if (include_children)
  {
    AccountGroup *children;

    children = xaccAccountGetChildren (account);
    balance += DxaccGroupGetBalance (children);
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
      return gnc_commodity_get_printname(xaccAccountGetCurrency(account));
      break;
    case ACCOUNT_SECURITY :
      return gnc_commodity_get_printname(xaccAccountGetSecurity(account));
      break;
    case ACCOUNT_BALANCE :
      {
        double balance = gnc_ui_account_get_balance(account, FALSE);

        return DxaccPrintAmount(balance,
                                gnc_account_value_print_info (account, TRUE));
      }
      break;
    case ACCOUNT_BALANCE_EURO :
      {
	const gnc_commodity * account_currency = 
          xaccAccountGetCurrency(account);
        double balance = gnc_ui_account_get_balance(account, FALSE);
	double euro_balance = gnc_convert_to_euro(account_currency, balance);

	return DxaccPrintAmount(euro_balance,
                                gnc_commodity_print_info (gnc_get_euro (),
                                                          TRUE));
      }
      break;
    case ACCOUNT_TOTAL :
      {
	double balance = gnc_ui_account_get_balance(account, TRUE);

        return DxaccPrintAmount(balance,
                                gnc_account_value_print_info (account, TRUE));
      }
      break;
    case ACCOUNT_TOTAL_EURO :
      {
	const gnc_commodity * account_currency = 
          xaccAccountGetCurrency(account);
	double balance = gnc_ui_account_get_balance(account, TRUE);
	double euro_balance = gnc_convert_to_euro(account_currency, balance);

	return DxaccPrintAmount(euro_balance,
                                gnc_commodity_print_info (gnc_get_euro (),
                                                          TRUE));
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
gnc_get_toolbar_style(void)
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
 * gnc_color_deficits                                               *
 *   return a boolean value indicating whether deficit quantities   *
 *   should be displayed using the gnc_get_deficit_color().         *
 *                                                                  *
 * Args: none                                                       *
 * Returns: boolean deficit color indicator                         *
 \*******************************************************************/
gboolean
gnc_color_deficits (void)
{
  return gnc_lookup_boolean_option ("General",
                                    "Display negative amounts in red",
                                    TRUE);
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
  gboolean deficit;
  GdkColormap *cm;
  GtkStyle *style;

  if (!gnc_color_deficits())
    return;

  cm = gtk_widget_get_colormap(GTK_WIDGET(label));
  style = gtk_widget_get_style(GTK_WIDGET(label));

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

  gtk_style_unref(style);
}


/********************************************************************\
 * gnc_get_toolbar_style                                            *
 *   returns the current account separator character                *
 *                                                                  *
 * Args: none                                                       *
 * Returns: account separator character                             *
 \*******************************************************************/
char
gnc_get_account_separator(void)
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

  if (gnc_lookup_boolean_option("General", "Save Window Geometry", TRUE))
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


/********************************************************************\
 * gnc_save_window_size                                             *
 *   save the window size into options whose names are determined   *
 *   by the string prefix.                                          *
 *                                                                  *
 * Args: prefix - determines the options used to save the values    *
 *       width  - width of the window to save                       *
 *       height - height of the window to save                      *
 * Returns: nothing                                                 *
\********************************************************************/
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


/********************************************************************\
 * gnc_fill_menu_with_data                                          *
 *   fill the user data values in the menu structure with the given *
 *   value. The filling is done recursively.                        *
 *                                                                  *
 * Args: info - the menu to fill                                    *
 *       data - the value to fill with                              *
 * Returns: nothing                                                 *
\********************************************************************/
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


/********************************************************************\
 * gnc_window_adjust_for_screen                                     *
 *   adjust the window size if it is bigger than the screen size.   *
 *                                                                  *
 * Args: window - the window to adjust                              *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_window_adjust_for_screen(GtkWindow * window)
{
  gint screen_width;
  gint screen_height;
  gint width;
  gint height;

  if (window == NULL)
    return;

  g_return_if_fail(GTK_IS_WINDOW(window));
  if (GTK_WIDGET(window)->window == NULL)
    return;

  screen_width = gdk_screen_width();
  screen_height = gdk_screen_height();
  gdk_window_get_size(GTK_WIDGET(window)->window, &width, &height);

  if ((width <= screen_width) && (height <= screen_height))
    return;

  width = MIN(width, screen_width - 10);
  width = MAX(width, 0);

  height = MIN(height, screen_height - 10);
  height = MAX(height, 0);

  gdk_window_resize(GTK_WIDGET(window)->window, width, height);
  gtk_widget_queue_resize(GTK_WIDGET(window));
}


/********************************************************************\
 * gnc_get_reconcile_str                                            *
 *   return the i18n'd string for the given reconciled flag         *
 *                                                                  *
 * Args: reconciled_flag - the flag to stringize                    *
 * Returns: the i18n'd reconciled string                            *
\********************************************************************/
const char *
gnc_get_reconcile_str(char reconciled_flag)
{
  switch (reconciled_flag)
  {
    case NREC: return _("not cleared:n"+12);
    case CREC: return _("cleared:c"+8);
    case YREC: return _("reconciled:y"+11);
    case FREC: return _("frozen:f"+7);
    default:
      PERR("Bad reconciled flag\n");
      return NULL;
  }
}
