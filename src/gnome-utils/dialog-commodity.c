/********************************************************************
 * dialog-commodity.c -- "select" and "new" commodity windows       *
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
/** @file dialog-commodity.c
    @brief "select" and "new" commodity windows
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
*/


#include "config.h"

#include <gnome.h>
#include <stdio.h>

#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-engine-util.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "messages.h"
#include "window-help.h"

static short module = MOD_GUI;

struct select_commodity_window {
  GtkWidget * dialog;
  GtkWidget * namespace_combo;
  GtkWidget * namespace_entry;
  GtkWidget * commodity_combo;
  GtkWidget * commodity_entry;
  GtkWidget * select_user_prompt;
  GtkWidget * ok_button;

  gnc_commodity * selection;

  const char * default_exchange_code;
  const char * default_fullname;
  const char * default_mnemonic;
  int          default_fraction;
};

struct commodity_window {
  GtkWidget * dialog;
  GtkWidget * commodity_info_frame;
  GtkWidget * fullname_entry;
  GtkWidget * mnemonic_entry;
  GtkWidget * namespace_combo;
  GtkWidget * code_entry;
  GtkWidget * fraction_spinbutton;
  GtkWidget * get_quote_check;
  GtkWidget * source_label;
  GtkWidget * source_menu;
  GtkWidget * quote_tz_label;
  GtkWidget * quote_tz_menu;
  GtkWidget * ok_button;

  gnc_commodity *edit_commodity;
};

typedef struct select_commodity_window SelectCommodityWindow;
typedef struct commodity_window CommodityWindow;

static gnc_commodity_help_callback help_callback = NULL;


/* The commodity selection window */
static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel);

void gnc_ui_select_commodity_new_cb(GtkButton * button,
				    gpointer user_data);
void gnc_ui_select_commodity_changed_cb(GtkEditable * entry,
					gpointer user_data);
void gnc_ui_select_commodity_namespace_changed_cb(GtkEditable * entry,
						  gpointer user_data);

/* The commodity creation window */
void gnc_ui_commodity_changed_cb(GtkWidget * dummy, gpointer user_data);
void gnc_ui_commodity_ok_cb(GtkButton * button, gpointer user_data);
void gnc_ui_commodity_help_cb(GtkButton * button, gpointer user_data);
void gnc_ui_commodity_quote_cb(GtkWidget *w, gpointer data);


/********************************************************************
 * gnc_ui_commodity_set_help_callback
 ********************************************************************/

void
gnc_ui_commodity_set_help_callback (gnc_commodity_help_callback cb)
{
  help_callback = cb;
}


/********************************************************************
 * gnc_ui_select_commodity_modal_full()
 ********************************************************************/
gnc_commodity * 
gnc_ui_select_commodity_modal_full(gnc_commodity * orig_sel, 
				   GtkWidget * parent,
				   const char * user_message,
				   const char * code,
				   const char * fullname,
				   const char * mnemonic)
{
  gnc_commodity * retval = NULL;
  const gchar *initial;
  gchar *user_prompt_text;
  SelectCommodityWindow * win;
  gboolean done;
  gint value;
  
  win = gnc_ui_select_commodity_create(orig_sel);
  win->default_exchange_code=code;
  win->default_fullname=fullname;
  win->default_mnemonic=mnemonic;
  
  if (parent)
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));

  if (user_message != NULL)
    initial = user_message;
  else if ((code != NULL) || (fullname != NULL) || (mnemonic != NULL))
    initial = _("\nPlease select a commodity to match:");
  else
    initial = "";

  user_prompt_text =
    g_strdup_printf("%s%s%s%s%s%s%s",
		    initial,
		    fullname ? _("\nCommodity: ") : "",
		    fullname ? fullname : "",
		    code     ? _("\nExchange code (CUSIP or similar): ") : "",
		    code     ? code : "",
		    mnemonic ? _("\nMnemonic(Ticker symbol or similar): ") : "",
		    mnemonic ? mnemonic : "");
   gtk_label_set_text ((GtkLabel *)(win->select_user_prompt),
		      user_prompt_text);

  /* Run the dialog, handling the terminal conditions. */
  done = FALSE;
  while (!done) {
    switch (value = gnome_dialog_run(GNOME_DIALOG(win->dialog))) {
     case 0:	/* OK */
      DEBUG("case 0");
      retval = win->selection;
      done = TRUE;
      break;
     case 1:	/* New */
      DEBUG("case 1");
      break;
     default:	/* Cancel, Escape, Close, etc. */
      DEBUG("default: %d", value);
      retval = NULL;
      done = TRUE;
      break;
    }
  }
  gnome_dialog_close(GNOME_DIALOG(win->dialog)); /* Close and destroy */
  g_free(win);
  
  return retval;
}

/********************************************************************
 * gnc_ui_select_commodity_modal()
 ********************************************************************/

gnc_commodity *
gnc_ui_select_commodity_modal(gnc_commodity * orig_sel,
                              GtkWidget * parent)
{
  return gnc_ui_select_commodity_modal_full(orig_sel, 
					    parent,
					    NULL,
					    NULL,
					    NULL,
					    NULL);
}


/********************************************************************
 * gnc_ui_select_commodity_create()
 ********************************************************************/

static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel)
{
  SelectCommodityWindow * retval = g_new0(SelectCommodityWindow, 1);
  GladeXML *xml;
  char * namespace;

  xml = gnc_glade_xml_new ("commodity.glade", "Commodity Selector Dialog");
  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     retval );

  retval->dialog = glade_xml_get_widget (xml, "Commodity Selector Dialog");
  retval->namespace_combo = glade_xml_get_widget (xml, "namespace_combo");
  retval->commodity_combo = glade_xml_get_widget (xml, "commodity_combo");
  retval->commodity_entry = glade_xml_get_widget (xml, "commodity_entry");
  retval->select_user_prompt = glade_xml_get_widget (xml, "select_user_prompt");
  retval->ok_button = glade_xml_get_widget (xml, "ok_button");

  gtk_label_set_text ((GtkLabel *)retval->select_user_prompt, "");

  /* build the menus of namespaces and commodities */
  namespace = 
    gnc_ui_update_namespace_picker(retval->namespace_combo, 
                                   gnc_commodity_get_namespace(orig_sel),
                                   TRUE, FALSE);
  gnc_ui_update_commodity_picker(retval->commodity_combo, namespace,
                                 gnc_commodity_get_printname(orig_sel));
  g_free(namespace);
  
  return retval;
}


/**
 *  This function is called whenever the user clicks on the "New"
 *  button in the commodity picker.  Its function is pop up a new
 *  dialog alling the user to create a new commodity.
 *
 *  @note This function is an internal helper function for the
 *  Commodity Selection dialog.  It should not be used outside of the
 *  dialog-commodity.c file.
 *
 *  @param entry A pointer to the "new" button widget in the dialog.
 *
 *  @param user_data A pointer to the data structure describing the
 *  current state of the commodity picker.
 */
void
gnc_ui_select_commodity_new_cb(GtkButton * button,
                               gpointer user_data)
{
  SelectCommodityWindow * w = user_data;

  const char * namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);

  const gnc_commodity * new_commodity = 
    gnc_ui_new_commodity_modal_full(namespace,
				    w->dialog,
				    w->default_exchange_code,
				    w->default_fullname,
				    w->default_mnemonic,
				    w->default_fraction);
  if(new_commodity) {
    char *namespace;
    
    namespace = 
      gnc_ui_update_namespace_picker(w->namespace_combo, 
                                     gnc_commodity_get_namespace
                                     (new_commodity),
                                     TRUE, FALSE);
    g_free(namespace);
    gnc_ui_update_commodity_picker(w->commodity_combo,
                                   gnc_commodity_get_namespace(new_commodity),
                                   gnc_commodity_get_printname(new_commodity));
  }
}


/**
 *  This function is called whenever the commodity combo box is
 *  changed.  Its function is to determine if a valid commodity has
 *  been selected, record the selection, and update the OK button.
 *
 *  @note This function is an internal helper function for the
 *  Commodity Selection dialog.  It should not be used outside of the
 *  dialog-commodity.c file.
 *
 *  @param entry A pointer to the commodity name entry widget in the
 *  dialog.
 *
 *  @param user_data A pointer to the data structure describing the
 *  current state of the commodity picker.
 */
void
gnc_ui_select_commodity_changed_cb(GtkEditable * entry,
				   gpointer user_data)
{
  SelectCommodityWindow * w = user_data;
  const char * namespace;
  const char * fullname;
  gboolean ok;

  ENTER("entry=%p, user_data=%p", entry, user_data);
  namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);
  fullname = gtk_entry_get_text(GTK_ENTRY(w->commodity_entry));
  DEBUG("namespace=%s, name=%s", namespace, fullname);
  w->selection = gnc_commodity_table_find_full(gnc_get_current_commodities(), 
					       namespace, fullname);

  ok = (w->selection != NULL);
  gtk_widget_set_sensitive(w->ok_button, ok);
  gnome_dialog_set_default(GNOME_DIALOG(w->dialog), ok ? 0 : 2);
  LEAVE("sensitive=%d, default = %d", ok, ok ? 0 : 2);
}


/**
 *  This function is called whenever the commodity namespace combo box
 *  is changed.  Its function is to update the commodity name combo
 *  box with the strings that are appropriate to the selected
 *  namespace.
 *
 *  @note This function is an internal helper function for the
 *  Commodity Selection dialog.  It should not be used outside of the
 *  dialog-commodity.c file.
 *
 *  @param entry A pointer to the commodity namespace entry widget in
 *  the dialog.
 *
 *  @param user_data A pointer to the data structure describing the
 *  current state of the commodity picker.
 */
void
gnc_ui_select_commodity_namespace_changed_cb(GtkEditable * entry,
                                             gpointer user_data)
{
  SelectCommodityWindow * w = user_data;
  const char * namespace;

  ENTER("entry=%p, user_data=%p", entry, user_data);
  namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);
  DEBUG("namespace=%s", namespace);
  gnc_ui_update_commodity_picker(w->commodity_combo, namespace, NULL);
  LEAVE(" ");
}


/********************************************************************
 * gnc_ui_update_commodity_picker
 ********************************************************************/
static int 
g_strcmp(gconstpointer a, gconstpointer b)
{
  return strcmp(a, b);
}


void
gnc_ui_update_commodity_picker(GtkWidget * combobox, 
                               const char * namespace,
                               const char * init_string)
{
  GList      * commodities; 
  GList      * iterator = NULL;
  GList      * commodity_items = NULL;
  gnc_commodity_table *table;
  const char * current;

  table = gnc_book_get_commodity_table (gnc_get_current_book ());
  commodities = gnc_commodity_table_get_commodities(table, namespace);

  for(iterator = commodities; iterator; iterator = iterator->next) {
    commodity_items = 
      g_list_append(commodity_items, 
                    (gpointer) gnc_commodity_get_printname(iterator->data));
  }
  commodity_items = g_list_sort(commodity_items, g_strcmp);

  if(!commodity_items) {
    commodity_items = g_list_append(commodity_items, "");
  }
  gtk_combo_set_popdown_strings(GTK_COMBO(combobox), 
                                commodity_items);

  if(init_string) {
    current = init_string;
  }
  else {
    current = commodity_items->data;
  }

  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combobox)->entry), current);

  /* free the lists */
  g_list_free(commodities);
  g_list_free(commodity_items);
}


/********************************************************************
 * gnc_ui_update_namespace_picker
 ********************************************************************/

char * 
gnc_ui_update_namespace_picker(GtkWidget * combobox, 
                               const char * init_string,
                               gboolean include_iso,
                               gboolean include_all)
{
  GList * namespaces;
  const char * active;

  /* fetch a list of the namespaces */
  if (!include_all)
    namespaces =
      gnc_commodity_table_get_namespaces (gnc_get_current_commodities());
  else
  {
    namespaces = NULL;
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_ISO);
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_NASDAQ);
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_NYSE);
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_EUREX);
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_MUTUAL);
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_ASX);
    namespaces = g_list_prepend (namespaces, GNC_COMMODITY_NS_AMEX);
  }

  namespaces = g_list_sort(namespaces, g_strcmp);

  {
    GList *node;

    node = g_list_find_custom (namespaces, GNC_COMMODITY_NS_ISO, g_strcmp);
    if (node && !include_iso)
    {
      namespaces = g_list_remove_link (namespaces, node);
      g_list_free_1 (node);
    }
    else
      node->data = "CURRENCY";

    node = g_list_find_custom (namespaces, GNC_COMMODITY_NS_LEGACY, g_strcmp);
    if (node)
    {
      namespaces = g_list_remove_link (namespaces, node);
      g_list_free_1 (node);
    }
  }

  /* stick them in the combobox */
  gtk_combo_set_popdown_strings (GTK_COMBO (combobox), namespaces);

  if (!include_iso && gnc_commodity_namespace_is_iso (init_string))
    init_string = NULL;

  /* set the entry text */
  if (init_string)
    active = init_string;
  else if (namespaces)
    active = namespaces->data;
  else
    active = "";

  if (safe_strcmp (active, GNC_COMMODITY_NS_ISO) == 0 ||
      safe_strcmp (active, "CURRENCY") == 0 ||
      safe_strcmp (init_string, "CURRENCY") == 0)
  {
    active = "CURRENCY";
    init_string = GNC_COMMODITY_NS_ISO;
  }
  else
    init_string = active;

  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combobox)->entry), active);
  g_list_free(namespaces);

  return g_strdup (init_string);
}

const char *
gnc_ui_namespace_picker_ns (GtkWidget *combobox)
{
  const char *namespace;

  g_return_val_if_fail (combobox != NULL, NULL);
  g_return_val_if_fail (GTK_IS_COMBO (combobox), NULL);

  namespace = gtk_entry_get_text (GTK_ENTRY(GTK_COMBO (combobox)->entry));

  if (safe_strcmp (namespace, "CURRENCY") == 0)
    return GNC_COMMODITY_NS_ISO;
  else
    return namespace;
}

/********************************************************************/

void
gnc_ui_commodity_quote_cb (GtkWidget *w, gpointer data)
{
  CommodityWindow *cw = data;
  gboolean get_quote, allow_src;
  const char *text;

  get_quote = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));

  text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(cw->namespace_combo)->entry));
  allow_src = gnc_commodity_namespace_is_iso(text);
  gtk_widget_set_sensitive(cw->source_label, get_quote && allow_src);
  gtk_widget_set_sensitive(cw->source_menu, get_quote && allow_src);
  gtk_widget_set_sensitive(cw->quote_tz_label, get_quote);
  gtk_widget_set_sensitive(cw->quote_tz_menu, get_quote);

}

void
gnc_ui_commodity_changed_cb(GtkWidget * dummy, gpointer user_data)
{
  CommodityWindow * w = user_data;
  const char * namespace;
  const char * fullname;
  const char * mnemonic;
  gboolean ok;

  ENTER("widget=%p, user_data=%p", dummy, user_data);
  if (GTK_WIDGET_SENSITIVE(w->commodity_info_frame)) {
    namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);
    fullname  = gtk_entry_get_text(GTK_ENTRY(w->fullname_entry));
    mnemonic  = gtk_entry_get_text(GTK_ENTRY(w->mnemonic_entry));
    DEBUG("namespace=%s, name=%s, mnemonic=%s", namespace, fullname, mnemonic);
    ok = (fullname    && namespace    && mnemonic &&
	  fullname[0] && namespace[0] && mnemonic[0]);
  } else {
    ok = TRUE;
  }
  gtk_widget_set_sensitive(w->ok_button, ok);
  gnome_dialog_set_default(GNOME_DIALOG(w->dialog), ok ? 0 : 1);
  LEAVE("sensitive=%d, default = %d", ok, ok ? 0 : 1);
}

/** Build the new/edit commodity dialog box
 */
static CommodityWindow *
gnc_ui_new_commodity_dialog(const char * selected_namespace,
			    GtkWidget  *parent,
			    const char * fullname,
			    const char * mnemonic,
			    const char * cusip,
			    int          fraction)
{
  CommodityWindow * retval = g_new0(CommodityWindow, 1);
  GtkWidget *help_button;
  GtkWidget *box;
  GladeXML *xml;
  gboolean include_iso;
  char *temp;

  ENTER(" ");
  xml = gnc_glade_xml_new ("commodity.glade", "Commodity Dialog");

  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     retval );

  retval->dialog = glade_xml_get_widget (xml, "Commodity Dialog");
  if (parent)
    gnome_dialog_set_parent(GNOME_DIALOG(retval->dialog), GTK_WINDOW(parent));
  retval->edit_commodity = NULL;

  help_button = glade_xml_get_widget (xml, "help_button");
  if (!help_callback)
    gtk_widget_hide (help_button);


  /* Get widget pointers */
  retval->commodity_info_frame = glade_xml_get_widget (xml, "commodity_info_frame");
  retval->fullname_entry = glade_xml_get_widget (xml, "fullname_entry");
  retval->mnemonic_entry = glade_xml_get_widget (xml, "mnemonic_entry");
  retval->namespace_combo = glade_xml_get_widget (xml, "namespace_combo");
  retval->code_entry = glade_xml_get_widget (xml, "code_entry");
  retval->fraction_spinbutton = glade_xml_get_widget (xml,
                                                      "fraction_spinbutton");
  retval->ok_button = glade_xml_get_widget (xml, "ok_button");
  retval->get_quote_check = glade_xml_get_widget (xml, "get_quote_check");
  retval->source_label = glade_xml_get_widget (xml, "source_label");
  retval->quote_tz_label = glade_xml_get_widget (xml, "quote_tz_label");


  /* Build custom widgets */
  box = glade_xml_get_widget (xml, "source_box");
  retval->source_menu = gnc_ui_source_menu_create();
  gtk_box_pack_start(GTK_BOX(box), retval->source_menu, TRUE, TRUE, 0);
  box = glade_xml_get_widget (xml, "quote_tz_box");
  retval->quote_tz_menu = gnc_ui_quote_tz_menu_create();
  gtk_box_pack_start(GTK_BOX(box), retval->quote_tz_menu, TRUE, TRUE, 0);


  /* Commodity editing is next to nil */
  if (gnc_commodity_namespace_is_iso(selected_namespace)) {
    gtk_widget_set_sensitive(retval->commodity_info_frame, FALSE);
    include_iso = TRUE;
  } else {
    include_iso = FALSE;
  }

  /* Are price quotes supported */
  if (gnc_price_source_have_fq()) {
    gtk_widget_destroy(glade_xml_get_widget (xml, "finance_quote_warning"));
  } else {
    gtk_widget_set_sensitive(glade_xml_get_widget (xml, "price_quote_frame"),
			     FALSE);
  }


  /* Fill in any data, top to bottom */
  
  gtk_entry_set_text (GTK_ENTRY (retval->fullname_entry), fullname ? fullname : "");
  gtk_entry_set_text (GTK_ENTRY (retval->mnemonic_entry), mnemonic ? mnemonic : "");
  temp = gnc_ui_update_namespace_picker(retval->namespace_combo,
					selected_namespace,
					include_iso, TRUE);
  g_free(temp);
  gtk_entry_set_text (GTK_ENTRY (retval->code_entry), cusip ? cusip : "");
  if (fraction > 0)
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (retval->fraction_spinbutton),
			       fraction);


  /* Set up so <CR> activates default button */
  gnome_dialog_editable_enters(GNOME_DIALOG(retval->dialog),
			       GTK_EDITABLE(retval->fullname_entry));
  gnome_dialog_editable_enters(GNOME_DIALOG(retval->dialog),
			       GTK_EDITABLE(retval->mnemonic_entry));
  gnome_dialog_editable_enters(GNOME_DIALOG(retval->dialog),
			       GTK_EDITABLE(retval->code_entry));

  LEAVE(" ");
  return retval;
}


static void
gnc_ui_commodity_update_quote_info(CommodityWindow *win,
				   gnc_commodity *commodity)
{
  gboolean has_quote_src;
  const char *quote_src, *quote_tz;
  int pos = 0;

  ENTER(" ");
  has_quote_src = gnc_commodity_get_quote_flag (commodity);
  quote_src = gnc_commodity_get_quote_source (commodity);
  quote_tz = gnc_commodity_get_quote_tz (commodity);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (win->get_quote_check),
				has_quote_src);
  if (!gnc_commodity_is_iso(commodity))
    gtk_option_menu_set_history (GTK_OPTION_MENU (win->source_menu),
				 gnc_price_source_internal2enum (quote_src));
  if (quote_tz) {
    pos = gnc_find_timezone_menu_position(quote_tz);
    if(pos == 0) {
      PWARN("Unknown price quote timezone (%s), resetting to default.",
	    quote_tz ? quote_tz : "(null)");
    }
  }
  gtk_option_menu_set_history (GTK_OPTION_MENU (win->quote_tz_menu), pos);
  LEAVE(" ");
}


static gnc_commodity *
gnc_ui_common_commodity_modal(gnc_commodity *commodity,
			      GtkWidget * parent,
			      const char * namespace,
			      const char * code,
			      const char * fullname,
			      const char * mnemonic,
			      int fraction)
{
  CommodityWindow * win;
  gnc_commodity *retval = NULL;
  gboolean done;
  gint value;

  ENTER(" ");

  /* If a commodity was provided, copy out the existing info */
  if (commodity) {
    namespace = gnc_commodity_get_namespace (commodity);
    fullname = gnc_commodity_get_fullname (commodity);
    mnemonic = gnc_commodity_get_mnemonic (commodity);
    code = gnc_commodity_get_exchange_code (commodity);
    fraction = gnc_commodity_get_fraction (commodity);
  } else {
    /* Not allowed to create new currencies */
    if (gnc_commodity_namespace_is_iso(namespace)) {
      namespace = NULL;
    }
  }

  win = gnc_ui_new_commodity_dialog(namespace, parent, fullname,
				    mnemonic, code, fraction);

  /* Update stock quote info based on existing commodity */
  if (commodity) {
    gnc_ui_commodity_update_quote_info(win, commodity);
    win->edit_commodity = commodity;
  }

  /* Update stock quote sensitivities based on check box */
  gnc_ui_commodity_quote_cb(win->get_quote_check, win);

  /* Run the dialog, handling the terminal conditions. */
  done = FALSE;
  while (!done) {
    value = gnome_dialog_run(GNOME_DIALOG(win->dialog));
    switch (value) {
     case 0:	/* OK */
      retval = win->edit_commodity;
      done = TRUE;
      DEBUG("case 0");
      break;
     case 2:	/* Help */
      DEBUG("case 2");
      break;
     default:	/* Cancel, Escape, Close, etc. */
      DEBUG("default: %d", value);
      retval = NULL;
      done = TRUE;
      break;
    }
  }
  gnome_dialog_close(GNOME_DIALOG(win->dialog)); /* Close and destroy */
  g_free(win);

  LEAVE(" ");
  return retval;
}



/** Create and run the new/edit commodity dialog.
 */
gnc_commodity * 
gnc_ui_new_commodity_modal_full(const char * namespace, 
				GtkWidget * parent,
				const char * exchange_code,
				const char * fullname,
				const char * mnemonic,
				int fraction)
{
  gnc_commodity *result;

  ENTER(" ");
  result = gnc_ui_common_commodity_modal(NULL, parent, namespace, fullname,
					 mnemonic, exchange_code, 10000);
  LEAVE(" ");
  return result;
}

/** External routine for popping up the new commodity dialog box.
 */
gnc_commodity *
gnc_ui_new_commodity_modal(const char * default_namespace,
                           GtkWidget * parent)
{
  gnc_commodity *result;

  ENTER(" ");
  result = gnc_ui_common_commodity_modal(NULL, parent, default_namespace, NULL,
					 NULL, NULL, 0);
  LEAVE(" ");
  return result;
}

/********************************************************************
 * gnc_ui_edit_commodity_modal()
 ********************************************************************/

/** Given an existing commodity, uses the
 *  gnc_ui_new_commodity_dialog() routine to build a basic edit
 *  dialog, then fills in the price quote information at the bottom of
 *  the dialog.
 */
gboolean
gnc_ui_edit_commodity_modal(gnc_commodity *commodity,
                            GtkWidget * parent)
{
  gnc_commodity *result;

  ENTER(" ");
  result = gnc_ui_common_commodity_modal(commodity, parent, NULL, NULL,
					 NULL, NULL, 0);
  LEAVE(" ");
  return result != NULL;
}


/********************************************************************
 * gnc_ui_commodity_ok_cb()
 ********************************************************************/

void
gnc_ui_commodity_ok_cb(GtkButton * button,
                       gpointer user_data)
{
  CommodityWindow * w = user_data;

  const char * fullname  = gtk_entry_get_text(GTK_ENTRY(w->fullname_entry));
  const char * namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);
  const char * mnemonic  = gtk_entry_get_text(GTK_ENTRY(w->mnemonic_entry));
  const char * code      = gtk_entry_get_text(GTK_ENTRY(w->code_entry));
  int fraction = gtk_spin_button_get_value_as_int
    (GTK_SPIN_BUTTON(w->fraction_spinbutton));
  const char *string;
  gnc_commodity * c;
  gint selection;

  ENTER(" ");
  /* Special case currencies */
  if (gnc_commodity_namespace_is_iso (namespace)) {
    if (w->edit_commodity) {
      c = w->edit_commodity;
      gnc_commodity_set_quote_flag (c, gtk_toggle_button_get_active
				    (GTK_TOGGLE_BUTTON (w->get_quote_check)));
      selection = gnc_option_menu_get_active (w->quote_tz_menu);
      string = gnc_timezone_menu_position_to_string(selection);
      gnc_commodity_set_quote_tz(c, string);
      return;
    }
    gnc_warning_dialog_parented(w->dialog,
                                _("You may not create a new national "
                                  "currency."));
    return;
  }

  if(fullname && fullname[0] &&
     namespace && namespace[0] &&
     mnemonic && mnemonic[0]) {
    c = gnc_commodity_table_lookup (gnc_get_current_commodities(),
                                    namespace, mnemonic);

    if ((!w->edit_commodity && c) ||
        (w->edit_commodity && c && (c != w->edit_commodity))) {
      gnc_warning_dialog_parented (w->dialog,
                                   _("That commodity already exists."));
      return;
    }

    if (!w->edit_commodity) {
      c = gnc_commodity_new(fullname, namespace, mnemonic, code, fraction);
      w->edit_commodity = c;
    }
    else {
      c = w->edit_commodity;

      gnc_commodity_table_remove (gnc_get_current_commodities(), c);

      gnc_commodity_set_fullname (c, fullname);
      gnc_commodity_set_mnemonic (c, mnemonic);
      gnc_commodity_set_namespace (c, namespace);
      gnc_commodity_set_exchange_code (c, code);
      gnc_commodity_set_fraction (c, fraction);
    }

    gnc_commodity_set_quote_flag (c, gtk_toggle_button_get_active
				  (GTK_TOGGLE_BUTTON (w->get_quote_check)));

    selection = gnc_option_menu_get_active (w->source_menu);
    string = gnc_price_source_enum2internal (selection);
    gnc_commodity_set_quote_source(c, string);

    selection = gnc_option_menu_get_active (w->quote_tz_menu);
    string = gnc_timezone_menu_position_to_string(selection);
    gnc_commodity_set_quote_tz(c, string);

    /* remember the commodity */
    c = gnc_commodity_table_insert(gnc_get_current_commodities(), c);
  }
  else {
    gnc_warning_dialog_parented(w->dialog,
                                _("You must enter a non-empty \"Full name\", "
                                  "\"Symbol/abbreviation\",\n"
                                  "and \"Type\" for the commodity."));
  }
  LEAVE(" ");
}


/********************************************************************
 * gnc_ui_commodity_help_cb()
 ********************************************************************/

void
gnc_ui_commodity_help_cb(GtkButton * button,
                         gpointer user_data)
{
  if (help_callback)
    help_callback ();
}
