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


struct select_commodity_window {
  GtkWidget * dialog;
  GtkWidget * namespace_combo;
  GtkWidget * commodity_combo;
  GtkWidget * commodity_entry;

  gnc_commodity_callback callback;
  void      * callback_data;
};

struct commodity_window {
  GtkWidget * dialog;
  GtkWidget * fullname_entry;
  GtkWidget * mnemonic_entry;
  GtkWidget * namespace_combo;
  GtkWidget * code_entry;
  GtkWidget * fraction_spinbutton;

  gnc_commodity_callback callback;
  void      * callback_data;

  gnc_commodity *edit_commodity;
};


static gnc_commodity_help_callback help_callback = NULL;

static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel,
                               gnc_commodity_callback callback,
                               void * callback_data);

static void gnc_ui_select_commodity_ok_cb(GtkButton * button,
                                          gpointer user_data);
static void gnc_ui_select_commodity_new_cb(GtkButton * button,
                                           gpointer user_data);
static void gnc_ui_select_commodity_cancel_cb(GtkButton * button,
                                              gpointer user_data);
static void gnc_ui_select_commodity_namespace_changed_cb(GtkEditable * entry,
                                                         gpointer user_data);
static void gnc_ui_commodity_ok_cb(GtkButton * button, gpointer user_data);
static void gnc_ui_commodity_cancel_cb(GtkButton * button, gpointer user_data);
static void gnc_ui_commodity_help_cb(GtkButton * button, gpointer user_data);

static void 
select_modal_callback(const gnc_commodity * arg, void * data) {
  *((const gnc_commodity **)data) = arg;
}


void
gnc_ui_commodity_set_help_callback (gnc_commodity_help_callback cb)
{
  help_callback = cb;
}

/********************************************************************
 * gnc_ui_select_commodity_modal()
 ********************************************************************/

gnc_commodity *
gnc_ui_select_commodity_modal(gnc_commodity * orig_sel,
                              GtkWidget * parent) {  
  gnc_commodity * retval = NULL;

  SelectCommodityWindow * win = 
    gnc_ui_select_commodity_create(orig_sel, &select_modal_callback, &retval);
  
  if(parent) {
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));
  }
  gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
  gtk_widget_show (win->dialog);
  gtk_main();

  return retval;
}


static gint
select_commodity_close (GnomeDialog *dialog, gpointer data)
{
  SelectCommodityWindow *scw = data;

  g_free(scw);

  gtk_main_quit ();

  return FALSE;
}

/********************************************************************
 * gnc_ui_select_commodity_create()
 ********************************************************************/

static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel,
                               gnc_commodity_callback callback,
                               void * callback_data) {
  SelectCommodityWindow * retval = g_new0(SelectCommodityWindow, 1);
  GladeXML *xml;
  char * namespace;

  xml = gnc_glade_xml_new ("commodity.glade", "Commodity Selector Dialog");

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_commodity_ok_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_commodity_ok_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_commodity_new_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_commodity_new_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_commodity_cancel_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_commodity_cancel_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_select_commodity_namespace_changed_cb",
     GTK_SIGNAL_FUNC (gnc_ui_select_commodity_namespace_changed_cb), retval);

  retval->dialog = glade_xml_get_widget (xml, "Commodity Selector Dialog");
  retval->namespace_combo = glade_xml_get_widget (xml, "namespace_combo");
  retval->commodity_combo = glade_xml_get_widget (xml, "commodity_combo");
  retval->commodity_entry = glade_xml_get_widget (xml, "commodity_entry");

  retval->callback = callback;
  retval->callback_data = callback_data;

  gtk_signal_connect (GTK_OBJECT(retval->dialog), "close",
                      GTK_SIGNAL_FUNC(select_commodity_close), retval);

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


/********************************************************************
 * gnc_ui_update_commodity_picker
 ********************************************************************/
static int 
g_strcmp(gconstpointer a, gconstpointer b) {
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
 * gnc_ui_select_commodity_destroy()
 ********************************************************************/

void
gnc_ui_select_commodity_destroy(SelectCommodityWindow * w) {
  if(w) {
    gnome_dialog_close(GNOME_DIALOG(w->dialog));
  }
}

/********************************************************************
 * gnc_ui_select_commodity_ok_cb()
 ********************************************************************/

static void
gnc_ui_select_commodity_ok_cb(GtkButton * button,
                              gpointer user_data)
{
  SelectCommodityWindow * w = user_data;
  const char    * namespace;  
  char          * fullname;
  gnc_commodity * retval = NULL;

  namespace       = gnc_ui_namespace_picker_ns (w->namespace_combo);
  fullname        = gtk_entry_get_text(GTK_ENTRY(w->commodity_entry));
  
  retval = gnc_commodity_table_find_full(gnc_get_current_commodities(), 
                                         namespace,
                                         fullname);
  if(retval) {
    if (w->callback)
      (w->callback)(retval, w->callback_data);
    gnc_ui_select_commodity_destroy(w);
  }
  else {
    gnc_warning_dialog(_("You must select a commodity.\n"
                         "To create a new one, click \"New\""));
  }
}


/********************************************************************
 * gnc_ui_select_commodity_new_cb()
 ********************************************************************/

static void
gnc_ui_select_commodity_new_cb(GtkButton * button,
                               gpointer user_data) {
  SelectCommodityWindow * w = user_data;

  const char * namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);

  const gnc_commodity * new_commodity = 
    gnc_ui_new_commodity_modal(namespace, w->dialog);

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


/********************************************************************
 * gnc_ui_select_commodity_cancel_cb()
 ********************************************************************/

static void
gnc_ui_select_commodity_cancel_cb(GtkButton * button,
                                  gpointer user_data) {
  SelectCommodityWindow * w = user_data;

  if (w->callback)
    (w->callback)(NULL, w->callback_data);

  gnc_ui_select_commodity_destroy(w);
}

/********************************************************************
 * gnc_ui_select_commodity_namespace_changed_cb()
 ********************************************************************/

static void
gnc_ui_select_commodity_namespace_changed_cb(GtkEditable * entry,
                                             gpointer user_data) {
  SelectCommodityWindow * w = user_data;
  const char * namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);
  
  gnc_ui_update_commodity_picker(w->commodity_combo, namespace, NULL);
}


/********************************************************************
 * gnc_ui_update_namespace_picker
 ********************************************************************/

char * 
gnc_ui_update_namespace_picker(GtkWidget * combobox, 
                               const char * init_string,
                               gboolean include_iso,
                               gboolean include_all) {
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

  if (!include_iso &&
      safe_strcmp (init_string, GNC_COMMODITY_NS_ISO) == 0)
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

static gint
commodity_close (GnomeDialog *dialog, gpointer data)
{
  CommodityWindow *ncw = data;

  g_free(ncw);

  gtk_main_quit ();

  return FALSE;
}

/********************************************************************
 * gnc_ui_new_commodity_create()
 ********************************************************************/

static CommodityWindow *
gnc_ui_new_commodity_create(const char * selected_namespace,
                            gnc_commodity_callback callback, 
                            void * callback_data) {
  CommodityWindow * retval = g_new0(CommodityWindow, 1);
  GtkWidget *help_button;
  GladeXML *xml;
  char *namespace;

  xml = gnc_glade_xml_new ("commodity.glade", "Commodity Dialog");

  glade_xml_signal_connect_data
    (xml, "gnc_ui_commodity_ok_cb",
     GTK_SIGNAL_FUNC (gnc_ui_commodity_ok_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_commodity_cancel_cb",
     GTK_SIGNAL_FUNC (gnc_ui_commodity_cancel_cb), retval);

  glade_xml_signal_connect_data
    (xml, "gnc_ui_commodity_help_cb",
     GTK_SIGNAL_FUNC (gnc_ui_commodity_help_cb), retval);

  help_button = glade_xml_get_widget (xml, "help_button");
  if (!help_callback)
    gtk_widget_hide (help_button);

  retval->dialog = glade_xml_get_widget (xml, "Commodity Dialog");

  retval->edit_commodity = NULL;

  retval->fullname_entry = glade_xml_get_widget (xml, "fullname_entry");
  retval->mnemonic_entry = glade_xml_get_widget (xml, "mnemonic_entry");
  retval->namespace_combo = glade_xml_get_widget (xml, "namespace_combo");
  retval->code_entry = glade_xml_get_widget (xml, "code_entry");
  retval->fraction_spinbutton = glade_xml_get_widget (xml,
                                                      "fraction_spinbutton");

  retval->callback = callback;
  retval->callback_data = callback_data;

  gtk_signal_connect (GTK_OBJECT(retval->dialog), "close",
                      GTK_SIGNAL_FUNC(commodity_close), retval);

  namespace = gnc_ui_update_namespace_picker(retval->namespace_combo,
                                             selected_namespace,
                                             FALSE, TRUE);
  g_free(namespace);

  return retval;
}

/********************************************************************
 * gnc_ui_edit_commodity_create()
 ********************************************************************/

static CommodityWindow *
gnc_ui_edit_commodity_create(gnc_commodity *commodity,
                             gnc_commodity_callback callback, 
                             void * callback_data) {
  CommodityWindow *retval;
  const char *str;
  char *namespace;

  g_return_val_if_fail (commodity != NULL, NULL);

  retval = gnc_ui_new_commodity_create (NULL, callback, callback_data);

  retval->edit_commodity = commodity;

  str = gnc_commodity_get_fullname (commodity);
  gtk_entry_set_text (GTK_ENTRY (retval->fullname_entry),
                      str ? str : "");

  str = gnc_commodity_get_mnemonic (commodity);
  gtk_entry_set_text (GTK_ENTRY (retval->mnemonic_entry),
                      str ? str : "");

  namespace = gnc_ui_update_namespace_picker
    (retval->namespace_combo,
     gnc_commodity_get_namespace (commodity),
     FALSE, TRUE);
  g_free (namespace);

  str = gnc_commodity_get_exchange_code (commodity);
  gtk_entry_set_text (GTK_ENTRY (retval->code_entry),
                      str ? str : "");

  gtk_spin_button_set_value (GTK_SPIN_BUTTON (retval->fraction_spinbutton),
                             gnc_commodity_get_fraction (commodity));

  return retval;
}

static void 
new_modal_callback(const gnc_commodity * arg, void * data) {
  *((const gnc_commodity **)data) = arg;
}

/********************************************************************
 * gnc_ui_new_commodity_modal()
 ********************************************************************/

gnc_commodity *
gnc_ui_new_commodity_modal(const char * selected_namespace,
                           GtkWidget * parent) {  
  gnc_commodity * retval = NULL;

  CommodityWindow * win = 
    gnc_ui_new_commodity_create(selected_namespace, &new_modal_callback, 
                                &retval);
  if(parent) {
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));
  }
  gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
  gtk_widget_show (win->dialog);
  gtk_main();

  return retval;
}

/********************************************************************
 * gnc_ui_edit_commodity_modal()
 ********************************************************************/

gboolean
gnc_ui_edit_commodity_modal(gnc_commodity *commodity,
                            GtkWidget * parent)
{
  gnc_commodity * retval = NULL;

  CommodityWindow * win = 
    gnc_ui_edit_commodity_create(commodity, &new_modal_callback, &retval);
  if(parent) {
    gnome_dialog_set_parent(GNOME_DIALOG(win->dialog), GTK_WINDOW(parent));
  }
  gtk_window_set_modal(GTK_WINDOW(win->dialog), TRUE);
  gtk_widget_show (win->dialog);
  gtk_main();

  return (retval != NULL);
}

/********************************************************************
 * gnc_ui_commodity_destroy()
 ********************************************************************/

void
gnc_ui_commodity_destroy(CommodityWindow * w) {
  if(w) {
    gnome_dialog_close(GNOME_DIALOG(w->dialog));
  }
}


/********************************************************************
 * gnc_ui_commodity_ok_cb()
 ********************************************************************/

static void
gnc_ui_commodity_ok_cb(GtkButton * button,
                       gpointer user_data) {
  CommodityWindow * w = user_data;

  const char * fullname  = gtk_entry_get_text(GTK_ENTRY(w->fullname_entry));
  const char * namespace = gnc_ui_namespace_picker_ns (w->namespace_combo);
  const char * mnemonic  = gtk_entry_get_text(GTK_ENTRY(w->mnemonic_entry));
  const char * code      = gtk_entry_get_text(GTK_ENTRY(w->code_entry));
  int fraction = gtk_spin_button_get_value_as_int
    (GTK_SPIN_BUTTON(w->fraction_spinbutton));

  gnc_commodity * c;

  if (safe_strcmp (namespace, GNC_COMMODITY_NS_ISO) == 0)
  {
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
      c = gnc_commodity_new(fullname, namespace, mnemonic, code, fraction, gnc_get_current_book());
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

    /* remember the commodity */
    c = gnc_commodity_table_insert(gnc_get_current_commodities(), c);

    /* if there's a callback (generally to fill in some fields with 
     * info about the commodity) call it */
    if(w->callback) {
      (w->callback)(c, w->callback_data);                              
    }

    /* close the dialog */
    gnc_ui_commodity_destroy(w);
  }
  else {
    gnc_warning_dialog_parented(w->dialog,
                                _("You must enter a non-empty \"Full name\", "
                                  "\"Symbol/abbreviation\",\n"
                                  "and \"Type\" for the commodity."));
  }
}


/********************************************************************
 * gnc_ui_commodity_help_cb()
 ********************************************************************/

static void
gnc_ui_commodity_help_cb(GtkButton * button,
                         gpointer user_data) {
  if (help_callback)
    help_callback ();
}


/********************************************************************
 * gnc_ui_commodity_cancel_cb()
 ********************************************************************/

static void
gnc_ui_commodity_cancel_cb(GtkButton * button,
                           gpointer user_data) {
  CommodityWindow * w = user_data;

  if (w->callback) {
    (w->callback)(NULL, w->callback_data);
  }

  gnc_ui_commodity_destroy(w);
}
