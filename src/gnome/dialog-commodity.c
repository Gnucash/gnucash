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
#include <glib.h>
#include <stdio.h>

#include "dialog-commodity.h"
#include "window-help.h"
#include "FileDialog.h"
#include "query-user.h"
#include "gnc-ui.h"


struct _selectcommoditywindow {
  GtkWidget * dialog;
  GtkWidget * namespace_combo;
  GtkWidget * namespace_entry;
  GtkWidget * commodity_combo;
  GtkWidget * commodity_entry;

  gnc_commodity_callback callback;
  void      * callback_data;
};

struct _newcommoditywindow {
  GtkWidget * dialog;
  GtkWidget * fullname_entry;
  GtkWidget * mnemonic_entry;
  GtkWidget * namespace_entry;
  GtkWidget * namespace_combo;
  GtkWidget * code_entry;
  GtkWidget * fraction_spinbutton;

  gnc_commodity_callback callback;
  void      * callback_data;
};


static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel,
                               gnc_commodity_callback callback,
                               void * callback_data);
static NewCommodityWindow *
gnc_ui_new_commodity_create(const char * selected_namespace,
                            gnc_commodity_callback callback, 
                            void * callback_data);


static void 
select_modal_callback(const gnc_commodity * arg, void * data) {
  *((const gnc_commodity **)data) = arg;
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
  char * namespace;

  retval->dialog = create_Commodity_Selector_Dialog();
  retval->namespace_combo = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "namespace_combo");
  retval->namespace_entry = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "namespace_entry");
  retval->commodity_combo = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "commodity_combo");
  retval->commodity_entry = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "commodity_entry");

  retval->callback = callback;
  retval->callback_data = callback_data;

  gtk_signal_connect (GTK_OBJECT(retval->dialog), "close",
                      GTK_SIGNAL_FUNC(select_commodity_close), retval);

  gtk_object_set_data(GTK_OBJECT(retval->dialog), "select_commodity_struct",
                      retval);

  /* build the menus of namespaces and commodities */
  namespace = 
    gnc_ui_update_namespace_picker(retval->namespace_combo, 
                                   gnc_commodity_get_namespace(orig_sel));
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
                               const char * init_string) {  
  GList      * commodities = 
    gnc_commodity_table_get_commodities(gnc_engine_commodities(),
                                        namespace);
  GList      * iterator = NULL;
  GList      * commodity_items = NULL;
  const char * current;

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

void
gnc_ui_select_commodity_ok_cb(GtkButton * button,
                              gpointer user_data) {
  GtkWidget             * dialog = GTK_WIDGET(user_data);
  SelectCommodityWindow * w = 
    gtk_object_get_data(GTK_OBJECT(dialog), "select_commodity_struct");

  char          * namespace;  
  char          * fullname;
  gnc_commodity * retval = NULL;

  namespace       = gtk_entry_get_text(GTK_ENTRY(w->namespace_entry));
  fullname        = gtk_entry_get_text(GTK_ENTRY(w->commodity_entry));
  
  retval = gnc_commodity_table_find_full(gnc_engine_commodities(), 
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

void
gnc_ui_select_commodity_new_cb(GtkButton * button,
                               gpointer user_data) {
  GtkWidget             * dialog = GTK_WIDGET(user_data);
  SelectCommodityWindow * w = 
    gtk_object_get_data(GTK_OBJECT(dialog), "select_commodity_struct");
  
  char * namespace = 
    gtk_entry_get_text(GTK_ENTRY(w->namespace_entry));
  
  const gnc_commodity * new_commodity = 
    gnc_ui_new_commodity_modal(namespace, dialog);
  
  if(new_commodity) {
    char *namespace;
    
    namespace = 
      gnc_ui_update_namespace_picker(w->namespace_combo, 
                                     gnc_commodity_get_namespace
                                     (new_commodity));
    g_free(namespace);
    gnc_ui_update_commodity_picker(w->commodity_combo,
                                   gnc_commodity_get_namespace(new_commodity),
                                   gnc_commodity_get_printname(new_commodity));
  }
}


/********************************************************************
 * gnc_ui_select_commodity_cancel_cb()
 ********************************************************************/

void
gnc_ui_select_commodity_cancel_cb(GtkButton * button,
                                  gpointer user_data) {
  GtkWidget             * dialog = GTK_WIDGET(user_data);
  SelectCommodityWindow * w = 
    gtk_object_get_data(GTK_OBJECT(dialog), "select_commodity_struct");

  if (w->callback)
    (w->callback)(NULL, w->callback_data);

  gnc_ui_select_commodity_destroy(w);
}

/********************************************************************
 * gnc_ui_select_commodity_namespace_changed_cb()
 ********************************************************************/

void
gnc_ui_select_commodity_namespace_changed_cb(GtkEditable * entry,
                                             gpointer user_data) {
  GtkWidget             * dialog = GTK_WIDGET(user_data);
  SelectCommodityWindow * w = 
    gtk_object_get_data(GTK_OBJECT(dialog), "select_commodity_struct");
  char * namespace = 
    gtk_entry_get_text(GTK_ENTRY(w->namespace_entry));
  
  gnc_ui_update_commodity_picker(w->commodity_combo,
                                 namespace, NULL);
}


/********************************************************************
 * gnc_ui_update_namespace_picker
 ********************************************************************/


char * 
gnc_ui_update_namespace_picker(GtkWidget * combobox, 
                               const char * init_string) {
  GList * namespaces;
  char  * active;

  /* fetch a list of the namespaces */
  namespaces = gnc_commodity_table_get_namespaces(gnc_engine_commodities());
  namespaces = g_list_sort(namespaces, g_strcmp);

  /* stick them in the combobox */
  gtk_combo_set_popdown_strings(GTK_COMBO(combobox), namespaces);

  /* set the entry text */
  if(init_string) {
    active = g_strdup(init_string);
  }
  else {
    active = g_strdup(namespaces->data);
  }    
  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(combobox)->entry), active);
  g_list_free(namespaces);

  return active;
}


static gint
new_commodity_close (GnomeDialog *dialog, gpointer data)
{
  NewCommodityWindow *ncw = data;

  g_free(ncw);

  gtk_main_quit ();

  return FALSE;
}

/********************************************************************
 * gnc_ui_new_commodity_create()
 ********************************************************************/

static NewCommodityWindow *
gnc_ui_new_commodity_create(const char * selected_namespace,
                            gnc_commodity_callback callback, 
                            void * callback_data) {
  NewCommodityWindow * retval = g_new0(NewCommodityWindow, 1);
  char *namespace;

  retval->dialog = create_New_Commodity_Dialog();

  retval->fullname_entry =
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "fullname_entry");
  retval->mnemonic_entry =
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "mnemonic_entry");
  retval->namespace_combo =
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "namespace_combo");
  retval->namespace_entry = 
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "namespace_entry");
  retval->code_entry =
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "code_entry");
  retval->fraction_spinbutton =
    gtk_object_get_data(GTK_OBJECT(retval->dialog), "fraction_spinbutton");

  retval->callback = callback;
  retval->callback_data = callback_data;

  gtk_object_set_data(GTK_OBJECT(retval->dialog), "new_commodity_struct",
                      (gpointer)retval);

  gtk_signal_connect (GTK_OBJECT(retval->dialog), "close",
                      GTK_SIGNAL_FUNC(new_commodity_close), retval);

  namespace = gnc_ui_update_namespace_picker(retval->namespace_combo,
                                             selected_namespace);
  g_free(namespace);

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

  NewCommodityWindow * win = 
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
 * gnc_ui_new_commodity_destroy()
 ********************************************************************/

void
gnc_ui_new_commodity_destroy(NewCommodityWindow * w) {
  if(w) {
    gnome_dialog_close(GNOME_DIALOG(w->dialog));
  }
}


/********************************************************************
 * gnc_ui_new_commodity_ok_cb()
 ********************************************************************/

void
gnc_ui_new_commodity_ok_cb(GtkButton * button,
                           gpointer user_data) {
  GtkWidget          * dialog = GTK_WIDGET(user_data);
  NewCommodityWindow * w = 
    gtk_object_get_data(GTK_OBJECT(dialog), "new_commodity_struct");

  char * fullname  = gtk_entry_get_text(GTK_ENTRY(w->fullname_entry));
  char * namespace = gtk_entry_get_text(GTK_ENTRY(w->namespace_entry));
  char * mnemonic  = gtk_entry_get_text(GTK_ENTRY(w->mnemonic_entry));

  gnc_commodity * c;

  if(fullname && fullname[0] &&
     namespace && namespace[0] &&
     mnemonic && mnemonic[0]) {
    c = gnc_commodity_new(fullname, namespace, mnemonic,
                          gtk_entry_get_text
                          (GTK_ENTRY(w->code_entry)),
                          gtk_spin_button_get_value_as_int
                          (GTK_SPIN_BUTTON(w->fraction_spinbutton)));
    
    /* remember the commodity */
    gnc_commodity_table_insert(gnc_engine_commodities(), c);

    /* if there's a callback (generally to fill in some fields with 
     * info about the commodity) call it */
    if(w->callback) {
      (w->callback)(c, w->callback_data);                              
    }

    /* close the dialog */
    gnc_ui_new_commodity_destroy(w);
  }
  else {
    gnc_warning_dialog(_("You must enter a non-empty \"Full name\", "
                         "\"Symbol/abbreviation\",\n"
                         "and \"Type\" for the commodity."));
  }
}


/********************************************************************
 * gnc_ui_new_commodity_new_cb()
 ********************************************************************/

void
gnc_ui_new_commodity_help_cb(GtkButton * button,
                             gpointer user_data) {
  /* GtkWidget             * dialog = GTK_WIDGET(user_data); */

  helpWindow(NULL, _("Help"), HH_COMMODITY);
}


/********************************************************************
 * gnc_ui_new_commodity_cancel_cb()
 ********************************************************************/

void
gnc_ui_new_commodity_cancel_cb(GtkButton * button,
                               gpointer user_data) {
  GtkWidget             * dialog = GTK_WIDGET(user_data);
  NewCommodityWindow * w = 
    gtk_object_get_data(GTK_OBJECT(dialog), "new_commodity_struct");  

  if (w->callback) {
    (w->callback)(NULL, w->callback_data);
  }

  gnc_ui_new_commodity_destroy(w);
}
