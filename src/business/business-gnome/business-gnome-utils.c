/*
 * business-gnome-utils.c -- General GUI Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001,2002,2006 Derek Atkins
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-component-manager.h"
#include "gnc-gtk-utils.h"

#include "gncCustomer.h"
#include "gncJob.h"
#include "gncVendor.h"
#include "gncOwner.h"
#include "gncInvoice.h"

#include "gnc-general-search.h"
#include "gncObject.h"
#include "business-gnome-utils.h"
#include "dialog-customer.h"
#include "dialog-job.h"
#include "dialog-vendor.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"

#include "gnc-commodity.h"

typedef enum {
  GNCSEARCH_TYPE_SELECT,
  GNCSEARCH_TYPE_EDIT
} GNCSearchType;

static GtkWidget * gnc_owner_new (GtkWidget *label, GtkWidget *hbox,
				  QofBook *book, GncOwner *owner,
				  GNCSearchType type)
{
  GtkWidget *edit;
  GNCSearchCB search_cb = NULL;
  const char *type_name = NULL;
  const char *text = NULL;

  switch (type) {
  case GNCSEARCH_TYPE_SELECT:
    text = _("Select...");
    break;
  case GNCSEARCH_TYPE_EDIT:
    text = _("Edit...");
  };

  switch (owner->type) {
  case GNC_OWNER_NONE:
  case GNC_OWNER_UNDEFINED:
    return NULL;

  case GNC_OWNER_CUSTOMER:
    if (type == GNCSEARCH_TYPE_SELECT)
      search_cb = gnc_customer_search_select;
    else
      search_cb = gnc_customer_search_edit;
    type_name = GNC_CUSTOMER_MODULE_NAME;
    break;

  case GNC_OWNER_JOB:
    if (type == GNCSEARCH_TYPE_SELECT)
      search_cb = gnc_job_search_select;
    else
      search_cb = gnc_job_search_edit;
    type_name = GNC_JOB_MODULE_NAME;
    break;

  case GNC_OWNER_VENDOR:
    if (type == GNCSEARCH_TYPE_SELECT)
      search_cb = gnc_vendor_search_select;
    else
      search_cb = gnc_vendor_search_edit;
    type_name = GNC_VENDOR_MODULE_NAME;
    break;

  case GNC_OWNER_EMPLOYEE:
    if (type == GNCSEARCH_TYPE_SELECT)
      search_cb = gnc_employee_search_select;
    else
      search_cb = gnc_employee_search_edit;
    type_name = GNC_EMPLOYEE_MODULE_NAME;
    break;

  default:
    g_warning ("Unknown type");
    return NULL;
  }

  edit = gnc_general_search_new (type_name, text, search_cb, book);
  if (!edit)
    return NULL;

  gnc_general_search_set_selected (GNC_GENERAL_SEARCH (edit),
				   owner->owner.undefined);
  gtk_box_pack_start (GTK_BOX (hbox), edit, FALSE, FALSE, 0);
  if (label)
    gtk_label_set_text (GTK_LABEL (label), _(gncObjectGetTypeLabel (type_name)));

  return edit;
}

GtkWidget * gnc_owner_select_create (GtkWidget *label, GtkWidget *hbox,
				     QofBook *book, GncOwner *owner)
{
  g_return_val_if_fail (hbox != NULL, NULL);
  g_return_val_if_fail (book != NULL, NULL);
  g_return_val_if_fail (owner != NULL, NULL);

  return gnc_owner_new (label, hbox, book, owner, GNCSEARCH_TYPE_SELECT);
}

GtkWidget * gnc_owner_edit_create (GtkWidget *label, GtkWidget *hbox,
				   QofBook *book, GncOwner *owner)
{
  g_return_val_if_fail (hbox != NULL, NULL);
  g_return_val_if_fail (book != NULL, NULL);
  g_return_val_if_fail (owner != NULL, NULL);

  return gnc_owner_new (label, hbox, book, owner, GNCSEARCH_TYPE_EDIT);
}

void gnc_owner_get_owner (GtkWidget *widget, GncOwner *owner)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (owner != NULL);

  /* We'll assume that the owner has the proper 'type' because we
   * can't change it here.  Hopefully the caller has it set properly
   */
  owner->owner.undefined =
    gnc_general_search_get_selected (GNC_GENERAL_SEARCH (widget));
}

void gnc_owner_set_owner (GtkWidget *widget, GncOwner *owner)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (owner != NULL);

  /* We'll assume that the owner has the proper 'type' because we
   * can't change it here.  Hopefully the caller has it set properly
   */

  gnc_general_search_set_selected (GNC_GENERAL_SEARCH (widget),
				   owner->owner.undefined);
}

typedef struct _invoice_select_info {
  GtkWidget *label;
  QofBook *book;
  GncOwner owner;
  gboolean have_owner;
} GncISI;

static GNCSearchWindow *
gnc_invoice_select_search_cb (gpointer start, gpointer isip)
{
  GncISI *isi = isip;

  if (!isi) return NULL;
  g_assert(isi->book);

  return gnc_invoice_search (start,
			     isi->have_owner ? &isi->owner : NULL,
			     isi->book);
}

static void
gnc_invoice_select_search_set_label(GncISI* isi)
{
  GncOwnerType owner_type;
  GncOwner *tmp;
  char *label;

  g_assert(isi);
  if (!isi->label) return;

  tmp = &isi->owner;
  owner_type = gncOwnerGetType(tmp);
  while (owner_type == GNC_OWNER_JOB) {
    tmp = gncOwnerGetEndOwner(tmp);
    owner_type = gncOwnerGetType(tmp);
  }

  /* Translators:  See comments in dialog-invoice.c:gnc_invoice_search() */
  switch (owner_type) {
  case GNC_OWNER_VENDOR:
    label = _("Bill");
    break;
  case GNC_OWNER_EMPLOYEE:
    label = _("Voucher");
    break;
  default:
    label = _("Invoice");
  }

  gtk_label_set_text(GTK_LABEL(isi->label), label);
}

GtkWidget * gnc_invoice_select_create (GtkWidget *hbox, QofBook *book,
				       const GncOwner *owner,
				       GncInvoice *invoice,
				       GtkWidget *label)
{
  GtkWidget *edit;
  GncISI *isi;

  g_return_val_if_fail (hbox != NULL, NULL);
  g_return_val_if_fail (book != NULL, NULL);
  /* Note: it is legal to have no owner or invoice */

  isi = g_new0(GncISI, 1);
  if (!isi)
    return NULL;

  if (owner) {
    gncOwnerCopy(owner, &isi->owner);
    isi->have_owner = TRUE;
  } else {
    gncOwnerInitCustomer(&isi->owner, NULL);
  }
  isi->book = book;
  isi->label = label;

  edit = gnc_general_search_new (GNC_INVOICE_MODULE_NAME, _("Select..."),
				 gnc_invoice_select_search_cb, isi);
  if (!edit) {
    g_free(isi);
    return NULL;
  }

  gnc_general_search_set_selected (GNC_GENERAL_SEARCH (edit), invoice);
  gtk_box_pack_start (GTK_BOX (hbox), edit, FALSE, FALSE, 0);
  g_object_set_data_full(G_OBJECT(edit), "isi-state", isi, g_free);

  /* Set the label */
  gnc_invoice_select_search_set_label(isi);

  return edit;
}

GncInvoice * gnc_invoice_get_invoice (GtkWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gnc_general_search_get_selected (GNC_GENERAL_SEARCH (widget));
}

void gnc_invoice_set_invoice (GtkWidget *widget, GncInvoice *invoice)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (invoice != NULL);

  gnc_general_search_set_selected (GNC_GENERAL_SEARCH (widget), invoice);
}

void gnc_invoice_set_owner (GtkWidget *widget, GncOwner *owner)
{
  GncISI *isi;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (owner != NULL);

  isi = g_object_get_data(G_OBJECT(widget), "isi-state");
  g_assert(isi);

  if (isi->owner.owner.undefined == owner->owner.undefined)
    return;

  gncOwnerCopy(owner, &isi->owner);
  isi->have_owner = TRUE;
  gnc_general_search_set_selected(GNC_GENERAL_SEARCH(widget), NULL);

  /* Reset the label */
  gnc_invoice_select_search_set_label(isi);
}

void
gnc_fill_account_select_combo (GtkWidget *combo, QofBook *book,
			       GList *acct_types, GList *acct_commodities)
{
  GtkListStore *store;
  GtkEntry *entry;
  GList *list, *node;
  char *text;

  g_return_if_fail (combo && GTK_IS_COMBO_BOX_ENTRY(combo));
  g_return_if_fail (book);
  g_return_if_fail (acct_types);

  /* Figure out if anything is set in the combo */
  text = gtk_combo_box_get_active_text(GTK_COMBO_BOX(combo));

  list = gnc_account_get_descendants (gnc_book_get_root_account (book));

  /* Clear the existing list */
  entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(combo)));
  gtk_entry_set_text(entry, "");
  store = GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(combo)));
  gtk_list_store_clear(store);

 /* Add the account names to the combo box */
  for (node = list; node; node = node->next) {
    Account *account = node->data;
    char *name;

    /* Only present accounts of the appropriate type */
    if (g_list_index (acct_types, (gpointer)xaccAccountGetType (account))
	== -1)
      continue;

    /* Only present accounts with the right commodity, if that's a
       restriction */
    if (acct_commodities)
    {
        if ( g_list_find_custom( acct_commodities,
                                 GINT_TO_POINTER(xaccAccountGetCommodity(account)),
                                 gnc_commodity_compare_void) == NULL ) {
            continue;
        }
    }

    name = gnc_account_get_full_name (account);
    gtk_combo_box_append_text(GTK_COMBO_BOX(combo), name);
    g_free(name);
  }
  gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);

  g_list_free (list);

  gnc_cbe_set_by_string(GTK_COMBO_BOX_ENTRY(combo), text);

  if (text)
    g_free (text);
}

GList *
gnc_business_account_types (GncOwner *owner)
{
  g_return_val_if_fail (owner, NULL);

  switch (gncOwnerGetType (owner)) {
  case GNC_OWNER_CUSTOMER:
    return (g_list_prepend (NULL, (gpointer)ACCT_TYPE_RECEIVABLE));
  case GNC_OWNER_VENDOR:
  case GNC_OWNER_EMPLOYEE:
    return (g_list_prepend (NULL, (gpointer)ACCT_TYPE_PAYABLE));
    break;
  default:
    return (g_list_prepend (NULL, (gpointer)ACCT_TYPE_NONE));
  }
}

GList *
gnc_business_commodities (GncOwner *owner)
{
  g_return_val_if_fail (owner, NULL);
  g_return_val_if_fail (gncOwnerGetCurrency(owner), NULL);

  return (g_list_prepend (NULL, gncOwnerGetCurrency(owner)));
}

/*********************************************************************/
/* Option Menu creation                                              */

typedef const char * (*GenericLookup_t)(gpointer);

typedef struct {
  gint		component_id;
  GtkWidget *	omenu;
  QofBook *	book;
  gboolean	none_ok;
  const char *	(*get_name)(gpointer);
  GList *	(*get_list)(QofBook*);

  gboolean	building_menu;
  gpointer	result;
  gpointer *	result_p;

  void		(*changed_cb)(GtkWidget*, gpointer);
  gpointer	cb_arg;
} OpMenuData;

#define DO_ADD_ITEM(s,o) { \
	add_menu_item (menu, (s), omd, (o)); \
	if (omd->result == (o)) current = index; \
	index++; \
}

static void
business_option_changed (GtkWidget *widget, gpointer data)
{
  OpMenuData *omd = data;

  g_return_if_fail (omd);
  omd->result = g_object_get_data (G_OBJECT (widget), "this_item");

  if (!omd->building_menu) {
    if (omd->result_p)
      *(omd->result_p) = omd->result;

    if (omd->changed_cb)
      (omd->changed_cb)(omd->omenu, omd->cb_arg);
  }
}

static void
add_menu_item (GtkWidget *menu, const char *label, OpMenuData *omd,
	       gpointer this_item)
{
  GtkWidget *item = gtk_menu_item_new_with_label (label);
  g_object_set_data (G_OBJECT (item), "this_item", this_item);
  g_signal_connect (G_OBJECT (item), "activate",
		    G_CALLBACK (business_option_changed), omd);
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  gtk_widget_show (item);
}

static void
generic_omenu_destroy_cb (GtkWidget *widget, gpointer data)
{
  OpMenuData *omd = data;

  gnc_unregister_gui_component (omd->component_id);
  g_free (omd);
}

static void
build_generic_optionmenu (OpMenuData *omd)
{
  GList *items;
  GtkWidget *menu;
  int current = 0, index = 0;

  /* Make sure we can "get_list" */
  if (omd->get_list == NULL)
    return;

  /* Get the list of items */
  items = (omd->get_list)(omd->book);

  /* Make a menu */
  menu = gtk_menu_new ();

  omd->building_menu = TRUE;

  if (omd->none_ok || items == NULL)
    DO_ADD_ITEM (_("None"), NULL);

  for ( ; items; items = items->next)
    DO_ADD_ITEM ((omd->get_name)(items->data), items->data);

  gtk_option_menu_set_menu (GTK_OPTION_MENU (omd->omenu), menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omd->omenu), current);
  gtk_widget_show (menu);

  omd->building_menu = FALSE;
}

static void
generic_omenu_refresh_handler (GHashTable *changes, gpointer user_data)
{
  OpMenuData *omd = user_data;
  build_generic_optionmenu (omd);
}

static OpMenuData *
make_generic_optionmenu (GtkWidget *omenu, QofBook *book,
			 gboolean none_ok, GNCIdType type_name,
			 GList * (*get_list)(QofBook*),
			 GenericLookup_t get_name,
			 gpointer *result)
{
  OpMenuData *omd;

  omd = g_object_get_data (G_OBJECT (omenu), "menu-data");

  /* If this is the first time we've been called, then build the
   * Option Menu Data object, register with the component manager, and
   * watch for changed items.  Then register for deletion, so we can
   * unregister and free the data when this menu is destroyed.
   */
  if (!omd) {
    omd = g_new0 (OpMenuData, 1);
    omd->omenu = omenu;
    omd->book = book;
    omd->result_p = result;
    omd->none_ok = none_ok;
    omd->get_name = get_name;
    omd->get_list = get_list;
    g_object_set_data (G_OBJECT (omenu), "menu-data", omd);

    if (result)
      omd->result = *result;

    omd->component_id =
      gnc_register_gui_component ("generic-omenu-refresh-hook",
				  generic_omenu_refresh_handler,
				  NULL, omd);


    if (type_name)
      gnc_gui_component_watch_entity_type (omd->component_id,
					   type_name,
					   QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    g_signal_connect (G_OBJECT (omenu), "destroy",
		      G_CALLBACK (generic_omenu_destroy_cb), omd);

  }

  build_generic_optionmenu (omd);

  return omd;
}

void
gnc_ui_optionmenu_set_changed_callback (GtkWidget *omenu,
					void (*changed_cb)(GtkWidget*,gpointer),
					gpointer cb_arg)
{
  OpMenuData *omd;

  if (!omenu) return;

  omd = g_object_get_data (G_OBJECT (omenu), "menu-data");
  g_return_if_fail (omd);

  omd->changed_cb = changed_cb;
  omd->cb_arg = cb_arg;
}

gpointer
gnc_ui_optionmenu_get_value (GtkWidget *omenu)
{
  OpMenuData *omd;

  if (!omenu) return NULL;

  omd = g_object_get_data (G_OBJECT (omenu), "menu-data");
  g_return_val_if_fail (omd, NULL);

  return omd->result;
}

void
gnc_ui_optionmenu_set_value (GtkWidget *omenu, gpointer data)
{
  OpMenuData *omd;
  GtkWidget *menu;
  GList *node;
  gint counter;

  if (!omenu) return;

  omd = g_object_get_data (G_OBJECT (omenu), "menu-data");
  g_return_if_fail (omd);

  menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (omenu));
  g_return_if_fail (menu);

  /* now walk all the children until we find our object */
  for (counter = 0, node = ((GTK_MENU_SHELL (menu))->children);
       node;
       node = node->next, counter++)
  {
    GObject *menuitem = node->data;
    gpointer this_object = g_object_get_data (menuitem, "this_item");

    if (this_object == data) {
      gtk_option_menu_set_history (GTK_OPTION_MENU (omd->omenu), counter);
      return;
    }
  }
}

/* Create an optionmenu of available billing terms and attach it to
 * the menu passed in.  If none_ok is true, then add "none" as a
 * choice (with data set to NULL).  Any time the menu changes,
 * 'choice' will be set to the chosen option.  If *choice is non-NULL,
 * then that will be the default option setting when the menu is
 * created.
 */
void
gnc_ui_billterms_optionmenu (GtkWidget *omenu, QofBook *book,
			     gboolean none_ok, GncBillTerm **choice)
{
  if (!omenu || !book) return;

  make_generic_optionmenu (omenu, book, none_ok, GNC_BILLTERM_MODULE_NAME,
			   gncBillTermGetTerms,
			   (GenericLookup_t)gncBillTermGetName,
			   (gpointer *)choice);
}

void
gnc_ui_taxtables_optionmenu (GtkWidget *omenu, QofBook *book,
			     gboolean none_ok, GncTaxTable **choice)
{
  if (!omenu || !book) return;

  make_generic_optionmenu (omenu, book, none_ok, GNC_TAXTABLE_MODULE_NAME,
			   gncTaxTableGetTables,
			   (GenericLookup_t)gncTaxTableGetName,
			   (gpointer *)choice);
}

void
gnc_ui_taxincluded_optionmenu (GtkWidget *omenu, GncTaxIncluded *choice)
{
  GtkWidget *menu;
  OpMenuData *omd;
  int current = 0, index = 0;

  if (!omenu) return;

  omd = make_generic_optionmenu (omenu, NULL, FALSE, NULL, NULL, NULL,
				 (gpointer *)choice);

  g_return_if_fail (omd);

  menu = gtk_menu_new ();

  add_menu_item (menu, _("Yes"), omd,
		 GINT_TO_POINTER (GNC_TAXINCLUDED_YES));
  if (*choice == GNC_TAXINCLUDED_YES) current = index;
  index++;

  add_menu_item (menu, _("No"), omd,
		 GINT_TO_POINTER (GNC_TAXINCLUDED_NO));
  if (*choice == GNC_TAXINCLUDED_NO) current = index;
  index++;

  add_menu_item (menu, _("Use Global"), omd,
		 GINT_TO_POINTER (GNC_TAXINCLUDED_USEGLOBAL));
  if (*choice == GNC_TAXINCLUDED_USEGLOBAL) current = index;
  index++;

  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omenu), current);
  gtk_widget_show (menu);
}
