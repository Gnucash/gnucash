/*
 * dialog-order.c -- Dialog for Order entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"
#include "gnucash-sheet.h"
#include "window-help.h"

#include "gncOrder.h"
#include "gncOrderP.h"

#include "gncEntryLedger.h"

#include "dialog-order.h"
#include "business-chooser.h"

#define DIALOG_NEW_ORDER_CM_CLASS "dialog-new-order"
#define DIALOG_EDIT_ORDER_CM_CLASS "dialog-edit-order"

typedef enum
{
  NEW_ORDER,
  EDIT_ORDER
} OrderDialogType;

struct _order_select_window {
  GNCBook *	book;
};

typedef struct _order_window {
  GtkWidget *	dialog;

  GnucashRegister *	reg;

  OrderDialogType	dialog_type;
  GUID		order_guid;
  gint		component_id;
  GNCBook *	book;
  GncOrder *	created_order;

} OrderWindow;

static GncOrder *
ow_get_order (OrderWindow *ow)
{
  if (!ow)
    return NULL;

  return gncOrderLookup (ow->book, &ow->order_guid);
}

static void gnc_ui_to_order (OrderWindow *ow, GncOrder *order)
{
  /* Fill this in */

  // gncOrderCommitEdit (order);
  gnc_resume_gui_refresh ();
}

static void
gnc_order_window_ok_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;
  GncOrder *order;

  /* Now save it off */
  {
    GncOrder *order = ow_get_order (ow);
    if (order) {
      gnc_ui_to_order (ow, order);
    }
    ow->created_order = order;
    ow->order_guid = *xaccGUIDNULL ();
  }

  gnc_close_gui_component (ow->component_id);
}

static void
gnc_order_window_cancel_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;

  gnc_close_gui_component (ow->component_id);
}

static void
gnc_order_window_help_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;
  char *help_file = "";		/* xxx */

  helpWindow(NULL, NULL, help_file);
}

static void
gnc_order_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  OrderWindow *ow = data;
  GncOrder *order = ow_get_order (ow);

  gnc_suspend_gui_refresh ();

  if (ow->dialog_type == NEW_ORDER && order != NULL) {
    gncOrderDestroy (order);
    ow->order_guid = *xaccGUIDNULL ();
  }

  gnc_unregister_gui_component (ow->component_id);
  gnc_resume_gui_refresh ();

  g_free (ow);
}

static int
gnc_order_on_close_cb (GnomeDialog *dialog, gpointer data)
{
  OrderWindow *ow;
  GncOrder **created_order = data;

  if (data) {
    ow = gtk_object_get_data (GTK_OBJECT (dialog), "dialog_info");
    *created_order = ow->created_order;
  }

  gtk_main_quit ();

  return FALSE;
}

static void
gnc_order_window_close_handler (gpointer user_data)
{
  OrderWindow *ow = user_data;

  gnome_dialog_close (GNOME_DIALOG (ow->dialog));
}

static void
gnc_order_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
  OrderWindow *ow = user_data;
  const EventInfo *info;
  GncOrder *order = ow_get_order (ow);

  /* If there isn't a order behind us, close down */
  if (!order) {
    gnc_close_gui_component (ow->component_id);
    return;
  }

  /* Next, close if this is a destroy event */
  if (changes) {
    info = gnc_gui_get_entity_events (changes, &ow->order_guid);
    if (info && (info->event_mask & GNC_EVENT_DESTROY)) {
      gnc_close_gui_component (ow->component_id);
      return;
    }
  }
}

static OrderWindow *
gnc_order_new_window (GtkWidget *parent, GNCBook *bookp,
			 GncOrder *order)
{
  OrderWindow *ow;
  GladeXML *xml;
  GtkWidget *vbox, *regWidget;
  GncEntryLedger *entry_ledger;
  GnomeDialog *owd;
  GList *entries;

  ow = g_new0 (OrderWindow, 1);

  ow->book = bookp;

  /* Find the dialog */
  xml = gnc_glade_xml_new ("order.glade", "Order Entry Dialog");
  ow->dialog = glade_xml_get_widget (xml, "Order Entry Dialog");
  owd = GNOME_DIALOG (ow->dialog);

  gtk_object_set_data (GTK_OBJECT (ow->dialog), "dialog_info", ow);

  /* default to ok */
  gnome_dialog_set_default (owd, 0);

  /* Build the ledger */
  entry_ledger = gnc_entry_ledger_new (ow->book, GNCENTRY_LEDGER);
  entries = gncOrderGetEntries (order);
  /* Set watches on entries*/
  gnc_entry_ledger_load (entry_ledger, entries);

  /* Watch the order of operations, here... */
  gnucash_register_set_initial_rows( 6 );
  regWidget = gnucash_register_new (gnc_entry_ledger_get_table (entry_ledger));
  gnc_table_init_gui( regWidget, entry_ledger );
  ow->reg = GNUCASH_REGISTER(regWidget);
  GNUCASH_SHEET(ow->reg->sheet)->window = GTK_WIDGET(ow->dialog);

  vbox = glade_xml_get_widget (xml, "ledger_vbox");
  // gtk_box_pack_start (GTK_BOX(vbox), toolbar, FALSE, FALSE, 2);
  gtk_box_pack_start (GTK_BOX(vbox), regWidget, TRUE, TRUE, 2);

  if (parent)
    gnome_dialog_set_parent (owd, GTK_WINDOW (parent));

  gtk_signal_connect (GTK_OBJECT (ow->dialog), "destroy",
		      GTK_SIGNAL_FUNC(gnc_order_window_destroy_cb), ow);

  gnome_dialog_button_connect (owd, 0,
			       GTK_SIGNAL_FUNC(gnc_order_window_ok_cb), ow);
  gnome_dialog_button_connect (owd, 1,
			       GTK_SIGNAL_FUNC(gnc_order_window_cancel_cb), ow);
  gnome_dialog_button_connect (owd, 2,
			       GTK_SIGNAL_FUNC(gnc_order_window_help_cb), ow);

  /* Setup initial values */
  if (order != NULL) {
    ow->dialog_type = EDIT_ORDER;
    ow->order_guid = *gncOrderGetGUID (order);

    ow->component_id =
      gnc_register_gui_component (DIALOG_EDIT_ORDER_CM_CLASS,
				  gnc_order_window_refresh_handler,
				  gnc_order_window_close_handler,
				  ow);

  } else {
    order = gncOrderCreate (bookp, GNC_ORDER_SALES); /* XXX */
    ow->order_guid = *gncOrderGetGUID (order);

    ow->dialog_type = NEW_ORDER;

    ow->component_id =
      gnc_register_gui_component (DIALOG_NEW_ORDER_CM_CLASS,
				  gnc_order_window_refresh_handler,
				  gnc_order_window_close_handler,
				  ow);
  }

  gnc_gui_component_watch_entity_type (ow->component_id,
				       GNC_ID_NONE,
				       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_widget_show_all (ow->dialog);

  return ow;
}

GncOrder *
gnc_order_new (GtkWidget *parent, GNCBook *bookp)
{
  OrderWindow *ow;
  GncOrder *created_order = NULL;

  /* Make sure required options exist */
  if (!bookp) return NULL;

  ow = gnc_order_new_window (parent, bookp, NULL);

  gtk_signal_connect (GTK_OBJECT (ow->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_order_on_close_cb),
		      &created_order);

  gtk_window_set_modal (GTK_WINDOW (ow->dialog), TRUE);

  gtk_main ();

  return created_order;
}

void
gnc_order_edit (GtkWidget *parent, GncOrder *order)
{
  OrderWindow *ow;

  if (!order) return;

  ow = gnc_order_new_window (parent, gncOrderGetBook(order), order);

  gtk_signal_connect (GTK_OBJECT (ow->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_order_on_close_cb),
		      NULL);

  gtk_window_set_modal (GTK_WINDOW (ow->dialog), TRUE);

  gtk_main ();

  return;
}

/* Functions for widgets for order selection */

static gpointer gnc_order_edit_new_cb (gpointer arg, GtkWidget *toplevel)
{
  struct _order_select_window *sw = arg;

  if (!arg) return NULL;

  return gnc_order_new (toplevel, sw->book);
}

static void gnc_order_edit_edit_cb (gpointer arg, gpointer obj, GtkWidget *toplevel)
{
  GncOrder *order = obj;
  struct _order_select_window *sw = arg;

  if (!arg || !obj) return;

  gnc_order_edit (toplevel, order);
}

gpointer gnc_order_edit_new_select (gpointer bookp, gpointer order,
				       GtkWidget *toplevel)
{
  GNCBook *book = bookp;
  struct _order_select_window sw;

  g_return_val_if_fail (bookp != NULL, NULL);

  sw.book = book;

  return
    gnc_ui_business_chooser_new (toplevel, order,
				 book, GNC_ORDER_MODULE_NAME,
				 gnc_order_edit_new_cb,
				 gnc_order_edit_edit_cb, &sw);
}

gpointer gnc_order_edit_new_edit (gpointer bookp, gpointer v,
				     GtkWidget *toplevel)
{
  GNCBook *book = bookp;
  GncOrder *order = v;

  g_return_val_if_fail (order != NULL, NULL);

  gnc_order_edit (toplevel, order);
  return order;
}
