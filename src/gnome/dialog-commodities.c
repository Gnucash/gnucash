/********************************************************************\
 * dialog-commodities.c -- commodities dialog                       *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "messages.h"


#define DIALOG_COMMODITIES_CM_CLASS "dialog-commodities"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;

  GtkWidget * commodity_list;
  GtkWidget * edit_button;
  GtkWidget * remove_button;

  gboolean show_currencies;

  gnc_commodity *commodity;
  gboolean new;
} CommoditiesDialog;


static gint last_width = 0;
static gint last_height = 0;


static int
commodity_compare (gconstpointer a, gconstpointer b)
{
  gnc_commodity *comm_a = (gnc_commodity *) a;
  gnc_commodity *comm_b = (gnc_commodity *) b;
  gint fraction_a;
  gint fraction_b;

  SAFE_STRCMP (gnc_commodity_get_namespace (comm_a),
               gnc_commodity_get_namespace (comm_b));

  SAFE_STRCMP (gnc_commodity_get_mnemonic (comm_a),
               gnc_commodity_get_mnemonic (comm_b));

  SAFE_STRCMP (gnc_commodity_get_fullname (comm_a),
               gnc_commodity_get_fullname (comm_b));

  SAFE_STRCMP (gnc_commodity_get_exchange_code (comm_a),
               gnc_commodity_get_exchange_code (comm_b));

  fraction_a = gnc_commodity_get_fraction (comm_a);
  fraction_b = gnc_commodity_get_fraction (comm_b);

  if (fraction_a < fraction_b)
    return -1;

  if (fraction_b < fraction_a)
    return 1;

  return 0;
}

static int
namespace_compare (gconstpointer a, gconstpointer b)
{
  return safe_strcmp (a, b);
}

static void
gnc_load_namespace (gpointer data, gpointer user_data)
{
  const char *namespace = data;
  CommoditiesDialog *cd = user_data;
  gnc_commodity_table *ct;
  GList *commodities;
  GList *node;

  if (!cd->show_currencies &&
      safe_strcmp (namespace, GNC_COMMODITY_NS_ISO) == 0)
    return;

  ct = gnc_get_current_commodities ();

  commodities = gnc_commodity_table_get_commodities (ct, namespace);

  commodities = g_list_sort (commodities, commodity_compare);

  for (node = commodities; node; node = node->next)
  {
    gnc_commodity *commodity = node->data;
    const char *text[5];
    gint row;

    text[0] = gnc_commodity_get_namespace (commodity);
    text[1] = gnc_commodity_get_mnemonic (commodity);
    text[2] = gnc_commodity_get_fullname (commodity);
    text[3] = gnc_commodity_get_exchange_code (commodity);
    text[4] = g_strdup_printf ("%d", gnc_commodity_get_fraction (commodity));

    row = gtk_clist_append (GTK_CLIST (cd->commodity_list), (char **)text);

    g_free ((char *) text[4]);

    gtk_clist_set_row_data (GTK_CLIST (cd->commodity_list), row, commodity);
  }

  g_list_free (commodities);
}

static void
gnc_commodities_set_sensitives (CommoditiesDialog *cd)
{
  gboolean sensitive;

  if (cd->commodity &&
      safe_strcmp (gnc_commodity_get_namespace (cd->commodity),
                   GNC_COMMODITY_NS_ISO) != 0)
    sensitive = TRUE;
  else
    sensitive = FALSE;

  gtk_widget_set_sensitive (cd->edit_button, sensitive);
  gtk_widget_set_sensitive (cd->remove_button, sensitive);
}

static void
gnc_commodities_load_commodities (CommoditiesDialog *cd)
{
  gnc_commodity_table *ct;
  GList *namespaces;
  int new_row;

  ct = gnc_get_current_commodities ();

  namespaces = gnc_commodity_table_get_namespaces (ct);
  namespaces = g_list_sort (namespaces, namespace_compare);

  gtk_clist_freeze (GTK_CLIST (cd->commodity_list));

  gtk_clist_clear (GTK_CLIST (cd->commodity_list));

  g_list_foreach (namespaces, gnc_load_namespace, cd);

  gtk_clist_thaw (GTK_CLIST (cd->commodity_list));

  gtk_clist_columns_autosize (GTK_CLIST (cd->commodity_list));

  new_row = gtk_clist_find_row_from_data (GTK_CLIST (cd->commodity_list),
                                          cd->commodity);
  if (new_row < 0)
    new_row = 0;

  gtk_clist_select_row (GTK_CLIST (cd->commodity_list), new_row, 0);
  if (gtk_clist_row_is_visible (GTK_CLIST (cd->commodity_list), new_row)
      != GTK_VISIBILITY_FULL)
    gtk_clist_moveto (GTK_CLIST (cd->commodity_list),
                      new_row, 0, 0.5, 0.0);

  g_list_free (namespaces);

  gnc_commodities_set_sensitives (cd);
}

static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  CommoditiesDialog *cd = data;

  gnc_unregister_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);

  g_free (cd);
}

static void
close_clicked (GtkWidget *widget, gpointer data)
{
  CommoditiesDialog *cd = data;

  gnc_close_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);
}

static void
edit_clicked (GtkWidget *widget, gpointer data)
{
  CommoditiesDialog *cd = data;

  if (!cd->commodity)
    return;

  if (gnc_ui_edit_commodity_modal (cd->commodity, cd->dialog))
    gnc_gui_refresh_all ();
}

static void
remove_clicked (GtkWidget *widget, gpointer data)
{
  GList *node;
  GList *accounts;
  gboolean do_delete;
  gboolean can_delete;
  CommoditiesDialog *cd = data;

  if (!cd->commodity)
    return;

  accounts = xaccGroupGetSubAccounts (gnc_get_current_group ());
  can_delete = TRUE;
  do_delete = FALSE;

  for (node = accounts; node; node = node->next)
  {
    Account *account = node->data;

    if (cd->commodity == xaccAccountGetCommodity (account))
    {
      can_delete = FALSE;
      break;
    }
  }

  /* FIXME check for transaction references */

  if (!can_delete)
  {
    const char *message = _("That commodity is currently used by\n"
                            "at least one of your accounts. You may\n"
                            "not delete it.");

    gnc_warning_dialog_parented (cd->dialog, message);
  }
  else
  {
    const char *message = _("Are you sure you want to delete the\n"
                            "current commodity?");

    do_delete = gnc_verify_dialog_parented (cd->dialog, message, TRUE);
  }

  if (do_delete)
  {
    gnc_commodity_table *ct = gnc_get_current_commodities ();

    gnc_commodity_table_remove (ct, cd->commodity);
    gnc_commodity_destroy (cd->commodity);
    cd->commodity = NULL;

    gnc_commodities_load_commodities (cd);
  }

  g_list_free (accounts);
}

static void
add_clicked (GtkWidget *widget, gpointer data)
{
  CommoditiesDialog *cd = data;
  gnc_commodity *commodity;
  const char *namespace;

  if (cd->commodity)
    namespace = gnc_commodity_get_namespace (cd->commodity);
  else
    namespace = NULL;

  commodity = gnc_ui_new_commodity_modal (namespace, cd->dialog);
  if (commodity)
  {
    cd->commodity = commodity;
    gnc_commodities_load_commodities (cd);
  }
}

static void
select_commodity_cb (GtkCList *clist, gint row, gint col,
                     GdkEventButton *event, gpointer data)
{
  CommoditiesDialog *cd = data;

  cd->commodity = gtk_clist_get_row_data (clist, row);
  cd->new = FALSE;

  gnc_commodities_set_sensitives (cd);
}

static void
unselect_commodity_cb (GtkCTree *ctre, gint row, gint col,
                       GdkEventButton *event, gpointer data)
{
  CommoditiesDialog *cd = data;

  cd->commodity = NULL;
  cd->new = FALSE;

  gnc_commodities_set_sensitives (cd);
}

static void
commodities_set_min_widths (CommoditiesDialog *cd)
{
  const char *titles[] = { _("Type"),
                           _("Symbol"),
                           _("Name"),
                           _("Code"),
                           _("Fraction") };

  GtkStyle *style = gtk_widget_get_style (cd->commodity_list);
  GdkFont *font = NULL;
  gint width;
  gint i;

  if (style != NULL)
    font = style->font;

  if (font != NULL)
    for (i = 0; i < 5; i++)
    {
      width = gdk_string_width (font, titles[i]);
      gtk_clist_set_column_min_width (GTK_CLIST (cd->commodity_list),
                                      i, width + 5);
    }
}

static void
show_currencies_toggled (GtkToggleButton *toggle, gpointer data)
{
  CommoditiesDialog *cd = data;

  cd->show_currencies = gtk_toggle_button_get_active (toggle);

  gnc_commodities_load_commodities (cd);
}

static void
gnc_commodities_dialog_create (GtkWidget * parent, CommoditiesDialog *cd)
{
  GtkWidget *dialog;
  GladeXML *xml;

  xml = gnc_glade_xml_new ("commodities.glade", "Commodities Dialog");

  dialog = glade_xml_get_widget (xml, "Commodities Dialog");
  cd->dialog = dialog;

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 0,
                               GTK_SIGNAL_FUNC (close_clicked), cd);

  gtk_signal_connect (GTK_OBJECT (dialog), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), cd);

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

  /* commodity tree */
  {
    GtkWidget *list;

    list = glade_xml_get_widget (xml, "commodity_list");
    cd->commodity_list = list;

    gtk_signal_connect (GTK_OBJECT(list), "select_row",
                        GTK_SIGNAL_FUNC(select_commodity_cb), cd);

    gtk_signal_connect (GTK_OBJECT(list), "unselect_row",
                        GTK_SIGNAL_FUNC(unselect_commodity_cb), cd);
  }

  /* buttons */
  {
    GtkWidget *button;

    button = glade_xml_get_widget (xml, "edit_button");
    cd->edit_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (edit_clicked), cd);

    button = glade_xml_get_widget (xml, "remove_button");
    cd->remove_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (remove_clicked), cd);

    button = glade_xml_get_widget (xml, "add_button");

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (add_clicked), cd);

    button = glade_xml_get_widget (xml, "show_currencies_button");

    gtk_signal_connect (GTK_OBJECT (button), "toggled",
                        GTK_SIGNAL_FUNC (show_currencies_toggled), cd);
  }

  gnc_commodities_load_commodities (cd);
  commodities_set_min_widths (cd);

  if (last_width == 0)
    gnc_get_window_size ("commodities_win", &last_width, &last_height);

  if (last_height == 0)
    last_height = 400;

  gtk_window_set_default_size (GTK_WINDOW(cd->dialog),
                               last_width, last_height);
}

static void
close_handler (gpointer user_data)
{
  CommoditiesDialog *cd = user_data;

  gdk_window_get_geometry (GTK_WIDGET(cd->dialog)->window,
                           NULL, NULL, &last_width, &last_height, NULL);

  gnc_save_window_size ("commodities_win", last_width, last_height);

  gnome_dialog_close (GNOME_DIALOG (cd->dialog));
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  CommoditiesDialog *cd = user_data;

  gnc_commodities_load_commodities (cd);
}

/********************************************************************\
 * gnc_commodities_dialog                                           *
 *   opens up a window to edit price information                    *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_commodities_dialog (GtkWidget * parent)
{
  CommoditiesDialog *cd;
  gint component_id;

  cd = g_new0 (CommoditiesDialog, 1);

  gnc_commodities_dialog_create (parent, cd);

  component_id = gnc_register_gui_component (DIALOG_COMMODITIES_CM_CLASS,
                                             refresh_handler, close_handler,
                                             cd);

  gtk_widget_grab_focus (cd->commodity_list);

  gtk_widget_show (cd->dialog);
}
