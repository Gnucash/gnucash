/********************************************************************\
 * dialog-new-user.c -- new user dialog for GnuCash                 *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
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
#include <guile/gh.h>

#include "dialog-new-user.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "druid-qif-import.h"
#include "global-options.h"
#include "gnc-ui.h"
#include "window-help.h"


static void gnc_ui_new_user_cancel_dialog (void);


void
gnc_set_first_startup (gboolean first_startup)
{
  gnc_set_boolean_option ("__new_user", "first_startup", first_startup);
}

void
gnc_ui_new_user_dialog (void)
{
  GtkWidget *dialog;
  GtkWidget *new_accounts_button;
  GtkWidget *import_qif_button;
  GtkWidget *tutorial_button;
  GladeXML  *xml;
  gint result;

  xml = gnc_glade_xml_new ("newuser.glade", "New User Dialog");

  dialog = glade_xml_get_widget (xml, "New User Dialog");

  gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);

  new_accounts_button = glade_xml_get_widget (xml, "new_accounts_button");
  import_qif_button = glade_xml_get_widget (xml, "import_qif_button");
  tutorial_button = glade_xml_get_widget (xml, "tutorial_button");

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
  if (result != 0)
  {
    gnc_ui_new_user_cancel_dialog ();
    gtk_widget_destroy (dialog);
    gncp_new_user_finish ();
    return;
  }

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (new_accounts_button)))
  {
    gnc_ui_hierarchy_druid ();
    gtk_widget_destroy (dialog);
    return;
  }

  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON (import_qif_button)))
  {
    gnc_ui_qif_import_druid_make ();
  }
  else if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (tutorial_button)))
  {
    helpWindow (NULL, NULL, HH_QUICKSTART);
  }

  gncp_new_user_finish ();
  gtk_widget_destroy (dialog);
}

static void
gnc_ui_new_user_cancel_dialog (void)
{
  GtkWidget *dialog;
  GtkWidget *toggle;
  GladeXML  *xml;
  gint result;

  xml = gnc_glade_xml_new ("newuser.glade", "New User Cancel Dialog");

  dialog = glade_xml_get_widget (xml, "New User Cancel Dialog");
  toggle = glade_xml_get_widget (xml, "run_again_toggle");

  gnome_dialog_close_hides (GNOME_DIALOG (dialog), TRUE);

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
  if (result == 0)
  {
    gboolean keepshowing = TRUE;

    keepshowing = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (toggle));

    gnc_set_first_startup (keepshowing);

    gncp_new_user_finish ();
  }
}

void
gncp_new_user_finish (void)
{
  gh_eval_str("(gnc:default-ui-start)");
  gh_eval_str("(gnc:show-main-window)");
  gh_eval_str("(gnc:hook-run-danglers gnc:*book-opened-hook* #f)");
}
