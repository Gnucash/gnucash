/********************************************************************\
 * top-level.c -- Gnome GUI main for GnuCash                        *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdlib.h>

#include "TransLog.h"
#include "combocell.h"
#include "dialog-account.h"
#include "dialog-commodity.h"
#include "dialog-options.h"
#include "dialog-scheduledxaction.h"
#include "dialog-transfer.h"
#include "dialog-totd.h"
#include "druid-hierarchy.h"
#include "file-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-file.h"
#include "gnc-filepath-utils.h"
#include "gnc-hooks.h"
#include "gnc-main-window.h"
#include "gnc-menu-extensions.h"
#include "gnc-plugin-menu-additions.h" /* FIXME Remove this line*/
#include "gnc-plugin-account-tree.h" /* FIXME Remove this line*/
#include "gnc-plugin-basic-commands.h" /* FIXME Remove this line*/
#include "gnc-plugin-file-history.h" /* FIXME Remove this line*/
#include "gnc-plugin-register.h" /* FIXME Remove this line*/
#include "gnc-plugin-budget.h"
#include "gnc-plugin-page-register.h"
#include "gnc-plugin-manager.h" /* FIXME Remove this line*/
#include "gnc-html.h"
#include "gnc-gnome-utils.h"
#include "gnc-report.h"
#include "gnc-split-reg.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnucash-color.h"
#include "gnucash-sheet.h"
#include "gnucash-style.h"
#include "guile-util.h"
#include "top-level.h"
#include "window-report.h"
#include "gnc-window.h"


/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/* ============================================================== */
/* HTML Handler for reports. */

#define IF_TYPE(URL_TYPE_STR,ENTITY_TYPE)                                   \
  if (strncmp (URL_TYPE_STR, location, strlen (URL_TYPE_STR)) == 0)         \
  {                                                                         \
    GUID guid;                                                              \
    QofCollection *col;                                                     \
    QofEntity *entity;                                                      \
    if (!string_to_guid (location + strlen(URL_TYPE_STR), &guid))           \
    {                                                                       \
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location); \
      return FALSE;                                                         \
    }                                                                       \
    col = qof_book_get_collection (book, ENTITY_TYPE);                      \
    entity = qof_collection_lookup_entity (col, &guid);                     \
    if (NULL == entity)                                                     \
    {                                                                       \
      result->error_message = g_strdup_printf (_("Entity Not Found: %s"), location); \
      return FALSE;                                                         \
    }                                                                       \


static gboolean
gnc_html_register_url_cb (const char *location, const char *label,
                          gboolean new_window, GNCURLResult *result)
{
  GncPluginPage *page = NULL;
  GNCSplitReg * gsr   = NULL;
  Split       * split = NULL;
  Account     * account = NULL;
  Transaction * trans;
  GList       * node;
  QofBook     * book = gnc_get_current_book();

  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="gnc-register:account=My Bank Account" */
  if (strncmp("account=", location, 8) == 0)
  {
    account = xaccGetAccountFromFullName (gnc_get_current_group (),
                                          location + 8, 
                                          gnc_get_account_separator ());
  }

  /* href="gnc-register:guid=12345678901234567890123456789012" */
  else IF_TYPE ("acct-guid=", GNC_ID_ACCOUNT)
    account = GNC_ACCOUNT(entity);
  }

  else IF_TYPE ("trans-guid=", GNC_ID_TRANS)
    trans = (Transaction *) entity;

    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
      split = node->data;
      account = xaccSplitGetAccount(split);
      if (account) break;
    }

    if (!account)
    {
      result->error_message =
        g_strdup_printf (_("Transaction with no Accounts: %s"), location);
      return FALSE;
    }
  }
  else IF_TYPE ("split-guid=", GNC_ID_SPLIT)
    split = (Split *) entity;
    account = xaccSplitGetAccount(split);
  }
  else
  {
    result->error_message =
          g_strdup_printf (_("Unsupported entity type: %s"), location);
    return FALSE;
  }

  page = gnc_plugin_page_register_new (account, FALSE);
  gnc_main_window_open_page (NULL, page);
  if (split) {
      gsr = gnc_plugin_page_register_get_gsr(page);
      gnc_split_reg_jump_to_split( gsr, split );
  }

  return TRUE;
}

/* ============================================================== */

static gboolean
gnc_html_price_url_cb (const char *location, const char *label,
                       gboolean new_window, GNCURLResult *result)
{
  QofBook * book = gnc_get_current_book();
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="gnc-register:guid=12345678901234567890123456789012" */
  IF_TYPE ("price-guid=", GNC_ID_PRICE)
    if (!gnc_price_edit_by_guid (NULL, &guid)) 
    {
        result->error_message = g_strdup_printf (_("No such price: %s"),
                                                 location);
        return FALSE;
    }
  }
  else
  {
      result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                               location);
      return FALSE;
  }

  return TRUE;
}

void
gnc_main_gui_init (void)
{
    ENTER(" ");

    if (!gnucash_style_init())
      gnc_shutdown(1);
    gnucash_color_init();

    gnc_html_register_url_handler (URL_TYPE_REGISTER,
                                   gnc_html_register_url_cb);

    gnc_html_register_url_handler (URL_TYPE_PRICE,
                                   gnc_html_price_url_cb);

    gnc_ui_sx_initialize();

    /* FIXME Remove this test code */
    gnc_plugin_manager_add_plugin (
        gnc_plugin_manager_get (), gnc_plugin_account_tree_new ());
    gnc_plugin_manager_add_plugin (
        gnc_plugin_manager_get (), gnc_plugin_basic_commands_new ());
    gnc_plugin_manager_add_plugin (
        gnc_plugin_manager_get (), gnc_plugin_file_history_new ());
    gnc_plugin_manager_add_plugin (
        gnc_plugin_manager_get (), gnc_plugin_menu_additions_new ());
    gnc_plugin_manager_add_plugin (
        gnc_plugin_manager_get (), gnc_plugin_register_new ());
    /* I'm not sure why the FIXME note says to remove this.  Maybe
       each module should be adding its own plugin to the manager?
       Anyway... Oh, maybe... nah */
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (),
                                   gnc_plugin_budget_new ());
    gnc_ui_hierarchy_druid_initialize();

    /* Run the ui startup hooks. */
    gnc_hook_run(HOOK_UI_STARTUP, NULL);
    LEAVE(" ");
    return;
}
/****************** END OF FILE **********************/
