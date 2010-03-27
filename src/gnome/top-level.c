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
#include "dialog-sx-editor.h"
#include "dialog-transfer.h"
#include "dialog-totd.h"
#include "druid-hierarchy.h"
#include "file-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "gfec.h"
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
#include "gnc-gkeyfile-utils.h"


/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/* ============================================================== */
/* HTML Handler for reports. */

#define IF_TYPE(URL_TYPE_STR,ENTITY_TYPE)                                   \
  if (strncmp (URL_TYPE_STR, location, strlen (URL_TYPE_STR)) == 0)         \
  {                                                                         \
    GncGUID guid;                                                              \
    QofCollection *col;                                                     \
    QofInstance *entity;                                                      \
    if (!string_to_guid (location + strlen(URL_TYPE_STR), &guid))           \
    {                                                                       \
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location); \
      return FALSE;                                                         \
    }                                                                       \
    col = qof_book_get_collection (book, ENTITY_TYPE);                      \
    entity = qof_collection_lookup_entity (col, &guid);                     \
    if (NULL == entity)                                                     \
    {                                                                       \
      result->error_message = g_strdup_printf (_("Entity Not Found: %s"),   \
                                               location);                   \
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
        account = gnc_account_lookup_by_full_name (gnc_get_current_root_account (),
                  location + 8);
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
       if (split)
{
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

       /** Restore all persistent program state.  This function finds the
        *  "new" state file associated with a specific book guid.  It then
        *  iterates through this state information, calling a helper function
        *  to recreate each open window.
        *
        *  @note The name of the state file is based on the name of the data
        *  file, not the path name of the data file.  If there are multiple
        *  data files with the same name, the state files will be suffixed
        *  with a number.  E.G. test_account, test_account_2, test_account_3,
        *  etc.
        *
        *  @param session A pointer to the current session.
        *
        *  @param unused An unused pointer. */
       static void
       gnc_restore_all_state (gpointer session, gpointer unused)
{
    GKeyFile *keyfile = NULL;
    QofBook *book;
    const GncGUID *guid;
    const gchar *url, *guid_string;
    gchar *file_guid, *filename = NULL;
    GError *error = NULL;

    url = qof_session_get_url(session);
    ENTER("session %p (%s)", session, url ? url : "(null)");
    if (!url)
    {
        LEAVE("no url, nothing to do");
        return;
    }

    /* Get the book GncGUID */
    book = qof_session_get_book(session);
    guid = qof_entity_get_guid(QOF_INSTANCE(book));
    guid_string = guid_to_string(guid);

    keyfile = gnc_find_state_file(url, guid_string, &filename);
    if (filename)
        g_free(filename);

    if (!keyfile)
    {
        gnc_main_window_restore_default_state();
        LEAVE("no state file");
        return;
    }

#ifdef DEBUG
    /*  Debugging: dump a copy to stdout and the trace log */
    {
        gchar *file_data;
        gsize file_length;
        file_data = g_key_file_to_data(keyfile, &file_length, NULL);
        DEBUG("=== File Data Read===\n%s\n=== File End ===\n", file_data);
        g_free(file_data);
    }
#endif

    /* validate top level info */
    file_guid = g_key_file_get_string(keyfile, STATE_FILE_TOP,
                                      STATE_FILE_BOOK_GUID, &error);
    if (error)
    {
        g_warning("error reading group %s key %s: %s",
                  STATE_FILE_TOP, STATE_FILE_BOOK_GUID, error->message);
        LEAVE("can't read guid");
        goto cleanup;
    }
    if (!file_guid || strcmp(guid_string, file_guid))
    {
        g_warning("guid mismatch: book guid %s, state file guid %s",
                  guid_string, file_guid);
        LEAVE("guid values do not match");
        goto cleanup;
    }

    gnc_main_window_restore_all_windows(keyfile);

    /* Clean up */
    LEAVE("ok");
cleanup:
    if (error)
        g_error_free(error);
    if (file_guid)
        g_free(file_guid);
    g_key_file_free(keyfile);
}


/** Save all persistent program state to disk.  This function finds the
 *  name of the "new" state file associated with a specific book guid.
 *  It saves some top level data, then iterates through the list of
 *  open windows calling a helper function to save each window.
 *
 *  @note The name of the state file is based on the name of the data
 *  file, not the path name of the data file.  If there are multiple
 *  data files with the same name, the state files will be suffixed
 *  with a number.  E.G. test_account, test_account_2, test_account_3,
 *  etc.
 *
 *  @param session The QofSession whose state should be saved.
 *
 *  @param unused */
static void
gnc_save_all_state (gpointer session, gpointer unused)
{
    QofBook *book;
    const char *url, *guid_string;
    gchar *filename;
    const GncGUID *guid;
    GError *error = NULL;
    GKeyFile *keyfile = NULL;


    url = qof_session_get_url(session);
    ENTER("session %p (%s)", session, url ? url : "(null)");
    if (!url)
    {
        LEAVE("no url, nothing to do");
        return;
    }

    /* Get the book GncGUID */
    book = qof_session_get_book(session);
    guid = qof_entity_get_guid(QOF_INSTANCE(book));
    guid_string = guid_to_string(guid);

    /* Find the filename to use.  This returns the data from the
     * file so its possible that we could reuse the data and
     * maintain comments that were added to the data file, but
     * that's not something we currently do. For now the existing
     * data is dumped and completely regenerated.*/
    keyfile = gnc_find_state_file(url, guid_string, &filename);
    if (keyfile)
        g_key_file_free(keyfile);

    keyfile = g_key_file_new();
    /* Store top level info in the data structure */
    g_key_file_set_string(keyfile, STATE_FILE_TOP, STATE_FILE_BOOK_GUID,
                          guid_string);

    gnc_main_window_save_all_windows(keyfile);

#ifdef DEBUG
    /*  Debugging: dump a copy to the trace log */
    {
        gchar *file_data;
        gsize file_length;
        file_data = g_key_file_to_data(keyfile, &file_length, NULL);
        DEBUG("=== File Data Written===\n%s\n=== File End ===\n", file_data);
        g_free(file_data);
    }
#endif

    /* Write it all out to disk */
    gnc_key_file_save_to_file(filename, keyfile, &error);
    if (error)
    {
        g_critical(_("Error: Failure saving state file.\n  %s"),
                   error->message);
        g_error_free(error);
    }
    g_free(filename);

    /* Clean up */
    g_key_file_free(keyfile);
    LEAVE("");
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

    gnc_hook_add_dangler(HOOK_BOOK_OPENED,
                         gnc_restore_all_state, NULL);
    gnc_hook_add_dangler(HOOK_BOOK_CLOSED,
                         gnc_save_all_state, NULL);

    /* CAS: I'm not really sure why we remove before adding. */
    gnc_hook_remove_dangler(HOOK_BOOK_CLOSED, (GFunc)gnc_reports_flush_global);
    gnc_hook_add_dangler(HOOK_BOOK_CLOSED,
                         (GFunc)gnc_reports_flush_global, NULL);


    LEAVE(" ");
    return;
}

/****************** END OF FILE **********************/
