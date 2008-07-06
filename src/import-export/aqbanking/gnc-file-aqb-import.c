/*
 * gnc-file-aqb-import.c --
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

/**
 * @internal
 * @file gnc-file-aqb-import.c
 * @brief DTAUS import module code
 * @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 * @author Copyright (C) 2003 Jan-Pascal van Best <janpascal@vanbest.org>
 * @author Copyright (C) 2006 Florian Steinel
 * @author Copyright (C) 2006 Christian Stimming
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <gwenhywfar/io_file.h>
#include <gwenhywfar/io_buffered.h>
#include <gwenhywfar/iomanager.h>

#include "dialog-ab-trans.h"
#include "gnc-ab-utils.h"
#include "gnc-file.h"
#include "gnc-file-aqb-import.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "import-account-matcher.h"
#include "import-main-matcher.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_IMPORT;

typedef struct _ImportData ImportData;

static const AB_TRANSACTION *transaction_cb(
    const AB_TRANSACTION *element, gpointer user_data);
static AB_IMEXPORTER_ACCOUNTINFO *accountinfo_cb(
    AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data);
static AB_JOB_LIST2 *import_context(
    AB_BANKING *api, AB_IMEXPORTER_CONTEXT *context,
    GNCImportMainMatcher *importer_generic_gui, gboolean execute_transactions);

struct _ImportData {
    AB_BANKING *api;
    GNCImportMainMatcher *importer_generic;
    gboolean execute_transactions;
    AB_JOB_LIST2 *job_list;
    Account *gnc_acc;
    AB_ACCOUNT *ab_acc;
};

static const AB_TRANSACTION *
transaction_cb(const AB_TRANSACTION *element, gpointer user_data)
{
    ImportData *data = user_data;
    Transaction *gnc_trans;
    AB_JOB *job;

    g_return_val_if_fail(element && data, NULL);

    /* Create a GnuCash transaction from ab_trans */
    gnc_trans = gnc_ab_trans_to_gnc(element, data->gnc_acc);

    /* Instead of xaccTransCommitEdit(gnc_trans)  */
    gnc_gen_trans_list_add_trans(data->importer_generic, gnc_trans);

    if (data->ab_acc) {
        AB_TRANSACTION *ab_trans = AB_Transaction_dup(element);

        /* NEW: The imported transaction has been imported into gnucash.
         * Now also add it as a job to aqbanking */
        AB_Transaction_SetLocalBankCode(
            ab_trans, AB_Account_GetBankCode(data->ab_acc));
        AB_Transaction_SetLocalAccountNumber(
            ab_trans, AB_Account_GetAccountNumber(data->ab_acc));
        AB_Transaction_SetLocalCountry(ab_trans, "DE");

        job = gnc_ab_get_trans_job(data->ab_acc, ab_trans, SINGLE_DEBITNOTE);

        /* Check whether we really got a job */
        if (!job) {
            /* Oops, no job, probably not supported by bank */
            if (gnc_verify_dialog(
                    NULL, FALSE, "%s",
                    _("The backend found an error during the preparation "
                      "of the job. It is not possible to execute this job. \n"
                      "\n"
                      "Most probable the bank does not support your chosen "
                      "job or your Online Banking account does not have the permission "
                      "to execute this job. More error messages might be "
                      "visible on your console log.\n"
                      "\n"
                      "Do you want to enter the job again?"))) {
                gnc_error_dialog(NULL, "Sorry, not implemented yet.");
            }
            /* else */
        }
        AB_Job_List2_PushBack(data->job_list, job);

        AB_Transaction_free(ab_trans);
    }

    return NULL;
}

static AB_IMEXPORTER_ACCOUNTINFO *
accountinfo_cb(AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    Account *gnc_acc;
    ImportData *data = user_data;
    const gchar *bank_code =
        AB_ImExporterAccountInfo_GetBankCode(element);
    const gchar *account_number =
        AB_ImExporterAccountInfo_GetAccountNumber(element);
    const gchar *account_name =
        AB_ImExporterAccountInfo_GetAccountName(element);
    gchar *online_id;

    g_return_val_if_fail(element && data, NULL);

    online_id = g_strconcat(bank_code, account_number, (gchar*)NULL);
    gnc_acc = gnc_import_select_account(NULL, online_id, 1, account_name, NULL,
                                        ACCT_TYPE_NONE, NULL, NULL);
    g_free(online_id);

    if (gnc_acc) {
        /* Store chosen gnucash account in callback data */
        data->gnc_acc = gnc_acc;

        if (data->execute_transactions) {
            /* Retrieve the aqbanking account that belongs to this gnucash
             * account */
            data->ab_acc = gnc_ab_get_ab_account(data->api, gnc_acc);
            if (!data->ab_acc) {
                gnc_error_dialog(NULL, "%s",
                                 _("No Online Banking account found for this "
                                   "gnucash account. These transactions will "
                                   "not be executed by Online Banking."));
            }
        } else {
            data->ab_acc = NULL;
        }

        /* Iterate through all transactions.  */
        AB_ImExporterAccountInfo_TransactionsForEach(element, transaction_cb,
                                                     data);
    }
    return NULL;
}

static AB_JOB_LIST2 *
import_context(AB_BANKING *api, AB_IMEXPORTER_CONTEXT *context,
               GNCImportMainMatcher *importer_generic_gui,
               gboolean execute_transactions)
{
    ImportData data;

    g_return_val_if_fail(api && context && importer_generic_gui, NULL);
    data.api = api;
    data.importer_generic = importer_generic_gui;
    data.execute_transactions = execute_transactions;
    data.job_list = NULL;

    /* Iterate through all accounts */
    AB_ImExporterContext_AccountInfoForEach(context, accountinfo_cb, &data);

    return data.job_list;
}

void
gnc_file_aqbanking_import(const gchar *aqbanking_importername,
                          const gchar *aqbanking_profilename,
                          gboolean execute_transactions)
{
    gchar *default_dir;
    gchar *selected_filename = NULL;
    gint dtaus_fd = -1;
    AB_BANKING *api = NULL;
    gboolean online = FALSE;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER *importer;
    GWEN_DB_NODE *db_profiles = NULL;
    GWEN_DB_NODE *db_profile;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GWEN_IO_LAYER *io, *buffio;
    GNCImportMainMatcher *importer_generic_gui = NULL;
    AB_JOB_LIST2 *job_list = NULL;

    /* Select a file */
    default_dir = gnc_get_default_directory(GCONF_SECTION_AQBANKING);
    selected_filename = gnc_file_dialog(_("Select a file to import"),
                                        NULL, default_dir,
                                        GNC_FILE_DIALOG_IMPORT);
    g_free(default_dir);

    if (!selected_filename)
        goto cleanup;
    DEBUG("filename: %s", selected_filename);

    /* Remember the directory as the default */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GCONF_SECTION_AQBANKING, default_dir);
    g_free(default_dir);

    dtaus_fd = g_open(selected_filename, O_RDONLY, 0);
    if (dtaus_fd == -1) {
      DEBUG("Could not open file %s", selected_filename);
      goto cleanup;
    }

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api) {
        g_warning("gnc_file_aqbanking_import: Couldn't get AqBanking API");
        goto cleanup;
    }
    if (AB_Banking_OnlineInit(api) != 0) {
        g_warning("gnc_file_aqbanking_import: "
                  "Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get a GUI object */
    gui = gnc_GWEN_Gui_get(NULL);
    if (!gui) {
        g_warning("gnc_ab_getbalance: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    /* Get import module */
    importer = AB_Banking_GetImExporter(api, aqbanking_importername);
    if (!importer) {
        g_warning("Import module %s not found", aqbanking_importername);
        gnc_error_dialog(NULL, "%s",
                         _("Import module for DTAUS import not found."));
        goto cleanup;
    }

    /* Load the import profile */
    db_profiles = AB_Banking_GetImExporterProfiles(api, aqbanking_importername);

    /* Select profile */
    db_profile = GWEN_DB_GetFirstGroup(db_profiles);
    while (db_profile) {
        const gchar *name;

        name = GWEN_DB_GetCharValue(db_profile, "name", 0, 0);
        g_return_if_fail(name);
        if (g_ascii_strcasecmp(name, aqbanking_profilename)==0)
            break;
        db_profile = GWEN_DB_GetNextGroup(db_profile);
    }
    if (!db_profile) {
        g_warning("Profile \"%s\" for importer \"%s\" not found",
                  aqbanking_profilename, aqbanking_importername);
        /* For debugging: Print those available names that have been found */
        db_profile = GWEN_DB_GetFirstGroup(db_profiles);
        while (db_profile) {
            const char *name = GWEN_DB_GetCharValue(db_profile, "name", 0, 0);
            g_warning("Only found profile \"%s\"\n", name ? name : "(null)");
            db_profile = GWEN_DB_GetNextGroup(db_profile);
        }
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    /* Wrap file in buffered gwen io */
    io = GWEN_Io_LayerFile_new(dtaus_fd, -1);
    dtaus_fd = -1;
    buffio = GWEN_Io_LayerBuffered_new(io);
    if (GWEN_Io_Manager_RegisterLayer(buffio)) {
        g_warning("gnc_file_aqbanking_import: Failed to wrap file");
        goto cleanup;
    }

    /* Run the import */
    if (AB_ImExporter_Import(importer, context, buffio, db_profile, 0)) {
        g_warning("gnc_file_aqbanking_import: Error on import");
        goto cleanup;
    }

    /* Close the file */
    GWEN_Io_Layer_free(buffio);

    /* Create importer GUI */
    importer_generic_gui = gnc_gen_trans_list_new(NULL, NULL, TRUE, 14);

    /* Import the transactions from the context into gnucash */
    job_list = import_context(api, context, importer_generic_gui,
                              execute_transactions);

    if (execute_transactions) {
        if (gnc_gen_trans_list_run(importer_generic_gui)) {
            /* FIXME */
            /* gnc_hbci_multijob_execute(NULL, api, job_list, gui); */
        }
    }

cleanup:
    if (job_list)
        AB_Job_List2_free(job_list);
    if (importer_generic_gui)
        gnc_gen_trans_list_delete(importer_generic_gui);
    if (context)
        AB_ImExporterContext_free(context);
    if (db_profiles)
        GWEN_DB_Group_free(db_profiles);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (online)
        AB_Banking_OnlineFini(api);
    if (api)
        gnc_AB_BANKING_fini(api);
    if (dtaus_fd != -1)
        close(dtaus_fd);
    if (selected_filename)
        g_free(selected_filename);
}
