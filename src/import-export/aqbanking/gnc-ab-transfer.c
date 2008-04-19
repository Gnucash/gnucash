/*
 * gnc-ab-transfer.c --
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
 * @file gnc-ab-utils.c
 * @brief AqBanking transfer functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2004 Bernd Wagner
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include <aqbanking/banking.h>

#include "gnc-ab-transfer.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-ab-trans-templ.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

void
gnc_ab_maketrans(GtkWidget *parent, Account *gnc_acc,
                 GncABTransType trans_type)
{
    AB_BANKING *api;
    gboolean online = FALSE;
    AB_ACCOUNT *ab_acc;
    ABTransDialog *dialog = NULL;
    GList *template_list = NULL;

    g_return_if_fail(parent && gnc_acc);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api) {
        g_warning("gnc_ab_maketrans: Couldn't get AqBanking API");
        return;
    }
    if (AB_Banking_OnlineInit(api) != 0) {
        g_warning("gnc_ab_maketrans: Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc) {
        g_warning("gnc_ab_gettrans: No AqBanking account found");
        goto cleanup;
    }

    /* Get list of template transactions */
    template_list = gnc_ab_trans_templ_list_new_from_kvp_list(
        gnc_ab_get_book_template_list(gnc_account_get_book(gnc_acc)));

    /* Create new ABTransDialog */

cleanup:
    /* if (context) */
    /*     AB_ImExporterContext_free(context); */
    /* if (gui) */
    /*     gnc_GWEN_Gui_release(gui); */
    /* if (job_list) */
    /*     AB_Job_List2_free(job_list); */
    /* if (job) */
    /*     AB_Job_free(job); */
    /* if (to_date) */
    /*     GWEN_Time_free(to_date); */
    /* if (from_date) */
    /*     GWEN_Time_free(from_date); */
    gnc_ab_trans_templ_list_free(template_list);
    if (online)
        AB_Banking_OnlineFini(api);
    gnc_AB_BANKING_fini(api);
}
