/*********************************************************************
 * gnc-locale-tax.c
 * hack to load the proper guile based tax system
 *
 * Copyright (c) 2019 Geert Janssens <geert@kobaltwit.be>
 *********************************************************************/
/********************************************************************\
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


#include <config.h>
#include <string.h>
#include <locale.h>
#include <libguile.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <Account.h>
#include <gnc-ui-util.h>
#include <guile-mappings.h>
#include <gnc-guile-utils.h>
#include "gnc-locale-tax.h"



void
gnc_locale_tax_init(void)
{
    /* This is a very simple hack that loads the (new, special) German
       tax definition file in a German locale, or (default) loads the
       US tax file. */
# ifdef G_OS_WIN32
    gchar *thislocale = g_win32_getlocale();
    gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
    g_free(thislocale);
# else /* !G_OS_WIN32 */
    const char *thislocale = setlocale(LC_ALL, NULL);
    gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
# endif /* G_OS_WIN32 */
    if (is_de_DE)
        scm_c_use_module("gnucash locale de_DE tax");
    else
        scm_c_use_module("gnucash locale us tax");
}

/* Caller is responsible for g_free'ing returned memory */
char *
gnc_ui_account_get_tax_info_string (const Account *account)
{
    static SCM get_form = SCM_UNDEFINED;
    static SCM get_desc = SCM_UNDEFINED;

    gboolean tax_related = FALSE;
    const char *code;

    if (!account)
        return NULL;

    tax_related = xaccAccountGetTaxRelated (account);
    code = xaccAccountGetTaxUSCode (account);

    if (!code)
    {
        if (!tax_related)
            return NULL;
        /* tax_related && !code */
        else
            /* Translators: This and the following strings appear on
               the account tab if the Tax Info column is displayed,
               i.e. if the user wants to record the tax form number
               and location on that tax form which corresponds to this
               gnucash account. For the US Income Tax support in
               gnucash, each tax code that can be assigned to an
               account generally corresponds to a specific line number
               on a paper form and each form has a unique
               identification (e.g., Form 1040, Schedule A). */
            return g_strdup (_("Tax-related but has no tax code"));
    }
    else  /* with tax code */
    {
        const gchar *tax_type;
        GNCAccountType atype;
        SCM tax_entity_type;
        SCM category;
        gchar *num_code = NULL;
        const gchar *prefix = "N";
        gchar *return_string = NULL;

        tax_type = gnc_get_current_book_tax_type ();
        if (tax_type == NULL || (g_strcmp0 (tax_type, "") == 0))
            return g_strdup (_("Tax entity type not specified"));

        atype = xaccAccountGetType (account);
        tax_entity_type = scm_from_utf8_string (tax_type);

        if (get_form == SCM_UNDEFINED)
        {
            /* load the tax info */
            gnc_locale_tax_init ();

            get_form = scm_c_eval_string
                       ("(false-if-exception gnc:txf-get-form)");
            get_desc = scm_c_eval_string
                       ("(false-if-exception gnc:txf-get-description)");
        }

        g_return_val_if_fail (scm_is_procedure (get_form), NULL);
        g_return_val_if_fail (scm_is_procedure (get_desc), NULL);

        category = scm_c_eval_string (atype == ACCT_TYPE_INCOME ?
                                      "txf-income-categories" :
                                      (atype == ACCT_TYPE_EXPENSE ?
                                       "txf-expense-categories" :
                                       (((atype == ACCT_TYPE_BANK)      ||
                                         (atype == ACCT_TYPE_CASH)      ||
                                         (atype == ACCT_TYPE_ASSET)     ||
                                         (atype == ACCT_TYPE_STOCK)     ||
                                         (atype == ACCT_TYPE_MUTUAL)    ||
                                         (atype == ACCT_TYPE_RECEIVABLE)) ?
                                        "txf-asset-categories" :
                                        (((atype == ACCT_TYPE_CREDIT)    ||
                                          (atype == ACCT_TYPE_LIABILITY) ||
                                          (atype == ACCT_TYPE_EQUITY)    ||
                                          (atype == ACCT_TYPE_PAYABLE)) ?
                                         "txf-liab-eq-categories" : ""))));

        if (g_str_has_prefix (code, prefix))
        {
            const gchar *num_code_tmp;
            num_code_tmp = g_strdup (code);
            num_code_tmp++; /* to lose the leading N */
            num_code = g_strdup (num_code_tmp);
            num_code_tmp--;
            g_free ((gpointer *) num_code_tmp);
        }
        else
        {
            num_code = g_strdup (code);
        }

        if (category == SCM_UNDEFINED)
        {
            if (tax_related)
                return_string = g_strdup_printf
                                (_("Tax type %s: invalid code %s for account type"),
                                 tax_type, num_code);
            else
                return_string = g_strdup_printf
                                (_("Not tax-related; tax type %s: invalid code %s for account type"),
                                 tax_type, num_code);
        }
        else
        {
            SCM code_scm;
            SCM form_scm;
            code_scm = scm_from_locale_symbol (code);
            form_scm = scm_call_3 (get_form, category, code_scm, tax_entity_type);
            if (!scm_is_string (form_scm))
            {
                if (tax_related)
                    return_string =  g_strdup_printf
                                     (_("Invalid code %s for tax type %s"),
                                      num_code, tax_type);
                else
                    return_string =  g_strdup_printf
                                     (_("Not tax-related; invalid code %s for tax type %s"),
                                      num_code, tax_type);
            }
            else
            {
                gchar *form = NULL;

                /* Note: using scm_to_utf8_stringn directly here instead
                   of our wrapper gnc_scm_to_utf8_string. 'form' should
                   be freed with 'free' instead of 'g_free'. This will
                   be taken care of automatically during scm_dynwind_end,
                   because we inform guile of this memory allocation via
                   scm_dynwind_free a little further. */
                form = scm_to_utf8_stringn (form_scm, NULL);
                if (!form)
                {
                    if (tax_related)
                        return_string = g_strdup_printf
                                        (_("No form: code %s, tax type %s"), num_code,
                                         tax_type);
                    else
                        return_string = g_strdup_printf
                                        (_("Not tax-related; no form: code %s, tax type %s"),
                                         num_code, tax_type);
                }
                else
                {
                    SCM desc_scm;

                    /* Create a dynwind context because we will be calling (scm) functions
                       that potentially exit non-locally */
                    scm_dynwind_begin (0);
                    scm_dynwind_free (form);
                    desc_scm = scm_call_3 (get_desc, category, code_scm,
                                           tax_entity_type);
                    if (!scm_is_string (desc_scm))
                    {
                        if (tax_related)
                            return_string = g_strdup_printf
                                            (_("No description: form %s, code %s, tax type %s"),
                                             form, num_code, tax_type);
                        else
                            return_string = g_strdup_printf
                                            (_("Not tax-related; no description: form %s, code %s, tax type %s"),
                                             form, num_code, tax_type);
                    }
                    else
                    {
                        gchar *desc = NULL;
                        desc = gnc_scm_to_utf8_string (desc_scm);
                        if (!desc)
                        {
                            if (tax_related)
                                return_string = g_strdup_printf
                                                (_("No description: form %s, code %s, tax type %s"),
                                                 form, num_code, tax_type);
                            else
                                return_string = g_strdup_printf
                                                (_("Not tax-related; no description: form %s, code %s, tax type %s"),
                                                 form, num_code, tax_type);
                        }
                        else
                        {
                            gint64 copy_number;
                            gchar *copy_txt = NULL;
                            copy_number = xaccAccountGetTaxUSCopyNumber (account);
                            copy_txt = (copy_number == 1) ?
                                       g_strdup ("") :
                                       g_strdup_printf ("(%d)",
                                                        (gint) copy_number);
                            if (tax_related)
                            {
                                if (g_strcmp0 (form, "") == 0)
                                    return_string = g_strdup_printf ("%s", desc);
                                else
                                    return_string = g_strdup_printf ("%s%s: %s",
                                                                     form, copy_txt, desc);
                            }
                            else
                            {
                                return_string = g_strdup_printf
                                                (_("Not tax-related; %s%s: %s (code %s, tax type %s)"),
                                                 form, copy_txt, desc, num_code, tax_type);
                            }
                            g_free (copy_txt);
                        }
                        g_free (desc);
                    }
                    scm_dynwind_end ();
                }
            }
        }
        g_free (num_code);
        return return_string;
    }
}

/* Caller is responsible for g_free'ing returned memory */
char *
gnc_ui_account_get_tax_info_sub_acct_string (const Account *account)
{
    GList *descendant, *account_descendants;

    if (!account)
        return NULL;

    account_descendants = gnc_account_get_descendants (account);
    if (account_descendants)
    {
        gint sub_acct_tax_number = 0;
        for (descendant = account_descendants; descendant;
                descendant = g_list_next(descendant))
        {
            if (xaccAccountGetTaxRelated (descendant->data))
                sub_acct_tax_number++;
        }
        g_list_free (account_descendants);
        g_list_free (descendant);
        /* Translators: This and the following strings appear on
           the account tab if the Tax Info column is displayed,
           i.e. if the user wants to record the tax form number
           and location on that tax form which corresponds to this
           gnucash account. For the US Income Tax support in
           gnucash, each tax code that can be assigned to an
           account generally corresponds to a specific line number
           on a paper form and each form has a unique
           identification (e.g., Form 1040, Schedule A). */
        return (sub_acct_tax_number == 0) ? NULL :
               g_strdup_printf (_("(Tax-related subaccounts: %d)"),
                                sub_acct_tax_number);
    }
    else
        return NULL;
}
