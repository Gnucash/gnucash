/*
 * gnucash-bin.c -- The program entry point for GnuCash
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <popt.h>
#include <libguile.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "glib.h"
#include "gnc-module.h"
#include "i18n.h"
#include "gnc-version.h"
#include "gnc-file.h"

static int gnucash_show_version;
/* GNUCASH_SVN is defined whenever we're building from an SVN tree */
#ifdef GNUCASH_SVN
static int is_development_version = TRUE;
#else
static int is_development_version = FALSE;
#endif

static void
gnc_print_unstable_message(void)
{
    if (!is_development_version) return;

    printf("\n\n%s%s%s%s%s\n%s%s\n\n",
           _("This is a development version. It may or may not work.\n"),
           _("Report bugs and other problems to gnucash-devel@gnucash.org.\n"),
           _("You can also lookup and file bug reports at http://bugzilla.gnome.org\n"),
           _("The last stable version was "), "GnuCash 1.8.12",
           _("The next stable version will be "), "GnuCash 2.0");
}


/* Note: Command-line argument parsing for Gtk+ applications has
 * evolved.  Gtk+-2.4 and before use the "popt" method.  We use that
 * here for compatibility.  Gnome-2.4 has a way of wrapping the "popt"
 * method (using GNOME_PARAM_POPT_CONTEXT).  Its advantages are that
 * it adds help messages for sound and the crash-dialog.  Its
 * disadvantages are that it prints a rather messy usage message
 * with lots of '?v's and it doesn't allow us to describe the
 * [DATAFILE] argument in the usage.  Weighing those factors, we're
 * just going to use popt directly.
 * 
 * Glib-2.6 introduced GOptionContext and GOptionGroup, which are
 * meant to replace popt usage.  In Gnome-2.14, the popt usage is
 * offically deprecated, and the GNOME_PARAM_GOPTION_CONTEXT can be
 * used.
 */

static void 
gnucash_command_line(int argc, char **argv)
{
    poptContext pc;
    char *p;
    int rc;

    struct poptOption options[] = {
        POPT_AUTOHELP
        {"version", 'v', POPT_ARG_NONE, &gnucash_show_version, 1, 
         _("Show GnuCash version"), NULL},
        {"debug", '\0', POPT_ARG_NONE, NULL, 0,
         _("Enable debugging mode"), NULL},
        {"devel", '\0', POPT_ARG_NONE, NULL, 0,
         _("Enable developers mode"), NULL},
        {"loglevel", '\0', POPT_ARG_INT, NULL, 0,
         _("Set the logging level from 0 (least) to 6 (most)"), 
         _("LOGLEVEL")},
        {"nofile", '\0', POPT_ARG_NONE, NULL, 0,
         _("Do not load the last file opened"), NULL},
        {"config-path", '\0', POPT_ARG_STRING, NULL, 0,
         _("Set configuration path"), _("CONFIGPATH")},
        {"share-path", '\0', POPT_ARG_STRING, NULL, 0,
         _("Set shared data file search path"), _("SHAREPATH")},
        {"doc-path", '\0', POPT_ARG_STRING, NULL, 0,
         _("Set the search path for documentation files"), _("DOCPATH")},
        {"add-price-quotes", '\0', POPT_ARG_STRING, NULL, 0,
         _("Add price quotes to given FILE"), _("FILE")},
        {"namespace", '\0', POPT_ARG_STRING, NULL, 0, 
         _("Regular expression determining which namespace commodities will be retrieved"), 
         _("REGEXP")},
        {"load-user-config", '\0', POPT_ARG_NONE, NULL, 0,
         _("Load the user configuration"), NULL},
        {"load-system-config", '\0', POPT_ARG_NONE, NULL, 0,
         _("Load the system configuration"), NULL},
        POPT_TABLEEND
    };
    
    /* Pretend that argv[0] is "gnucash" */
    if ((p = strstr(argv[0], "-bin"))) *p = '\0';

    pc = poptGetContext(NULL, argc, (const char **)argv, options, 0);
    poptSetOtherOptionHelp(pc, "[OPTIONS...] [datafile]");
    
    while ((rc = poptGetNextOpt(pc)) > 0);

    if (gnucash_show_version) {
        printf("GnuCash %s %s\n", VERSION, 
               is_development_version ? _("development version") : "");
        printf(_("built %s from r%s\n"), GNUCASH_BUILD_DATE, GNUCASH_SVN_REV);
        exit(0);
    }
    
    poptFreeContext(pc);
}

static void
inner_main (void *closure, int argc, char **argv)
{
    SCM main_mod;

    main_mod = scm_c_resolve_module("gnucash main");
    scm_set_current_module(main_mod);

    /* module initializations go here */
    gnc_module_load("gnucash/app-utils", 0);
    gnc_module_load("gnucash/gw-engine", 0);
    gnc_module_load("gnucash/engine", 0);
    gnc_module_load("gnucash/register/ledger-core", 0);
    gnc_module_load("gnucash/register/register-core", 0);
    gnc_module_load("gnucash/register/register-gnome", 0);
    gnc_module_load("gnucash/import-export/binary-import", 0);
    gnc_module_load("gnucash/import-export/qif-import", 0);
    gnc_module_load_optional("gnucash/import-export/ofx", 0);
    gnc_module_load_optional("gnucash/import-export/mt940", 0);
    gnc_module_load_optional("gnucash/import-export/log-replay", 0);
    gnc_module_load_optional("gnucash/import-export/hbci", 0);
    gnc_module_load("gnucash/report/report-system", 0);
    gnc_module_load("gnucash/report/stylesheets", 0);
    gnc_module_load("gnucash/report/standard-reports", 0);
    gnc_module_load("gnucash/report/utility-reports", 0);
    gnc_module_load("gnucash/report/locale-specific/us", 0);
    gnc_module_load("gnucash/report/report-gnome", 0);
    gnc_module_load_optional("gnucash/business-gnome", 0);

    scm_c_eval_string("(gnc:main)");
    return;
}

int main(int argc, char ** argv)
{

#ifdef HAVE_GETTEXT
    /* setlocale (LC_ALL, ""); is already called by gtk_set_locale()
       via gtk_init(). */
    bindtextdomain (TEXT_DOMAIN, LOCALE_DIR);
    textdomain (TEXT_DOMAIN);
    bind_textdomain_codeset (TEXT_DOMAIN, "UTF-8");
#endif

    gtk_init (&argc, &argv);
    gnc_module_system_init();
    gnucash_command_line(argc, argv);
    gnc_print_unstable_message();

    scm_boot_guile(argc, argv, inner_main, 0);
    exit(0); /* never reached */
}
