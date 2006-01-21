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
#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "top-level.h"
#include "gfec.h"
#include "gnc-main.h"

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

/* Priority of paths: The default is set at build time.  It may be
   overridden by environment variables, which may, in turn, be
   overridden by command line options.  */
static char *config_path = GNC_CONFIGDIR;
static char *share_path = GNC_SHAREDIR;
static char *help_path = GNC_HELPDIR;

static void
envt_override()
{
    char *path;
    
    if ((path = getenv("GNC_CONFIG_PATH")))
        config_path = path;
    if ((path = getenv("GNC_SHARE_PATH")))
        share_path = path;
    if ((path = getenv("GNC_DOC_PATH")))
        help_path = path;
}

static int error_in_scm_eval = FALSE;

static void
error_handler(const char *msg)
{
    g_warning(msg);
    error_in_scm_eval = TRUE;
}

static gboolean
try_load(gchar *fn)
{    
    g_message("looking for %s", fn);
    if (g_file_test(fn, G_FILE_TEST_EXISTS)) {
        g_message("trying to load %s", fn);
        error_in_scm_eval = FALSE;
        gfec_eval_file(fn, error_handler);
        return !error_in_scm_eval;
    }
    return FALSE;
}

static gboolean
try_load_config_array(const gchar *fns[])
{
    gchar *filename;
    int i;

    for (i = 0; fns[i]; i++) {
        filename = gnc_build_dotgnucash_path(fns[i]);
        if (try_load(filename)) {
            g_free(filename);
            return TRUE;
        }
        g_free(filename);
    }
    return FALSE;
}
 
static void
load_system_config(void)
{
    static int is_system_config_loaded = FALSE;
    gchar *system_config;

    if (is_system_config_loaded) return;

    g_message("loading system configuration");
    system_config = g_build_filename(config_path, "config", NULL);
    is_system_config_loaded = try_load(system_config);
    g_free(system_config);
}

static void
load_user_config(void)
{
    /* Don't continue adding to this list. When 2.0 rolls around bump
       the 1.4 (unnumbered) files off the list. */
    static const gchar *user_config_files[] = {
        "config-2.0.user", "config-1.8.user", "config-1.6.user", 
        "config.user", NULL };
    static const gchar *auto_config_files[] = {
        "config-2.0.auto", "config-1.8.auto", "config-1.6.auto",
	"config.auto", NULL};
    static const gchar *saved_report_files[] = {
        "saved-reports-2.0", "saved-reports-1.8", NULL};
    static const gchar *stylesheet_files[] = { "stylesheets-2.0", NULL};
    static int is_user_config_loaded = FALSE;

    if (is_user_config_loaded)
        return;
    else is_user_config_loaded = TRUE;

    g_message("loading user configuration");
    try_load_config_array(user_config_files);
    g_message("loading auto configuration");
    try_load_config_array(auto_config_files);
    g_message("loading saved reports");
    try_load_config_array(saved_report_files);
    g_message("loading stylesheets");
    try_load_config_array(stylesheet_files);
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
    char *namespace_regexp = NULL;

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
        {"config-path", '\0', POPT_ARG_STRING, &config_path, 0,
         _("Set configuration path"), _("CONFIGPATH")},
        {"share-path", '\0', POPT_ARG_STRING, &share_path, 0,
         _("Set shared data file search path"), _("SHAREPATH")},
        {"doc-path", '\0', POPT_ARG_STRING, &help_path, 0,
         _("Set the search path for documentation files"), _("DOCPATH")},
        {"add-price-quotes", '\0', POPT_ARG_STRING, NULL, 0,
         _("Add price quotes to given FILE"), _("FILE")},
        {"namespace", '\0', POPT_ARG_STRING, &namespace_regexp, 0, 
         _("Regular expression determining which namespace commodities will be retrieved"), 
         _("REGEXP")},
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

    if (namespace_regexp)
        gnc_main_set_namespace_regexp(namespace_regexp);
    
    poptFreeContext(pc);
}

static void
shutdown(int status) 
{
    if (gnucash_ui_is_running()) {
        if (!gnucash_ui_is_terminating()) {
            if (gnc_file_query_save()) {
                gnc_hook_run(HOOK_UI_SHUTDOWN, NULL);
                gnc_gui_shutdown();
            }
        }
    } else {
        gnc_gui_destroy();
        gnc_hook_run(HOOK_SHUTDOWN, NULL);
        gnc_engine_shutdown();
        exit(status);
    }   
}


static void
inner_main (void *closure, int argc, char **argv)
{
    SCM main_mod;

    main_mod = scm_c_resolve_module("gnucash main");
    scm_set_current_module(main_mod);

    /* module initializations go here */
    gnc_module_load("gnucash/app-utils", 0);
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

    load_system_config();
    load_user_config();

    scm_c_eval_string("(gnc:main)");
    shutdown(0);
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
    envt_override();
    gnucash_command_line(argc, argv);
    gnc_print_unstable_message();

    scm_boot_guile(argc, argv, inner_main, 0);
    exit(0); /* never reached */
}
