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

#include <stdlib.h>
#include <stdio.h>
#include <popt.h>
#include <libguile.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "glib.h"
#include "gnc-module.h"
#include "i18n.h"
#include "gnc-version.h"
#include "config.h"

static int gnucash_show_version;
static int is_development_version = TRUE;

void 
gnucash_command_line(int argc, char **argv)
{
    poptContext pc;
    int rc;

    struct poptOption options[] = {
        //POPT_AUTOHELP
        {"version", 'v', POPT_ARG_NONE, &gnucash_show_version, 1, 
         N_("Display GnuCash version"), NULL},
        POPT_TABLEEND
    };

    pc = poptGetContext(NULL, argc, (const char **)argv, options, 0);
    
    while ((rc = poptGetNextOpt(pc)) > 0);

    if (gnucash_show_version) {
        printf("GnuCash %s %s\n", VERSION, 
               is_development_version ? _("development version") : "");
#ifdef GNUCASH_SVN
        printf("built %s from svn r%s\n", GNUCASH_BUILD_DATE, GNUCASH_SVN_REV);
#endif 
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

    /* module initializations would go here */
    gnc_module_load("gnucash/app-utils", 0);
    gnc_module_load("gnucash/gw-engine", 0);

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

    scm_boot_guile(argc, argv, inner_main, 0);
    exit(0); /* never reached */
}
