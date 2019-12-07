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
