/*********************************************************************
 * gncmod-tax-us.c
 * module definition/initialization for us tax info
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"
#ifdef LOCALE_SPECIFIC_TAX
#include <string.h>
#include <locale.h>
#endif // LOCALE_SPECIFIC_TAX
#include <gmodule.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

GNC_MODULE_API_DECL(libgncmod_tax_us)

/* version of the gnc module system interface we require */
int libgncmod_tax_us_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_tax_us_gnc_module_current  = 0;
int libgncmod_tax_us_gnc_module_revision = 0;
int libgncmod_tax_us_gnc_module_age      = 0;


char *
libgncmod_tax_us_gnc_module_path(void)
{
#ifdef LOCALE_SPECIFIC_TAX
# ifdef G_OS_WIN32
    gchar *thislocale = g_win32_getlocale();
    gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
    g_free(thislocale);
# else /* !G_OS_WIN32 */
    const char *thislocale = setlocale(LC_ALL, NULL);
    gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
# endif /* G_OS_WIN32 */
    if (is_de_DE)
        return g_strdup("gnucash/tax/de_DE");
    else
        return g_strdup("gnucash/tax/us");
#endif /* LOCALE_SPECIFIC_TAX */
    return g_strdup("gnucash/tax/us");
}

char *
libgncmod_tax_us_gnc_module_description(void)
{
    return g_strdup("US income tax information");
}

static void
lmod(char * mn)
{
    char * form = g_strdup_printf("(use-modules %s)\n", mn);
    scm_c_eval_string(form);
    g_free(form);
}

int
libgncmod_tax_us_gnc_module_init(int refcount)
{
    /* This is a very simple hack that loads the (new, special) German
       tax definition file in a German locale, or (default) loads the
       previous US tax file. */
#ifdef LOCALE_SPECIFIC_TAX
# ifdef G_OS_WIN32
    gchar *thislocale = g_win32_getlocale();
    gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
    g_free(thislocale);
# else /* !G_OS_WIN32 */
    const char *thislocale = setlocale(LC_ALL, NULL);
    gboolean is_de_DE = (strncmp(thislocale, "de_DE", 5) == 0);
# endif /* G_OS_WIN32 */
    if (is_de_DE)
        lmod("(gnucash tax de_DE)");
    else
#endif /* LOCALE_SPECIFIC_TAX */
        lmod("(gnucash tax us)");
    return TRUE;
}

int
libgncmod_tax_us_gnc_module_end(int refcount)
{
    return TRUE;
}
