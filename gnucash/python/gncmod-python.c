/*********************************************************************
 * gncmod-python.c
 * Python in GnuCash?! Sweet.
 *
 * Copyright (c) 2011 Andy Clayton
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


#include <Python.h>
#include <config.h>
#include <gmodule.h>
#include <stdio.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-path.h"

GNC_MODULE_API_DECL(libgncmod_python)

/* version of the gnc module system interface we require */
int libgncmod_python_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_python_gnc_module_current  = 0;
int libgncmod_python_gnc_module_revision = 0;
int libgncmod_python_gnc_module_age      = 0;


char *
libgncmod_python_gnc_module_path(void)
{
    return g_strdup("gnucash/python");
}

char *
libgncmod_python_gnc_module_description(void)
{
    return g_strdup("An embedded Python interpreter");
}

#if PY_VERSION_HEX >= 0x030b0000
// PySys_SetArgv is deprecated in 3.11
#pragma GCC diagnostic warning "-Wdeprecated-declarations"
#endif

int
libgncmod_python_gnc_module_init(int refcount)
{
#ifdef __WIN32
    wchar_t* argv = NULL;
#else
    char* argv = NULL;
#endif
    PyStatus status;
    PyConfig config;
    PyConfig_InitPythonConfig(&config);
    status = PyConfig_SetBytesArgv(&config, 0, &argv);
    if (PyStatus_Exception(status))
    {
        PyConfig_Clear(&config);
        return FALSE;
    }
    Py_Initialize();
    gchar *pkgdatadir = gnc_path_get_pkgdatadir();
    gchar *init_filename = g_build_filename(pkgdatadir, "python/init.py", (char*)NULL);
    g_debug("Looking for python init script at %s", init_filename);
#ifdef __WIN32
    FILE *fp = fopen(init_filename, "rb");
#else
    FILE *fp = fopen(init_filename, "r");
#endif
    if (fp)
    {
        PyRun_SimpleFile(fp, init_filename);
        fclose(fp);

        /* PyRun_InteractiveLoop(stdin, "foo"); */
    }
    else
    {
        g_warning("Unable to initialize Python module (unable to open %s)", init_filename);
    }
    g_free(init_filename);
    g_free(pkgdatadir);
    PyConfig_Clear(&config);

    return TRUE;
}

int
libgncmod_python_gnc_module_end(int refcount)
{
    Py_Finalize();
    return TRUE;
}
