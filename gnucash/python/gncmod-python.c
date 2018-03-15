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

#if PY_VERSION_HEX >= 0x03000000
extern PyObject* PyInit__sw_app_utils(void);
extern PyObject* PyInit__sw_core_utils(void);
#else
extern void init_sw_app_utils(void);
extern void init_sw_core_utils(void);
#endif

int
libgncmod_python_gnc_module_init(int refcount)
{
    /* There isn't yet a python module to init.
    PyObject *pName, *pModule;
    */
    FILE *fp;
    gchar *pkgdatadir, *init_filename;
#if PY_VERSION_HEX >= 0x03000000
    wchar_t* argv = NULL;
#else
    char* argv = "";
#endif
    Py_Initialize();
    PySys_SetArgv(0, &argv);
#if PY_VERSION_HEX >= 0x03000000
    PyInit__sw_app_utils();
    PyInit__sw_core_utils();
#else
    init_sw_app_utils();
    init_sw_core_utils();
#endif

    /* There isn't yet a python module to init.
    pName = PyString_FromString("path/to/init.py");
    pModule = PyImport_Import(pName);

    if (!pModule) {
        PyErr_Print();
        return FALSE;
    }

    Py_DECREF(pName);
    Py_DECREF(pModule);
    */

    pkgdatadir = gnc_path_get_pkgdatadir();
    init_filename = g_build_filename(pkgdatadir, "python/init.py", (char*)NULL);
    g_debug("Looking for python init script at %s", (init_filename ? init_filename : "<null>"));
    fp = fopen(init_filename, "r");
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

    return TRUE;
}

int
libgncmod_python_gnc_module_end(int refcount)
{
    Py_Finalize();
    return TRUE;
}
