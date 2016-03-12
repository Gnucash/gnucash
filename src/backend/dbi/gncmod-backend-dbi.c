/*********************************************************************
 * gncmod-backend-dbi.c
 * module definition/initialization for the dbi backend module
 *
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
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


#include <stdio.h>
#include <gmodule.h>
/* #include <glib-gobject.h> */

#include "gnc-module.h"
#include "gnc-module-api.h"

/* version of the gnc module system interface we require */
int gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int gnc_module_current  = 0;
int gnc_module_revision = 0;
int gnc_module_age      = 0;


static GNCModule engine;

gchar *
gnc_module_path(void)
{
    return g_strdup( "gnucash/backend/dbi" );
}

gchar *
gnc_module_description(void)
{
    return g_strdup( "The DBI/SQL backend for GnuCash" );
}

int
gnc_module_init(int refcount)
{
    engine = gnc_module_load( "gnucash/engine", 0 );
    if ( !engine ) return FALSE;

    return TRUE;
}

int
gnc_module_end(int refcount)
{
    int unload = TRUE;

    if ( engine != NULL )
    {
        unload = gnc_module_unload(engine);
    }

    if ( refcount == 0 )
    {
        engine = NULL;
    }

    return unload;
}
