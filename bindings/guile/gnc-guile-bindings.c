/*********************************************************************
 * gnc-guile-bindings.c
 * library to enable guile bindings for libgnucash
 *
 * Copyright (c) 2019 GnuCash Development Team
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
#include <libguile.h>

void gnc_guile_bindings_init(void);

static int is_initialized = 0;

extern SCM scm_init_sw_core_utils_module (void);
extern SCM scm_init_sw_engine_module (void);

void
gnc_guile_bindings_init(void)
{
    if (!is_initialized)
    {
        /* Do what's necessary to initialize the bindings */
        scm_init_sw_core_utils_module();
        scm_init_sw_engine_module();

        is_initialized = 1;
    }
}
