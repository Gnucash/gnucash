/*********************************************************************
 * Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>         *
 *
 * gncmod-budget.c
 * module definition/initialization for budget
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
#include <stdio.h>
#include <glib.h>
#include <libguile.h>

#include "gnc-module.h"
#include "gnc-module-api.h"
#include "gnc-plugin-budget.h"

/* version of the gnc module system interface we require */
int libgncmod_budget_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_budget_gnc_module_current  = 0;
int libgncmod_budget_gnc_module_revision = 0;
int libgncmod_budget_gnc_module_age      = 0;

/* forward references */
char *libgncmod_budget_gnc_module_path(void);
char *libgncmod_budegt_gnc_module_description(void);
int libgncmod_budget_gnc_module_init(int refcount);
int libgncmod_budget_gnc_module_end(int refcount);

char * libgncmod_budget_gnc_module_path(void) {
  return g_strdup("gnucash/gnome/");
}

char * libgncmod_budget_gnc_module_description(void) {
  return g_strdup("Support for Budgets");
}

int libgncmod_budget_gnc_module_init(int refcount)
{
  /* load the engine (we depend on it) */
  if(!gnc_module_load("gnucash/engine", 0)) {
    return FALSE;
  }

  /* Add menu items with C callbacks */
  gnc_plugin_budget_create_plugin();

  return TRUE;
}

int libgncmod_budget_gnc_module_end(int refcount) {
  return TRUE;
}
