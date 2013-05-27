/*
 * gnc-hooks-scm.h -- scheme helpers for using Glib hook functions
 * Copyright (C) 2005 Derek Atkins <derek@ihtfp.com
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
 *
 */

#ifndef GNC_HOOKS_SCM_H
#define GNC_HOOKS_SCM_H

#include "gnc-hooks.h"
#include <libguile.h>

/**
 * add and remove Scheme-style danglers from a hook
 */
void gnc_hook_add_scm_dangler(const gchar *name, SCM proc);
void gnc_hook_del_scm_dangler(const gchar *name, SCM proc);

#endif /* GNC_HOOKS_SCM_H */
