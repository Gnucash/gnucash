/* 
 * gnc-help-utils.h
 *
 * Copyright (C) 2007 Andreas Koehler <andi5.py@gmx.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#ifndef __GNC_HELP_UTILS_H__
#define __GNC_HELP_UTILS_H__

#include "config.h"
#include <glibconfig.h>

#ifdef G_OS_WIN32

void
gnc_show_htmlhelp(const gchar *chmfile, const gchar *anchor);

#endif /* G_OS_WIN32 */

#endif /* __GNC_HELP_UTILS_H__ */
