/********************************************************************\
 * File: core-utils.c
 *
 * Copyright (C) 2001 Linux Developers Group
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
\********************************************************************/

#include <string.h>
#include <messages.h>

#include "i18n.h"

#include "gnc-gettext-util.h"

/* ============================================================== */

char *
gnc_gettext_helper(const char *string)
{
  return strdup(_(string));
}

/* ============================================================== */

void
gnc_setup_gettext(void)
{
#ifdef HAVE_GETTEXT
  bindtextdomain (TEXT_DOMAIN, LOCALE_DIR);
  textdomain (TEXT_DOMAIN);
#endif
}

/* ============================================================== */
