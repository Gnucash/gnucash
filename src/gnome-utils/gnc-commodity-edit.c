/*
 * gnc-commodity-edit.c --  Commodity editor widget
 *
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
 */

/*
 * Commodity editor widget
 *
 * Authors: Dave Peticolas <dave@krondo.com>
 * 	    Derek Atkins <warlord@MIT.EDU>
 */

#include <config.h>

#include "dialog-commodity.h"
#include "gnc-commodity-edit.h"

const char * gnc_commodity_edit_get_string (gpointer ptr)
{
  gnc_commodity * comm = (gnc_commodity *)ptr;
  return gnc_commodity_get_printname(comm);
}

gpointer gnc_commodity_edit_new_select (gpointer ptr, GtkWidget *toplevel)
{
  gnc_commodity * comm = (gnc_commodity *)ptr;
  return gnc_ui_select_commodity_modal(comm, toplevel);
}

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
