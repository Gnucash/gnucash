/*
 * FILE:
 * gncQuery.x
 *
 * FUNCTION:
 * The RPC definition for a Gnucash Query
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#ifndef __GNC_GUID_X
#define __GNC_GUID_X

typedef opaque gncGUID[16];

struct gnc_guidlist {
  gncGUID *		guid;
  gnc_guidlist *	next;
};

struct gnc_vers_list {
  gncGUID *		guid;
  int			vers;
  gnc_vers_list *	next;
};

struct gncTimespec {
  int64_t	tv_sec;
  int		tv_nsec;
};

#endif /* __GNC_GUID_X */
