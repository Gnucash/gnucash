/*
 * FILE:
 * gncKVP.x
 *
 * FUNCTION:
 * The RPC definition for a Gnucash kvp_frame
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

#ifndef __GNC_KVP_X
#define __GNC_KVP_X

#include "gncGUID.x"

#ifdef RPC_HDR
%#include "kvp_frame.h"
#endif

struct gncNumeric {
  int64_t	num;
  int64_t	denom;
};

struct gnc_kvp_valuelist {
  struct gnc_kvp_value *	val;
  gnc_kvp_valuelist *		next;
};

union gnc_kvp_value switch (enum_t type) { /* kvp_value_t */
 case KVP_TYPE_GINT64:
   int64_t			int64;
 case KVP_TYPE_DOUBLE:
   double			dbl;
 case KVP_TYPE_NUMERIC:
   gncNumeric			numeric;
 case KVP_TYPE_STRING:
   string			str<>;
 case KVP_TYPE_GUID:
   gncGUID			guid;
 case KVP_TYPE_BINARY:
   opaque			binary<>;
 case KVP_TYPE_GLIST:
   gnc_kvp_valuelist *		glist;
 case KVP_TYPE_FRAME:
   struct gnc_kvp_frame *	frame;
};

struct gnc_kvp {
  string		key<>;
  gnc_kvp_value *	value;
};

struct gnc_kvp_frame {
  gnc_kvp *		data;
  gnc_kvp_frame *	next;
};

#endif /* __GNC_KVP_X */
