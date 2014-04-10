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
