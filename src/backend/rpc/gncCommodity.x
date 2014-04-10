/*
 * FILE:
 * gncCommodity.x
 *
 * FUNCTION:
 * The RPC definition for a Gnucash Commodity
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef __GNC_COMMODITY_X
#define __GNC_COMMODITY_X

/* This is an actual commodity entry; unique_name is generated */
struct gncCommodity {
  string	fullname<>;
  string	namespace<>;
  string	mnemonic<>;
  string	printname<>;
  string	exchange_code<>;
  int		fraction;
};

/* List of commodities */
struct gnc_commoditylist {
  gncCommodity *	commodity;
  gnc_commoditylist *	next;
};

/* This is sufficient information to find a commodity in the commodity
 * table using gnc_commodity_table_lookup(), although while it does
 * save on network transfer space, it makes it harder on the sender
 * to actually package up an account.  So let's keep it here but not
 * use it.
 */
struct gncCommodityPtr {
  string	namespace<>;
  string	mnemonic<>;
};

#endif /* __GNC_COMMODITY_X */
