
XXX TODO:
-- instance should somehow reference the qoftype, so that
   things like gnc_engine_generate_event wouldn't 
   need three args ... 
   (in fact, so that many things wouldn't need to pass type explicitly)

   entity node already stores type, so entity_node should be merged
   with instance (or be parent of instance ?!)

-- bus obj has per-type hash tables that are identical to the entity
   per-type hash tables, should be merged together.

-- bus object foreach should be made into a for-each by type
   (requires modes to book for each to be a type-foreach).
   (this would also add a per-type edit/commmit-edit, clone, dirty,
   etc. functions!) Yahoo!

-- billterm and taxtables are incompletely cloned, not sure 
   what to do with refcount, ask warlord, need to explore.

-- onwers incomplete cloned, neeed to handle remaining 'owners'

-- turn clone into a generic object callback, so that 
   the ObtainTwin could be qof_instance_obtain_twin,
   a generic function. (right now its copied everywhere)

-- The following busines objects have an id/name/desc/active
   this could be abstracted into an common object.
   vendor (customer)
   bill term (but bill terms doesn't have id or active)
   job 

-- gnVendor should be a base class to gncCustomer (they're
   identical, but customer adds more stuff).

   Enployee could be base class for vendor, its got some of the 
   things (name, addr, active, currency)

-- TaxTable and BillTerm have common parent/child code.
   this could be abstracted into common code.

=======
-- finish onwer cclone
-- vendor, invoice, entry, order


#include "gncBusiness.h"

/** Copy all customers from one book to another.  Do this
 *  by iterating over all existing customers in the src book,
 *  and creating a clone for the dest book. */

void
gncCustomerCopyAll (QofBook *dest_book, QofBook *src_book)
{
  
}

static void 
bill_term_copy_helper (gpointer object, gpointer user_data)
{
  QofBook *dest_book = user_data;
  gncBillTerm *src_term = object;
  gncCloneBillTerm (src_term, dest_book);
}

void
gncBillTermCopyAll (QofBook *dest_book, QofBook *src_book)
{
   qof_object_foreach (GNC_BILLTERM_MODULE_NAME, 
                   src_book, bill_term_copy_helper, dest_book);
}



partition (QofBook *dest_book, QofBook *src_book)
{

  /* Copy all bill terms first, since the CustomerCopy expects
   * these to be in place already. */
/* XXX not strictly needed, the customer can pull thier own .. ? */
  gncBillTermCopyAll (dest_book, src_book);
}


