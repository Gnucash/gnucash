
XXX TODO:
-- billterm and taxtermss are incompletely cloned, not sure 
   what to do with refcount, ask warlord, need to explore.

-- jobs incomplete cloned, neeed to handle owners

-- turn clone into a generic object callback, so that 
   the ObtainTwin could be qof_instance_obtain_twin,
   a generic function. (right now its copied everywhere)

-- most of the busienss objects have an id/name/desc 
   this could be abstracted into an common object.

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


