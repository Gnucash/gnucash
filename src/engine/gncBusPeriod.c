
/*
XXX TODO:
-- contemplate a per-collection (per type) edit/commmit-edit,
   clone, dirty, etc. functions some more ...

-- turn clone into a generic object callback, so that
   the ObtainTwin could be qof_instance_obtain_twin,
   a generic function. (right now its copied everywhere)

-- contemplate copy-on-write, and the true need for cloning,
   and how to avoid excess cloning for the SQL backend.

-- billterm and taxtables are incompletely cloned, not sure
   what to do with refcount, ask warlord, need to explore.

-- The following business objects have an id/name/desc/active
   this could be abstracted into an common object.
   vendor (customer)
   bill term (but bill terms doesn't have id or active)
   job

-- gncVendor should be a base class to gncCustomer (they're
   identical, but customer adds more stuff).

   Employee could be base class for vendor, its got some of the
   things (name, addr, active, currency)

-- TaxTable and BillTerm have common parent/child code.
   this could be abstracted into common code.

=======
-- finish clone of invoice, entry,


-- jobs in the vendor job list that are no longer active should
   be kept back in old book, removed from new book??
   ditto jobs in the customer list ??

-- closed orders can be removed from new book ?
*/



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
    /* XXX not strictly needed, the customer can pull their own .. ? */
    gncBillTermCopyAll (dest_book, src_book);
}
