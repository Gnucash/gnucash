

XXX how to deal with customer tax table, 
also jobs

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
  gncBillTermCopyAll (dest_book, src_book);
}


