#if 0

#include "qofid.h"

/* Return the type of an identifier.
 * Equivalent function prototype:
 * QofIdType xaccGUIDType (const GUID * guid, QofBook *book); 
 */

#define xaccGUIDType(guid,book)      \
    qof_guid_type ((guid), qof_book_get_entity_table (book))

/* Equivalent function prototype:
 * void xaccGUIDNew (GUID *guid, QofBook *book)
 */
#define xaccGUIDNew(guid,book)     \
      qof_entity_guid_new (qof_book_get_entity_table (book), (guid))
                                                                                

#endif
