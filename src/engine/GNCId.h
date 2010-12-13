
#include "qof.h"

/* Equivalent function prototype:
 * void xaccGUIDNew (GncGUID *guid, QofBook *book)
 */
#define xaccGUIDNew(guid,book)     \
      qof_instance_guid_new (qof_book_get_entity_table (book), (guid))

