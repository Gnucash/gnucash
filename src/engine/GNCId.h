
#include "qof.h"

/* Equivalent function prototype:
 * void xaccGUIDNew (GncGUID *guid, QofBook *book)
 */
#define xaccGUIDNew(guid,book)     \
      qof_instance_guid_new (qof_book_get_entity_table (book), (guid))


#define xaccGUIDNULL guid_null
#define xaccGUIDMalloc guid_malloc
#define xaccGUIDFree guid_free

#define GNCIdTypeConst QofIdTypeConst
#define GNCIdType QofIdType
#define GNCEntityTable QofInstanceTable
#define xaccGUIDTypeEntityTable qof_guid_type

#define xaccEntityTableNew qof_instance_new
#define xaccEntityTableDestroy qof_instance_destroy
#define xaccGUIDNewEntityTable qof_instance_guid_new
#define xaccLookupEntity qof_instance_lookup
#define xaccStoreEntity qof_instance_store
#define xaccRemoveEntity qof_instance_remove
#define xaccForeachEntity qof_instance_foreach

#define foreachObjectCB QofInstanceForeachCB

