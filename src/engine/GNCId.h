
#include "qofid.h"

/* Return the type of an identifier.
 * Equivalent function prototype:
 * QofIdType xaccGUIDType (const GUID * guid, QofBook *book); 
 */

#define xaccGUIDType(guid,book)      \
    qof_entity_type (qof_book_get_entity_table (book), (guid))

/* Equivalent function prototype:
 * void xaccGUIDNew (GUID *guid, QofBook *book)
 */
#define xaccGUIDNew(guid,book)     \
      qof_entity_guid_new (qof_book_get_entity_table (book), (guid))
                                                                                


		#define xaccGUIDNULL guid_null
		#define xaccGUIDMalloc guid_malloc
		#define xaccGUIDFree guid_free

		#define GNCIdTypeConst QofIdTypeConst
		#define GNCIdType QofIdType
		#define GNCEntityTable QofEntityTable
		#define xaccGUIDTypeEntityTable qof_guid_type

		#define xaccEntityTableNew qof_entity_new
		#define xaccEntityTableDestroy qof_entity_destroy
		#define xaccGUIDNewEntityTable qof_entity_guid_new
		#define xaccLookupEntity qof_entity_lookup
		#define xaccStoreEntity qof_entity_store
		#define xaccRemoveEntity qof_entity_remove
		#define xaccForeachEntity qof_entity_foreach

		#define foreachObjectCB QofEntityForeachCB

