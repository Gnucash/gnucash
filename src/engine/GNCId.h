
#include "qof.h"
#include <glib.h>

#ifndef GNC_ID_H
#define GNC_ID_H

/* Equivalent function prototype:
 * void xaccGUIDNew (GUID *guid, QofBook *book)
 */
#define xaccGUIDNew(guid,book)     \
      qof_instanceguid_new (qof_book_get_entity_table (book), (guid))
                                                                                

		#define xaccGUIDNULL guid_null
		#define xaccGUIDMalloc guid_malloc
		#define xaccGUIDFree guid_free

		typedef gchar* GNCIdTypeConst;
		typedef gchar* GNCIdType;
		#define GNCEntityTable QofEntityTable
		#define xaccGUIDTypeEntityTable qof_guid_type

		#define xaccEntityTableNew qof_instancenew
		#define xaccEntityTableDestroy qof_instancedestroy
		#define xaccGUIDNewEntityTable qof_instanceguid_new
		#define xaccLookupEntity qof_instancelookup
		#define xaccStoreEntity qof_instancestore
		#define xaccRemoveEntity qof_instanceremove
		#define xaccForeachEntity qof_instanceforeach

		#define foreachObjectCB QofEntityForeachCB

#endif
