/*
 * gncAddress.h -- an Address object
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ADDRESS_H_
#define GNC_ADDRESS_H_

#include "gnc-book.h"

struct _gncAddress;
typedef struct _gncAddress GncAddress;

/* Create/Destroy functions */

GncAddress * gncAddressCreate (GNCBook *book);
void gncAddressDestroy (GncAddress *addr);

/* Set functions */

void gncAddressSetName (GncAddress *addr, const char *name);
void gncAddressSetAddr1 (GncAddress *addr, const char *addr1);
void gncAddressSetAddr2 (GncAddress *addr, const char *addr2);
void gncAddressSetAddr3 (GncAddress *addr, const char *addr3);
void gncAddressSetAddr4 (GncAddress *addr, const char *addr4);
void gncAddressSetPhone (GncAddress *addr, const char *phone);
void gncAddressSetFax (GncAddress *addr, const char *fax);
void gncAddressSetEmail (GncAddress *addr, const char *email);
void gncAddressClearDirty (GncAddress *address);

/* Get Functions */

const char * gncAddressGetName (const GncAddress *addr);
const char * gncAddressGetAddr1 (const GncAddress *addr);
const char * gncAddressGetAddr2 (const GncAddress *addr);
const char * gncAddressGetAddr3 (const GncAddress *addr);
const char * gncAddressGetAddr4 (const GncAddress *addr);
const char * gncAddressGetPhone (const GncAddress *addr);
const char * gncAddressGetFax (const GncAddress *addr);
const char * gncAddressGetEmail (const GncAddress *addr);
gboolean gncAddressIsDirty (const GncAddress *addr);

#endif /* GNC_ADDRESS_H_ */
