/********************************************************************\
 * gncAddress.c -- an Address object                                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "gnc-engine-util.h"	/* safe_strcmp */
#include "qofquerycore.h"
#include "qofclass.h"
#include "guid.h"
#include "gnc-event-p.h"

#include "gncAddress.h"
#include "gncAddressP.h"

struct _gncAddress 
{
  QofBook *	book;
  QofEntity * parent;
  gboolean	dirty;
  char *	name;
  char *	addr1;
  char *	addr2;
  char *	addr3;
  char *	addr4;
  char *	phone;
  char *	fax;
  char *	email;
};

#define _GNC_MOD_NAME	GNC_ADDRESS_MODULE_NAME

#define CACHE_INSERT(str) g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str));
#define CACHE_REMOVE(str) g_cache_remove(gnc_engine_get_string_cache(), (str));

G_INLINE_FUNC void mark_address (GncAddress *address);
G_INLINE_FUNC void
mark_address (GncAddress *address)
{
  address->dirty = TRUE;

  gnc_engine_gen_event (address->parent, GNC_EVENT_MODIFY);
}

/* Create/Destroy functions */

GncAddress * 
gncAddressCreate (QofBook *book, QofEntity *prnt)
{
  GncAddress *addr;

  if (!book) return NULL;

  addr = g_new0 (GncAddress, 1);
  addr->book = book;
  addr->dirty = FALSE;
  addr->parent = prnt;

  addr->name = CACHE_INSERT ("");
  addr->addr1 = CACHE_INSERT ("");
  addr->addr2 = CACHE_INSERT ("");
  addr->addr3 = CACHE_INSERT ("");
  addr->addr4 = CACHE_INSERT ("");
  addr->phone = CACHE_INSERT ("");
  addr->fax = CACHE_INSERT ("");
  addr->email = CACHE_INSERT ("");

  return addr;
}

GncAddress * 
gncCloneAddress (GncAddress *from, QofEntity *new_parent, QofBook *book)
{
  GncAddress *addr;

  if (!book) return NULL;

  addr = g_new0 (GncAddress, 1);
  addr->book = book;
  addr->dirty = TRUE;
  addr->parent = new_parent;

  addr->name = CACHE_INSERT (from->name);
  addr->addr1 = CACHE_INSERT (from->addr1);
  addr->addr2 = CACHE_INSERT (from->addr2);
  addr->addr3 = CACHE_INSERT (from->addr3);
  addr->addr4 = CACHE_INSERT (from->addr4);
  addr->phone = CACHE_INSERT (from->phone);
  addr->fax = CACHE_INSERT (from->fax);
  addr->email = CACHE_INSERT (from->email);

  return addr;
}

void 
gncAddressDestroy (GncAddress *addr)
{
  if (!addr) return;

  CACHE_REMOVE (addr->name);
  CACHE_REMOVE (addr->addr1);
  CACHE_REMOVE (addr->addr2);
  CACHE_REMOVE (addr->addr3);
  CACHE_REMOVE (addr->addr4);
  CACHE_REMOVE (addr->phone);
  CACHE_REMOVE (addr->fax);
  CACHE_REMOVE (addr->email);

  g_free (addr);
}

/* Set functions */

#define SET_STR(member, str) { \
	char * tmp; \
	\
	if (member == str) return; \
	if (!safe_strcmp (member, str)) return; \
	tmp = CACHE_INSERT (str); \
	CACHE_REMOVE (member); \
	member = tmp; \
	}

void gncAddressSetName (GncAddress *addr, const char *name)
{
  if (!addr) return;
  if (!name) return;
  SET_STR(addr->name, name);
  mark_address (addr);
}

void gncAddressSetAddr1 (GncAddress *addr, const char *addr1)
{
  if (!addr) return;
  if (!addr1) return;
  SET_STR(addr->addr1, addr1);
  mark_address (addr);
}

void gncAddressSetAddr2 (GncAddress *addr, const char *addr2)
{
  if (!addr) return;
  if (!addr2) return;
  SET_STR(addr->addr2, addr2);
  mark_address (addr);
}

void gncAddressSetAddr3 (GncAddress *addr, const char *addr3)
{
  if (!addr) return;
  if (!addr3) return;
  SET_STR(addr->addr3, addr3);
  mark_address (addr);
}

void gncAddressSetAddr4 (GncAddress *addr, const char *addr4)
{
  if (!addr) return;
  if (!addr4) return;
  SET_STR(addr->addr4, addr4);
  mark_address (addr);
}

void gncAddressSetPhone (GncAddress *addr, const char *phone)
{
  if (!addr) return;
  if (!phone) return;
  SET_STR(addr->phone, phone);
  mark_address (addr);
}

void gncAddressSetFax (GncAddress *addr, const char *fax)
{
  if (!addr) return;
  if (!fax) return;
  SET_STR(addr->fax, fax);
  mark_address (addr);
}

void gncAddressSetEmail (GncAddress *addr, const char *email)
{
  if (!addr) return;
  if (!email) return;
  SET_STR(addr->email, email);
  mark_address (addr);
}

/* Get Functions */

const char * gncAddressGetName (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->name;
}

const char * gncAddressGetAddr1 (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->addr1;
}

const char * gncAddressGetAddr2 (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->addr2;
}

const char * gncAddressGetAddr3 (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->addr3;
}

const char * gncAddressGetAddr4 (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->addr4;
}

const char * gncAddressGetPhone (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->phone;
}

const char * gncAddressGetFax (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->fax;
}

const char * gncAddressGetEmail (const GncAddress *addr)
{
  if (!addr) return NULL;
  return addr->email;
}

gboolean gncAddressIsDirty (const GncAddress *addr)
{
  if (!addr) return FALSE;
  return addr->dirty;
}

void gncAddressClearDirty (GncAddress *addr)
{
  if (!addr) return;
  addr->dirty = FALSE;
}

int gncAddressCompare (const GncAddress *a, const GncAddress *b)
{
  if (!a && !b) return 0;
  if (!a && b) return 1;
  if (a && !b) return -1;

  return safe_strcmp (a->name, b->name);
}

gboolean gncAddressRegister (void)
{
  static QofParam params[] = {

    { ADDRESS_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncAddressGetName, NULL },
    { ADDRESS_PHONE, QOF_TYPE_STRING, (QofAccessFunc)gncAddressGetPhone, NULL },
    { ADDRESS_FAX, QOF_TYPE_STRING, (QofAccessFunc)gncAddressGetFax, NULL },
    { ADDRESS_EMAIL, QOF_TYPE_STRING, (QofAccessFunc)gncAddressGetEmail, NULL },
    { NULL },
  };

  qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncAddressCompare, params);

  return TRUE;
}
