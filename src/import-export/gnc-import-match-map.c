/**@file
 * \brief Generic import mapper service, maps strings->accounts
 *
 An import mapper service that stores Account Maps for the
 generic importer.  This allows importers to map various
 "strings" to Gnucash accounts in a generic manner.
 \author Derek Atkins <derek@ihtfp.com>
 */
#include "gnc-import-match-map.h"
#include "kvp_frame.h"

struct _GncImportMatchMap {
  kvp_frame *	frame;
  Account *	acc;
  GNCBook *	book;
};

#define IMAP_FRAME	"import-map"

static GncImportMatchMap *
gnc_imap_create_from_frame (kvp_frame *frame, Account *acc, GNCBook *book)
{
  GncImportMatchMap *imap;

  g_return_val_if_fail (frame != NULL, NULL);
  g_return_val_if_fail ((acc && !book) || (!acc && book), NULL);

  imap = g_new0(GncImportMatchMap, 1);
  imap->frame = frame;

  /* Cache the book for easy lookups; store the account/book for
   * marking dirtiness
   */
  if (acc)
    book = xaccAccountGetBook (acc);
  imap->acc = acc;
  imap->book = book;
  
  return imap;
}

/* Obtain an ImportMatchMap object from an Account or a Book */
GncImportMatchMap * gnc_imap_create_from_account (Account *acc)
{
  kvp_frame * frame;

  if (!acc) return NULL;
  frame = xaccAccountGetSlots (acc);
  g_return_val_if_fail (frame != NULL, NULL);

  return gnc_imap_create_from_frame (frame, acc, NULL);
}

GncImportMatchMap * gnc_imap_create_from_book (GNCBook *book)
{
  kvp_frame * frame;

  if (!book) return NULL;
  frame = gnc_book_get_slots (book);
  g_return_val_if_fail (frame != NULL, NULL);

  return gnc_imap_create_from_frame (frame, NULL, book);
}

/* Destroy an import map */
void gnc_imap_destroy (GncImportMatchMap *imap)
{
  if (!imap) return;
  g_free (imap);
}

/* Clear an import map -- this removes ALL entries in the map */
void gnc_imap_clear (GncImportMatchMap *imap)
{
  if (!imap) return;

  /* Clear the IMAP_FRAME kvp */
  kvp_frame_set_slot_path (imap->frame, NULL, IMAP_FRAME);

  /* XXX: mark the account (or book) as dirty! */
}

/* Look up an Account in the map */
Account * gnc_imap_find_account (GncImportMatchMap *imap, const char *category,
				 const char *key)
{
  kvp_value *value;
  GUID * guid;

  if (!imap || !key) return NULL;
  if (!category) {
    category = key;
    key = NULL;
  }

  value = kvp_frame_get_slot_path (imap->frame, IMAP_FRAME, category, key, NULL);
  if (!value) return NULL;

  guid = kvp_value_get_guid (value);
  return xaccAccountLookup (guid, imap->book);
}

/* Store an Account in the map */
void gnc_imap_add_account (GncImportMatchMap *imap, const char *category,
			   const char *key, Account *acc)
{
  kvp_value *value;

  if (!imap || !key || !acc) return;
  if (!category) {
    category = key;
    key = NULL;
  }

  value = kvp_value_new_guid (xaccAccountGetGUID (acc));
  g_return_if_fail (value != NULL);

  kvp_frame_set_slot_path (imap->frame, value, IMAP_FRAME, category, key, NULL);
  kvp_value_delete (value);

  /* XXX Mark the account (or book) as dirty! */
}
