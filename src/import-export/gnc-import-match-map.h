/*
 * gnc-import-match-map.h:
 *	an import mapper service that stores Account Maps for the
 *	generic importer.  This allows importers to map various
 *	"strings" to Gnucash accounts in a generic manner.
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef GNC_IMPORT_MATCH_MAP_H
#define GNC_IMPORT_MATCH_MAP_H

typedef struct _GncImportMatchMap GncImportMatchMap;

#include "Account.h"
#include "gnc-book.h"

/* Obtain an ImportMatchMap object from an Account or a Book */
GncImportMatchMap * gnc_imap_create_from_account (Account *acc);
GncImportMatchMap * gnc_imap_create_from_book (GNCBook *book);

/* Destroy an import map */
void gnc_imap_destroy (GncImportMatchMap *imap);

/* Clear an import map -- this removes ALL entries in the map */
void gnc_imap_clear (GncImportMatchMap *imap);

/* Look up an Account in the map */
Account * gnc_imap_find_account (GncImportMatchMap *imap, const char *category,
				 const char *key);

/* Store an Account in the map */
void gnc_imap_add_account (GncImportMatchMap *imap, const char *category,
			   const char *key, Account *acc);


/* some well-known categories
 *
 * NOTE: You DO NOT have to use these values in your importer -- these
 * are just "well known" values, not "mandatory" values.  You are free
 * to use these if they apply, map your own fields to these labels, or
 * create your own category strings.
 */
#define GNCIMPORT_DESC	"desc"
#define GNCIMPORT_MEMO	"memo"
#define GNCIMPORT_PAYEE	"payee"

#endif /* GNC_IMPORT_MATCH_MAP_H */
