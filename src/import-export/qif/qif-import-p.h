/* qif-import-p.h -- a QIF Importer module (private headers)
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef QIF_IMPORT_P_H
#define QIF_IMPORT_P_H

#include "qif-import.h"
#include "qif-objects.h"
#include "qif-parse.h"
#include "qif-file.h"

#include <stdio.h>

struct _QifHandler {
  void		(*init)(QifContext ctx);
  QifError	(*parse_record)(QifContext ctx, GList *record);
  QifError	(*end)(QifContext ctx);
};

struct _QifContext {
  /* The parent context */
  QifContext	parent;

  /* file information */
  FILE *	fp;
  gint		lineno;

  /* This describes what we are parsing right now */
  QifType	parse_type;
  QifHandler	handler;

  /* A bunch of flags for the current handler */
  gint		parse_flags;

  /* The current and last seen account */
  QifAccount	current_acct;
  QifAccount	last_seen_acct;

  /* Current parse state */
  QifObject	parse_state;

  /* HashTable of Lists of data objects */
  GHashTable *	object_lists;

  /* HashTable of Maps of data objects */
  GHashTable *	object_maps;
};

/* Object Maps */
void qif_object_map_foreach(QifContext ctx, const char *type,
			    GHFunc func, gpointer arg);
void qif_object_map_insert(QifContext ctx, const char *key, QifObject obj);
void qif_object_map_remove(QifContext ctx, const char *type, const char *key);
QifObject qif_object_map_lookup(QifContext ctx, const char *type, const char *key);
void qif_object_map_destroy(QifContext ctx);
/* GList _SHOULD_ be freed by the caller */
GList * qif_object_map_get(QifContext ctx, const char *type);

/* Object Lists */
void qif_object_list_foreach(QifContext ctx, const char *type,
			     GFunc func, gpointer arg);
void qif_object_list_insert(QifContext ctx, QifObject obj);
void qif_object_list_remove(QifContext ctx, QifObject obj);
void qif_object_list_destroy(QifContext ctx);
/* GList should NOT be freed by the caller */
GList *qif_object_list_get(QifContext ctx, const char *type);

#endif /* QIF_IMPORT_P_H */
