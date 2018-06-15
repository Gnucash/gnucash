/* qif-import-p.h -- a QIF Importer module (private headers)
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef QIF_IMPORT_P_H
#define QIF_IMPORT_P_H

#include "qif-import.h"
#include "qif-objects.h"
#include "qif-parse.h"
#include "qif-file.h"

#include <stdio.h>

struct _QifHandler
{
    void		(*init)(QifContext ctx);
    QifError	(*parse_record)(QifContext ctx, GList *record);
    QifError	(*end)(QifContext ctx);
};

struct _QifContext
{
    /* The parent context */
    QifContext	parent;

    /* file information */
    char *	filename;
    FILE *	fp;
    gint		lineno;

    /* This describes what we are parsing right now */
    QifType	parse_type;
    QifHandler	handler;
    gpointer	parse_state;

    /* A bunch of flags for the current handler */
    gint		parse_flags;
    gboolean	parsed;

    /* The current and "opening balance" account */
    QifAccount	current_acct;
    QifAccount	opening_bal_acct;

    /* HashTable of Maps of data objects */
    GHashTable *	object_maps;

    /* HashTable of Lists of data objects */
    GHashTable *	object_lists;

    /* List of files */
    GList *files;
};

/* Object Maps */
gint qif_object_map_count(QifContext ctx, const char *type);
void qif_object_map_foreach(QifContext ctx, const char *type,
                            GHFunc func, gpointer arg);
void qif_object_map_insert(QifContext ctx, const char *key, QifObject obj);
void qif_object_map_remove(QifContext ctx, const char *type, const char *key);
QifObject qif_object_map_lookup(QifContext ctx, const char *type, const char *key);
void qif_object_map_destroy(QifContext ctx);
/* GList _SHOULD_ be freed by the caller */
GList * qif_object_map_get(QifContext ctx, const char *type);

/* Object Lists */
void qif_object_list_reverse(QifContext ctx, const char *type);
gint qif_object_list_count(QifContext ctx, const char *type);
void qif_object_list_foreach(QifContext ctx, const char *type,
                             GFunc func, gpointer arg);
void qif_object_list_insert(QifContext ctx, QifObject obj);
void qif_object_list_remove(QifContext ctx, QifObject obj);
void qif_object_list_destroy(QifContext ctx);
/* GList should NOT be freed by the caller */
GList *qif_object_list_get(QifContext ctx, const char *type);

/* Set and clear flags in bit-flags */
#define qif_set_flag(i,f) (i |= f)
#define qif_clear_flag(i,f) (i &= ~f)

#endif /* QIF_IMPORT_P_H */
