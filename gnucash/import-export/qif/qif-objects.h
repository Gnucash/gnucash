/*
 * qif-objects.h -- QIF objects for the QIF importer
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

#ifndef QIF_OBJECTS_H
#define QIF_OBJECTS_H

typedef struct _QifObject *QifObject;
typedef struct _QifData *QifData;

struct _QifObject
{
    const char*	type;
    void		(*destroy)(QifObject);

    /* QIF Objects contain data beyond this point.. */
};

#define QIF_O_ACCOUNT	"qif-acct"
typedef struct _QifAccount *QifAccount;

#define QIF_O_CATEGORY	"qif-cat"
typedef struct _QifCategory *QifCategory;

#define QIF_O_CLASS	"qif-class"
typedef struct _QifClass *QifClass;

#define QIF_O_SECURITY	"qif-security"
typedef struct _QifSecurity *QifSecurity;

#define QIF_O_TXN	"qif-txn"
typedef struct _QifTxn *QifTxn;
typedef struct _QifSplit *QifSplit;
typedef struct _QifInvstTxn *QifInvstTxn;

void qif_object_init(void);

QifAccount find_or_make_acct(QifContext ctx, char *name, GList *types);
QifCategory find_or_make_cat(QifContext ctx, char *name);
QifClass find_or_make_class(QifContext ctx, char *name);

/* merge the object into the context.  Returns the object that's in
 * the context, which is either the supplied object or the
 * already-existing object.
 */
QifAccount qif_account_merge(QifContext ctx, QifAccount acct);
QifCategory qif_cat_merge(QifContext ctx, QifCategory cat);
QifClass qif_class_merge(QifContext ctx, QifClass qclass);
QifSecurity qif_security_merge(QifContext ctx, QifSecurity security);

#endif /* QIF_OBJECTS_H */
