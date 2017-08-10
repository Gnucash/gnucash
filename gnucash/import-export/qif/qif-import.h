/*
 * qif-import.h -- a QIF Import module
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

#ifndef QIF_IMPORT_H
#define QIF_IMPORT_H

#include <stdio.h>
#include "qof.h"

typedef enum
{
    QIF_TYPE_BANK = 1,
    QIF_TYPE_CASH,
    QIF_TYPE_CCARD,
    QIF_TYPE_INVST,
    QIF_TYPE_PORT,
    QIF_TYPE_OTH_A,
    QIF_TYPE_OTH_L,
    QIF_TYPE_CLASS,
    QIF_TYPE_CAT,
    QIF_TYPE_SECURITY,
    QIF_ACCOUNT,
    QIF_AUTOSWITCH,
    QIF_CLEAR_AUTOSWITCH
} QifType;

/* Make sure this patches */
#define QIF_TYPE_MAX QIF_CLEAR_AUTOSWITCH

typedef struct _QifHandler *QifHandler;
typedef struct _QifContext *QifContext;
typedef struct _QifLine *QifLine;

/* Qif Flags */
#define QIF_F_IGNORE_ACCOUNTS	(1 << 0)
#define QIF_F_TXN_NEEDS_ACCT	(1 << 1)
#define QIF_F_ITXN_NEEDS_ACCT	(1 << 2)

/* Qif Reconciled Flag */
typedef enum
{
    QIF_R_NO = 0,
    QIF_R_CLEARED,
    QIF_R_RECONCILED,
    QIF_R_BUDGETED,
} QifRecnFlag;

/* Qif Errors */

typedef enum
{
    QIF_E_OK = 0,
    QIF_E_INTERNAL,
    QIF_E_BADSTATE,
    QIF_E_BADARGS,
    QIF_E_NOFILE,
} QifError;


/* Qif (investment?) Actions */
typedef enum
{
    QIF_A_NONE = 0,
    QIF_A_BUY,
    QIF_A_BUYX,
    QIF_A_CGLONG,
    QIF_A_CGLONGX,
    QIF_A_CGMID,
    QIF_A_CGMIDX,
    QIF_A_CGSHORT,
    QIF_A_CGSHORTX,
    QIF_A_DIV,
    QIF_A_DIVX,
    QIF_A_EXERCISE,
    QIF_A_EXERCISEX,
    QIF_A_EXPIRE,
    QIF_A_GRANT,
    QIF_A_INTINC,
    QIF_A_INTINCX,
    QIF_A_MARGINT,
    QIF_A_MARGINTX,
    QIF_A_MISCEXP,
    QIF_A_MISCEXPX,
    QIF_A_MISCINC,
    QIF_A_MISCINCX,
    QIF_A_REINVDIV,
    QIF_A_REINVINT,
    QIF_A_REINVLG,
    QIF_A_REINVMD,
    QIF_A_REINVSG,
    QIF_A_REINVSH,
    QIF_A_REMINDER,
    QIF_A_RTRNCAP,
    QIF_A_RTRNCAPX,
    QIF_A_SELL,
    QIF_A_SELLX,
    QIF_A_SHRSIN,
    QIF_A_SHRSOUT,
    QIF_A_STKSPLIT,
    QIF_A_VEST,
    QIF_A_XIN,
    QIF_A_XOUT,
} QifAction;

/* Public API Functions */

/* Create a QIF Import Context */
QifContext qif_context_new(void);
void qif_context_destroy(QifContext ctx);

/* Open and read a QIF File.  You must pass in the parent
 * context; it will return the child (file) context
 */
QifContext qif_file_new(QifContext ctx, const char* filename);

/* Does a qif-file need a default QIF account? */
gboolean qif_file_needs_account(QifContext ctx);

/* Return the filename of the QIF file */
const char * qif_file_filename(QifContext ctx);

/* Provide a default QIF Account for the QIF File */
void qif_file_set_default_account(QifContext ctx, const char *acct_name);

/* Parse the QIF File */
QifError qif_file_parse(QifContext ctx, gpointer ui_arg);

/* Merge all the qif-files from the children and into the context */
void qif_parse_merge_files(QifContext ctx);

/* Obtain the list of USED QifAccounts and QifCategories.  Finds all
 * references from the transactions in the QifContext.  The returned
 * GList must be freed by the caller.
 */
GList *qif_context_get_accounts(QifContext ctx);
GList *qif_context_get_categories(QifContext ctx);

#endif /* QIF_IMPORT_H */
