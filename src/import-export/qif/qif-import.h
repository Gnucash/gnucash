/*
 * qif-import.h -- a QIF Import module
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef QIF_IMPORT_H
#define QIF_IMPORT_H

#include <stdio.h>
#include "gnc-numeric.h"

typedef enum {
  QIF_TYPE_BANK,
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

/* Qif Reconciled Flag */
typedef enum {
  QIF_R_NO = 0,
  QIF_R_CLEARED,
  QIF_R_RECONCILED,
  QIF_R_BUDGETED,
} QifRecnFlag;

/* Qif Errors */

typedef enum {
  QIF_E_OK = 0,
  QIF_E_INTERNAL,
  QIF_E_BADSTATE,
} QifError;


/* Qif (investment?) Actions */
typedef enum {
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

QifContext qif_context_new(QifContext parent);
void qif_context_destroy(QifContext ctx);

/* Reads the file into the qif context */
QifError qif_read_file(QifContext ctx, FILE *f);

/* Parse all objects */
void qif_parse_all(QifContext ctx, gpointer arg);

#endif /* QIF_IMPORT_H */
