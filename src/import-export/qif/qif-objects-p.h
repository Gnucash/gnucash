/*
 * qif-objects-p.h -- Private header: QIF objects for the QIF importer
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef QIF_OBJECTS_P_H
#define QIF_OBJECTS_P_H

#include "gnc-date.h"

#include "qif-import.h"
#include "qif-objects.h"
#include "gnc-numeric.h"

struct _QifAccount {
  struct _QifObject obj;

  char *	name;
  char *	desc;

  char *	limitstr;
  gnc_numeric	limit;

  char *	budgetstr;
  gnc_numeric	budget;

  GList *	type_list;

};

struct _QifCategory {
  struct _QifObject obj;

  char *	name;
  char *	desc;
  char *	taxclass;

  gboolean	taxable;
  gboolean	expense;
  gboolean	income;

  char *	budgetstr;
  gnc_numeric	budget;

};

struct _QifClass {
  struct _QifObject obj;

  char *	name;
  char *	desc;
  char *	taxdesig;

};

struct _QifSecurity {
  struct _QifObject obj;

  char *	name;
  char *	symbol;
  char *	type;

};

struct _QifTxn {
  struct _QifObject obj;

  QifType	txn_type;

  char *	datestr;
  Timespec	date;

  char *	payee;
  char *	address;
  char *	num;

  QifRecnFlag	cleared;

  /* Investment info */
  QifInvstTxn	invst_info;

  /* The default_split is the default (forward) part of the QIF transaction */
  QifSplit	default_split;

  /* The current_split (if any) defines the current "qif split" we are handling */
  QifSplit	current_split;

  /* The "from" account */
  QifAccount	from_acct;

  /* The list of splits for this txn */
  GList *	splits;

};

struct _QifSplit {
  char *	memo;

  char *	amountstr;
  gnc_numeric	amount;
  gnc_numeric	value;

  char *	catstr;

  /* parsed category/account info */

  union {
    QifObject	obj;
    QifCategory	cat;
    QifAccount	acct;
  } cat;
  gboolean	cat_is_acct;
  QifClass	cat_class;

};

struct _QifInvstTxn {
  QifAction	action;

  gnc_numeric	amount;
  gnc_numeric	d_amount;
  gnc_numeric	price;
  gnc_numeric	shares;
  gnc_numeric	commission;

  char *	amountstr;
  char *	d_amountstr;
  char *	pricestr;
  char *	sharesstr;
  char *	commissionstr;

  char *	security;
  char *	catstr;

  union {
    QifObject	obj;
    QifCategory	cat;
    QifAccount	acct;
  } far_cat;
  gboolean	far_cat_is_acct;
};

/* to be run after parsing all the dates and amounts */
void qif_txn_setup_splits(QifTxn txn);
void qif_invst_txn_setup_splits(QifContext ctx, QifTxn txn);

#endif /* QIF_OBJECTS_P_H */
