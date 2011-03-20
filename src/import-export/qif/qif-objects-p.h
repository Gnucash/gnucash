/*
 * qif-objects-p.h -- Private header: QIF objects for the QIF importer
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

#ifndef QIF_OBJECTS_P_H
#define QIF_OBJECTS_P_H

#include "qof.h"

#include "qif-import.h"
#include "qif-objects.h"

struct _QifAccount
{
    struct _QifObject obj;

    char *	name;
    char *	desc;

    char *	limitstr;
    gnc_numeric	limit;

    char *	budgetstr;
    gnc_numeric	budget;

    GList *	type_list;

};

struct _QifCategory
{
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

struct _QifClass
{
    struct _QifObject obj;

    char *	name;
    char *	desc;
    char *	taxdesig;

};

struct _QifSecurity
{
    struct _QifObject obj;

    char *	name;
    char *	symbol;
    char *	type;

};

struct _QifTxn
{
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

struct _QifSplit
{
    char *	memo;

    char *	amountstr;
    gnc_numeric	amount;
    gnc_numeric	value;

    char *	catstr;

    /* parsed category/account info */

    union
    {
        QifObject	obj;
        QifCategory	cat;
        QifAccount	acct;
    } cat;
    gboolean	cat_is_acct;
    QifClass	cat_class;

};

struct _QifInvstTxn
{
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

    union
    {
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
