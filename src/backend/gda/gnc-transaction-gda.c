/********************************************************************
 * gnc-transaction-gda.c: load and save data to SQL via libgda      *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @file gnc-transaction-gda.c
 *  @brief load and save data to SQL 
 *  @author Copyright (c) 2006 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libgda
 */

#include "config.h"

#include <glib.h>
#include <libgda/libgda.h>

#include "qof.h"

#include "gnc-backend-gda.h"
#include "gnc-transaction-gda.h"

#include "gnc-engine.h"

#include "Account.h"
#include "Transaction.h"

static QofLogModule log_module = GNC_MOD_BACKEND;

#define TRANSACTION_TABLE "transactions"
#define SPLIT_TABLE "splits"

typedef struct {
	GncGdaBackend* be;
	const GUID* guid;
} split_info_t;

static gpointer get_guid( gpointer pObject );
static void set_guid( gpointer pObject, const gpointer pValue );
static gpointer get_tx_currency( gpointer pObject );
static void set_tx_currency( gpointer pObject, const gpointer pValue );
static gpointer get_tx_post_date( gpointer pObject );
static void set_tx_post_date( gpointer pObject, const gpointer pValue );
static gpointer get_tx_enter_date( gpointer pObject );
static void set_tx_enter_date( gpointer pObject, const gpointer pValue );

static col_cvt_t tx_col_table[] =
{
	{ "guid",			CT_GUID,	  0, COL_NNUL|COL_PKEY, NULL,
			get_guid, set_guid },
	{ "currency_guid",	CT_GUID,	  0, COL_NNUL,	NULL,
			get_tx_currency, set_tx_currency },
	{ "num",			CT_STRING,	 50, COL_NNUL, TRANS_NUM },
	{ "post_date",		CT_TIMESPEC,  0, COL_NNUL, NULL,
			get_tx_post_date, set_tx_post_date },
	{ "enter_date",		CT_TIMESPEC,  0, COL_NNUL, NULL,
			get_tx_enter_date, set_tx_enter_date },
	{ "description",	CT_STRING,	500, 0,	TRANS_DESCRIPTION },
	{ NULL }
};

static gpointer get_split_tx_guid( gpointer pObject );
static void set_split_tx_guid( gpointer pObject, const gpointer pValue );
static gpointer get_split_reconcile_state( gpointer pObject );
static void set_split_reconcile_state( gpointer pObject, const gpointer pValue );
static gpointer get_split_reconcile_date( gpointer pObject );
static void set_split_reconcile_date( gpointer pObject, const gpointer pValue );
static gpointer get_split_value( gpointer pObject );
static void set_split_value( gpointer pObject, const gpointer pValue );
static gpointer get_split_quantity( gpointer pObject );
static void set_split_quantity( gpointer pObject, const gpointer pValue );
static gpointer get_split_account( gpointer pObject );
static void set_split_account( gpointer pObject, const gpointer pValue );

static col_cvt_t split_col_table[] =
{
	{ "guid",			CT_GUID,	  0, COL_NNUL|COL_PKEY,	NULL,
			get_guid, set_guid },
	{ "tx_guid",		CT_GUID,	  0, COL_NNUL,	NULL,
			get_split_tx_guid, set_split_tx_guid },
	{ "memo",			CT_STRING,	 50, COL_NNUL,	SPLIT_MEMO },
	{ "action",			CT_STRING,	 50, COL_NNUL,	SPLIT_ACTION },
	{ "reconcile_state", CT_STRING,	  1, COL_NNUL,	NULL,
			get_split_reconcile_state, set_split_reconcile_state },
	{ "reconcile_date",	CT_TIMESPEC,  0, COL_NNUL,	NULL,
			get_split_reconcile_date, set_split_reconcile_date },
	{ "value",			CT_NUMERIC,	  0, COL_NNUL,	NULL,
			get_split_value, set_split_value },
	{ "quantity",		CT_NUMERIC,	  0, COL_NNUL,	NULL,
			get_split_quantity, set_split_quantity },
	{ "account_guid",	CT_GUID,	  0, COL_NNUL,	NULL,
			get_split_account, set_split_account },
	{ NULL }
};

static col_cvt_t guid_col_table[] =
{
	{ "tx_guid", CT_GUID, 0, 0, NULL, get_guid, set_guid },
	{ NULL }
};

/* ================================================================= */
static gpointer
get_guid( gpointer pObject )
{
	return (gpointer)qof_entity_get_guid( (QofEntity*)pObject );
}

static void 
set_guid( gpointer pObject, const gpointer pValue )
{
	QofEntity* pEntity = (QofEntity*)pObject;
	GUID* guid = (GUID*)pValue;

	qof_entity_set_guid( pEntity, guid );
}

static gpointer
get_tx_currency( gpointer pObject )
{
	Transaction* pTx = (Transaction*)pObject;

	return (gpointer)qof_instance_get_guid(
						(QofInstance*)xaccTransGetCurrency( pTx ) );
}

static void 
set_tx_currency( gpointer pObject, const gpointer pValue )
{
	Transaction* pTx = (Transaction*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pTx );
	gnc_commodity* pCurrency;
	GUID* guid = (GUID*)pValue;

	pCurrency = gnc_commodity_find_commodity_by_guid( guid, pBook );
	xaccTransSetCurrency( pTx, pCurrency );
}

static gpointer
get_tx_post_date( gpointer pObject )
{
	Transaction* pTx = (Transaction*)pObject;
	static Timespec ts;

	ts = xaccTransRetDatePostedTS( pTx );
	return (gpointer)&ts;
}

static void 
set_tx_post_date( gpointer pObject, const gpointer pValue )
{
	Transaction* pTx = (Transaction*)pObject;
	Timespec* pTS = (Timespec*)pValue;

	xaccTransSetDatePostedTS( pTx, pTS );
}

static gpointer
get_tx_enter_date( gpointer pObject )
{
	Transaction* pTx = (Transaction*)pObject;
	static Timespec ts;

	ts = xaccTransRetDateEnteredTS( pTx );
	return (gpointer)&ts;
}

static void 
set_tx_enter_date( gpointer pObject, const gpointer pValue )
{
	Transaction* pTx = (Transaction*)pObject;
	Timespec* pTS = (Timespec*)pValue;

	xaccTransSetDateEnteredTS( pTx, pTS );
}

static gpointer
get_split_tx_guid( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	Transaction* pTx = xaccSplitGetParent( pSplit );

	return (gpointer)qof_instance_get_guid( (QofInstance*)pTx );
}

static void 
set_split_tx_guid( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pSplit );
	GUID* guid = (GUID*)pValue;
	Transaction* pTx = xaccTransLookup( guid, pBook );

	xaccSplitSetParent( pSplit, pTx );
}

static gpointer
get_split_reconcile_state( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static gchar c[2];

	c[0] = xaccSplitGetReconcile( pSplit );
	c[1] = '\0';
	return (gpointer)c;
}

static void 
set_split_reconcile_state( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	const gchar* s = (const gchar*)pValue;

	xaccSplitSetReconcile( pSplit, s[0] );
}

static gpointer
get_split_reconcile_date( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static Timespec ts;

	ts = xaccSplitRetDateReconciledTS( pSplit );
	return (gpointer)&ts;
}

static void 
set_split_reconcile_date( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	Timespec* pTS = (Timespec*)pValue;

	xaccSplitSetDateReconciledTS( pSplit, pTS );
}

static gpointer
get_split_value( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static gnc_numeric v;

	v = xaccSplitGetValue( pSplit );
	return (gpointer)&v;
}

static void 
set_split_value( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	gnc_numeric* pV = (gnc_numeric*)pValue;

	xaccSplitSetValue( pSplit, *pV );
}

static gpointer
get_split_quantity( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	static gnc_numeric v;

	v = xaccSplitGetAmount( pSplit );
	return (gpointer)&v;
}

static void 
set_split_quantity( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	gnc_numeric* pV = (gnc_numeric*)pValue;

	xaccSplitSetAmount( pSplit, *pV );
}

static gpointer
get_split_account( gpointer pObject )
{
	Split* pSplit = (Split*)pObject;
	Account* pAccount = xaccSplitGetAccount( pSplit );

	return (gpointer)qof_instance_get_guid( (QofInstance*)pAccount );
}

static void 
set_split_account( gpointer pObject, const gpointer pValue )
{
	Split* pSplit = (Split*)pObject;
	QofBook* pBook = qof_instance_get_book( (QofInstance*)pSplit );
	GUID* guid = (GUID*)pValue;
	Account* pAccount = xaccAccountLookup( guid, pBook );

	xaccSplitSetAccount( pSplit, pAccount );
}

/* ================================================================= */
static void
create_transaction_tables( GncGdaBackend* be )
{
	gnc_gda_create_table_if_needed( be, TRANSACTION_TABLE, tx_col_table );
	gnc_gda_create_table_if_needed( be, SPLIT_TABLE, split_col_table );
}
/* ================================================================= */
static void
delete_splits( GncGdaBackend* be, Transaction* pTx )
{
	(void)gnc_gda_do_db_operation( be, OP_DB_DELETE, SPLIT_TABLE,
								SPLIT_TABLE, pTx, guid_col_table );
}

static void
save_split( gpointer data, gpointer user_data )
{
	split_info_t* split_info = (split_info_t*)user_data;
	Split* pSplit = (Split*)data;

	(void)gnc_gda_do_db_operation( split_info->be, OP_DB_ADD, SPLIT_TABLE,
									GNC_ID_SPLIT, pSplit, split_col_table );
}

static void
save_splits( GncGdaBackend* be, const GUID* tx_guid, SplitList* pSplitList )
{
	split_info_t split_info;

	split_info.be = be;
	split_info.guid = tx_guid;
	g_list_foreach( pSplitList, save_split, &split_info );
}

static void
commit_transaction( GncGdaBackend* be, QofInstance* inst )
{
	Transaction* pTx = (Transaction*)inst;

	(void)gnc_gda_do_db_operation( be,
							(inst->do_free ? OP_DB_DELETE : OP_DB_ADD_OR_UPDATE ),
							TRANSACTION_TABLE,
							GNC_ID_TRANS, pTx,
							tx_col_table );

	// Delete any old splits for this transaction
	delete_splits( be, pTx );

	// Now, commit any slots
	save_splits( be, qof_instance_get_guid( inst ),
				xaccTransGetSplitList( pTx ) );
}

/* ================================================================= */
void
gnc_gda_init_transaction_handler( void )
{
	static GncGdaDataType_t be_data =
	{
		GNC_GDA_BACKEND_VERSION,
		GNC_ID_TRANS,
		commit_transaction,			/* commit */
		NULL,						/* initial_load */
		create_transaction_tables	/* create tables */
	};

	qof_object_register_backend( GNC_ID_TRANS, GNC_GDA_BACKEND, &be_data );
}

/* ========================== END OF FILE ===================== */
