/***************************************************************************
 *            dialog-chart-export.c
 *
 *  Sun Feb 27 14:19:12 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
#include <time.h>
#include "qofsession.h"
#include "AccountP.h"
#include "Transaction.h"
#include "qofobject.h"
#include "dialog-chart-export.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "gnc-engine-util.h"
#include "global-options.h"
#include "gnc-trace.h"
static short int module = MOD_GUI;

#define EQUITY_ACCOUNT_NAME  _("Opening Balances")
#define OPENING_BALANCE_DESC _("Opening Balance")

static GtkWidget *chart_export;
static GtkWidget *chart_filechooserdialog;
void on_dateok_clicked (GtkButton *button, gpointer user_data);
void on_exportok_clicked (GtkButton *button, gpointer user_data);

typedef struct chart_data_s
{
	time_t chart_time_t;
	QofSession *chart_session;
	Account *equity_account;
}chart_data;

static void
chart_collection_cb(QofEntity *ent, gpointer user_data)
{
	chart_data *data;
	Account *acc;
	gboolean success;
	const GUID *guid;
	QofCollection *copy_coll;
	QofBook *book;

	g_return_if_fail(user_data != NULL);
	data = (chart_data*)user_data;
	acc = (Account*)ent;
	if(0 == safe_strcmp(EQUITY_ACCOUNT_NAME, xaccAccountGetName(acc)) 
		&& (xaccAccountGetType(acc) == EQUITY))
	{
		success = qof_entity_copy_to_session(data->chart_session, ent);
		if(!success) { return; }
		guid = qof_entity_get_guid(ent);
		book = qof_session_get_book(data->chart_session);
		copy_coll = qof_book_get_collection(book, GNC_ID_ACCOUNT);
		data->equity_account = (Account*)qof_collection_lookup_entity(copy_coll, guid);
		return;
	}
}

static void
chart_entity_cb(QofEntity *ent, gpointer user_data)
{
	chart_data *data;
	Account *acc_ent, *equity_account;
	Transaction *trans;
	Split *split;
	gnc_numeric balance;
	QofBook *book;
	QofCollection *coll;
	const GUID *guid;
	gboolean success;
	
	success = FALSE;
	g_return_if_fail(user_data != NULL);
	data = (chart_data*)user_data;
	guid = qof_entity_get_guid(ent);
	acc_ent = (Account*)ent;
	equity_account = data->equity_account;
	g_return_if_fail(equity_account != NULL);
	ENTER (" Acc=%p\tEquity=%p ", acc_ent, equity_account);
	balance = xaccAccountGetBalanceAsOfDate(acc_ent, data->chart_time_t);
	success = qof_entity_copy_to_session(data->chart_session, ent);
	if(!success) {
		PWARN("%s - %s",  "ERR_BACKEND_MISC" , "entity already exists");
	}
	book = qof_session_get_book(data->chart_session);
	coll = qof_book_get_collection(book, GNC_ID_ACCOUNT);
	acc_ent = (Account*)qof_collection_lookup_entity(coll, guid);
	if(xaccAccountGetCommodity(acc_ent) == NULL)
	{
		xaccAccountSetCommodity(acc_ent, gnc_default_currency());
	}
	/* can't use gnc_account_create_opening_balance directly - the partial 
	QofBook doesn't have an AccountGroup that is used to locate the Equity Account. */
	xaccAccountBeginEdit (acc_ent);
	xaccAccountBeginEdit (equity_account);
	trans = xaccMallocTransaction (book);
	xaccTransBeginEdit (trans);
	xaccTransSetCurrency (trans, xaccAccountGetCommodity (acc_ent));
	xaccTransSetDateSecs (trans, data->chart_time_t);
	xaccTransSetDescription (trans, OPENING_BALANCE_DESC);
	split = xaccMallocSplit (book);
	xaccTransAppendSplit (trans, split);
	xaccAccountInsertSplit (acc_ent, split);
	xaccSplitSetAmount (split, balance);
	xaccSplitSetValue (split, balance);
	balance = gnc_numeric_neg (balance);
	split = xaccMallocSplit (book);
	xaccTransAppendSplit (trans, split);
	xaccAccountInsertSplit (equity_account, split);
	xaccSplitSetAmount (split, balance);
	xaccSplitSetValue (split, balance);
	xaccTransCommitEdit (trans);
	xaccAccountCommitEdit (equity_account);
	xaccAccountCommitEdit (acc_ent);
}

static GtkWidget *
create_chartfilechooserdialog ( void )
{
	GtkWidget *dialog;
	GladeXML *xml;
	
	xml = gnc_glade_xml_new ("chart-export.glade", "chartfilechooserdialog");
	glade_xml_signal_connect(xml, "on_exportok_clicked",
		GTK_SIGNAL_FUNC (on_exportok_clicked));
	dialog = glade_xml_get_widget (xml, "chartfilechooserdialog");
	return dialog;
}


static GtkWidget *
create_chart_export ( void )
{
  GtkWidget *dialog;
  GladeXML *xml;

	xml = gnc_glade_xml_new ("chart-export.glade", "chart-export");

	glade_xml_signal_connect(xml, "on_dateok_clicked",
		GTK_SIGNAL_FUNC	(on_dateok_clicked));
	dialog = glade_xml_get_widget (xml, "chart-export");
	return dialog;	
}

void
gnc_main_window_chart_export(void)
{
	chart_export = create_chart_export ();
	gtk_widget_show (chart_export);
}

void
on_dateok_clicked (GtkButton *button, gpointer user_data)
{
	guint year, month, day;
	chart_data data;
	GtkCalendar *calendar;
	struct tm *chart_tm;

	calendar = (GtkCalendar*)gnc_glade_lookup_widget(chart_export, "chart-calendar");
	data.chart_time_t = time(NULL);
	chart_tm = gmtime(&data.chart_time_t);
	/* set today - calendar will omit any zero/NULL values */
	year = chart_tm->tm_year + 1900;
	month = chart_tm->tm_mon + 1;
	day = chart_tm->tm_yday + 1;
	gtk_calendar_get_date(calendar, &year, &month, &day);
	chart_tm->tm_year = year - 1900;
	chart_tm->tm_mon = month;
	chart_tm->tm_yday = day - 1;
	data.chart_time_t = mktime(chart_tm);
	gtk_widget_destroy(chart_export);
	chart_filechooserdialog = create_chartfilechooserdialog ();
	gtk_widget_show (chart_filechooserdialog);
}

void
on_exportok_clicked (GtkButton *button, gpointer user_data)
{
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;
	const char *filename;
	chart_data data;

	current_session = qof_session_get_current_session();
	book = qof_session_get_book(current_session);
	chart_session = qof_session_new();
	filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chart_filechooserdialog));
	qof_session_begin(chart_session, filename, TRUE, TRUE);
	data.chart_session = chart_session;
	data.equity_account = NULL;

	coll = qof_book_get_collection(book, GNC_ID_ACCOUNT);
	qof_collection_foreach(coll, chart_collection_cb, &data);
	if(data.equity_account == NULL)
	{
		data.equity_account = xaccMallocAccount (qof_session_get_book(chart_session));
		xaccAccountBeginEdit (data.equity_account);
		xaccAccountSetName (data.equity_account, EQUITY_ACCOUNT_NAME);
		xaccAccountSetType (data.equity_account, EQUITY);
		xaccAccountSetCommodity (data.equity_account, gnc_default_currency());
	}
	qof_object_foreach(GNC_ID_ACCOUNT, book, chart_entity_cb, &data);
	qof_session_save(chart_session, NULL);
	qof_session_end(chart_session);
	gtk_widget_destroy(chart_filechooserdialog);
	qof_session_set_current_session(current_session);
}
