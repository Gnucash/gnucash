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
#include "qof.h"
#include "AccountP.h"
#include "Transaction.h"
#include "dialog-chart-export.h"
#include "gnc-ui-util.h"
#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-file.h"

#define EQUITY_ACCOUNT_NAME  _("Opening Balances")
#define OPENING_BALANCE_DESC _("Opening Balance")

static GtkWidget *chart_export;
void on_dateok_clicked (GtkButton *button, gpointer user_data);

typedef struct chart_data_s
{
	time_t chart_time_t;
	QofSession *chart_session;
	Account *equity_account;
	GList      *param_ref_list;
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
chart_reference_cb(QofEntity *ent, gpointer user_data)
{
	QofEntityReference *reference;
	QofParam     *ref_param;
	chart_data   *data;

	g_return_if_fail(user_data != NULL);
	data = (chart_data*)user_data;
	while(data->param_ref_list != NULL) {
		ref_param = data->param_ref_list->data;
		reference = qof_entity_get_reference_from(ent, ref_param);
		qof_session_update_reference_list(data->chart_session, reference);
		data->param_ref_list = data->param_ref_list->next;
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
	time_t trans_time;
	GList *ref;
	QofEntityReference *ent_ref;
	
	g_return_if_fail(user_data != NULL);
	data = (chart_data*)user_data;
	trans_time = data->chart_time_t;
	data->param_ref_list = NULL;
	guid = qof_entity_get_guid(ent);
	acc_ent = (Account*)ent;
	ref = NULL;
	equity_account = data->equity_account;
	g_return_if_fail(equity_account != NULL);
	balance = xaccAccountGetBalanceAsOfDate(acc_ent, data->chart_time_t);
	qof_entity_copy_to_session(data->chart_session, ent);
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
	xaccTransSetDateSecs (trans, trans_time);
	xaccTransSetDateEnteredSecs (trans, trans_time);
	xaccTransSetDescription (trans, OPENING_BALANCE_DESC);
	/* User account split */
	split = xaccMallocSplit (book);
	xaccTransAppendSplit (trans, split);
	xaccAccountInsertSplit (acc_ent, split);
	xaccSplitSetAmount (split, balance);
	xaccSplitSetValue (split, balance);
	ref = qof_class_get_referenceList(GNC_ID_SPLIT);
	while(ref != NULL) {
		ent_ref = qof_entity_get_reference_from((QofEntity*)split, ref->data);
		qof_session_update_reference_list(data->chart_session, ent_ref);
		ref = g_list_next(ref);
	}
	g_list_free(ref);
	balance = gnc_numeric_neg (balance);
	/* Equity account split */
	split = xaccMallocSplit (book);
	xaccTransAppendSplit (trans, split);
	xaccAccountInsertSplit (equity_account, split);
	xaccSplitSetAmount (split, balance);
	xaccSplitSetValue (split, balance);
	xaccTransCommitEdit (trans);
	xaccAccountCommitEdit (equity_account);
	xaccAccountCommitEdit (acc_ent);
	ref = qof_class_get_referenceList(GNC_ID_TRANS);
	while(ref != NULL) {
		ent_ref = qof_entity_get_reference_from((QofEntity*)trans, ref->data);
		qof_session_update_reference_list(data->chart_session, ent_ref);
		ref = g_list_next(ref);
	}
	g_list_free(ref);
	ref = qof_class_get_referenceList(GNC_ID_SPLIT);
	while(ref != NULL) {
		ent_ref = qof_entity_get_reference_from((QofEntity*)split, ref->data);
		qof_session_update_reference_list(data->chart_session, ent_ref);
		ref = g_list_next(ref);
	}
	g_list_free(ref);
}

static GtkWidget *
create_chart_export ( void )
{
  GtkWidget *dialog;
  GladeXML *xml;
	chart_data *data;

	xml = gnc_glade_xml_new ("chart-export.glade", "chart-export");
	data = g_new0(chart_data, 1);
	glade_xml_signal_connect_data(xml, "on_dateok_clicked",
		GTK_SIGNAL_FUNC (on_dateok_clicked), data);
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
	chart_data  *data;
	GtkCalendar *calendar;
	struct tm *chart_tm;
	GtkWindow   *parent;
	gchar *filename;
	QofSession *current_session, *chart_session;
	QofBook *book;
	QofCollection *coll;

	calendar = (GtkCalendar*)gnc_glade_lookup_widget(chart_export, "chart-calendar");
	parent = (GtkWindow*)gtk_widget_get_parent ((GtkWidget*)chart_export);
	data = (chart_data*)user_data;
	data->chart_time_t = time(NULL);
	chart_tm = gmtime(&data->chart_time_t);
	/* set today - calendar will omit any zero/NULL values */
	year = chart_tm->tm_year + 1900;
	month = chart_tm->tm_mon + 1;
	day = chart_tm->tm_mday;
	gtk_calendar_get_date(calendar, &year, &month, &day);
	if((year + 1900) != chart_tm->tm_year) { 
	chart_tm->tm_year = year - 1900;
	}
	if(month != chart_tm->tm_mon) { 
	chart_tm->tm_mon = month;
	}
	if(day != chart_tm->tm_yday) { 
		chart_tm->tm_mday = day; 
	}
	data->chart_time_t = mktime(chart_tm);
	gtk_widget_destroy(chart_export);
	current_session = qof_session_get_current_session();
	book = qof_session_get_book(current_session);
	filename = g_strdup("/tmp/qsf-chartofaccounts.xml");
	chart_session = qof_session_new();
	filename = gnc_file_dialog(_("Export Chart of Accounts to QSF XML"),
				   NULL, NULL, GNC_FILE_DIALOG_EXPORT);
	if (filename)
	{
		gnc_engine_suspend_events();
	qof_session_begin(chart_session, filename, TRUE, TRUE);
		data->chart_session = chart_session;
		data->equity_account = NULL;
	coll = qof_book_get_collection(book, GNC_ID_ACCOUNT);
		qof_collection_foreach(coll, chart_collection_cb, data);
		if(data->equity_account == NULL)
	{
			data->equity_account = xaccMallocAccount (qof_session_get_book(chart_session));
			xaccAccountBeginEdit (data->equity_account);
			xaccAccountSetName (data->equity_account, EQUITY_ACCOUNT_NAME);
			xaccAccountSetDescription(data->equity_account, EQUITY_ACCOUNT_NAME);
			xaccAccountSetType (data->equity_account, EQUITY);
			xaccAccountSetCommodity (data->equity_account, gnc_default_currency());
	}
		qof_object_foreach(GNC_ID_ACCOUNT, book, chart_entity_cb, data);
		data->param_ref_list = qof_class_get_referenceList(GNC_ID_TRANS);
		qof_object_foreach(GNC_ID_TRANS, book, chart_reference_cb, data);
		g_list_free(data->param_ref_list);
		data->param_ref_list = qof_class_get_referenceList(GNC_ID_SPLIT);
		qof_object_foreach(GNC_ID_SPLIT, book, chart_reference_cb, data);
		g_list_free(data->param_ref_list);
	qof_session_save(chart_session, NULL);
		show_session_error(qof_session_get_error(chart_session), filename);
		gnc_engine_resume_events();
	}
	qof_session_end(chart_session);
	g_free(data);
	qof_session_set_current_session(current_session);
}
