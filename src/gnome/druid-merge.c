/********************************************************************\
 * druid-merge.c  --  account hierarchy merge functionality         *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2004 Neil Williams <linux@codehelp.co.uk>          *
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

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "dialog-utils.h"
#include "druid-merge.h"
#include "druid-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gui-query.h"
#include "qof.h"
#include "druid-hierarchy.h"
#include "gnc-ui-util.h"
#include "Account.h"

GtkWidget               *druid_hierarchy_window = NULL;
static GtkWidget        *qsf_import_merge_window = NULL;
QofSession              *previous_session = NULL;
gint                    count = 0;
QofBookMergeData        *mergeData = NULL;
QofSession              *merge_session = NULL;
QofBook                 *mergeBook = NULL;
QofBook                 *targetBook = NULL;

static QofLogModule log_module = GNC_QSF_IMPORT;

void collision_rule_loop (QofBookMergeData*, QofBookMergeRule*, guint );

static GtkWidget*
merge_get_widget (const char *name)
{
	if (!qsf_import_merge_window) return NULL;

	return gnc_glade_lookup_widget (qsf_import_merge_window, name);
}

static void
delete_merge_window (void)
{
	if (!qsf_import_merge_window) return;
	
	gtk_widget_destroy (qsf_import_merge_window);
	qsf_import_merge_window = NULL;
}

static void
qof_book_merge_destroy_cb (GtkObject *obj, gpointer user_data)
{
}

static gboolean
on_import_start_page_next(GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       gpointer         user_data)
{

	return FALSE;
}


static void
on_MergeUpdate_clicked 	(GtkButton       *button,
              		    gpointer         user_data)
{
	GtkLabel *output;

	g_return_if_fail(mergeData != NULL);
	ENTER (" ");
	mergeData = qof_book_merge_update_result(mergeData, MERGE_UPDATE); 
	count = 0;
	qof_book_merge_rule_foreach(mergeData, collision_rule_loop, MERGE_REPORT);
	if(count == 0)
	{
		output = GTK_LABEL(merge_get_widget("OutPut"));
		gtk_label_set_text(output,_("No conflicts to be resolved."));
		gtk_widget_show(GTK_WIDGET(output));
	}
	LEAVE (" ");
}

static void
on_MergeDuplicate_clicked 	(GtkButton       *button,
              			    gpointer         user_data)
{
	QofBookMergeRule *currentRule;
	GtkLabel *output;
	
	g_return_if_fail(mergeData != NULL);
	ENTER (" ");
	currentRule = mergeData->currentRule;
	if(currentRule->mergeAbsolute == FALSE) { 
		mergeData = qof_book_merge_update_result(mergeData, MERGE_DUPLICATE); 
		count = 0;
	}
	if(currentRule->mergeAbsolute == TRUE) { 
		mergeData = qof_book_merge_update_result(mergeData, MERGE_ABSOLUTE); 
		count = 0;
	}
 	qof_book_merge_rule_foreach(mergeData, collision_rule_loop, MERGE_REPORT);
	if(count == 0)
	{
    		output = GTK_LABEL(merge_get_widget("OutPut"));
	    	gtk_label_set_text(output,_("No conflicts to be resolved."));
		gtk_widget_show(GTK_WIDGET(output));
	}
	LEAVE (" ");
}

static void
on_MergeNew_clicked (GtkButton       *button,
              		gpointer         user_data)
{
	QofBookMergeRule *currentRule;
	GtkLabel *output;

	g_return_if_fail(mergeData != NULL);
	currentRule = mergeData->currentRule;
	g_return_if_fail(currentRule != NULL);
	ENTER (" ");
	if(currentRule->mergeAbsolute == FALSE) { 
		mergeData = qof_book_merge_update_result(mergeData, MERGE_NEW);
	}
	count = 0;
 	qof_book_merge_rule_foreach(mergeData, collision_rule_loop, MERGE_REPORT);
	if(count == 0)
	{
		output = GTK_LABEL(merge_get_widget("OutPut"));
		gtk_label_set_text(output,_("No conflicts to be resolved."));
		gtk_widget_show(GTK_WIDGET(output));
	}
	LEAVE (" ");
}

static gboolean
on_import_next (GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       gpointer         user_data)
{
	GtkWidget *top;
	gchar *message;

	if(count > 0)
	{
		message = g_strdup_printf(_("Error: Please resolve all %d "
			"conflicts before trying to commit the data."), count);
		top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
		gnc_error_dialog(top, "%s", message);
		g_free(message);
		return TRUE;
	}
	return FALSE;
}

static void
on_merge_cancel (GnomeDruid *gnomedruid, gpointer user_data)
{
	g_return_if_fail(mergeBook != NULL);
	gnc_suspend_gui_refresh ();
	delete_merge_window();
	qof_book_destroy(mergeBook);
	qof_session_end(merge_session);	
	gnc_resume_gui_refresh ();
}

void currency_transfer_cb ( QofInstance* ent, gpointer user_data)
{
	if(!ent) return;
	if(xaccAccountGetCommodity((Account*)ent) == NULL) {
		xaccAccountSetCommodity((Account*)ent, gnc_default_currency());
	}
}

/* If the account has no parent, shove it into the top level under the root. */
void reference_parent_cb ( QofInstance* ent, gpointer user_data)
{
	Account *root;

	if(!ent) return;
	if(gnc_account_get_parent((Account*)ent) == NULL) {
		root = gnc_book_get_root_account(targetBook);
		gnc_account_append_child(root, (Account*)ent);
	}
}

static void
on_merge_finish (GnomeDruidPage  *gnomedruidpage,
           gpointer         arg1,
           gpointer         user_data)
{
	gint result;
	GtkWidget *top;
	const char *message;

	ENTER (" ");
	g_return_if_fail(mergeData != NULL);
	gnc_suspend_gui_refresh ();
	result = qof_book_merge_commit(mergeData);
	if(result != 0) {
		message = g_strdup_printf(_("Error: the Commit operation failed, error code %d."), result);
		top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
		gnc_error_dialog(top, "%s", message);
	}
	g_return_if_fail(result == 0);
	delete_merge_window ();
	qof_object_foreach(GNC_ID_ACCOUNT, targetBook, currency_transfer_cb, NULL);
	qof_object_foreach(GNC_ID_ACCOUNT, targetBook, reference_parent_cb,  NULL);
	qof_book_destroy(mergeBook);
	qof_session_end(merge_session);
	gnc_resume_gui_refresh ();
	LEAVE (" ");
}

static void
on_merge_prepare (GnomeDruidPage  *gnomedruidpage,
                            gpointer         arg1,
                            gpointer         user_data)
{
	GtkLabel *progress, *output;

	gnc_suspend_gui_refresh ();
	ENTER (" ");
	progress = GTK_LABEL (merge_get_widget("ResultsBox"));
	/* blank out old data */
	gtk_label_set_text(progress, "");
	g_return_if_fail(mergeBook || targetBook);
	mergeData = qof_book_merge_init(mergeBook, targetBook);
	g_return_if_fail(mergeData != NULL);
	count = 0;
 	qof_book_merge_rule_foreach(mergeData, collision_rule_loop, MERGE_REPORT);
	if(count == 0)
	{
		output = GTK_LABEL(merge_get_widget("OutPut"));
		gtk_label_set_text(output,_("No conflicts to be resolved."));
		gtk_widget_show(GTK_WIDGET(output));
	}
	gnc_resume_gui_refresh ();
	LEAVE (" ");
}

static GtkWidget *
gnc_create_import_druid ( void )
{
	GtkWidget *dialog, *druid, *start_page;
	GladeXML *xml;

	xml = gnc_glade_xml_new ("merge.glade", "Merge Druid");

	dialog = glade_xml_get_widget (xml, "Merge Druid");
	druid = glade_xml_get_widget (xml, "merge_druid");
	gnc_druid_set_colors (GNOME_DRUID (druid));
	start_page = glade_xml_get_widget (xml, "start_page");
	gtk_widget_show (start_page);
	gtk_widget_show (glade_xml_get_widget (xml, "MergeDruidFinishPage"));

	glade_xml_signal_connect(xml, "on_start_page_next",
		G_CALLBACK (on_import_start_page_next));
	
	glade_xml_signal_connect(xml, "on_qof_book_merge_prepare",
		G_CALLBACK (on_merge_prepare));

	glade_xml_signal_connect(xml, "on_qof_book_merge_next",
		G_CALLBACK (on_import_next));

	glade_xml_signal_connect (xml, "on_finish", 
		G_CALLBACK (on_merge_finish));

	glade_xml_signal_connect (xml, "on_cancel", 
		G_CALLBACK (on_merge_cancel));
	
	glade_xml_signal_connect (xml, "on_MergeUpdate_clicked",
		G_CALLBACK (on_MergeUpdate_clicked));
		
	glade_xml_signal_connect (xml, "on_MergeDuplicate_clicked",
		G_CALLBACK (on_MergeDuplicate_clicked));
		
	glade_xml_signal_connect (xml, "on_MergeNew_clicked",
		G_CALLBACK (on_MergeNew_clicked));

	g_signal_connect (dialog, "destroy",
			  G_CALLBACK(qof_book_merge_destroy_cb), NULL);
	return dialog;
}

void collision_rule_loop(QofBookMergeData *mergeData, QofBookMergeRule *rule, 
                        guint remainder)
{
	GSList *user_reports;
	QofParam *one_param;
	gchar *importstring, *targetstring;
	GtkLabel *output;
	gchar *buffer, *buffer2, *buffer3;
	
	g_return_if_fail(rule != NULL);
	buffer = "";
	/* there is a rule awaiting resolution, don't print any more */
	if(count > 0) { return; }
	ENTER (" remainder=%d", remainder);
	gnc_suspend_gui_refresh ();
	user_reports = rule->mergeParam;
	mergeData->currentRule = rule;
	output = GTK_LABEL(merge_get_widget("OutPut"));
	gtk_label_set_text(output, buffer);
	gtk_widget_show(GTK_WIDGET(output));
	gnc_resume_gui_refresh ();
	count = 1; /* user display text counts from 1, not zero */
	importstring = targetstring = NULL;
	gnc_suspend_gui_refresh ();
	/* Translators: %i is the number of conflicts. This is a
	   ngettext(3) message. */
	buffer2 = g_strdup_printf(ngettext("%i conflict needs to be resolved.", 
					  "%i conflicts need to be resolved.", 
					  remainder),
				 remainder); 
	/* Translators: %i is the number of values. This is a
	   ngettext(3) message. */
	buffer3 = g_strdup_printf(ngettext("%i parameter value for this \"%s\" object.",
					   "%i parameter values for this \"%s\" object.",
					   g_slist_length(user_reports)), 
				  g_slist_length(user_reports), rule->targetEnt->e_type);
	buffer = g_strconcat("\n", buffer2, "\n", "\n", buffer3, "\n", NULL);
	g_free(buffer2);
	g_free(buffer3);
	while(user_reports != NULL) {
		one_param = user_reports->data;
		/* FIXME: each g_strdup_printf as well as g_strconcat
		   will allocate a new string; all of these need to be
		   freed later. Currently this causes a lot of memory
		   leaks. */
		buffer = g_strconcat(buffer, g_strdup_printf(_("%i: Parameter name: %s "), 
			count, one_param->param_name), NULL);
		importstring = qof_book_merge_param_as_string(one_param, rule->importEnt);
		buffer = g_strconcat(buffer, 
			g_strdup_printf(_("Import data : %s "), importstring), NULL);
		targetstring = qof_book_merge_param_as_string(one_param, rule->targetEnt);
		buffer = g_strconcat(buffer, 
			g_strdup_printf(_("Original data : %s\n"), targetstring), NULL);
		user_reports = g_slist_next(user_reports);
		count++;
	}
	gtk_label_set_text(output,buffer);
	gtk_widget_show(GTK_WIDGET(output));
	gnc_resume_gui_refresh ();
	g_free(buffer);
	g_free(importstring);
	g_free(targetstring);
	LEAVE (" ");
}

void
gnc_ui_qsf_import_merge_druid (QofSession *original, QofSession *import)
{
	if (qsf_import_merge_window) { return; }
	qof_event_suspend ();
	qsf_import_merge_window = gnc_create_import_druid();
	g_return_if_fail(qsf_import_merge_window != NULL);
	previous_session = original;
	targetBook = qof_session_get_book(previous_session);
	merge_session = import;
	mergeBook = qof_session_get_book(merge_session);
	gtk_widget_show(qsf_import_merge_window);
}
