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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <libgnomeui/gnome-window-icon.h>

#include "dialog-utils.h"
#include "druid-merge.h"
#include "druid-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gui-query.h"
#include "qof_book_merge.h"
#include "druid-hierarchy.h"
#include "gnc-ui-util.h"
#include "Account.h"
#include "global-options.h"
#include "gnc-trace.h"

static GtkWidget			*qof_book_merge_window = NULL;
static GtkWidget			*druid_hierarchy_window = NULL;
static QofSession			*previous_session = NULL;
static gint					count = 0;
static qof_book_mergeRule	*currentRule = NULL;
static QofSession 			*merge_session = NULL;
static QofBook				*mergeBook = NULL;
static QofBook				*targetBook = NULL;
static gchar 				*buffer = "";

void collision_rule_loop	( qof_book_mergeRule*, 	guint );
void progress_rule_loop 	( qof_book_mergeRule*, 	guint );

static GtkWidget*
merge_get_widget (const char *name)
{
  if (!qof_book_merge_window) return NULL;

  return gnc_glade_lookup_widget (qof_book_merge_window, name);
}

static void
delete_merge_window (void)
{
  if (!qof_book_merge_window) return;

  gtk_widget_destroy (qof_book_merge_window);
  qof_book_merge_window = NULL;
}

static void
gnc_merge_destroy_cb (GtkObject *obj, gpointer user_data)
{

}

static gboolean
on_qof_start_page_next(GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       gpointer         user_data)
{
	gtk_widget_show(druid_hierarchy_window);
	gtk_widget_hide(qof_book_merge_window);
	return FALSE;
}

static void
on_MergeUpdate_clicked 	(GtkButton       *button,
              		    gpointer         user_data)
{
	qof_book_mergeUpdateResult(currentRule, MERGE_UPDATE); 
	count = 0;
 	qof_book_mergeRuleForeach(collision_rule_loop, MERGE_REPORT);
}

static void
on_MergeDuplicate_clicked 	(GtkButton       *button,
              			    gpointer         user_data)
{
	if(currentRule->mergeAbsolute == FALSE) { 
		qof_book_mergeUpdateResult(currentRule, MERGE_DUPLICATE); 
		count = 0;
	}
	if(currentRule->mergeAbsolute == TRUE) { 
		qof_book_mergeUpdateResult(currentRule, MERGE_ABSOLUTE); 
		count = 0;
	}
 	qof_book_mergeRuleForeach(collision_rule_loop, MERGE_REPORT);
}

static void
on_MergeNew_clicked (GtkButton       *button,
              		gpointer         user_data)
{
	if(currentRule->mergeAbsolute == FALSE) { 
		qof_book_mergeUpdateResult(currentRule, MERGE_NEW);
	}
	count = 0;
 	qof_book_mergeRuleForeach(collision_rule_loop, MERGE_REPORT);
}

static gboolean
on_qof_book_merge_next (GnomeDruidPage  *gnomedruidpage,
                       gpointer         arg1,
                       gpointer         user_data)
{
    GtkWidget *top;
	GtkLabel *output;
    const char *message = _("You must resolve all collisions.");

	if(count > 0) {
		top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
	    gnc_error_dialog(top, message);
		return TRUE;
	}
	buffer = "";
	gnc_suspend_gui_refresh ();
	output = GTK_LABEL(merge_get_widget("OutPut"));
	gtk_label_set_text(output, buffer);
	gtk_widget_show(GTK_WIDGET(output));
	gnc_resume_gui_refresh ();
	return FALSE;
}

static void
on_cancel (	GnomeDruid      *gnomedruid,
			gpointer         user_data)
{
	gnc_suspend_gui_refresh ();
	delete_merge_window();
	qof_session_set_current_session(previous_session);
	qof_book_destroy(mergeBook);
	qof_session_end(merge_session);	
	gnc_resume_gui_refresh ();
}

void currency_transfer_cb ( QofEntity* ent, gpointer user_data)
{
	if(!ent) return;
	if(xaccAccountGetCommodity((Account*)ent) == NULL) {
		xaccAccountSetCommodity((Account*)ent, gnc_default_currency());
	}
}

void reference_parent_cb ( QofEntity* ent, gpointer user_data)
{
	if(!ent) return;
	if(xaccAccountGetParent((Account*)ent) == NULL) {
		xaccGroupInsertAccount(xaccGroupGetRoot(xaccGetAccountGroup(gnc_get_current_book())), (Account*)ent);
	}
}

static void
on_finish (GnomeDruidPage  *gnomedruidpage,
           gpointer         arg1,
           gpointer         user_data)
{
	gint result;
    GtkWidget *top;
    const char *message = _("Error: the Commit operation failed.");

	gnc_suspend_gui_refresh ();
	result = qof_book_mergeCommit();
	if(result != 0) {
		top = gtk_widget_get_toplevel (GTK_WIDGET (gnomedruidpage));
	    gnc_error_dialog(top, message);
	}
	delete_merge_window ();
	/*
	Account has a new setparent parameter that takes 
	a QofEntity. Account converts this into an AccountGroup based on
	the GUID in the reference. This needs improving as child accounts 
	are currently being re-parented to top-level.
	*/
	qof_session_set_current_session(previous_session);
	qof_object_foreach(GNC_ID_ACCOUNT, gnc_get_current_book(), reference_parent_cb,  NULL);
	qof_object_foreach(GNC_ID_ACCOUNT, gnc_get_current_book(), currency_transfer_cb, NULL);
	qof_book_destroy(mergeBook);
	qof_session_end(merge_session);
	gnc_resume_gui_refresh ();
}

static void
on_qof_book_merge_prepare (GnomeDruidPage  *gnomedruidpage,
                            gpointer         arg1,
                            gpointer         user_data)
{
	gint result;
	GtkLabel *progress;

    gnc_suspend_gui_refresh ();
	progress = GTK_LABEL (merge_get_widget("ResultsBox"));
	/* blank out old data */
	gtk_label_set_text(progress, "");
	result = 0;
	g_return_if_fail(mergeBook != NULL);
	g_return_if_fail(targetBook != NULL);
	result = qof_book_mergeInit(mergeBook, targetBook);
	g_return_if_fail(result == 0);
	qof_book_mergeRuleForeach(progress_rule_loop, MERGE_NEW);
 	qof_book_mergeRuleForeach(progress_rule_loop, MERGE_ABSOLUTE);
 	qof_book_mergeRuleForeach(progress_rule_loop, MERGE_DUPLICATE);
 	qof_book_mergeRuleForeach(progress_rule_loop, MERGE_UPDATE);
	gtk_label_set_text(progress, buffer);
 	qof_book_mergeRuleForeach(collision_rule_loop, MERGE_REPORT);
	gnc_resume_gui_refresh ();
}

static GtkWidget *
gnc_create_merge_druid (void)
{
  GtkWidget *dialog;
  GtkWidget *druid;
  GladeXML *xml;

	xml = gnc_glade_xml_new ("merge.glade", "Merge Druid");

	glade_xml_signal_connect(xml, "on_start_page_next",
		GTK_SIGNAL_FUNC (on_qof_start_page_next));
	
	glade_xml_signal_connect(xml, "on_qof_book_merge_prepare",
		GTK_SIGNAL_FUNC (on_qof_book_merge_prepare));

	glade_xml_signal_connect(xml, "on_qof_book_merge_next",
		GTK_SIGNAL_FUNC (on_qof_book_merge_next));

	glade_xml_signal_connect (xml, "on_finish", GTK_SIGNAL_FUNC (on_finish));

	glade_xml_signal_connect (xml, "on_cancel", GTK_SIGNAL_FUNC (on_cancel));
	
	glade_xml_signal_connect(xml, "on_MergeUpdate_clicked",
		GTK_SIGNAL_FUNC (on_MergeUpdate_clicked));
		
	glade_xml_signal_connect(xml, "on_MergeDuplicate_clicked",
		GTK_SIGNAL_FUNC (on_MergeDuplicate_clicked));
		
	glade_xml_signal_connect(xml, "on_MergeNew_clicked",
		GTK_SIGNAL_FUNC (on_MergeNew_clicked));

	dialog = glade_xml_get_widget (xml, "Merge Druid");
	gnome_window_icon_set_from_default (GTK_WINDOW (dialog));

	druid = glade_xml_get_widget (xml, "merge_druid");
	gnc_druid_set_colors (GNOME_DRUID (druid));

	gtk_signal_connect (GTK_OBJECT(dialog), "destroy",
                      GTK_SIGNAL_FUNC(gnc_merge_destroy_cb), NULL);
	return dialog;
}

void progress_rule_loop(qof_book_mergeRule *rule, guint remainder)
{
	GtkLabel *progress;
	
	progress = GTK_LABEL(merge_get_widget("ResultsBox"));
	buffer = "";
	g_return_if_fail(rule != NULL);
	currentRule = rule;
	if(rule->mergeResult == MERGE_NEW) {
		if (remainder == 1) { 
			buffer = g_strconcat(buffer, 
				g_strdup_printf("%i %s tagged as NEW.\n", remainder, rule->mergeLabel), NULL);
			gtk_label_set_text (progress, buffer);
			}
		else { 
			buffer = g_strconcat(buffer, 
				g_strdup_printf("%i entities of type %s are tagged as NEW.\n", 
				remainder, rule->mergeLabel), NULL); 
			gtk_label_set_text (progress, buffer);
		}
		gtk_widget_show(GTK_WIDGET(progress));
		return;
	}
	if(rule->mergeResult ==  MERGE_ABSOLUTE) {
		if (remainder == 1) { 
			buffer = g_strconcat(buffer, 
				g_strdup_printf("%i %s tagged as an absolute GUID match.\n", 
				remainder, rule->mergeLabel), NULL); 
			gtk_label_set_text (progress, buffer);
		}
		else { 
			buffer = g_strconcat(buffer, 
				g_strdup_printf("%i entities of type %s tagged as an absolute GUID match.\n", 
				remainder, rule->mergeLabel), NULL);
			gtk_label_set_text (progress, buffer);
		}
		gtk_widget_show(GTK_WIDGET(progress));
		return;
	}
	if(rule->mergeResult == MERGE_DUPLICATE) {
		if (remainder == 1) { 
			buffer = g_strconcat(buffer, g_strdup_printf("%i %s tagged as a duplicate.\n", 
				remainder, rule->mergeLabel), NULL); 
			gtk_label_set_text (progress, buffer);
		}
		else { 
			buffer = g_strconcat(buffer, g_strdup_printf("%i entities of type %s tagged as a duplicate.\n", 
				remainder, rule->mergeLabel), NULL);
			gtk_label_set_text (progress, buffer);
		}
		gtk_widget_show(GTK_WIDGET(progress));
		return;
	}
	if(rule->mergeResult == MERGE_UPDATE) {
		if (remainder == 1) { 
			buffer = g_strconcat(buffer, g_strdup_printf("%i %s tagged as to be updated.\n", 
				remainder, rule->mergeLabel), NULL); 
			gtk_label_set_text (progress, buffer);
		}
		else { 
			buffer = g_strconcat(buffer, g_strdup_printf("%i entities of type %s tagged as to be updated.\n", 
				remainder, rule->mergeLabel), NULL);
			gtk_label_set_text (progress, buffer);
		}
		gtk_widget_show(GTK_WIDGET(progress));
		return;
	}
	g_free(buffer);
}

void collision_rule_loop(qof_book_mergeRule *rule, guint remainder)
{
	GSList *user_reports;
	QofParam *one_param;
	gchar *importstring, *targetstring;
	gchar *buffer;
	GtkLabel *output;
	
	g_return_if_fail(rule != NULL);
	buffer = "";
	/* there is a rule awaiting resolution, don't print any more */
	if(count > 0) return;
	gnc_suspend_gui_refresh ();
	user_reports = rule->mergeParam;
	currentRule = rule;
	output = GTK_LABEL(merge_get_widget("OutPut"));
	gtk_label_set_text(output, buffer);
	gtk_widget_show(GTK_WIDGET(output));
	gnc_resume_gui_refresh ();
	count = 1; /* user display text counts from 1, not zero */
	importstring = targetstring = NULL;
	gnc_suspend_gui_refresh ();
 	if(remainder == 1) { 
		buffer = g_strdup_printf("\n%i conflict needs to be resolved.\n", remainder);
	}
	else { 
		buffer = g_strdup_printf("\n%i conflicts need to be resolved.\n", remainder); 
	}
	buffer = g_strconcat(buffer, g_strdup_printf("\n%i parameter values for this \"%s\" object.\n", 
		   g_slist_length(user_reports), rule->targetEnt->e_type), NULL);
	while(user_reports != NULL) {
		one_param = user_reports->data;
		buffer = g_strconcat(buffer, g_strdup_printf("%i:Parameter name: %s ", 
			count, one_param->param_name), NULL);
		importstring = qof_book_merge_param_as_string(one_param, rule->importEnt);
		buffer = g_strconcat(buffer, g_strdup_printf("Import data : %s ", importstring), NULL);
		targetstring = qof_book_merge_param_as_string(one_param, rule->targetEnt);
		buffer = g_strconcat(buffer, g_strdup_printf("Original data : %s\n", targetstring), NULL);
		user_reports = g_slist_next(user_reports);
		count++;
	}
	gtk_label_set_text(output,buffer);
	gtk_widget_show(GTK_WIDGET(output));
	gnc_resume_gui_refresh ();
	g_free(buffer);
}

GtkWidget*
qof_book_merge_running (void)
{
	if (qof_book_merge_window) return qof_book_merge_window;
	return NULL;
}


void
gnc_ui_qof_book_merge_druid (void)
{
	
	if (qof_book_merge_window) return;
	/*	QofSession changes to avoid using current book */
    gnc_engine_suspend_events ();
	previous_session = qof_session_get_current_session();
	targetBook = qof_session_get_book(previous_session);
	merge_session = qof_session_new();
	qof_session_set_current_session(merge_session);
	mergeBook = qof_session_get_book(merge_session);
	gnc_engine_resume_events ();
	g_return_if_fail(targetBook != NULL);
	g_return_if_fail(mergeBook != NULL);
	g_return_if_fail(merge_session != NULL);
	qof_book_merge_window = gnc_create_merge_druid();
	g_return_if_fail(qof_book_merge_window != NULL);
	gnc_ui_hierarchy_druid();
	druid_hierarchy_window = gnc_ui_hierarchy_running();
	gtk_widget_hide (druid_hierarchy_window);
	gtk_object_set_data (GTK_OBJECT (druid_hierarchy_window), "Merge Druid", qof_book_merge_window);
	gtk_widget_show (qof_book_merge_window);
	g_return_if_fail(targetBook != NULL);
	g_return_if_fail(mergeBook != NULL);
	g_return_if_fail(merge_session != NULL);
	return;
}
