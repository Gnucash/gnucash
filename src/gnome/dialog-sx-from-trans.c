/********************************************************************
 * dialog-sx-from-trans.c -- a simple dialog for creating a         *
 *                           scheduled transaction for a "real      *
 *                           one                                    *
 * Copyright (C) 2000 Robert Merkel <rgmerk@mira.net>               *
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
 ********************************************************************/

#include "config.h"

#include <gnome.h>

#include "SX-ttinfo.h"
#include "SchedXaction.h"
#include "dialog-scheduledxaction.h"
#include "dialog-sx-from-trans.h"
#include "dialog-utils.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"

#define SX_GLADE_FILE "sched-xact.glade"
#define SXFTD_DIALOG_GLADE_NAME "sx_from_real_trans"
#define SXFTD_OK_BUTTON "ok_button"
#define SXFTD_ADVANCED_BUTTON "advanced_button"
#define SXFTD_CANCEL_BUTTON "cancel_button"
#define SXFTD_NEVER_END_BUTTON "never_end_button"
#define SXFTD_END_ON_DATE_BUTTON "end_on_date_button"
#define SXFTD_N_OCCURRENCES_BUTTON "n_occurrences_button"
#define SXFTD_NAME_ENTRY "name_entry"
#define SXFTD_N_OCCURRENCES_ENTRY "n_occurrences_entry"
#define SXFTD_FREQ_OPTION_MENU "freq_option_menu"
#define SXFTD_END_DATE_EDIT "end_date_edit"

static short module = MOD_SX;

static void sxftd_ok_clicked(GtkWidget *w, gpointer user_data);
static void sxftd_advanced_clicked(GtkWidget *w, gpointer user_data);
static void sxftd_cancel_clicked(GtkWidget *w, gpointer user_data);

typedef enum {NEVER_END, END_ON_DATE, END_AFTER_N_OCCS, BAD_END} endType;

typedef enum {FREQ_DAILY = 0,  /* I know the =0 is redundant, but I'm using
                                * the numeric equivalences explicitly here
				*/
	      FREQ_WEEKLY, 
	      FREQ_MONTHLY, 
	      FREQ_QUARTERLY,
              FREQ_ANNUALLY} SxftiFreqType;

typedef struct
{
  GladeXML *gxml;
  GtkWidget *dialog;
  Transaction *trans;
  SchedXaction *sx;
} SXFromTransInfo;


typedef struct
{
  endType type;
  GDate end_date;
  guint n_occurrences;
} getEndTuple;

/* Stolen from jsled - nice and neat, actually (if a little light on 
 * for typechecking, but we'll be careful) . . . 
 */
typedef struct
{
  gchar *name;
  gchar *signal;
  void (*handlerFn)();
} widgetSignalHandlerTuple;
    
static void
sxfti_attach_callbacks(SXFromTransInfo *sxfti)
{

  widgetSignalHandlerTuple callbacks[] =
    {
      {SXFTD_OK_BUTTON, "clicked", sxftd_ok_clicked},
      {SXFTD_ADVANCED_BUTTON, "clicked", sxftd_advanced_clicked},
      {SXFTD_CANCEL_BUTTON, "clicked", sxftd_cancel_clicked},
      {NULL, NULL, NULL}
    };
  
  int i;

  GtkWidget *w;
  for(i = 0; callbacks[i].name != NULL; i++)
  {
    w = glade_xml_get_widget(sxfti->gxml, callbacks[i].name);
    
    gtk_signal_connect( GTK_OBJECT(w), callbacks[i].signal, 
			GTK_SIGNAL_FUNC(callbacks[i].handlerFn),
			sxfti);
			
  }

  return;
}
  

static getEndTuple
sxftd_get_end_info(SXFromTransInfo *sxfti)
{
  getEndTuple retval;
  GtkWidget *w;

  w = glade_xml_get_widget(sxfti->gxml, SXFTD_NEVER_END_BUTTON);
  
  if(gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(w)))
  {
    retval.type = NEVER_END;
    return retval;
  }
  
  w = glade_xml_get_widget(sxfti->gxml, SXFTD_END_ON_DATE_BUTTON);

  if(gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(w)))
  {
    time_t end_t_jul;
    retval.type = END_ON_DATE;
    g_date_clear( &(retval.end_date), 1 );

    w = glade_xml_get_widget(sxfti->gxml, SXFTD_END_DATE_EDIT);
    end_t_jul = gnome_date_edit_get_date(GNOME_DATE_EDIT(w));

    g_date_set_julian( &(retval.end_date), end_t_jul);
    return retval;
  }
    
  w = glade_xml_get_widget(sxfti->gxml, SXFTD_N_OCCURRENCES_BUTTON);
  if(gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(w) ))
  {
    gchar *text, *endptr;
    guint n_occs;
    w = glade_xml_get_widget(sxfti->gxml, SXFTD_N_OCCURRENCES_ENTRY);
    text = gtk_editable_get_chars(GTK_EDITABLE(w), 0, -1);
    
    n_occs = strtoul(text, &endptr, 10);

    free(text);
    if(!endptr && n_occs > 0)
    {
      retval.type = END_AFTER_N_OCCS;
      retval.n_occurrences = n_occs;
      return retval;
    }
  }

  retval.type = BAD_END;
  return retval;
}
 

static guint
sxftd_add_template_trans(SXFromTransInfo *sxfti)
{
  
  Transaction *tr = sxfti->trans;
  GList *tt_list= NULL;
  GList *splits, *template_splits = NULL;
  TTInfo *tti = gnc_ttinfo_malloc();
  TTSplitInfo *ttsi;
  Split *sp;
  gnc_numeric split_value;

  gnc_ttinfo_set_description(tti, xaccTransGetDescription(tr));
  gnc_ttinfo_set_num(tti, xaccTransGetNum(tr));
  gnc_ttinfo_set_currency(tti, xaccTransGetCurrency(tr));

  for(splits = xaccTransGetSplitList(tr); splits; splits = splits->next)
  {
    sp = splits->data;
    ttsi = gnc_ttsplitinfo_malloc();
    gnc_ttsplitinfo_set_action(ttsi, xaccSplitGetAction(sp));
    split_value = xaccSplitGetValue(sp);
    gnc_ttsplitinfo_set_memo(ttsi, xaccSplitGetMemo(sp));

    if(gnc_numeric_positive_p(split_value)) /* FIXME:I still can't remember which one is stored positive :) */
    {
      gnc_ttsplitinfo_set_debit_formula_numeric(ttsi, split_value);
    }
    else
    {
      gnc_ttsplitinfo_set_credit_formula_numeric(ttsi, split_value);
    }

    template_splits = g_list_append(template_splits, ttsi);
  }			       

  gnc_ttinfo_set_template_splits(tti, template_splits);

  tt_list = g_list_append(tt_list, tti);

  xaccSchedXactionSetTemplateTrans(sxfti->sx, tt_list);

  return 0;
}

static guint
sxftd_compute_sx(SXFromTransInfo *sxfti)
{
  GtkWidget *w;
  gchar *name;
  GDate date;
  time_t trans_t;
  int index;

  getEndTuple end_info;
  guint sxftd_errno = 0; /* 0 == OK, > 0 means dialog needs to be run again */

  FreqSpec *fs, *tmpfs;

  SchedXaction *sx = sxfti->sx;

  /* get the name */

  w = glade_xml_get_widget(sxfti->gxml, SXFTD_NAME_ENTRY);
  
  name = gtk_editable_get_chars(GTK_EDITABLE(w), 0, -1);

  xaccSchedXactionSetName(sx, name);
  g_free(name);

  /* get the date (basis for start date */

  trans_t = xaccTransGetDate(sxfti->trans);

  
  g_date_set_time(&date, trans_t);
 
  fs = xaccFreqSpecMalloc();

  /* get the frequency */

  w = glade_xml_get_widget(sxfti->gxml, SXFTD_FREQ_OPTION_MENU);
  
  index = gnc_option_menu_get_active(w);

  /* Note that we make the start date the *NEXT* instance, not the
   * present one
   */

  switch(index)
  {
  case FREQ_DAILY:

    g_date_add_days(&date, 1); 
    xaccFreqSpecSetDaily(fs, &date, 1);
    xaccFreqSpecSetUIType(fs, UIFREQ_DAILY);
    break;

  case FREQ_WEEKLY:
    g_date_add_days(&date, 7);
    
    tmpfs = xaccFreqSpecMalloc();
    xaccFreqSpecSetComposite(fs);
    xaccFreqSpecSetWeekly(tmpfs, &date, 1);
    xaccFreqSpecSetUIType(fs, UIFREQ_WEEKLY);
    xaccFreqSpecCompositeAdd(fs,tmpfs);

    break;

  case FREQ_MONTHLY:
    g_date_add_months(&date, 1);
    xaccFreqSpecSetMonthly(fs, &date, 1);
    xaccFreqSpecSetUIType(fs, UIFREQ_MONTHLY);
    break;

  case FREQ_QUARTERLY:
    g_date_add_months(&date, 3);
    xaccFreqSpecSetMonthly(fs, &date, 3);
    xaccFreqSpecSetUIType(fs, UIFREQ_QUARTERLY);
    break;

  case FREQ_ANNUALLY:
    g_date_add_years(&date, 1);
    xaccFreqSpecSetMonthly(fs, &date, 12);
    xaccFreqSpecSetUIType(fs, UIFREQ_YEARLY);
    break;

  default:

    PERR("Nonexistent frequency selected.  This is a bug.");
    
    break;
  }

  if (sxftd_errno == 0)
  {
    xaccSchedXactionSetFreqSpec( sx, fs);
    xaccSchedXactionSetStartDate(sx, &date);
  }

  end_info = sxftd_get_end_info(sxfti);

  switch(end_info.type)
  {
  case NEVER_END:
    break;

  case END_ON_DATE:
    xaccSchedXactionSetEndDate(sx, &(end_info.end_date));
    break;

  case END_AFTER_N_OCCS:
    xaccSchedXactionSetNumOccur(sx, end_info.n_occurrences);
    break;
    
  default:
    sxftd_errno = 2;
    break;
  }

  sxftd_add_template_trans( sxfti);

  return sxftd_errno;
}

static void
sxftd_delete(SXFromTransInfo *sxfti, gboolean delete_sx)
{

  /* FIXME: do we need to clean up the GladeXML pointer ?*/

  gnome_dialog_close( GNOME_DIALOG(sxfti->dialog));

  if(sxfti->sx && delete_sx)
  {
    xaccSchedXactionFree(sxfti->sx);
  }

  g_free(sxfti);
  return;
}

static void
sxftd_ok_clicked(GtkWidget *w, gpointer user_data)
{
  SXFromTransInfo *sxfti = user_data;
  GNCBook *book;
  GList *sx_list;
  guint sx_error = sxftd_compute_sx(sxfti);

  if (sx_error == 0)
  {
    book = gnc_get_current_book ();
    sx_list = gnc_book_get_schedxactions(book);
    sx_list = g_list_append(sx_list, sxfti->sx);
    gnc_book_set_schedxactions(book, sx_list);
  }

  sxftd_delete(sxfti, FALSE);
  return;
}
  

static void
sxftd_cancel_clicked(GtkWidget *w, gpointer user_data)
{
  SXFromTransInfo *sxfti = user_data;
  sxftd_delete(sxfti, TRUE);
}

static void
sxftd_advanced_clicked(GtkWidget *w, gpointer user_data)
{
   SXFromTransInfo *sxfti = user_data;
  GNCBook *book;
  GList *sx_list;
  guint sx_error = sxftd_compute_sx(sxfti);
  SchedXactionDialog *adv_dlg;
  SchedXactionEditorDialog *adv_edit_dlg;

  if (sx_error == 0)
  {
    adv_dlg = gnc_ui_scheduled_xaction_dialog_create();
    adv_edit_dlg = gnc_ui_scheduled_xaction_editor_dialog_create(adv_dlg, 
								 sxfti->sx);
    
  }

  else
  {
    PWARN("something bad happened in sxftd_compute_sx");
  }

  return;
}

  
void
gnc_sx_create_from_trans(Transaction *trans)
{
  GtkWidget *w;
  SXFromTransInfo *sxfti = g_malloc(sizeof(SXFromTransInfo));

  sxfti->gxml = gnc_glade_xml_new(SX_GLADE_FILE,
				  SXFTD_DIALOG_GLADE_NAME);
  
  sxfti->dialog = glade_xml_get_widget(sxfti->gxml,
				       SXFTD_DIALOG_GLADE_NAME);

  sxfti->trans = trans;
  
  sxfti->sx = xaccSchedXactionMalloc(gnc_get_current_book ());

  sxfti_attach_callbacks(sxfti);

  w = glade_xml_get_widget(sxfti->gxml,
			   SXFTD_FREQ_OPTION_MENU);

  gnc_option_menu_init(w);
  
  gtk_widget_show_all(sxfti->dialog);

  return;
}


