/********************************************************************
 * dialog-sx-from-trans.c -- a simple dialog for creating a         *
 *                           scheduled transaction from a real one  *
 * Copyright (C) 2001 Robert Merkel <rgmerk@mira.net>               *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 ********************************************************************/

#include "config.h"

#include "dialog-sx-editor.h"
#include "dialog-sx-from-trans.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-date-edit.h"
#include "gnc-dense-cal-store.h"
#include "gnc-dense-cal.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "qof.h"
#include "Recurrence.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-ttinfo.h"
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <stdlib.h>

#define SX_GLADE_FILE "sched-xact.glade"
#define SXFTD_DIALOG_GLADE_NAME "sx_from_real_trans"
#define SXFTD_OK_BUTTON "ok_button"
#define SXFTD_ADVANCED_BUTTON "advanced_button"
#define SXFTD_CANCEL_BUTTON "cancel_button"
#define SXFTD_NEVER_END_BUTTON "never_end_button"
#define SXFTD_END_ON_DATE_BUTTON "end_on_date_button"
#define SXFTD_N_OCCURRENCES_BUTTON "n_occurrences_button"
#define SXFTD_PARAM_TABLE "param_table"
#define SXFTD_NAME_ENTRY "name_entry"
#define SXFTD_N_OCCURRENCES_ENTRY "n_occurrences_entry"
#define SXFTD_FREQ_COMBO_BOX "freq_combo_box"
/* #define SXFTD_END_DATE_EDIT "end_date_edit" */
#define SXFTD_START_DATE_EDIT "start_date_edit"
#define SXFTD_EX_CAL_FRAME "ex_cal_frame"
#define SXFTD_END_DATE_BOX "end_date_hbox"

#define SXFTD_ERRNO_UNBALANCED_XACTION 3
#define SXFTD_ERRNO_OPEN_XACTION -3

#define SXFTD_EXCAL_NUM_MONTHS 4
#define SXFTD_EXCAL_MONTHS_PER_COL 4

#define SXFTD_RESPONSE_ADVANCED 100 /* 'Advanced' button response code */

static QofLogModule log_module = GNC_MOD_GUI_SX;

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN GNC_MOD_GUI_SX

static void sxftd_freq_combo_changed( GtkWidget *w, gpointer user_data );
static void gnc_sx_trans_window_response_cb(GtkDialog *dialog, gint response, gpointer data);

static void sxftd_destroy( GtkWidget *w, gpointer user_data );

typedef enum { FREQ_DAILY = 0,  /* I know the =0 is redundant, but I'm using
                                 * the numeric equivalences explicitly here */
               FREQ_WEEKLY,
               FREQ_BIWEEKLY,
               FREQ_MONTHLY,
               FREQ_QUARTERLY,
               FREQ_ANNUALLY
} SxftiFreqType;

typedef struct
{
  GladeXML *gxml;
  GtkWidget *dialog;
  Transaction *trans;
  SchedXaction *sx;

  GncDenseCalStore *dense_cal_model;
  GncDenseCal *example_cal;

  GNCDateEdit *startDateGDE, *endDateGDE;

} SXFromTransInfo;

typedef struct
{
  gdcs_end_type type;
  GDate end_date;
  guint n_occurrences;
} getEndTuple;

static void sxftd_update_example_cal( SXFromTransInfo *sxfti );
static void sxftd_update_excal_adapt( GObject *o, gpointer ud );

typedef struct
{
  gchar *name;
  gchar *signal;
  void (*handlerFn)();
} widgetSignalHandlerTuple;

static void sxftd_ok_clicked(SXFromTransInfo *sxfti);
static void sxftd_advanced_clicked(SXFromTransInfo *sxfti);

static void
sxfti_attach_callbacks(SXFromTransInfo *sxfti)
{

  widgetSignalHandlerTuple callbacks[] =
    {
      /* Whenever any of the controls change, we want to update the
       * calendar. */
      { SXFTD_NEVER_END_BUTTON,     "clicked",      sxftd_update_excal_adapt },
      { SXFTD_END_ON_DATE_BUTTON,   "clicked",      sxftd_update_excal_adapt },
      { SXFTD_N_OCCURRENCES_BUTTON, "clicked",      sxftd_update_excal_adapt },
      { SXFTD_N_OCCURRENCES_ENTRY,  "changed",      sxftd_update_excal_adapt },
      { NULL,                  NULL,      NULL }
    };

  int i;

  GtkWidget *w;
  for(i = 0; callbacks[i].name != NULL; i++)
  {
    w = glade_xml_get_widget(sxfti->gxml, callbacks[i].name);

    g_signal_connect (GTK_OBJECT(w), callbacks[i].signal,
		      G_CALLBACK(callbacks[i].handlerFn),
		      sxfti );
  }

  g_signal_connect (G_OBJECT(sxfti->dialog), "response",
                    G_CALLBACK (gnc_sx_trans_window_response_cb),
                    sxfti);
}


static getEndTuple
sxftd_get_end_info(SXFromTransInfo *sxfti)
{
  getEndTuple retval;
  GtkWidget *w;

  retval.type = BAD_END;
  g_date_clear( &(retval.end_date), 1 );
  retval.n_occurrences = 0;

  w = glade_xml_get_widget(sxfti->gxml, SXFTD_NEVER_END_BUTTON);
  if(gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(w))) {
    retval.type = NEVER_END;
    return retval;
  }

  w = glade_xml_get_widget(sxfti->gxml, SXFTD_END_ON_DATE_BUTTON);
  if(gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(w))) {
    time_t end_tt;
    retval.type = END_ON_DATE;
    g_date_clear( &(retval.end_date), 1 );
    end_tt = gnc_date_edit_get_date(sxfti->endDateGDE);
    g_date_set_time_t( &(retval.end_date), end_tt);
    return retval;
  }

  w = glade_xml_get_widget(sxfti->gxml, SXFTD_N_OCCURRENCES_BUTTON);
  if(gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(w) )) {
    gchar *text, *endptr;
    guint n_occs;
    w = glade_xml_get_widget(sxfti->gxml, SXFTD_N_OCCURRENCES_ENTRY);
    text = gtk_editable_get_chars(GTK_EDITABLE(w), 0, -1);

    if (text == NULL || strlen(text) == 0)
    {
        n_occs = 0;
    }
    else
    {
        n_occs = strtoul(text, &endptr, 10);
        if ( !endptr )
        {
            n_occs = -1;
        }
    }

    g_free(text);

    if(n_occs > 0) {
      retval.type = END_AFTER_N_OCCS;
      retval.n_occurrences = n_occs;
      return retval;
    }
  }

  return retval;
}


static guint
sxftd_add_template_trans(SXFromTransInfo *sxfti)
{

  Transaction *tr = sxfti->trans;
  GList *tt_list = NULL;
  GList *splits, *template_splits = NULL;
  TTInfo *tti = gnc_ttinfo_malloc();
  TTSplitInfo *ttsi;
  Split *sp;
  gnc_numeric runningBalance;
  gnc_numeric split_value;
  const char *tmpStr;

  runningBalance = gnc_numeric_zero();

  gnc_ttinfo_set_description(tti, xaccTransGetDescription(tr));
  gnc_ttinfo_set_num(tti, xaccTransGetNum(tr));
  gnc_ttinfo_set_currency(tti, xaccTransGetCurrency(tr));

  for(splits = xaccTransGetSplitList(tr); splits; splits = splits->next) {
    sp = splits->data;
    ttsi = gnc_ttsplitinfo_malloc();
    gnc_ttsplitinfo_set_action(ttsi, xaccSplitGetAction(sp));
    split_value = xaccSplitGetValue(sp);
    gnc_ttsplitinfo_set_memo(ttsi, xaccSplitGetMemo(sp));

    runningBalance = gnc_numeric_add( runningBalance, split_value,
                                      100, (GNC_DENOM_AUTO | GNC_DENOM_LCD) );

    if(gnc_numeric_positive_p(split_value)) {
            tmpStr = xaccPrintAmount( split_value,
                                      gnc_default_print_info(FALSE) );
            gnc_ttsplitinfo_set_debit_formula( ttsi, tmpStr );
    }
    else {
            /* Negate the numeric so it prints w/o the sign at the front. */
            tmpStr = xaccPrintAmount( gnc_numeric_neg( split_value ),
                                      gnc_default_print_info(FALSE) );
            gnc_ttsplitinfo_set_credit_formula( ttsi, tmpStr );
    }

    /* Copy over per-split account info */
    gnc_ttsplitinfo_set_account( ttsi, xaccSplitGetAccount( sp ) );

    template_splits = g_list_append(template_splits, ttsi);
  }

  if ( ! gnc_numeric_zero_p( runningBalance )
          && !gnc_verify_dialog( (gncUIWidget)sxfti->dialog,
                                 FALSE, "%s",
                                 _("The Scheduled Transaction Editor "
                                   "cannot automatically balance "
                                   "this transaction. "
                                   "Should it still be "
                                   "entered?") ) ) {
        return SXFTD_ERRNO_UNBALANCED_XACTION;
  }

  gnc_ttinfo_set_template_splits(tti, template_splits);

  tt_list = g_list_append(tt_list, tti);

  gnc_suspend_gui_refresh ();
  xaccSchedXactionSetTemplateTrans(sxfti->sx, tt_list,
                                   gnc_get_current_book ());
  gnc_resume_gui_refresh ();

  return 0;
}

static void
sxftd_update_schedule( SXFromTransInfo *sxfti, GDate *date, GList **recurrences)
{
  gint index;
  GtkWidget *w;

  /* Note that we make the start date the *NEXT* instance, not the
   * present one. */
  w = glade_xml_get_widget(sxfti->gxml, SXFTD_FREQ_COMBO_BOX);
  index = gtk_combo_box_get_active(GTK_COMBO_BOX(w));

  switch (index)
  {
  case FREQ_DAILY: {
      Recurrence *r = g_new0(Recurrence, 1);
      recurrenceSet(r, 1, PERIOD_DAY, date, WEEKEND_ADJ_NONE);
      *recurrences = g_list_append(*recurrences, r);
    } break;

  case FREQ_WEEKLY:
  case FREQ_BIWEEKLY: {
      Recurrence *r = g_new0(Recurrence, 1);
      int mult = (index == FREQ_BIWEEKLY ? 2 : 1);
      recurrenceSet(r, mult, PERIOD_WEEK, date, WEEKEND_ADJ_NONE);
      *recurrences = g_list_append(*recurrences, r);
  } break;

  case FREQ_MONTHLY:
  case FREQ_QUARTERLY:
  case FREQ_ANNUALLY: {
      Recurrence *r = g_new0(Recurrence, 1);
      int mult = (index == FREQ_MONTHLY
                  ? 1
                  : (index == FREQ_QUARTERLY
                     ? 3
                     : 12));
      recurrenceSet(r, mult, PERIOD_MONTH, date, recurrenceGetWeekendAdjust(r));
      *recurrences = g_list_append(*recurrences, r);
  } break;

  default:
       g_critical("nonexistent frequency selected");
       break;
  }
}

static gint
sxftd_init( SXFromTransInfo *sxfti )
{
  GtkWidget *w;
  const char *transName;
  gint pos;
  GList *schedule = NULL;
  time_t start_tt;
  struct tm *tmpTm;
  GDate date, nextDate;

  if ( ! sxfti->sx ) {
    return -1;
  }
  if ( ! sxfti->trans ) {
    return -2;
  }
  if ( xaccTransIsOpen( sxfti->trans ) ) {
          return SXFTD_ERRNO_OPEN_XACTION;
  }

  sxfti_attach_callbacks(sxfti);

  /* Setup the example calendar and related data structures. */
  {
    int num_marks = SXFTD_EXCAL_NUM_MONTHS * 31;

    w = GTK_WIDGET(glade_xml_get_widget( sxfti->gxml, SXFTD_EX_CAL_FRAME ));
    sxfti->dense_cal_model = gnc_dense_cal_store_new(num_marks);
    sxfti->example_cal = GNC_DENSE_CAL(gnc_dense_cal_new_with_model(GNC_DENSE_CAL_MODEL(sxfti->dense_cal_model)));
    g_object_ref_sink(sxfti->example_cal);

    g_assert(sxfti->example_cal);
    gnc_dense_cal_set_num_months( sxfti->example_cal, SXFTD_EXCAL_NUM_MONTHS );
    gnc_dense_cal_set_months_per_col( sxfti->example_cal, SXFTD_EXCAL_MONTHS_PER_COL );
    gtk_container_add( GTK_CONTAINER(w), GTK_WIDGET(sxfti->example_cal) );
  }

  /* Setup the start and end dates as GNCDateEdits */
  {
    GtkWidget *paramTable = glade_xml_get_widget( sxfti->gxml,
                                                  SXFTD_PARAM_TABLE );
    sxfti->startDateGDE =
      GNC_DATE_EDIT( gnc_date_edit_new( time( NULL ),
                                        FALSE, FALSE ) );
    gtk_table_attach( GTK_TABLE(paramTable),
                      GTK_WIDGET( sxfti->startDateGDE ),
                      1, 2, 2, 3,
                      (GTK_EXPAND | GTK_FILL),
                      GTK_FILL,
                      0, 0 );
    g_signal_connect( sxfti->startDateGDE, "date-changed",
                      G_CALLBACK( sxftd_update_excal_adapt ),
                      sxfti );
  }
  {
    GtkWidget *endDateBox = glade_xml_get_widget( sxfti->gxml,
                                                  SXFTD_END_DATE_BOX );
    sxfti->endDateGDE =
      GNC_DATE_EDIT( gnc_date_edit_new( time( NULL ),
                                        FALSE, FALSE ) );
    gtk_box_pack_start( GTK_BOX( endDateBox ),
                        GTK_WIDGET( sxfti->endDateGDE ),
                        FALSE, TRUE, 0 );
    g_signal_connect( sxfti->endDateGDE, "date-changed",
                      G_CALLBACK( sxftd_update_excal_adapt ),
                      sxfti );
  }

  /* Get the name from the transaction, try that as the initial SX name. */
  transName = xaccTransGetDescription( sxfti->trans );
  xaccSchedXactionSetName( sxfti->sx, transName );

  /* Setup the initial start date for user display/confirmation */
  /* compute good initial date. */
  start_tt = xaccTransGetDate( sxfti->trans );
  g_date_set_time_t( &date, start_tt );
  w = glade_xml_get_widget(sxfti->gxml,
			   SXFTD_FREQ_COMBO_BOX);
  gtk_combo_box_set_active(GTK_COMBO_BOX(w), 0);
  g_signal_connect( w, "changed",
                    G_CALLBACK(sxftd_freq_combo_changed),
                    sxfti );
  sxftd_update_schedule( sxfti, &date, &schedule);
  recurrenceListNextInstance(schedule, &date, &nextDate);
  recurrenceListFree(&schedule);

  tmpTm = g_new0( struct tm, 1 );
  g_date_to_struct_tm( &nextDate, tmpTm );
  start_tt = mktime( tmpTm );
  g_free( tmpTm );
  gnc_date_edit_set_time( sxfti->startDateGDE, start_tt );

  w = glade_xml_get_widget( sxfti->gxml, SXFTD_NAME_ENTRY );
  pos = 0;
  gtk_editable_insert_text( GTK_EDITABLE(w), transName,
                            (strlen(transName) * sizeof(char)), &pos );

  g_signal_connect( GTK_OBJECT(w), "destroy",
                    G_CALLBACK(sxftd_destroy),
                    sxfti );

  sxftd_update_example_cal( sxfti );

  return 0;
}

static guint
sxftd_compute_sx(SXFromTransInfo *sxfti)
{
  GtkWidget *w;
  gchar *name;
  GDate date;
  GList *schedule = NULL;
  getEndTuple end_info;
  guint sxftd_errno = 0; /* 0 == OK, > 0 means dialog needs to be run again */

  SchedXaction *sx = sxfti->sx;

  /* get the name */
  w = glade_xml_get_widget(sxfti->gxml, SXFTD_NAME_ENTRY);
  name = gtk_editable_get_chars(GTK_EDITABLE(w), 0, -1);
  xaccSchedXactionSetName(sx, name);
  g_free(name);

  g_date_set_time_t( &date, gnc_date_edit_get_date( sxfti->startDateGDE ) );

  sxftd_update_schedule(sxfti, &date, &schedule);
  if (sxftd_errno == 0) {
      gnc_sx_set_schedule(sx, schedule);
      xaccSchedXactionSetStartDate( sx, &date );
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

  gnc_sx_set_instance_count( sx, 1 );

  /* Set the autocreate, days-in-advance and remind-in-advance values from
   * options. */
  {
    gboolean autoCreateState, notifyState;
    gint daysInAdvance;

    autoCreateState =
      gnc_gconf_get_bool( SXED_GCONF_SECTION, KEY_CREATE_AUTO, NULL );
    notifyState =
      gnc_gconf_get_bool( SXED_GCONF_SECTION, KEY_NOTIFY, NULL );
    xaccSchedXactionSetAutoCreate( sx,
                                   autoCreateState,
                                   (autoCreateState & notifyState) );

    daysInAdvance =
      gnc_gconf_get_float( SXED_GCONF_SECTION, KEY_CREATE_DAYS, NULL );
    xaccSchedXactionSetAdvanceCreation( sx, daysInAdvance );

    daysInAdvance =
      gnc_gconf_get_float( SXED_GCONF_SECTION, KEY_REMIND_DAYS, NULL );
    xaccSchedXactionSetAdvanceReminder( sx, daysInAdvance );
  }

  if ( sxftd_add_template_trans( sxfti ) != 0 ) {
          sxftd_errno = SXFTD_ERRNO_UNBALANCED_XACTION;
  }

  return sxftd_errno;
}

static void
sxftd_close(SXFromTransInfo *sxfti, gboolean delete_sx)
{
  if ( sxfti->sx && delete_sx ) {
	gnc_sx_begin_edit(sxfti->sx);
    xaccSchedXactionDestroy(sxfti->sx);
  }
  sxfti->sx = NULL;

  gtk_widget_destroy (GTK_WIDGET (sxfti->dialog));
}

static void
sxftd_ok_clicked(SXFromTransInfo *sxfti)
{
  QofBook *book;
  SchedXactions *sxes;
  guint sx_error = sxftd_compute_sx(sxfti);

  if (sx_error != 0
      && sx_error != SXFTD_ERRNO_UNBALANCED_XACTION)
  {
       g_critical("sxftd_compute_sx after ok_clicked [%d]", sx_error);
  }
  else
  {
    if ( sx_error == SXFTD_ERRNO_UNBALANCED_XACTION ) {
            gnc_error_dialog( gnc_ui_get_toplevel(), "%s",
                              _( "The Scheduled Transaction is unbalanced. "
                                 "You are strongly encouraged to correct this situation." ) );
    }
    book = gnc_get_current_book ();
    sxes = gnc_book_get_schedxactions(book);
    gnc_sxes_add_sx(sxes, sxfti->sx);
  }

  sxftd_close(sxfti, FALSE);
  return;
}

/**
 * Update start date... right now we always base this off the transaction
 * start date, but ideally we want to respect what the user has in the field,
 * somehow.
 **/
static void
sxftd_freq_combo_changed( GtkWidget *w, gpointer user_data )
{
  SXFromTransInfo *sxfti = (SXFromTransInfo*)user_data;
  GDate date, nextDate;
  time_t tmp_tt;
  struct tm *tmpTm;
  GList *schedule = NULL;

  tmp_tt = xaccTransGetDate( sxfti->trans );
  g_date_set_time_t( &date, tmp_tt );

  g_date_clear(&nextDate, 1);
  sxftd_update_schedule(sxfti, &date, &schedule);
  recurrenceListNextInstance(schedule, &date, &nextDate);

  tmpTm = g_new0( struct tm, 1 );
  g_date_to_struct_tm( &nextDate, tmpTm );
  tmp_tt = mktime( tmpTm );
  g_free( tmpTm );
  gnc_date_edit_set_time( sxfti->startDateGDE, tmp_tt );

  recurrenceListFree(&schedule);
  sxftd_update_example_cal( sxfti );
}

static void
sxftd_advanced_clicked(SXFromTransInfo *sxfti)
{
  guint sx_error = sxftd_compute_sx(sxfti);
  GncSxEditorDialog *adv_edit_dlg;
  GMainContext *context;

  if ( sx_error != 0
       && sx_error != SXFTD_ERRNO_UNBALANCED_XACTION )
  {
       // unbalanced-xaction is "okay", since this is also checked for by
       // the advanced editor.
       g_warning("something bad happened in sxftd_compute_sx [%d]", sx_error);
       return;
  }
  gtk_widget_hide( sxfti->dialog );
  /* force a gui update. */
  context = g_main_context_default();
  while (g_main_context_iteration(context, FALSE));

  adv_edit_dlg =
    gnc_ui_scheduled_xaction_editor_dialog_create(sxfti->sx,
                                                  TRUE /* newSX */);
  /* close ourself, since advanced editing entails us, and there are sync
   * issues otherwise. */
  sxftd_close(sxfti, FALSE);
}

static void
sxftd_destroy( GtkWidget *w, gpointer user_data )
{
  SXFromTransInfo *sxfti = (SXFromTransInfo*)user_data;

  if ( sxfti->sx ) {
	gnc_sx_begin_edit(sxfti->sx);
    xaccSchedXactionDestroy(sxfti->sx);
    sxfti->sx = NULL;
  }

  g_object_unref(G_OBJECT(sxfti->dense_cal_model));
  g_object_unref(G_OBJECT(sxfti->example_cal));

  /* FIXME: do we need to clean up the GladeXML pointer? */

  g_free(sxfti);
}

static void
gnc_sx_trans_window_response_cb (GtkDialog *dialog,
                                gint response,
                                gpointer data)
{
	SXFromTransInfo *sxfti = (SXFromTransInfo *)data;

	ENTER(" dialog %p, response %d, sx %p", dialog, response, sxfti);
    switch (response) {
    case GTK_RESPONSE_OK:
         g_debug(" OK");
         sxftd_ok_clicked(sxfti);
         break;
    case SXFTD_RESPONSE_ADVANCED:
         g_debug(" ADVANCED");
         sxftd_advanced_clicked(sxfti);
         break;
    case GTK_RESPONSE_CANCEL:
    default:
         g_debug(" CANCEL");
         sxftd_close(sxfti, TRUE);
         break;

	}
	LEAVE(" ");
}

/**
 * Update the example calendar; make sure to take into account the end
 * specification.
 **/
static void
sxftd_update_example_cal( SXFromTransInfo *sxfti )
{
  struct tm *tmpTm;
  time_t tmp_tt;
  GDate date, startDate, nextDate;
  GList *schedule = NULL;
  getEndTuple get;

  get = sxftd_get_end_info( sxfti );

  tmp_tt = gnc_date_edit_get_date( sxfti->startDateGDE );
  tmpTm = g_new0( struct tm, 1 );
  *tmpTm = *localtime( &tmp_tt );
  g_date_clear(&date, 1);
  g_date_set_day( &date, tmpTm->tm_mday );
  g_date_set_month( &date, tmpTm->tm_mon+1 );
  g_date_set_year( &date, tmpTm->tm_year+1900 );
  g_free( tmpTm );

  sxftd_update_schedule(sxfti, &date, &schedule);

  /* go one day before what's in the box so we can get the correct start
   * date. */
  startDate = date;
  g_date_subtract_days(&date, 1);
  g_date_clear(&nextDate, 1);
  recurrenceListNextInstance(schedule, &date, &nextDate);

  {
      GtkWidget *w;
      gchar *name;
      /* get the name */
      w = glade_xml_get_widget(sxfti->gxml, SXFTD_NAME_ENTRY);
      name = gtk_editable_get_chars(GTK_EDITABLE(w), 0, -1);
      gnc_dense_cal_store_update_name(sxfti->dense_cal_model, name);
      g_free(name);
  }

  {
      gchar *schedule_desc;
      schedule_desc = recurrenceListToCompactString(schedule);
      gnc_dense_cal_store_update_info(sxfti->dense_cal_model, schedule_desc);
      g_free(schedule_desc);
  }

  switch (get.type)
  {
  case NEVER_END:
    gnc_dense_cal_store_update_recurrences_no_end(sxfti->dense_cal_model, &startDate, schedule);
    break;
  case END_ON_DATE:
    gnc_dense_cal_store_update_recurrences_date_end(sxfti->dense_cal_model, &startDate, schedule, &get.end_date);
    break;
  case END_AFTER_N_OCCS:
    gnc_dense_cal_store_update_recurrences_count_end(sxfti->dense_cal_model, &startDate, schedule, get.n_occurrences);
    break;
  default:
      g_warning("unknown get.type [%d]\n", get.type);
    break;
  }

  gnc_dense_cal_set_month( sxfti->example_cal, g_date_get_month( &startDate ) );
  gnc_dense_cal_set_year( sxfti->example_cal, g_date_get_year( &startDate ) );

  recurrenceListFree(&schedule);
}

/**
 * Callback to update the calendar
 **/
static void
sxftd_update_excal_adapt( GObject *o, gpointer ud )
{
  SXFromTransInfo *sxfti = (SXFromTransInfo*)ud;
  sxftd_update_example_cal( sxfti );
}

void
gnc_sx_create_from_trans( Transaction *trans )
{
  int errno;
  SXFromTransInfo *sxfti = g_new0( SXFromTransInfo, 1);

  sxfti->gxml = gnc_glade_xml_new(SX_GLADE_FILE,
				  SXFTD_DIALOG_GLADE_NAME);

  sxfti->dialog = glade_xml_get_widget(sxfti->gxml,
				       SXFTD_DIALOG_GLADE_NAME);

  sxfti->trans = trans;

  sxfti->sx = xaccSchedXactionMalloc(gnc_get_current_book ());

  if ( (errno = sxftd_init( sxfti )) < 0 ) {
          if ( errno == SXFTD_ERRNO_OPEN_XACTION )
          {
                  gnc_error_dialog( gnc_ui_get_toplevel(), "%s",
                                    _( "Cannot create a Scheduled Transaction "
                                       "from a Transaction currently "
                                       "being edited. Please Enter the "
                                       "Transaction before Scheduling." ) );
                  sxftd_close( sxfti, TRUE );
                  return;
          }
          else
          {
               g_error("sxftd_init: %d", errno);
          }
  }

  gtk_widget_show(GTK_WIDGET(sxfti->dialog));
}
