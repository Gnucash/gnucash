/********************************************************************\
 * gnc-frequency.c -- GnuCash widget for frequency editing.         *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <time.h>
#include <glib.h>

#include "gnc-engine-util.h"
#include "gnc-frequency.h"
#include "dialog-utils.h"
#include "FreqSpec.h"

static short module = MOD_SX;

/** Private Defs ********************/

typedef enum {
  GNCFREQ_VALUE_CHANGED,
  LAST_SIGNAL
} GNCF_Signals;

static gint gnc_frequency_signals[LAST_SIGNAL] = { 0 };

/** Private Prototypes ********************/

static void update_cal( GNCFrequency *gf, GtkCalendar *cal );
static void mark_calendar( GtkCalendar *cal, FreqSpec *fs );
static void update_appropriate_cal( GNCFrequency *gf );

static void gnc_frequency_class_init( GNCFrequencyClass *klass );

static void free_resources( GtkObject *o, gpointer d );

static void freq_option_value_changed( GtkMenuShell *b, gpointer d );
static void start_date_changed( GnomeDateEdit *gde, gpointer d );

static void spin_changed_helper( GtkAdjustment *adj, gpointer d );

static void monthly_sel_changed( GtkButton *b, gpointer d );
static void semimonthly_sel_changed( GtkButton *b, gpointer d );
static void yearly_sel_changed( GtkButton *b, gpointer d );


/** Static Inits ********************/

static struct pageDataTuple PAGES[] = {
  {  0, UIFREQ_NONE,         "None" },
  {  1, UIFREQ_ONCE,         "Once" },
  {  2, UIFREQ_DAILY,        "Daily" },
  {  3, UIFREQ_DAILY_MF,     "Daily [M-F]" },
  {  4, UIFREQ_WEEKLY,       "Weekly" },
  {  5, UIFREQ_BI_WEEKLY,    "Bi-Weekly" },
  {  6, UIFREQ_SEMI_MONTHLY, "Semi-Monthly" },
  {  7, UIFREQ_MONTHLY,      "Monthly" },
  {  8, UIFREQ_QUARTERLY,    "Quarterly" },
  {  9, UIFREQ_TRI_ANUALLY,  "Tri-Anually" },
  { 10, UIFREQ_SEMI_YEARLY,  "Semi-Yearly" },
  { 11, UIFREQ_YEARLY,       "Yearly" },
  { 0, 0, 0 }
};

static char *CHECKBOX_NAMES[] = {
  "wd_check_sun",
  "wd_check_mon",
  "wd_check_tue",
  "wd_check_wed",
  "wd_check_thu",
  "wd_check_fri",
  "wd_check_sat",
  NULL
};

/** Implementations ********************/

guint
gnc_frequency_get_type()
{
  static guint gncfreq_type = 0;
  if ( ! gncfreq_type ) {
    GtkTypeInfo gncfreq_info =
    {
      "GNCFrequency",
      sizeof(GNCFrequency),
      sizeof(GNCFrequencyClass),
      (GtkClassInitFunc)gnc_frequency_class_init,
      (GtkObjectInitFunc)gnc_frequency_init,
      (GtkArgSetFunc)NULL,
      (GtkArgGetFunc)NULL
    };
    
    gncfreq_type = gtk_type_unique( gtk_vbox_get_type(), &gncfreq_info );
  }
  return gncfreq_type;
}

static void
gnc_frequency_class_init( GNCFrequencyClass *klass )
{
  GtkObjectClass *objectClass;


  objectClass = (GtkObjectClass*)klass;
  gnc_frequency_signals[GNCFREQ_VALUE_CHANGED] =
    gtk_signal_new( "value_changed",
                    GTK_RUN_FIRST,
                    objectClass->type,
                    GTK_SIGNAL_OFFSET( GNCFrequencyClass, value_changed ),
                    gtk_signal_default_marshaller, GTK_TYPE_NONE, 0 );

  gtk_object_class_add_signals( objectClass, gnc_frequency_signals, LAST_SIGNAL );

  klass->value_changed = NULL;
}

void
gnc_frequency_init( GNCFrequency *gf )
{
  int    i, j;
  GtkVBox  *vb;
  GtkWidget   *o, *win;
  GtkAdjustment  *adj;

  static char *cals[] = {
    "daily_cal",
    "dailymf_cal",
    "weekly_cal",
    "semimonthly_cal",
    "monthly_cal",
    NULL
  };

  static struct optionMenuTuple {
    char *name;
    void (*fn)();
  } optionMenus[] = {
    { "freq_option",        freq_option_value_changed },
    { "semimonthly_first",  semimonthly_sel_changed },
    { "semimonthly_second", semimonthly_sel_changed },
    { "monthly_day",        monthly_sel_changed },
    { "quarterly_occur",    NULL },
    { "quarterly_day",      NULL },
    { "triyearly_occur",    NULL },
    { "triyearly_day",      NULL },
    { "semiyearly_occur",   NULL },
    { "semiyearly_day",     NULL },
    { "yearly_month",       yearly_sel_changed },
    { "yearly_day",         yearly_sel_changed },
    { NULL,                 NULL }
  };

  static struct spinvalTuple {
    char *name;
    void (*fn)();
  } spinVals[] = {
    { "daily_spin",       spin_changed_helper },
    { "dailymf_spin",     spin_changed_helper },
    { "weekly_spin",      spin_changed_helper },
    { "semimonthly_spin", spin_changed_helper },
    { "monthly_spin",     spin_changed_helper },
    { NULL,               NULL }
  };

  gf->gxml = gnc_glade_xml_new( "sched-xact.glade", "gncfreq_vbox" );
  o = glade_xml_get_widget( gf->gxml, "gncfreq_nb" );
  gf->nb = GTK_NOTEBOOK(o);
  o = glade_xml_get_widget( gf->gxml, "freq_option" );
  gf->freqOpt = GTK_OPTION_MENU(o);
  o = glade_xml_get_widget( gf->gxml, "sxe_start_date" );
  gf->startDate = GNOME_DATE_EDIT(o);
  vb = GTK_VBOX( glade_xml_get_widget( gf->gxml, "gncfreq_vbox" ) );
  gf->vb = vb;
  gtk_container_add( GTK_CONTAINER(&gf->widget), GTK_WIDGET(gf->vb) );

  /* connect to the destroy signal for cleanup */
  gtk_signal_connect( GTK_OBJECT(gf->vb), "destroy",
                      GTK_SIGNAL_FUNC(free_resources), gf );

  /* intially fix the calendars. */
  for ( j=0; cals[j] != NULL; j++ ) {
    o = glade_xml_get_widget( gf->gxml, cals[j] );
    gtk_calendar_select_day( GTK_CALENDAR(o), 0 );
  }

  /* initialize the option menus */
  for ( i=0; optionMenus[i].name != NULL; i++ ) {
    o = glade_xml_get_widget( gf->gxml, optionMenus[i].name );
    gnc_option_menu_init( GTK_WIDGET(o) );
    if ( optionMenus[i].fn != NULL ) {
      o = gtk_option_menu_get_menu(GTK_OPTION_MENU(o));
      /* FIXME: having the user-data be a struct of a
         calendar name and the GNCFrequency would allow a
         single callback fn */
      gtk_signal_connect( GTK_OBJECT(o), "selection-done",
                          GTK_SIGNAL_FUNC(optionMenus[i].fn), gf );
    }
  }

  /* initialize the spin buttons */
  for ( i=0; spinVals[i].name != NULL; i++ ) {
    o = glade_xml_get_widget( gf->gxml,
                              spinVals[i].name );
    if ( spinVals[i].fn != NULL ) {
      adj = gtk_spin_button_get_adjustment( GTK_SPIN_BUTTON(o) );
      gtk_signal_connect( GTK_OBJECT(adj), "value_changed",
                          GTK_SIGNAL_FUNC(spinVals[i].fn), gf );
    }
  }

  gtk_widget_show_all( GTK_WIDGET(&gf->widget) );

  /* respond to start date changes */
  gtk_signal_connect( GTK_OBJECT(gf->startDate), "date_changed",
                      GTK_SIGNAL_FUNC(start_date_changed), gf );

}

GtkWidget *
gnc_frequency_new( FreqSpec *fs )
{
  GNCFrequency  *toRet;
  toRet = gtk_type_new( gnc_frequency_get_type() );
  gnc_frequency_setup( toRet, fs );
  return GTK_WIDGET(toRet);
}

void
gnc_frequency_setup( GNCFrequency *gf, FreqSpec *fs )
{
  UIFreqType  uift;
  int    page;
  time_t    tmpTT;
  struct tm  *tmpTm;
  GtkWidget  *o;
  FreqSpec  *subFS;
  GList    *list;
  int    tmpInt;
  int    i;
  int    idx;
  char    *str;

  page = 0;
  if ( fs != NULL ) {
    uift = xaccFreqSpecGetUIType( fs );
  } else {
    uift = UIFREQ_NONE;
  }

  for ( i=0; i<UIFREQ_NUM_UI_FREQSPECS+1; i++ ) {
    if ( PAGES[i].uiFTVal == uift ) {
      page = PAGES[i].idx;
    }
  }

  if ( page == 0 ) {
    PWARN( "WARN: Page index \"%d\" probably not right\n",
           page );
  }
  gtk_notebook_set_page( gf->nb, page );
  gtk_option_menu_set_history( gf->freqOpt, page );

  if ( fs == NULL ) {
    return;
  }

  switch ( uift ) {
  case UIFREQ_NONE:
    break;
  case UIFREQ_ONCE:
    /* set the date */

#if 0
    tmpTT = fs->specData.dateAnchor[0];
#endif /* 0 */
    tmpTT = 0;
    if ( tmpTT == 0 ) {
      tmpTT = time(NULL);
    }
    tmpTm = gmtime( &tmpTT );
    o = glade_xml_get_widget( gf->gxml, "once_cal" );
    gtk_calendar_select_month( GTK_CALENDAR(o),
                               tmpTm->tm_year+1900,
                               tmpTm->tm_mon );
    gtk_calendar_select_day( GTK_CALENDAR(o),
                             tmpTm->tm_mday );
    break;
  case UIFREQ_DAILY:
    /* set the mult */
    o = glade_xml_get_widget( gf->gxml, "daily_spin" );
#if 0
    gtk_spin_button_set_value( GTK_SPIN_BUTTON( o ),
                               fs->specData.dateAnchor[0] );
#endif /* 0 */
    
    o = glade_xml_get_widget( gf->gxml, "daily_cal" );
    update_cal( gf, GTK_CALENDAR(o) );
    break;
  case UIFREQ_DAILY_MF:
    /* FIXME */
    /* set the mult */
    break;
  case UIFREQ_WEEKLY:
    tmpInt = -1;
    list = xaccFreqSpecCompositeGet( fs );
    do {
      subFS = (FreqSpec*)(list->data);
      if ( tmpInt == -1 ) {
#if 0
        tmpInt = subFS->specData.dateAnchor[0];
#endif /* 0 */
      }
#if 0
      str = CHECKBOX_NAMES[subFS->specData.dateAnchor[1]];
#endif /* 0 */
      str = "Monday";
      o = glade_xml_get_widget( gf->gxml, str );
      gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(o), TRUE );
    } while ( (list = g_list_next(list)) );
    o = glade_xml_get_widget( gf->gxml, "weekly_spin" );
    gtk_spin_button_set_value( GTK_SPIN_BUTTON(o), tmpInt );
    o = glade_xml_get_widget( gf->gxml, "weekly_cal" );
    update_cal( gf, GTK_CALENDAR(o) );
    break;
  case UIFREQ_BI_WEEKLY:
    /* set the initial date...? */
    break;
  case UIFREQ_SEMI_MONTHLY:
    list = xaccFreqSpecCompositeGet( fs );
    /* mult */
    o = glade_xml_get_widget( gf->gxml, "semimonthly_spin" );
    subFS = (FreqSpec*)(g_list_nth( list, 0 )->data);
#if 0
    gtk_spin_button_set_value( GTK_SPIN_BUTTON(o),
                               subFS->specData.dateAnchor[0] );
#endif /* 0 */
    /* first date */
    o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 subFS->specData.dateAnchor[1]-1 );
#endif /* 0 */
    /* second date */
    subFS = (FreqSpec*)(g_list_nth( list, 1 )->data);
    o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 subFS->specData.dateAnchor[1]-1 );
#endif /* 0 */

    o = glade_xml_get_widget( gf->gxml, "semimonthly_cal" );
    update_cal( gf, GTK_CALENDAR(o) );
    break;
  case UIFREQ_MONTHLY:
    /* set the mult */
    o = glade_xml_get_widget( gf->gxml, "monthly_spin" );
    
#if 0
    gtk_spin_button_set_value( GTK_SPIN_BUTTON(o),
                               fs->specData.dateAnchor[0] );
#endif /* 0 */
    o = glade_xml_get_widget( gf->gxml, "monthly_day" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 fs->specData.dateAnchor[1]-1 );
#endif /* 0 */
    /* set the day-of-month */
    update_cal( gf, GTK_CALENDAR(o) );
    break;
  case UIFREQ_QUARTERLY:
    o = glade_xml_get_widget( gf->gxml, "quarterly_occur" );
    /* FIXME: based off the start date? */
    o = glade_xml_get_widget( gf->gxml, "quarterly_day" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 fs->specData.dateAnchor[1]-1 );
#endif /* 0 */
    break;
  case UIFREQ_TRI_ANUALLY:
    o = glade_xml_get_widget( gf->gxml, "triyearly_occur" );
    /* FIXME: based off the start date */
    o = glade_xml_get_widget( gf->gxml, "triyearly_day" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 fs->specData.dateAnchor[1]-1 );
#endif /* 0 */
    break;
  case UIFREQ_SEMI_YEARLY:
    o = glade_xml_get_widget( gf->gxml, "semiyearly_occur" );
    /* FIXME: based off the start date */
    o = glade_xml_get_widget( gf->gxml, "semiyearly_day" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 fs->specData.dateAnchor[1]-1 );
#endif /* 0 */
    break;
  case UIFREQ_YEARLY:
    /* set the mult */
    o = glade_xml_get_widget( gf->gxml, "yearly_spin" );
#if 0
    gtk_spin_button_set_value( GTK_SPIN_BUTTON(o),
                               fs->specData.dateAnchor[0]/12 );
#endif /* 0 */
    /* month: wrongly done, natch. */
    o = glade_xml_get_widget( gf->gxml, "yearly_month" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 fs->specData.dateAnchor[2]-1 );
#endif /* 0 */
    /* mday */
    o = glade_xml_get_widget( gf->gxml, "yearly_day" );
#if 0
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 fs->specData.dateAnchor[1]-1 );
#endif /* 0 */
    break;
  default:
    PERR( "unknown ui freq type [%d, %s]\n", __LINE__, __FILE__ );
    break;
  }
  update_appropriate_cal( gf );
}

void
gnc_frequency_save_state( GNCFrequency *gf, FreqSpec *fs, GDate *outStartDate )
{
  gint    page;
  struct tm  *tmpTm;
  guint    day, month, year;
  GtkWidget  *o;
  UIFreqType  uift;
  FreqSpec  *tmpFS;
  gint    tmpInt;
  char    *str;
  int    i;
  GDate    *gd;
  GDate    *gd2;
  time_t    tmpTimeT;

  /* get the current tab */
  page = gtk_notebook_get_current_page( gf->nb );
  /* save into UIFreqSpec */
  uift = PAGES[page].uiFTVal;

  tmpTimeT = gnome_date_edit_get_date( gf->startDate );
  gd = g_date_new();
  g_date_set_time( gd, tmpTimeT );
  if ( outStartDate != NULL ) {
    *outStartDate = *gd;
  }

  /* based on value, parse widget values into FreqSpec */
  switch ( uift ) {
  case UIFREQ_NONE:
    /* hmmm... shouldn't really be allowed. */
    break;
  case UIFREQ_ONCE:
    xaccFreqSpecSetOnceDate( fs, gd );
    xaccFreqSpecSetUIType( fs, uift );
    g_date_free( gd );
    break;
  case UIFREQ_DAILY:
    o = glade_xml_get_widget( gf->gxml, "daily_spin" );
    /* FIXME: initial date should be set correctly. */
    xaccFreqSpecSetDaily( fs, gd, 
                          gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) ) );
    xaccFreqSpecSetUIType( fs, uift );
    g_date_free( gd );
    break;
  case UIFREQ_DAILY_MF:
    /* xaccFreqSpecSetTypes( fs, COMPOSITE, uift ); */
    xaccFreqSpecSetComposite( fs );
    xaccFreqSpecSetUIType( fs, uift );
    gd2 = g_date_new();
    o = glade_xml_get_widget( gf->gxml, "dailymf_spin" );
    tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
    /* Okay.  Assume that the calendar is upgraded to
       support selecting weeks, returning the Sunday selected. */

    /* Normalize to sunday. */
    tmpTm = g_new0( struct tm, 1 );
    g_date_to_struct_tm( gd, tmpTm );
    /* month-day += (week-day - current-week-day ) % 7
       week-day <- 0 */
    tmpTm->tm_mday -= ( tmpTm->tm_wday ) % 7;
    g_date_set_time( gd, mktime( tmpTm ) );

    /* 1 == "mon", 5 == "fri" */
    for ( i=1; i<6; i++ ) {
      *gd2 = *gd; 
      g_date_add_days( gd2, i );
      tmpFS = xaccFreqSpecMalloc();
      xaccFreqSpecSetWeekly( tmpFS, gd2, tmpInt );
      xaccFreqSpecCompositeAdd( fs, tmpFS );
    }
    g_date_free( gd );
    g_date_free( gd2 );
    g_free( tmpTm );
    break;
  case UIFREQ_WEEKLY:
    /* xaccFreqSpecSetTypes( fs, COMPOSITE, uift ); */
    xaccFreqSpecSetComposite( fs );
    o = glade_xml_get_widget( gf->gxml, "weekly_spin" );
    tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );

    /* assume we have a good calendar that allows week selection. */
    
    /* for-now hack: normalize to Sunday. */
    tmpTm = g_new0( struct tm, 1 );
    g_date_to_struct_tm( gd, tmpTm );
    tmpTm->tm_mday -= tmpTm->tm_wday % 7;
    g_date_set_time( gd, mktime( tmpTm ) );

    /* now, go through the check boxes and add composites based on
       that date. */
    for ( i=0; CHECKBOX_NAMES[i]!=NULL; i++ ) {
      str = CHECKBOX_NAMES[i];
      o = glade_xml_get_widget( gf->gxml, str );
      if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) ) ) {
        tmpFS = xaccFreqSpecMalloc();
        /* struct-copy is expected to work, here
           [wish we didn't have to know about the GDate implementation...] */
        gd2 = g_date_new();
        *gd2 = *gd;
        /* Add 'i' days off of Sunday */
        g_date_add_days( gd2, i );
        xaccFreqSpecSetWeekly( tmpFS, gd2, tmpInt );
        g_date_free( gd2 );
        xaccFreqSpecCompositeAdd( fs, tmpFS );
      }
    }
    g_date_free( gd );
    g_free( tmpTm );
    break;
  case UIFREQ_BI_WEEKLY:
    xaccFreqSpecSetUIType( fs, uift );
    o = glade_xml_get_widget( gf->gxml, "biweekly_cal" );
    xaccFreqSpecSetWeekly( fs, gd, 2 );
    g_date_free( gd );
    break;
  case UIFREQ_SEMI_MONTHLY:
    /* FIXME: this is b0rken date calculation for mday>28 */

    /* xaccFreqSpecSetTypes( fs, COMPOSITE, uift ); */
    xaccFreqSpecSetComposite( fs );
    xaccFreqSpecSetUIType( fs, uift );
    
    o = glade_xml_get_widget( gf->gxml, "semimonthly_spin" );
    tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );

    o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
    day = gnc_option_menu_get_active( GTK_WIDGET(o) )+1;
    tmpFS = xaccFreqSpecMalloc();
    tmpTm = g_new0( struct tm, 1 );
    g_date_to_struct_tm( gd, tmpTm );
    if ( day >= tmpTm->tm_mday ) {
      /* next month */
      tmpTm->tm_mon += 1;
    }
    /* else, this month */
    tmpTm->tm_mday = day;
    g_date_set_time( gd, mktime( tmpTm ) );
    xaccFreqSpecSetMonthly( tmpFS, gd, tmpInt );
    g_date_free( gd );
    g_free( tmpTm );
    xaccFreqSpecCompositeAdd( fs, tmpFS );

    o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
    day = gnc_option_menu_get_active( GTK_WIDGET(o) )+1;
    tmpFS = xaccFreqSpecMalloc();
    tmpTimeT = gnome_date_edit_get_date( gf->startDate );
    gd = g_date_new();
    g_date_set_time( gd, tmpTimeT );
    tmpTm = g_new0( struct tm, 1 );
    g_date_to_struct_tm( gd, tmpTm );
    if ( day >= tmpTm->tm_mday ) {
      /* next month */
      tmpTm->tm_mon += 1;
    }
    /* else, this month */
    tmpTm->tm_mday = day;
    g_date_set_time( gd, mktime( tmpTm ) );
    xaccFreqSpecSetMonthly( tmpFS, gd, tmpInt );
    g_date_free( gd );
    g_free( tmpTm );
    xaccFreqSpecCompositeAdd( fs, tmpFS );

    break;
  case UIFREQ_MONTHLY:
    /* xaccFreqSpecSetTypes( fs, MONTHLY, uift ); */
    o = glade_xml_get_widget( gf->gxml, "monthly_spin" );
    tmpInt = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(o));
    tmpTm = g_new0( struct tm, 1 );
    g_date_to_struct_tm( gd, tmpTm );

    o = glade_xml_get_widget( gf->gxml, "monthly_day" );
    day = gnc_option_menu_get_active( GTK_WIDGET(o) ) + 1;
    tmpTm->tm_mday = day;
    g_date_set_time( gd, mktime( tmpTm ) );
    xaccFreqSpecSetMonthly( fs, gd, tmpInt );
    xaccFreqSpecSetUIType( fs, uift );

    g_free( tmpTm );
    g_date_free( gd );
    break;
  case UIFREQ_QUARTERLY:
    /* xaccFreqSpecSetTypes( fs, MONTHLY, uift ); */

    /* o = glade_xml_get_widget( gf->gxml, "quarterly_day" ); */
    /* tmpInt = gnc_option_menu_get_active( GTK_WIDGET(o) ); */
    xaccFreqSpecSetMonthly( fs, gd, 3 );
    xaccFreqSpecSetUIType( fs, uift );
    g_date_free( gd );
    
    break;
  case UIFREQ_TRI_ANUALLY:
    /* xaccFreqSpecSetTypes( fs, MONTHLY, uift ); */
    /* o = glade_xml_get_widget( gf->gxml, "triyearly_day" ); */
    /* tmpInt = gnc_option_menu_get_active( GTK_WIDGET(o) ); */
    xaccFreqSpecSetMonthly( fs, gd, 4 );
    xaccFreqSpecSetUIType( fs, uift );
    g_date_free( gd );
    break;
  case UIFREQ_SEMI_YEARLY:
    /* xaccFreqSpecSetTypes( fs, MONTHLY, uift ); */
    /* o = glade_xml_get_widget( gf->gxml, "semiyearly_day" ); */
    /* tmpInt = gnc_option_menu_get_active( GTK_WIDGET(o) ); */
    xaccFreqSpecSetMonthly( fs, gd, 6 );
    xaccFreqSpecSetUIType( fs, uift );
    g_date_free( gd );
    break;
  case UIFREQ_YEARLY:
    /* xaccFreqSpecSetTypes( fs, MONTHLY, uift ); */
    /*  get the multiplier */
    /* monthly:12*mult,start date + mday */
    o = glade_xml_get_widget( gf->gxml, "yearly_spin" );
    tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
    xaccFreqSpecSetMonthly( fs, gd, tmpInt * 12 );
    xaccFreqSpecSetUIType( fs, uift );
    g_date_free( gd );
    break;
  default:
    PERR( "Unknown UIFreqType %d [%d, %s]\n",
          uift, __LINE__, __FILE__ );
    break;
  }
}

static void
mark_calendar( GtkCalendar *cal, FreqSpec *fs )
{
  GDate    *gdNow;
  GDate    *gdNext;
  struct tm  *tmpTm;
  guint    year, month, day;

  gdNow = g_date_new();
  gdNext = g_date_new();

  gtk_calendar_get_date( cal, &year, &month, &day );
  tmpTm = g_new0( struct tm, 1 );
  tmpTm->tm_hour = tmpTm->tm_min = tmpTm->tm_sec = 0;
  tmpTm->tm_year = year-1900;
  tmpTm->tm_mon  = month;
  tmpTm->tm_mday = 0;
  g_date_set_time( gdNow, mktime(tmpTm) );
  
  gtk_calendar_freeze( cal );
  gtk_calendar_clear_marks( cal );
  gtk_calendar_select_day( cal, 0 );
  
  xaccFreqSpecGetNextInstance( fs, gdNow, gdNext );
  g_date_to_struct_tm( gdNext, tmpTm );
  while ( tmpTm->tm_mon == month ) {
    gtk_calendar_mark_day( cal, tmpTm->tm_mday );
    *gdNow = *gdNext;
    xaccFreqSpecGetNextInstance( fs, gdNow, gdNext );
    g_date_to_struct_tm( gdNext, tmpTm );
  }
  gtk_calendar_thaw( cal );

  g_free( tmpTm );
  g_date_free( gdNow );
  g_date_free( gdNext );
}

static void
update_cal( GNCFrequency *gf, GtkCalendar *cal )
{
  FreqSpec  *fs;
  fs = xaccFreqSpecMalloc();
  gnc_frequency_save_state( gf, fs, NULL );
  mark_calendar( cal, fs );
  xaccFreqSpecFree( fs );
}

static void
update_appropriate_cal( GNCFrequency *gf )
{
  GtkWidget  *o;
  gint    page;
  UIFreqType  uift;

  o = NULL;
  page = gtk_notebook_get_current_page( gf->nb );
  /* save into UIFreqSpec */
  uift = PAGES[page].uiFTVal;
  /* based on value, parse widget values into FreqSpec */
  switch ( uift ) {
  case UIFREQ_NONE:
  case UIFREQ_DAILY:
    o = glade_xml_get_widget( gf->gxml, "daily_cal" );
    break;
  case UIFREQ_DAILY_MF:
    o = glade_xml_get_widget( gf->gxml, "daily_mf_cal" );
    break;
  case UIFREQ_WEEKLY:
    o = glade_xml_get_widget( gf->gxml, "weekly_cal" );
    break;
  case UIFREQ_BI_WEEKLY:
    o = glade_xml_get_widget( gf->gxml, "biweekly_cal" );
    break;
  case UIFREQ_SEMI_MONTHLY:
    o = glade_xml_get_widget( gf->gxml, "semimonthly_cal" );
    break;
  case UIFREQ_MONTHLY:
    o = glade_xml_get_widget( gf->gxml, "monthly_cal" );
    break;
  case UIFREQ_QUARTERLY:
  case UIFREQ_TRI_ANUALLY:
  case UIFREQ_SEMI_YEARLY:
  case UIFREQ_YEARLY:
  default:
    break;
  }

  if ( o )
    update_cal( gf, GTK_CALENDAR(o) );
}

static void 
free_resources( GtkObject *o, gpointer d )
{
  GNCFrequency *gf = (GNCFrequency*)d;
  DEBUG( "In GNCFrequency::free_resources\n" );

  gtk_widget_destroy( glade_xml_get_widget( gf->gxml,
                                            "GNCFrequency widget" ) );
}

static void
spin_changed_helper( GtkAdjustment *adj, gpointer d )
{
  update_appropriate_cal( (GNCFrequency*)d );
}

static void
daily_spin_value_changed( GtkAdjustment *adj, gpointer d )
{
  GtkCalendar  *cal;

  cal = GTK_CALENDAR( glade_xml_get_widget( ((GNCFrequency*)d)->gxml,
                                            "daily_cal" ) );
  update_cal( d, cal );
}

static void
dailymf_spin_value_changed( GtkAdjustment *adj, gpointer d )
{
  GtkCalendar  *cal;

  cal = GTK_CALENDAR( glade_xml_get_widget( ((GNCFrequency*)d)->gxml,
                                            "dailymf_cal" ) );
  update_cal( d, cal );
}

static void
weekly_spin_value_changed( GtkAdjustment *adj, gpointer d )
{
  GtkCalendar  *cal;

  cal = GTK_CALENDAR( glade_xml_get_widget( ((GNCFrequency*)d)->gxml,
                                            "weekly_cal" ) );

  update_cal( d, cal );
}

static void
monthly_sel_changed( GtkButton *b, gpointer d )
{
  GNCFrequency  *gf;
  GtkWidget  *o;
  GDate    *gd;
  guint    dayOfMonth;
  struct tm  *tmptm;
  time_t    tmptt;
  
  gf = (GNCFrequency*)d;

  o = glade_xml_get_widget( ((GNCFrequency*)d)->gxml,
                            "monthly_day" );
  dayOfMonth = gnc_option_menu_get_active( GTK_WIDGET(o) ) + 1;

  tmptt = gnome_date_edit_get_date( gf->startDate );
  tmptm = localtime( &tmptt );
  while ( ! g_date_valid_dmy( dayOfMonth,
                              tmptm->tm_mon + 1,
                              tmptm->tm_year+1900 ) ) {
    dayOfMonth -= 1;
  }
  tmptm->tm_mday = dayOfMonth;
  tmptt = mktime( tmptm );
  gnome_date_edit_set_time( gf->startDate, tmptt );

  update_appropriate_cal( (GNCFrequency*)d );

}

static void
monthly_spin_value_changed( GtkAdjustment *adj, gpointer d )
{
  update_appropriate_cal( (GNCFrequency*)d );
  /* monthly_sel_changed( NULL, d ); */
}

static void
semimonthly_spin_value_changed( GtkAdjustment *adj, gpointer d )
{
  update_appropriate_cal( (GNCFrequency*)d );
  /* semimonthly_sel_changed( NULL, d ); */
}

static void
semimonthly_sel_changed( GtkButton *b, gpointer d )
{
  GNCFrequency  *gf;
  GtkWidget  *o;
  gint    tmpint;
  time_t    tmptt;
  struct tm  *tmptm;

  gf = (GNCFrequency*)d;

  tmptt = gnome_date_edit_get_date( gf->startDate );
  tmptm = localtime( &tmptt );

  o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
  tmpint = gnc_option_menu_get_active( GTK_WIDGET(o) )+1;
  o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
  if ( tmpint > gnc_option_menu_get_active( GTK_WIDGET(o) )+1 ) {
    tmpint = gnc_option_menu_get_active( GTK_WIDGET(o) )+1;
  }

  tmptm->tm_mday = tmpint;
  while ( ! g_date_valid_dmy( tmptm->tm_mday,
                              tmptm->tm_mon+1,
                              tmptm->tm_year+1900 ) ) {
    tmptm->tm_mday -= 1;
  }
  tmptt = mktime( tmptm );
  gnome_date_edit_set_time( gf->startDate, tmptt );

  update_appropriate_cal( gf );
}

static void
yearly_sel_changed( GtkButton *b, gpointer d )
{
  GNCFrequency  *gf;
  GtkWidget  *o;
  time_t    tmptt;
  struct tm  *tmptm;
  gint    tmpint;

  gf = (GNCFrequency*)d;

  tmptt = gnome_date_edit_get_date( gf->startDate );
  tmptm = localtime( &tmptt );

  o = glade_xml_get_widget( gf->gxml, "yearly_month" );
  tmptm->tm_mon = gnc_option_menu_get_active( GTK_WIDGET(o) );
  o = glade_xml_get_widget( gf->gxml, "yearly_day" );
  tmptm->tm_mday = gnc_option_menu_get_active( GTK_WIDGET(o) )+1;

  /* correct for
     option_menu_selected_day > min(31,correct_days_in_month)
     problem */
  while ( ! g_date_valid_dmy( tmptm->tm_mday,
                              tmptm->tm_mon+1,
                              tmptm->tm_year+1900 ) ) {
    tmptm->tm_mday -= 1;
  }

  tmptt = mktime( tmptm );
  gnome_date_edit_set_time( gf->startDate, tmptt );

  update_appropriate_cal( gf );
}

void
freq_option_value_changed( GtkMenuShell *b, gpointer d )
{
  GtkObject  *o;
  int    optIdx;

  optIdx = gnc_option_menu_get_active( GTK_WIDGET(((GNCFrequency*)d)->freqOpt) );
  gtk_notebook_set_page( ((GNCFrequency*)d)->nb, optIdx );
}

static void
year_range_menu_helper( GtkWidget *dayOptMenu,
                        GtkWidget *occurOptMenu,
                        gint monthsInRange,
                        time_t startDate )
{
  struct tm  *tmpTm;
  tmpTm = localtime( &startDate );
  gtk_option_menu_set_history( GTK_OPTION_MENU(occurOptMenu),
                               tmpTm->tm_mon % monthsInRange );
  gtk_option_menu_set_history( GTK_OPTION_MENU(dayOptMenu),
                               tmpTm->tm_mday - 1 );
}

static void
start_date_changed( GnomeDateEdit *gde, gpointer d )
{
  GNCFrequency  *gf;
  GtkWidget  *o;
  struct tm  *tmpTm;
  time_t    dateFromGDE;
  gint    tmpint;
  gint    page;
  UIFreqType  uift;
  
  gf = (GNCFrequency*)d;

  dateFromGDE = gnome_date_edit_get_date( gde );

  /* FIXME: update...
     X monthly: day of month
     X semi-monthly: lowest day
     {quarterly, tri-yearly, semi-yearly, X yearly}: day of month
  */

  page = gtk_notebook_get_current_page( gf->nb );
  uift = PAGES[page].uiFTVal;

  o = NULL;

  switch (uift) {
  case UIFREQ_MONTHLY:
    o = glade_xml_get_widget( gf->gxml, "monthly_day" );
    tmpTm = localtime( &dateFromGDE );
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 (tmpTm->tm_mday-1) );
    break;
  case UIFREQ_SEMI_MONTHLY:
    /* if first < second, update first
       else, update second. */
    o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
    tmpint = gnc_option_menu_get_active( GTK_WIDGET(o) )+1;
    o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
    if ( tmpint < gnc_option_menu_get_active( GTK_WIDGET(o) )+1 ) {
      o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
    }

    tmpTm = localtime( &dateFromGDE );
    gtk_option_menu_set_history( GTK_OPTION_MENU(o), tmpTm->tm_mday-1 );
    break;
  case UIFREQ_QUARTERLY:
    year_range_menu_helper( glade_xml_get_widget( gf->gxml, "quarterly_day" ),
                            glade_xml_get_widget( gf->gxml, "quarterly_occur" ),
                            3, dateFromGDE );
    break;
  case UIFREQ_TRI_ANUALLY:
    year_range_menu_helper( glade_xml_get_widget( gf->gxml, "triyearly_day" ),
                            glade_xml_get_widget( gf->gxml, "triyearly_occur" ),
                            4, dateFromGDE );
    break;
  case UIFREQ_SEMI_YEARLY:
    year_range_menu_helper( glade_xml_get_widget( gf->gxml, "semiyearly_day" ),
                            glade_xml_get_widget( gf->gxml, "semiyearly_occur" ),
                            6, dateFromGDE );
    break;
  case UIFREQ_YEARLY:
    o = glade_xml_get_widget( gf->gxml, "yearly_month" );
    tmpTm = localtime( &dateFromGDE );
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 tmpTm->tm_mon );
    o = glade_xml_get_widget( gf->gxml, "yearly_day" );
    gtk_option_menu_set_history( GTK_OPTION_MENU(o),
                                 tmpTm->tm_mday-1 );
    break;
  default:
    PERR( "unknown uift value %d\n", uift );
    break;
  }
  update_appropriate_cal( gf );
}
