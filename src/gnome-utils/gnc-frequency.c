/********************************************************************\
 * gnc-frequency.c -- GnuCash widget for frequency editing.         *
 * Copyright (C) 2001,2002 Joshua Sled <jsled@asynchronous.org>     *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gtypes.h>
#include "glib-compat.h"
#include <math.h>
#include <time.h>

#include "FreqSpec.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-frequency.h"
#include "gnc-ui-util.h"

static QofLogModule log_module = GNC_MOD_SX;

/** Private Defs ********************/

typedef enum {
  GNCFREQ_CHANGED,
  LAST_SIGNAL
} GNCF_Signals;

static guint gnc_frequency_signals[LAST_SIGNAL] = { 0 };

/** Private Prototypes ********************/

static void gnc_frequency_class_init( GNCFrequencyClass *klass );

static void freq_combo_changed( GtkComboBox *b, gpointer d );
static void start_date_changed( GNCDateEdit *gde, gpointer d );
static void spin_changed_helper( GtkAdjustment *adj, gpointer d );

static void weekly_days_changed( GtkButton *b, gpointer d );

static void monthly_sel_changed( GtkButton *b, gpointer d );
static void semimonthly_sel_changed( GtkButton *b, gpointer d );
static void yearly_sel_changed( GtkButton *b, gpointer d );
static void quarterly_sel_changed( GtkButton *b, gpointer d );
static void triyearly_sel_changed( GtkButton *b, gpointer d );
static void semiyearly_sel_changed( GtkButton *b, gpointer d );

static void year_range_sels_changed( GNCFrequency *gf,
                                     int monthsInRange,
                                     GtkWidget *occurW,
                                     GtkWidget *dayOfMonthW );
static void year_range_menu_helper( GtkWidget *dayOptMenu,
                                    GtkWidget *occurOptMenu,
                                    gint monthsInRange,
                                    time_t date );

/** Static Inits ********************/

static const struct pageDataTuple PAGES[] = {
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

static const char *CHECKBOX_NAMES[] = {
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

GType
gnc_frequency_get_type()
{
	static GType gncfreq_type = 0;
        if (gncfreq_type == 0) {
                static GTypeInfo gncfreq_info = {
			sizeof(GNCFrequencyClass),
			NULL,
			NULL,
			(GClassInitFunc)gnc_frequency_class_init,
			NULL,
			NULL,
			sizeof(GNCFrequency),
			0,
			(GInstanceInitFunc)gnc_frequency_init
		};

                gncfreq_type = g_type_register_static (GTK_TYPE_VBOX,
						       "GNCFrequency",
						       &gncfreq_info, 0);
        }

	return gncfreq_type;
}

static void
gnc_frequency_class_init( GNCFrequencyClass *klass )
{
	GObjectClass *object_class;
	
	object_class = G_OBJECT_CLASS (klass);

        gnc_frequency_signals[GNCFREQ_CHANGED] =
		g_signal_new ("changed",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (GNCFrequencyClass, changed),
			      NULL,
			      NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);
}

void
gnc_frequency_init( GNCFrequency *gf )
{
        int    i;
        GtkVBox  *vb;
        GtkWidget   *o;
        GtkAdjustment  *adj;

        static const struct comboBoxTuple {
                char *name;
                void (*fn)();
        } comboBoxes[] = {
                { "freq_combobox",      freq_combo_changed },
                { "semimonthly_first",  semimonthly_sel_changed },
                { "semimonthly_second", semimonthly_sel_changed },
                { "monthly_day",        monthly_sel_changed },
                { "quarterly_occur",    quarterly_sel_changed },
                { "quarterly_day",      quarterly_sel_changed },
                { "triyearly_occur",    triyearly_sel_changed },
                { "triyearly_day",      triyearly_sel_changed },
                { "semiyearly_occur",   semiyearly_sel_changed },
                { "semiyearly_day",     semiyearly_sel_changed },
                { "yearly_month",       yearly_sel_changed },
                { "yearly_day",         yearly_sel_changed },
                { NULL,                 NULL }
        };

        static const struct spinvalTuple {
                char *name;
                void (*fn)();
        } spinVals[] = {
                { "daily_spin",       spin_changed_helper },
                { "dailymf_spin",     spin_changed_helper },
                { "weekly_spin",      spin_changed_helper },
                { "semimonthly_spin", spin_changed_helper },
                { "monthly_spin",     spin_changed_helper },
                { "yearly_spin",      spin_changed_helper },
                { NULL,               NULL }
        };

        gf->gxml = gnc_glade_xml_new( "sched-xact.glade", "gncfreq_vbox" );
        o = glade_xml_get_widget( gf->gxml, "gncfreq_nb" );
        gf->nb = GTK_NOTEBOOK(o);
        o = glade_xml_get_widget( gf->gxml, "freq_combobox" );
        gf->freqComboBox = GTK_COMBO_BOX(o);
        gf->startDate = GNC_DATE_EDIT(gnc_date_edit_new( time(NULL), FALSE, FALSE ));
        /* Add the new widget to the table. */
        {
                GtkWidget *table = glade_xml_get_widget( gf->gxml, "gncfreq_table" );
                gtk_table_attach( GTK_TABLE(table), GTK_WIDGET(gf->startDate),
                                  1, 2, 1, 2,
                                  ( GTK_EXPAND | GTK_FILL ), 0,
                                  0, 0 );
        }
        vb = GTK_VBOX( glade_xml_get_widget( gf->gxml, "gncfreq_vbox" ) );
        gf->vb = vb;
        gtk_container_add( GTK_CONTAINER(&gf->widget), GTK_WIDGET(gf->vb) );

        /* initialize the combo boxes */
        for ( i=0; comboBoxes[i].name != NULL; i++ ) {
                o = glade_xml_get_widget( gf->gxml, comboBoxes[i].name );
		gtk_combo_box_set_active(GTK_COMBO_BOX(o), 0);
                if ( comboBoxes[i].fn != NULL ) {
                        g_signal_connect( o, "changed",
					  G_CALLBACK(comboBoxes[i].fn), gf );
                }
        }

        /* initialize the spin buttons */
        for ( i=0; spinVals[i].name != NULL; i++ ) 
        {
                if ( spinVals[i].fn != NULL ) 
                {
                        o = glade_xml_get_widget( gf->gxml,
                                          spinVals[i].name );
                        adj = gtk_spin_button_get_adjustment( GTK_SPIN_BUTTON(o) );
                        g_signal_connect( adj, "value_changed",
					  G_CALLBACK(spinVals[i].fn), gf );
                }
        }

        /* initialize the weekly::day-of-week checkbox-selection hooks */
        for ( i=0; i<7; i++ ) {
                o = glade_xml_get_widget( gf->gxml, CHECKBOX_NAMES[i] );
                g_signal_connect( o, "clicked",
				  G_CALLBACK(weekly_days_changed), gf );
        }

        gtk_widget_show_all( GTK_WIDGET(&gf->widget) );

        /* respond to start date changes */
        g_signal_connect( gf->startDate, "date_changed",
			  G_CALLBACK(start_date_changed), gf );

}

static void
do_frequency_setup( GNCFrequency *gf, FreqSpec *fs, time_t *secs)
{
        UIFreqType uift;
        int i, page;

        /* Set the start date, but only if present. */
        if (secs)
        {
                gnc_date_edit_set_time( gf->startDate, *secs);
                if (NULL == fs) 
                {
                        g_signal_emit_by_name( gf, "changed" );
                }
        }
 
        /* If freq spec not present, then we are done; 
         * don't change any other settings.  */
        if (NULL == fs) return;

        uift = xaccFreqSpecGetUIType( fs );
        page = -1;
        for ( i=0; i < UIFREQ_NUM_UI_FREQSPECS+1; i++ ) 
        {
                if ( PAGES[i].uiFTVal == uift ) 
                {
                         page = PAGES[i].idx;
                         break;
                }
        }
        g_assert( page != -1 );

        gtk_notebook_set_current_page( gf->nb, page );
        gtk_combo_box_set_active( gf->freqComboBox, page );

        switch ( uift ) 
        {
        case UIFREQ_NONE:
                break;
        case UIFREQ_ONCE:
        {
                GDate theDate;
                struct tm stm;
                /* set the date */
                if ( xaccFreqSpecGetOnce( fs, &theDate ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d vs. FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                g_date_to_struct_tm( &theDate, &stm );
                gnc_date_edit_set_time( gf->startDate, mktime(&stm) );
        }
        break;
        case UIFREQ_DAILY:
        { 
                GtkWidget *o;
                int dailyMult = -1;
                if ( xaccFreqSpecGetDaily( fs, &dailyMult ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d vs. FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                o = glade_xml_get_widget( gf->gxml, "daily_spin" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON( o ), dailyMult );
        }
        break;
        case UIFREQ_DAILY_MF:
        {
                GtkWidget *o;
                GList *fsList;
                FreqSpec *subFS;
                int weekMult, dayOfWeek;

                /*  set the mult */
                fsList = xaccFreqSpecCompositeGet( fs );
                if ( g_list_length( fsList ) != 5 ) {
                        PERR( "Invalid Daily[M-F] FreqSpec" );
                        return;
                }
                subFS = (FreqSpec*)fsList->data;
                if ( xaccFreqSpecGetWeekly( subFS, &weekMult, &dayOfWeek ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d vs. FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                o = glade_xml_get_widget( gf->gxml, "dailymf_spin" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(o), weekMult );
        }
        break;
        case UIFREQ_WEEKLY:
        {
                const char * str;
                int weeklyMult = -1;
                int dayOfWeek;
                GtkWidget *o;
                FreqSpec *subFS;
                GList *list;

                for ( list = xaccFreqSpecCompositeGet( fs );
                      list; list = g_list_next(list) ) {
                        subFS = (FreqSpec*)(list->data);
                        if ( weeklyMult == -1 ) {
                                if ( subFS == NULL ) {
                                        PERR( "subFS is null\n" );
                                        return;
                                }
                                if ( xaccFreqSpecGetWeekly( subFS,
                                                            &weeklyMult,
                                                            &dayOfWeek ) < 0 ) {
                                        PERR( "Inappropriate FreqSpec type "
                                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                                              uift, xaccFreqSpecGetUIType( fs ) );
                                        return;
                                }
                        } else {
                                int otherWeeklyMult = -1;

                                if ( subFS == NULL ) {
                                        PERR( "subFS is null\n" );
                                        return;
                                }
                                if ( xaccFreqSpecGetWeekly( subFS,
                                                            &otherWeeklyMult,
                                                            &dayOfWeek ) < 0 ) {
                                        PERR( "Inappropriate FreqSpec type "
                                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                                              uift, xaccFreqSpecGetUIType( fs ) );
                                        return;
                                }
                                if ( weeklyMult != otherWeeklyMult ) {
                                        PERR( "Inconsistent weekly FreqSpec "
                                              "multipliers seen "
                                              "[first: %d vs. other: %d]\n",
                                              weeklyMult, otherWeeklyMult );
                                        return;
                                }
                        }
                        if ( dayOfWeek > 6 ) {
                                PERR( "dayOfWeek > 6 [%d]", dayOfWeek );
                                return;
                        }
                        str = CHECKBOX_NAMES[dayOfWeek];
                        o = glade_xml_get_widget( gf->gxml, str );
                        gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON(o), TRUE );
                }
                o = glade_xml_get_widget( gf->gxml, "weekly_spin" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(o), weeklyMult );
        }
        break;
        case UIFREQ_BI_WEEKLY:
                /*  set the initial date...? */
                break;
        case UIFREQ_SEMI_MONTHLY:
        {
                GtkWidget *o;
                GList *list;
                int monthlyMult;
                FreqSpec *subFS;
                int firstDayOfMonth, secondDayOfMonth, monthOffset;

                list = xaccFreqSpecCompositeGet( fs );
                /*  mult */
                o = glade_xml_get_widget( gf->gxml, "semimonthly_spin" );
                subFS = (FreqSpec*)(g_list_nth( list, 0 )->data);
                if ( xaccFreqSpecGetMonthly( subFS, &monthlyMult,
                                             &firstDayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(o), monthlyMult );
                /*  first date */
                o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o), firstDayOfMonth-1 );
                /*  second date */
                subFS = (FreqSpec*)(g_list_nth( list, 1 )->data);
                o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
                if ( xaccFreqSpecGetMonthly( subFS, &monthlyMult,
                                             &secondDayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type\n" );
                        return;
                }
                gtk_combo_box_set_active( GTK_COMBO_BOX(o), secondDayOfMonth-1 );
        }
        break;
        case UIFREQ_MONTHLY:
        {
                GtkWidget *o;
                int monthlyMult, dayOfMonth, monthOffset;

                if ( xaccFreqSpecGetMonthly( fs, &monthlyMult,
                                             &dayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                o = glade_xml_get_widget( gf->gxml, "monthly_spin" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(o), monthlyMult );
                o = glade_xml_get_widget( gf->gxml, "monthly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o), dayOfMonth-1 );
                /*  set the day-of-month */
        }
        break;
        case UIFREQ_QUARTERLY:
        {
                int monthlyMult, dayOfMonth, monthOffset;

                if ( xaccFreqSpecGetMonthly( fs, &monthlyMult,
                                             &dayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                if ( monthlyMult != 3 ) {
                        PERR( "monthly multiplier != 3 [=%d]", monthlyMult );
                        return;
                }
                year_range_menu_helper( glade_xml_get_widget( gf->gxml,
                                                              "quarterly_day" ),
                                        glade_xml_get_widget( gf->gxml,
                                                              "quarterly_occur" ),
                                        3, gnc_date_edit_get_date( gf->startDate ) );
        }
        break;
        case UIFREQ_TRI_ANUALLY:
        {
                int monthlyMult, dayOfMonth, monthOffset;

                if ( xaccFreqSpecGetMonthly( fs, &monthlyMult,
                                             &dayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                if ( monthlyMult != 4 ) {
                        PERR( "Month-multiplier != 4 [=%d]", monthlyMult );
                        return;
                }
                year_range_menu_helper( glade_xml_get_widget( gf->gxml,
                                                              "triyearly_day" ),
                                        glade_xml_get_widget( gf->gxml,
                                                              "triyearly_occur" ),
                                        4, gnc_date_edit_get_date( gf->startDate ) );
        }
        break;
        case UIFREQ_SEMI_YEARLY:
        {
                int monthlyMult, dayOfMonth, monthOffset;

                if ( xaccFreqSpecGetMonthly( fs, &monthlyMult,
                                             &dayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                if ( monthlyMult != 6 ) {
                        PERR( "month-mult != 6 [=%d]", monthlyMult );
                        return;
                }
                year_range_menu_helper( glade_xml_get_widget( gf->gxml,
                                                              "semiyearly_day" ),
                                        glade_xml_get_widget( gf->gxml,
                                                              "semiyearly_occur" ),
                                        6, gnc_date_edit_get_date( gf->startDate ) );
        }
        break;
        case UIFREQ_YEARLY:
        {
                GtkWidget *o;
                int monthlyMult, dayOfMonth, monthOffset;

                if ( xaccFreqSpecGetMonthly( fs, &monthlyMult,
                                             &dayOfMonth, &monthOffset ) < 0 ) {
                        PERR( "Inappropriate FreqSpec type "
                              "[gnc-frequency: %d, FreqSpec: %d]\n",
                              uift, xaccFreqSpecGetUIType( fs ) );
                        return;
                }
                if ( (monthlyMult % 12) != 0) {
                        PERR( "monthly-mult %% 12 != 0 [=%d]", ( monthlyMult % 12 ) );
                        return;
                }

                /* set the mult */
                o = glade_xml_get_widget( gf->gxml, "yearly_spin" );
                gtk_spin_button_set_value( GTK_SPIN_BUTTON(o),
                                           (int)rint(floor(monthlyMult / 12)) );
                o = glade_xml_get_widget( gf->gxml, "yearly_month" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o), monthOffset );
                o = glade_xml_get_widget( gf->gxml, "yearly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o), dayOfMonth-1 );
        }
        break;
        default:
                PERR( "unknown ui freq type %d [%d, %s]\n",
                      uift, __LINE__, __FILE__ );
                break;
        }

        g_signal_emit_by_name( gf, "changed" );
}

void
gnc_frequency_setup_default( GNCFrequency *gf, FreqSpec *fs, GDate *date )
{
   time_t secs;

   /* If no freq-spec, then set the widget to blank */
   if (NULL == fs)
   {
      UIFreqType uift = UIFREQ_NONE;
      int i, page;

      page = -1;
      for ( i=0; i < UIFREQ_NUM_UI_FREQSPECS+1; i++ ) 
      {
         if ( PAGES[i].uiFTVal == uift ) 
         {
             page = PAGES[i].idx;
             break;
         }
      }
      g_assert( page != -1 );
   
      gtk_notebook_set_current_page( gf->nb, page );
      gtk_combo_box_set_active( gf->freqComboBox, page );
   }

   /* Setup the start date */
   if (!date ||  ! g_date_valid(date) ) 
   {
      secs = time(NULL);
   } 
   else 
   {
      struct tm stm;
      g_date_to_struct_tm( date, &stm);
      secs = mktime (&stm);
   }
 
   do_frequency_setup(gf, fs, &secs);
}

void
gnc_frequency_setup( GNCFrequency *gf, FreqSpec *fs, GDate *date )
{
   time_t secs;

   if (!gf) return;

   /* Setup the start date */
   if (!date ||  ! g_date_valid(date) ) 
   {
      do_frequency_setup(gf, fs, NULL);
   } 
   else 
   {
      struct tm stm;
      g_date_to_struct_tm( date, &stm);
      secs = mktime (&stm);
      do_frequency_setup(gf, fs, &secs);
   }
}

GtkWidget *
gnc_frequency_new( FreqSpec *fs, GDate *date )
{
        GNCFrequency  *toRet;
        toRet = g_object_new( gnc_frequency_get_type(), NULL );
        gnc_frequency_setup_default( toRet, fs, date );
        return GTK_WIDGET(toRet);
}

void
gnc_frequency_save_state( GNCFrequency *gf, FreqSpec *fs, GDate *outDate )
{
        gint page;
        gint day;
        GtkWidget *o;
        UIFreqType uift;
        FreqSpec *tmpFS;
        gint tmpInt;
        int i;
        GDate gd;
        time_t start_tt;

        start_tt = gnc_date_edit_get_date( gf->startDate );
        if ( NULL != outDate ) 
        {
                g_date_set_time_t( outDate, start_tt );
        }

        if (NULL == fs) return;

        /* Get the current tab */
        page = gtk_notebook_get_current_page( gf->nb );

        /* We're going to be creating/destroying FreqSpecs, 
         * which will cause GUI refreshes. :( */
        gnc_suspend_gui_refresh();

        g_date_clear (&gd, 1);
        g_date_set_time_t( &gd, start_tt );

        /*uift = xaccFreqSpecGetUIType( fs );*/
        uift = PAGES[page].uiFTVal;

        /* Based on value, parse widget values into FreqSpec */
        switch ( uift ) {
        case UIFREQ_NONE:
                /* hmmm... shouldn't really be allowed. */
                break;
        case UIFREQ_ONCE:
                xaccFreqSpecSetOnceDate(fs, &gd);
                xaccFreqSpecSetUIType( fs, uift );
                break;
        case UIFREQ_DAILY:
                o = glade_xml_get_widget( gf->gxml, "daily_spin" );
                /* FIXME: initial date should be set correctly. */
                {
                        gint foo;

                        foo = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
                        xaccFreqSpecSetDaily( fs, &gd, foo );
                        xaccFreqSpecSetUIType( fs, uift );
                }
                break;
        case UIFREQ_DAILY_MF:
                xaccFreqSpecSetComposite( fs );
                xaccFreqSpecSetUIType( fs, uift );
                o = glade_xml_get_widget( gf->gxml, "dailymf_spin" );
                tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
                /*  Okay.  Assume that the calendar is upgraded to */
                /*  support selecting weeks, returning the Sunday selected. */
                /*  Normalize to sunday. */
                {
                        struct tm stm;
                        g_date_to_struct_tm( &gd, &stm );
                        /*  month-day += (week-day - current-week-day ) % 7 */
                        /*  week-day <- 0 */
                        stm.tm_mday -= ( stm.tm_wday ) % 7;
                        g_date_set_time_t( &gd, mktime(&stm) );
                }

                /*  1 == "mon", 5 == "fri" */
                for ( i=1; i<6; i++ ) {
                        g_date_add_days( &gd, 1 );
                        tmpFS = xaccFreqSpecMalloc(gnc_get_current_book ());
                        xaccFreqSpecSetWeekly( tmpFS, &gd, tmpInt );
                        xaccFreqSpecCompositeAdd( fs, tmpFS );
                }
                break;
        case UIFREQ_WEEKLY:
        {
                struct tm stm;
                GDate gd2;
                xaccFreqSpecSetComposite( fs );
                xaccFreqSpecSetUIType( fs, uift );
                o = glade_xml_get_widget( gf->gxml, "weekly_spin" );
                tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );

                /*  assume we have a good calendar that allows week selection. */
                /*  for-now hack: normalize to Sunday. */
                g_date_to_struct_tm( &gd, &stm);
                stm.tm_mday -= stm.tm_wday % 7;
                g_date_set_time_t( &gd, mktime(&stm) );

                /*  now, go through the check boxes and add composites based on that date. */
                for ( i=0; CHECKBOX_NAMES[i]!=NULL; i++ ) {
                        o = glade_xml_get_widget( gf->gxml, CHECKBOX_NAMES[i] );
                        if ( gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON(o) ) ) {

                                tmpFS = xaccFreqSpecMalloc
                                        (gnc_get_current_book ());
                                xaccFreqSpecSetUIType( tmpFS, uift );

                                g_date_clear (&gd2, 1);
                                gd2 = gd;
                                /*  Add 'i' days off of Sunday... */
                                g_date_add_days( &gd2, i );
                                xaccFreqSpecSetWeekly( tmpFS, &gd2, tmpInt );
                                xaccFreqSpecCompositeAdd( fs, tmpFS );
                        }
                }
                break;
        }
        case UIFREQ_BI_WEEKLY:
                xaccFreqSpecSetUIType( fs, uift );
                o = glade_xml_get_widget( gf->gxml, "biweekly_cal" );
                xaccFreqSpecSetWeekly( fs, &gd, 2 );
                break;
        case UIFREQ_SEMI_MONTHLY:
        {
                struct tm stm;
                /* FIXME: this is b0rken date calculation for mday>28 */
                xaccFreqSpecSetComposite( fs );
                xaccFreqSpecSetUIType( fs, uift );
    
                o = glade_xml_get_widget( gf->gxml, "semimonthly_spin" );
                tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );

                o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
                day = gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1;
                tmpFS = xaccFreqSpecMalloc(gnc_get_current_book ());
                g_date_to_struct_tm( &gd, &stm);
                if ( day >= stm.tm_mday ) {
                        /* next month */
                        stm.tm_mon += 1;
                }
                /* else, this month */
                stm.tm_mday = day;
                g_date_set_time_t( &gd, mktime( &stm) );
                xaccFreqSpecSetMonthly( tmpFS, &gd, tmpInt );
                xaccFreqSpecCompositeAdd( fs, tmpFS );

                o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
                day = gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1;
                tmpFS = xaccFreqSpecMalloc(gnc_get_current_book ());
                start_tt = gnc_date_edit_get_date( gf->startDate );
                g_date_set_time_t( &gd, start_tt );
                g_date_to_struct_tm( &gd, &stm);
                if ( day >= stm.tm_mday ) {
                        /* next month */
                        stm.tm_mon += 1;
                }
                /* else, this month */
                stm.tm_mday = day;
                g_date_set_time_t( &gd, mktime( &stm ) );
                xaccFreqSpecSetMonthly( tmpFS, &gd, tmpInt );
                xaccFreqSpecCompositeAdd( fs, tmpFS );

                break;
        }
        case UIFREQ_MONTHLY:
        {
                o = glade_xml_get_widget( gf->gxml, "monthly_spin" );
                tmpInt = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(o));

                o = glade_xml_get_widget( gf->gxml, "monthly_day" );
                day = gtk_combo_box_get_active( GTK_COMBO_BOX(o) ) + 1;
                g_date_set_time_t(&gd, time(NULL));
                g_date_set_month(&gd, 1);
                g_date_set_day(&gd, day);
                {
                     gchar buf[128];
                     g_date_strftime(buf, 127, "%c", &gd);
                     printf("monthly date [%s]\n", buf);
                }
                xaccFreqSpecSetMonthly( fs, &gd, tmpInt );
                xaccFreqSpecSetUIType( fs, uift );
                break;
        }
        case UIFREQ_QUARTERLY:
                xaccFreqSpecSetMonthly( fs, &gd, 3 );
                xaccFreqSpecSetUIType( fs, uift );
                break;
        case UIFREQ_TRI_ANUALLY:
                xaccFreqSpecSetMonthly( fs, &gd, 4 );
                xaccFreqSpecSetUIType( fs, uift );
                break;
        case UIFREQ_SEMI_YEARLY:
                xaccFreqSpecSetMonthly( fs, &gd, 6 );
                xaccFreqSpecSetUIType( fs, uift );
                break;
        case UIFREQ_YEARLY:
                o = glade_xml_get_widget( gf->gxml, "yearly_spin" );
                tmpInt = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON(o) );
                xaccFreqSpecSetMonthly( fs, &gd, tmpInt * 12 );
                xaccFreqSpecSetUIType( fs, uift );
                break;
        default:
                PERR( "Unknown UIFreqType %d [%d, %s]\n",
                      uift, __LINE__, __FILE__ );
                break;
        }
        gnc_resume_gui_refresh();
}

static void
spin_changed_helper( GtkAdjustment *adj, gpointer d )
{
        g_signal_emit_by_name( GNC_FREQUENCY(d), "changed" );
}

static void
weekly_days_changed( GtkButton *b, gpointer d )
{
        GNCFrequency *gf;

        gf = GNC_FREQUENCY(d);
        g_signal_emit_by_name( gf, "changed" );
}

static void
monthly_sel_changed( GtkButton *b, gpointer d )
{
        GNCFrequency  *gf;
        GtkWidget  *o;
        guint    dayOfMonth;
        struct tm  *tmptm;
        time_t    tmptt;
  
        gf = (GNCFrequency*)d;

        o = glade_xml_get_widget( ((GNCFrequency*)d)->gxml,
                                  "monthly_day" );
        dayOfMonth = gtk_combo_box_get_active( GTK_COMBO_BOX(o) ) + 1;

        tmptt = gnc_date_edit_get_date( gf->startDate );
        tmptm = localtime( &tmptt );
        while ( ! g_date_valid_dmy( dayOfMonth,
                                    tmptm->tm_mon + 1,
                                    tmptm->tm_year+1900 ) ) {
                dayOfMonth -= 1;
        }
        tmptm->tm_mday = dayOfMonth;
        tmptt = mktime( tmptm );
        gnc_date_edit_set_time( gf->startDate, tmptt );

        g_signal_emit_by_name( d, "changed" );
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

        tmptt = gnc_date_edit_get_date( gf->startDate );
        tmptm = localtime( &tmptt );

        o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
        tmpint = gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1;
        o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
        if ( tmpint > gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1 ) {
                tmpint = gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1;
        }

        tmptm->tm_mday = tmpint;
        while ( ! g_date_valid_dmy( tmptm->tm_mday,
                                    tmptm->tm_mon+1,
                                    tmptm->tm_year+1900 ) ) {
                tmptm->tm_mday -= 1;
        }
        tmptt = mktime( tmptm );
        gnc_date_edit_set_time( gf->startDate, tmptt );

        g_signal_emit_by_name( gf, "changed" );
}

static void
quarterly_sel_changed( GtkButton *b, gpointer d )
{
        GNCFrequency *gf;
        gf = (GNCFrequency*)d;
        year_range_sels_changed( gf, 3,
                                 glade_xml_get_widget( gf->gxml, "quarterly_occur" ),
                                 glade_xml_get_widget( gf->gxml, "quarterly_day" ) );
}

static void
triyearly_sel_changed( GtkButton *b, gpointer d )
{
        GNCFrequency *gf;
        gf = (GNCFrequency*)d;
        year_range_sels_changed( gf, 4,
                                 glade_xml_get_widget( gf->gxml, "triyearly_occur" ),
                                 glade_xml_get_widget( gf->gxml, "triyearly_day" ) );
}

static void
semiyearly_sel_changed( GtkButton *b, gpointer d )
{
        GNCFrequency *gf;
        gf = (GNCFrequency*)d;
        year_range_sels_changed( gf, 6,
                                 glade_xml_get_widget( gf->gxml, "semiyearly_occur" ),
                                 glade_xml_get_widget( gf->gxml, "semiyearly_day" ) );
}

static void
year_range_sels_changed( GNCFrequency *gf,
                         int monthsInRange,
                         GtkWidget *occurW,
                         GtkWidget *dayOfMonthW )
{
        int occur, day;
        time_t tmpTT;
        struct tm *tmpTm;

        occur = gtk_combo_box_get_active( GTK_COMBO_BOX(occurW) );
        day = gtk_combo_box_get_active( GTK_COMBO_BOX(dayOfMonthW) ) + 1;

        tmpTT = gnc_date_edit_get_date( gf->startDate );
        tmpTm = localtime( &tmpTT );
        tmpTm->tm_mday = day;
        // jump the month to the closest appropriate month.
        // FIXME: this could probably be made more "appropriate".
        tmpTm->tm_mon += occur - (tmpTm->tm_mon % monthsInRange);
        tmpTT = mktime( tmpTm );
        gnc_date_edit_set_time( gf->startDate, tmpTT );

        g_signal_emit_by_name( gf, "changed" );
}

static void
yearly_sel_changed( GtkButton *b, gpointer d )
{
        GNCFrequency  *gf;
        GtkWidget  *o;
        time_t    tmptt;
        struct tm  *tmptm;

        gf = (GNCFrequency*)d;

        tmptt = gnc_date_edit_get_date( gf->startDate );
        tmptm = localtime( &tmptt );

        o = glade_xml_get_widget( gf->gxml, "yearly_month" );
        tmptm->tm_mon = gtk_combo_box_get_active( GTK_COMBO_BOX(o) );
        o = glade_xml_get_widget( gf->gxml, "yearly_day" );
        tmptm->tm_mday = gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1;

        /* FIXME: correct for
           option_menu_selected_day > minn(31,correct_days_in_month)
           problem */
        while ( ! g_date_valid_dmy( tmptm->tm_mday,
                                    tmptm->tm_mon+1,
                                    tmptm->tm_year+1900 ) ) {
                tmptm->tm_mday -= 1;
        }

        tmptt = mktime( tmptm );
        gnc_date_edit_set_time( gf->startDate, tmptt );

        g_signal_emit_by_name( gf, "changed" );
}

static inline guint32 minn( guint32 a, guint32 b )
{
        return a > b ? b : a;
}

static inline guint32 maxn( guint32 a, guint32 b )
{
        return a > b ? a : b;
}

static void
freq_combo_changed( GtkComboBox *b, gpointer d )
{
        GNCFrequency *gf = (GNCFrequency*)d;
        int optIdx;
        UIFreqType uift;
        time_t startDate, tmpDate;
        struct tm *tmpTm;
        GtkWidget *o;

        /* Set the new page. */
        optIdx = gtk_combo_box_get_active( GTK_COMBO_BOX(((GNCFrequency*)d)->freqComboBox) );
        gtk_notebook_set_current_page( ((GNCFrequency*)d)->nb, optIdx );

        /* setup initial values for new page, as possible. */
        uift = PAGES[optIdx].uiFTVal;
        startDate = gnc_date_edit_get_date( gf->startDate );
        tmpTm = localtime( &startDate );

        switch ( uift ) {
        case UIFREQ_SEMI_MONTHLY:
        {
                gint tmpDayOfMonth;
                /* first on the <startdate_dom>, then on the <startdate_dom+2w> */
                o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
                tmpDayOfMonth = tmpTm->tm_mday;
                tmpTm->tm_mday += 14;
                tmpDate = mktime( tmpTm );
                tmpTm = localtime( &tmpDate );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  minn( tmpTm->tm_mday, tmpDayOfMonth ) - 1 );
                o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  maxn( tmpTm->tm_mday, tmpDayOfMonth ) - 1 );
        }
        break;
        case UIFREQ_MONTHLY:
                /* on the <startdate_dom> */
                o = glade_xml_get_widget( gf->gxml, "monthly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mday - 1 );
                break;
        case UIFREQ_QUARTERLY:
                /* on the <startdate_dom> */
                o = glade_xml_get_widget( gf->gxml, "quarterly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mday - 1 );
                break;
        case UIFREQ_TRI_ANUALLY:
                /* on the <startdate_dom> */
                o = glade_xml_get_widget( gf->gxml, "triyearly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mday - 1 );
                break;
        case UIFREQ_SEMI_YEARLY:
                /* on the <startdate_dom> */
                o = glade_xml_get_widget( gf->gxml, "semiyearly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mday - 1 );
                break;
        case UIFREQ_YEARLY:
                /* on the <startdate_mon>, <startdate_dom> */
                o = glade_xml_get_widget( gf->gxml, "yearly_month" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mon );
                o = glade_xml_get_widget( gf->gxml, "yearly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mday - 1 );
                break;
        default:
                /* nuttin can be done, for whatever reason. */
                break;
        }
        g_signal_emit_by_name( gf, "changed" );
}

static void
year_range_menu_helper( GtkWidget *dayOptMenu,
                        GtkWidget *occurOptMenu,
                        gint monthsInRange,
                        time_t startDate )
{
        struct tm  *tmpTm;
        tmpTm = localtime( &startDate );
        gtk_combo_box_set_active( GTK_COMBO_BOX(occurOptMenu),
				  tmpTm->tm_mon % monthsInRange );
        gtk_combo_box_set_active( GTK_COMBO_BOX(dayOptMenu),
				   tmpTm->tm_mday - 1 );
}

static void
start_date_changed( GNCDateEdit *gde, gpointer d )
{
        GNCFrequency  *gf;
        GtkWidget  *o;
        struct tm  *tmpTm;
        time_t    dateFromGDE;
        gint    page;
        UIFreqType  uift;
  
        gf = (GNCFrequency*)d;

        dateFromGDE = gnc_date_edit_get_date( gde );

        page = gtk_notebook_get_current_page( gf->nb );
        uift = PAGES[page].uiFTVal;

        o = NULL;

        switch (uift) {
        case UIFREQ_ONCE:      /* FALLTHROUGH */
        case UIFREQ_DAILY:     /* FALLTHROUGH */
        case UIFREQ_DAILY_MF:  /* FALLTHROUGH */
        case UIFREQ_WEEKLY:    /* FALLTHROUGH */
        case UIFREQ_BI_WEEKLY:
                break;

        case UIFREQ_SEMI_MONTHLY:
        {
                gint first_day;
                o = glade_xml_get_widget( gf->gxml, "semimonthly_first" );
                first_day = gtk_combo_box_get_active( GTK_COMBO_BOX(o) )+1;
                o = glade_xml_get_widget( gf->gxml, "semimonthly_second" );
                if ( first_day < gtk_combo_box_get_active(
                             GTK_COMBO_BOX(o) )+1 ) {
                        o = glade_xml_get_widget( gf->gxml,
                                                  "semimonthly_first" );
                }

                tmpTm = localtime( &dateFromGDE );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
                                             tmpTm->tm_mday-1 );
        }
        break;
        case UIFREQ_MONTHLY:
                o = glade_xml_get_widget( gf->gxml, "monthly_day" );
                tmpTm = localtime( &dateFromGDE );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
                                             (tmpTm->tm_mday-1) );
                break;
        case UIFREQ_QUARTERLY:
                year_range_menu_helper( glade_xml_get_widget(
                                                gf->gxml, "quarterly_day" ),
                                        glade_xml_get_widget(
                                                gf->gxml, "quarterly_occur" ),
                                        3, dateFromGDE );
                break;
        case UIFREQ_TRI_ANUALLY:
                year_range_menu_helper( glade_xml_get_widget(
                                                gf->gxml, "triyearly_day" ),
                                        glade_xml_get_widget(
                                                gf->gxml, "triyearly_occur" ),
                                        4, dateFromGDE );
                break;
        case UIFREQ_SEMI_YEARLY:
                year_range_menu_helper( glade_xml_get_widget(
                                                gf->gxml, "semiyearly_day" ),
                                        glade_xml_get_widget(
                                                gf->gxml, "semiyearly_occur" ),
                                        6, dateFromGDE );
                break;
        case UIFREQ_YEARLY:
                o = glade_xml_get_widget( gf->gxml, "yearly_month" );
                tmpTm = localtime( &dateFromGDE );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mon );
                o = glade_xml_get_widget( gf->gxml, "yearly_day" );
                gtk_combo_box_set_active( GTK_COMBO_BOX(o),
					  tmpTm->tm_mday-1 );
                break;
        default:
                PERR( "unknown uift value %d\n", uift );
                break;
        }
        g_signal_emit_by_name( gf, "changed" );
}

/* ================================================================= */
/* Relabel some of the labels */

void 
gnc_frequency_set_frequency_label_text (GNCFrequency *gf, const gchar *txt)
{
   GtkLabel *lbl;
	if (!gf || !txt) return;
   lbl = GTK_LABEL (glade_xml_get_widget (gf->gxml, "freq label"));
   gtk_label_set_text (lbl, txt);
}

void 
gnc_frequency_set_date_label_text (GNCFrequency *gf, const gchar *txt)
{
   GtkLabel *lbl;
	if (!gf || !txt) return;
   lbl = GTK_LABEL (glade_xml_get_widget (gf->gxml, "startdate label"));
   gtk_label_set_text (lbl, txt);
}

/* ========================= END OF FILE =========================== */
