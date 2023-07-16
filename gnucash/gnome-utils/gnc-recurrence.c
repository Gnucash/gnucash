/********************************************************************
 * gnc-recurrence.c -- GncRecurrence is a minimal GUI for           *
 *                      specifying a Recurrence.                    *
 *                                                                  *
 * You see, small is _nice_.  :)                                    *
 * Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>        *
 * Copyright (C) 2011, Robert Fewell                                *
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
 *******************************************************************/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-date.h"
#include "gnc-recurrence.h"
#include "gnc-date-edit.h"
#include "Recurrence.h"
#include "gnc-engine.h"

static QofLogModule log_module = GNC_MOD_GUI;

struct _GncRecurrence
{
    GtkBox widget;

    GtkWidget *gde_start;
    GtkComboBox *gcb_period;
    GtkCheckButton *gcb_eom;
    GtkSpinButton *gsb_mult;
    GtkCheckButton *nth_weekday;

    Recurrence recurrence;
};

typedef enum
{
    GNCRECURRENCE_CHANGED,
    LAST_SIGNAL
} GNCR_Signals;

typedef enum
{
    GNCR_DAY,
    GNCR_WEEK,
    GNCR_MONTH,
    GNCR_YEAR,
} UIPeriodType;

G_DEFINE_TYPE (GncRecurrence, gnc_recurrence, GTK_TYPE_BOX)

static UIPeriodType get_pt_ui(GncRecurrence *gr)
{
    return (gtk_combo_box_get_active(gr->gcb_period));
}


static void set_pt_ui(GncRecurrence *gr, PeriodType pt)
{
    UIPeriodType idx;
    switch (pt)
    {
    case PERIOD_DAY:
        idx = 0;
        break;
    case PERIOD_WEEK:
        idx = 1;
        break;
    case PERIOD_MONTH:
    case PERIOD_END_OF_MONTH:
    case PERIOD_NTH_WEEKDAY:
    case PERIOD_LAST_WEEKDAY:
        idx = 2;
        break;
    case PERIOD_YEAR:
        idx = 3;
        break;
    default:
        return;
    }
    gtk_combo_box_set_active(gr->gcb_period, idx);

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(gr->nth_weekday),
        (pt == PERIOD_NTH_WEEKDAY || pt == PERIOD_LAST_WEEKDAY));

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(gr->gcb_eom),
        (pt == PERIOD_END_OF_MONTH || pt == PERIOD_LAST_WEEKDAY));
}


static gboolean
is_ambiguous_relative(const GDate *date)
{
    GDateDay d;
    guint8 dim;

    d = g_date_get_day(date);
    dim = g_date_get_days_in_month(
              g_date_get_month(date), g_date_get_year(date));
    return ((d - 1) / 7 == 3) && (dim - d < 7);
}


static gboolean
is_ambiguous_absolute(const GDate *date)
{
    return (g_date_is_last_of_month(date) &&
            (g_date_get_day(date) < 31));
}


static void
something_changed( GtkWidget *wid, gpointer d )
{
    UIPeriodType pt;
    GDate start;
    gboolean show_last, use_wd;
    GncRecurrence *gr = GNC_RECURRENCE(d);


    pt = get_pt_ui(gr);
    gnc_date_edit_get_gdate(GNC_DATE_EDIT(gr->gde_start), &start);

    if (pt == GNCR_MONTH)
        g_object_set(G_OBJECT(gr->nth_weekday), "visible", TRUE, NULL);
    else
    {
        g_object_set(G_OBJECT(gr->nth_weekday), "visible", FALSE, NULL);
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(gr->nth_weekday), FALSE);
    }
    use_wd = gtk_toggle_button_get_active(
                 GTK_TOGGLE_BUTTON(gr->nth_weekday));
    //TODO: change label

    /* The case under which we show the "end of month" flag is very
       narrow, because we can almost always DTRT without it. */
    if (pt == GNCR_MONTH)
    {
        if (use_wd)
            show_last = is_ambiguous_relative(&start);
        else
            show_last = is_ambiguous_absolute(&start);
    }
    else
    {
        show_last = FALSE;
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gr->gcb_eom), FALSE);
    }
    g_object_set(G_OBJECT(gr->gcb_eom), "visible", show_last, NULL);

    g_signal_emit_by_name(d, "changed");
}


static void
gnc_recurrence_init( GncRecurrence *gr )
{
    GtkBox  *vb;
    GtkBox  *hb;
    GtkWidget *w;
    GtkBuilder *builder;

    recurrenceSet(&gr->recurrence, 1, PERIOD_MONTH, NULL, WEEKEND_ADJ_NONE);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gr), "gnc-id-recurrence");

    /* Open up the builder file */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-recurrence.glade", "GCB_PeriodType_liststore");
    gnc_builder_add_from_file (builder, "gnc-recurrence.glade", "GSB_Mult_Adj");
    gnc_builder_add_from_file (builder, "gnc-recurrence.glade", "RecurrenceEntryVBox");

    vb = GTK_BOX(gtk_builder_get_object (builder, "RecurrenceEntryVBox"));
    hb = GTK_BOX(gtk_builder_get_object (builder, "Startdate_hbox"));
    w = gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE);
    gr->gde_start = w;
    gtk_box_pack_start (GTK_BOX (hb), w, TRUE, TRUE, 0);
    gtk_widget_show (w);

    gtk_widget_set_no_show_all(GTK_WIDGET(gr->gde_start), TRUE);
    gr->gcb_period = GTK_COMBO_BOX(gtk_builder_get_object (builder, "GCB_PeriodType"));
    gr->gsb_mult = GTK_SPIN_BUTTON(gtk_builder_get_object (builder, "GSB_Mult"));
    gr->gcb_eom = GTK_CHECK_BUTTON(gtk_builder_get_object (builder, "GCB_EndOfMonth"));
    gr->nth_weekday = GTK_CHECK_BUTTON(gtk_builder_get_object (builder, "GCB_NthWeekday"));
    gtk_widget_set_no_show_all(GTK_WIDGET(gr->gcb_eom), TRUE);
    gtk_widget_set_no_show_all(GTK_WIDGET(gr->nth_weekday), TRUE);

    gtk_container_add( GTK_CONTAINER(&gr->widget), GTK_WIDGET(vb) );

    gnc_recurrence_set(gr, &gr->recurrence);
    something_changed( GTK_WIDGET(gr), gr);

    /* Setup the signals */
    g_signal_connect( G_OBJECT(gr->gde_start), "date_changed",
                      G_CALLBACK(something_changed), gr );
    g_signal_connect( G_OBJECT(gr->gcb_period), "changed",
                      G_CALLBACK(something_changed), gr );
    g_signal_connect( G_OBJECT(gr->gsb_mult), "value-changed",
                      G_CALLBACK(something_changed), gr );
    g_signal_connect( G_OBJECT(gr->gcb_eom), "toggled",
                      G_CALLBACK(something_changed), gr );
    g_signal_connect( G_OBJECT(gr->nth_weekday), "toggled",
                      G_CALLBACK(something_changed), gr );

    gtk_widget_show_all( GTK_WIDGET(&gr->widget) );

    gtk_builder_connect_signals(builder, gr);
    g_object_unref(G_OBJECT(builder));
}


void
gnc_recurrence_set(GncRecurrence *gr, const Recurrence *r)
{
    PeriodType pt;
    guint mult;
    GDate start;

    g_return_if_fail(gr && r);
    pt = recurrenceGetPeriodType(r);
    mult = recurrenceGetMultiplier(r);
    start = recurrenceGetDate(r);

    gtk_spin_button_set_value(gr->gsb_mult, (gdouble) mult);

    // is there some better way?
    {
        time64 t;
        t = gnc_time64_get_day_start_gdate (&start);
        gnc_date_edit_set_time (GNC_DATE_EDIT(gr->gde_start), t);
    }

    set_pt_ui(gr, pt);
}


const Recurrence *
gnc_recurrence_get(GncRecurrence *gr)
{
    guint mult;
    UIPeriodType period;
    PeriodType pt;
    GDate start;
    gboolean use_eom = FALSE, rel;

    mult = (guint) gtk_spin_button_get_value_as_int(gr->gsb_mult);
    gnc_date_edit_get_gdate(GNC_DATE_EDIT(gr->gde_start), &start);
    period = get_pt_ui(gr);

    switch (period)
    {
    case GNCR_DAY:
        pt = PERIOD_DAY;
        break;
    case GNCR_WEEK:
        pt = PERIOD_WEEK;
        break;
    case GNCR_MONTH:
        rel = gtk_toggle_button_get_active(
                  GTK_TOGGLE_BUTTON(gr->nth_weekday));
        if (rel)
        {
            if (is_ambiguous_relative(&start))
            {
                use_eom = gtk_toggle_button_get_active(
                              GTK_TOGGLE_BUTTON(gr->gcb_eom));
            }
            else
            {
                GDateDay d;
                d = g_date_get_day(&start);

                use_eom = ((d - 1) / 7 == 4);
            }
            if (use_eom)
                pt = PERIOD_LAST_WEEKDAY;
            else pt = PERIOD_NTH_WEEKDAY;
        }
        else
        {
            if (g_date_is_last_of_month(&start) &&
                    (g_date_get_day(&start) < 31))
            {
                // ambiguous, need to examine the checkbox
                use_eom = gtk_toggle_button_get_active(
                              GTK_TOGGLE_BUTTON(gr->gcb_eom));
            }
            else
            {
                // if it's the last dom, use eom anyway because it's the 31st.
                use_eom = g_date_is_last_of_month(&start);
            }
            if (use_eom)
                pt = PERIOD_END_OF_MONTH;
            else pt = PERIOD_MONTH;
        }
        break;
    case GNCR_YEAR:
        pt = PERIOD_YEAR;
        break;
    default:
        pt = PERIOD_INVALID;
    }

    recurrenceSet(&gr->recurrence, mult, pt, &start, WEEKEND_ADJ_NONE);
    return &gr->recurrence;
}


static void
gnc_recurrence_finalize(GObject *o)
{
    GncRecurrence *gr = GNC_RECURRENCE(o);

    if (gr)
        G_OBJECT_CLASS (gnc_recurrence_parent_class)->finalize (o);
}


static void
gnc_recurrence_class_init( GncRecurrenceClass *klass )
{
    GObjectClass *object_class;

    object_class = G_OBJECT_CLASS (klass);
    g_signal_new ("changed",
		  G_OBJECT_CLASS_TYPE (object_class),
		  G_SIGNAL_RUN_FIRST,
		  0,
		  NULL,
		  NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE,
		  0);

    object_class->finalize = gnc_recurrence_finalize;
}

GtkWidget *
gnc_recurrence_new()
{
    GncRecurrence *gr;

    ENTER(" ");
    gr = g_object_new(gnc_recurrence_get_type(), NULL);
    LEAVE(" ");
    return GTK_WIDGET(gr);
}

/* ========================= END OF FILE =========================== */
