/********************************************************************\
 * gnc-frequency.c -- GnuCash widget for frequency editing.         *
 * Copyright (C) 2001,2002,2007 Joshua Sled <jsled@asynchronous.org>*
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (C) 2011 Robert Fewell                                 *
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

#include <glib.h>
#include <gtk/gtk.h>
#include <math.h>
#include <time.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-frequency.h"
#include "FreqSpec.h"
#include "gnc-ui-util.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.frequency"

static QofLogModule log_module = GNC_MOD_GUI;

#define LAST_DAY_OF_MONTH_OPTION_INDEX 31

/** Private Defs ********************/

typedef enum
{
    GNCFREQ_CHANGED,
    LAST_SIGNAL
} GNCF_Signals;

static guint gnc_frequency_signals[LAST_SIGNAL] = { 0 };

/** Private Prototypes ********************/

static void gnc_frequency_class_init( GncFrequencyClass *klass );
static void gnc_frequency_class_destroy( GtkObject *object );

static void freq_combo_changed( GtkComboBox *b, gpointer d );
static void start_date_changed( GNCDateEdit *gde, gpointer d );
static void spin_changed_helper( GtkAdjustment *adj, gpointer d );

static void weekly_days_changed( GtkButton *b, gpointer d );

static void monthly_sel_changed( GtkButton *b, gpointer d );
static void semimonthly_sel_changed( GtkButton *b, gpointer d );

/** Static Inits ********************/

static GObjectClass *parent_class = NULL;

enum
{
    PAGE_NONE = 0,
    PAGE_ONCE,
    PAGE_DAILY,
    PAGE_WEEKLY,
    PAGE_SEMI_MONTHLY,
    PAGE_MONTHLY
};

static const struct pageDataTuple PAGES[] =
{
    { PAGE_NONE,         UIFREQ_NONE,         "None" },
    { PAGE_ONCE,         UIFREQ_ONCE,         "Once" },
    { PAGE_DAILY,        UIFREQ_DAILY,        "Daily" },
    { PAGE_WEEKLY,       UIFREQ_WEEKLY,       "Weekly" },
    { PAGE_SEMI_MONTHLY, UIFREQ_SEMI_MONTHLY, "Semi-Monthly" },
    { PAGE_MONTHLY,      UIFREQ_MONTHLY,      "Monthly" },
    { 0, 0, 0 }
};

static const char *CHECKBOX_NAMES[] =
{
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
    if (gncfreq_type == 0)
    {
        static GTypeInfo gncfreq_info =
        {
            sizeof(GncFrequencyClass),
            NULL,
            NULL,
            (GClassInitFunc)gnc_frequency_class_init,
            NULL,
            NULL,
            sizeof(GncFrequency),
            0,
            (GInstanceInitFunc)gnc_frequency_init
        };

        gncfreq_type = g_type_register_static (GTK_TYPE_VBOX,
                                               "GncFrequency",
                                               &gncfreq_info, 0);
    }

    return gncfreq_type;
}


static void
gnc_frequency_class_init( GncFrequencyClass *klass )
{
    GObjectClass *object_class;
    GtkObjectClass *gtkobject_class;

    parent_class = g_type_class_peek_parent (klass);

    object_class = G_OBJECT_CLASS (klass);
    gtkobject_class = GTK_OBJECT_CLASS (klass);

    gnc_frequency_signals[GNCFREQ_CHANGED] =
        g_signal_new ("changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GncFrequencyClass, changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    /* GtkObject signals */
    gtkobject_class->destroy = gnc_frequency_class_destroy;
}


void
gnc_frequency_init(GncFrequency *gf)
{
    int i;
    GtkVBox* vb;
    GtkWidget* o;
    GtkAdjustment* adj;
    GtkBuilder *builder;

    static const struct comboBoxTuple
    {
        char *name;
        void (*fn)();
    } comboBoxes[] =
    {
        { "freq_combobox",              freq_combo_changed },
        { "semimonthly_first",          semimonthly_sel_changed },
        { "semimonthly_first_weekend",  semimonthly_sel_changed },
        { "semimonthly_second",         semimonthly_sel_changed },
        { "semimonthly_second_weekend", semimonthly_sel_changed },
        { "monthly_day",                monthly_sel_changed },
        { "monthly_weekend",            monthly_sel_changed },
        { NULL,                         NULL }
    };

    static const struct spinvalTuple
    {
        char *name;
        void (*fn)();
    } spinVals[] =
    {
        { "daily_spin",       spin_changed_helper },
        { "weekly_spin",      spin_changed_helper },
        { "semimonthly_spin", spin_changed_helper },
        { "monthly_spin",     spin_changed_helper },
        { NULL,               NULL }
    };

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "adjustment1");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "adjustment2");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "adjustment3");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "adjustment4");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore1");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore2");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore3");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore4");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore5");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore6");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "liststore7");
    gnc_builder_add_from_file  (builder , "gnc-frequency.glade", "gncfreq_vbox");

    gf->builder = builder;
    o = GTK_WIDGET(gtk_builder_get_object (builder, "gncfreq_nb"));
    gf->nb = GTK_NOTEBOOK(o);
    o = GTK_WIDGET(gtk_builder_get_object (builder, "freq_combobox"));
    gf->freqComboBox = GTK_COMBO_BOX(o);
    gf->startDate = GNC_DATE_EDIT(gnc_date_edit_new(time(NULL), FALSE, FALSE));
    /* Add the new widget to the table. */
    {
        gint dont_expand_or_fill = 0;
        GtkWidget *table = GTK_WIDGET(gtk_builder_get_object (builder, "gncfreq_table"));
        gtk_table_attach(GTK_TABLE(table), GTK_WIDGET(gf->startDate),
                         4, 5, 0, 1, dont_expand_or_fill, 0,
                         0, 0);
    }
    vb = GTK_VBOX(gtk_builder_get_object (builder, "gncfreq_vbox"));
    gf->vb = vb;
    gtk_container_add(GTK_CONTAINER(&gf->widget), GTK_WIDGET(gf->vb));

    /* initialize the combo boxes */
    for (i = 0; comboBoxes[i].name != NULL; i++)
    {
        o = GTK_WIDGET(gtk_builder_get_object (builder, comboBoxes[i].name));
        gtk_combo_box_set_active(GTK_COMBO_BOX(o), 0);
        if (comboBoxes[i].fn != NULL)
        {
            g_signal_connect(o, "changed", G_CALLBACK(comboBoxes[i].fn), gf);
        }
    }

    /* initialize the spin buttons */
    for (i = 0; spinVals[i].name != NULL; i++)
    {
        if (spinVals[i].fn != NULL)
        {
            o = GTK_WIDGET(gtk_builder_get_object (builder, spinVals[i].name));
            adj = gtk_spin_button_get_adjustment(GTK_SPIN_BUTTON(o));
            g_signal_connect(adj, "value_changed", G_CALLBACK(spinVals[i].fn), gf);
        }
    }

    /* initialize the weekly::day-of-week checkbox-selection hooks */
    for (i = 0; i < 7; i++)
    {
        o = GTK_WIDGET(gtk_builder_get_object (builder, CHECKBOX_NAMES[i]));
        g_signal_connect(o, "clicked",
                         G_CALLBACK(weekly_days_changed), gf);
    }

    gtk_widget_show_all(GTK_WIDGET(&gf->widget));

    /* respond to start date changes */
    g_signal_connect(gf->startDate, "date_changed", G_CALLBACK(start_date_changed), gf);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gf);

}


/** Destroy the GncFrequency object.  This function is called (possibly
 *  multiple times) from the Gtk_Object level to destroy the object.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_frequency_class_destroy (GtkObject *object)
{
    GncFrequency *gf;

    ENTER("frequency %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_FREQUENCY (object));

    gf = GNC_FREQUENCY (object);

    if (gf->builder)
    {
        DEBUG("removing builder");
        g_object_unref(G_OBJECT(gf->builder));
        gf->builder = NULL;
    }

    if (GTK_OBJECT_CLASS (parent_class)->destroy)
        GTK_OBJECT_CLASS (parent_class)->destroy (object);
    LEAVE(" ");
}


static void
spin_changed_helper( GtkAdjustment *adj, gpointer d )
{
    g_signal_emit_by_name(GNC_FREQUENCY(d), "changed");
}


static void
weekly_days_changed( GtkButton *b, gpointer d )
{
    g_signal_emit_by_name(GNC_FREQUENCY(d), "changed");
}


static void
monthly_sel_changed( GtkButton *b, gpointer d )
{
    g_signal_emit_by_name(GNC_FREQUENCY(d), "changed");
}


static void
semimonthly_sel_changed( GtkButton *b, gpointer d )
{
    g_signal_emit_by_name(GNC_FREQUENCY(d), "changed");
}


static void
freq_combo_changed(GtkComboBox *b, gpointer d)
{
    GncFrequency *gf = GNC_FREQUENCY(d);
    int option_index;

    /* Set the new page. */
    option_index = gtk_combo_box_get_active(GTK_COMBO_BOX(gf->freqComboBox));
    gtk_notebook_set_current_page(gf->nb, option_index);
    g_signal_emit_by_name(gf, "changed");
}


static void
start_date_changed( GNCDateEdit *gde, gpointer d )
{
    g_signal_emit_by_name(GNC_FREQUENCY(d), "changed");
}


/**************************************
 * Relabel some of the labels         *
 *************************************/
void
gnc_frequency_set_frequency_label_text(GncFrequency *gf, const gchar *txt)
{
    GtkLabel *lbl;
    if (!gf || !txt) return;
    lbl = GTK_LABEL (gtk_builder_get_object (gf->builder, "freq_label"));
    gtk_label_set_text (lbl, txt);
}


void
gnc_frequency_set_date_label_text(GncFrequency *gf, const gchar *txt)
{
    GtkLabel *lbl;
    if (!gf || !txt) return;
    lbl = GTK_LABEL (gtk_builder_get_object (gf->builder, "startdate_label"));
    gtk_label_set_text (lbl, txt);
}


GtkWidget*
gnc_frequency_new_from_recurrence(GList *recurrences, const GDate *start_date)
{
    return gnc_frequency_new(recurrences, start_date);
}


GtkWidget*
gnc_frequency_new(GList *recurrences, const GDate *start_date)
{
    GncFrequency *toRet;
    toRet = g_object_new(gnc_frequency_get_type(), NULL);
    gnc_frequency_setup_recurrence(toRet, recurrences, start_date);
    return GTK_WIDGET(toRet);
}


static void
_setup_weekly_recurrence(GncFrequency *gf, Recurrence *r)
{
    GDate recurrence_date;
    GDateWeekday day_of_week;
    guint multiplier = recurrenceGetMultiplier(r);
    const char *checkbox_widget_name;
    GtkWidget *weekday_checkbox;

    GtkWidget *multipler_spin = GTK_WIDGET(gtk_builder_get_object (gf->builder, "weekly_spin"));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(multipler_spin), multiplier);

    recurrence_date = recurrenceGetDate(r);
    day_of_week = g_date_get_weekday(&recurrence_date);
    g_assert(day_of_week >= G_DATE_MONDAY && day_of_week <= G_DATE_SUNDAY);
    // this `mod 7' is explicit knowledge of the values of (monday=1)-based
    // GDateWeekday, vs. our (sunday=0)-based checkbox names array.
    checkbox_widget_name = CHECKBOX_NAMES[day_of_week % 7];
    weekday_checkbox = GTK_WIDGET(gtk_builder_get_object (gf->builder, checkbox_widget_name));
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(weekday_checkbox), TRUE);
}


static int
_get_monthly_combobox_index(Recurrence *r)
{
    GDate recurrence_date = recurrenceGetDate(r);
    int week = 0;
    int day_of_month_index = g_date_get_day(&recurrence_date) - 1;
    if (recurrenceGetPeriodType(r) == PERIOD_END_OF_MONTH)
    {
        day_of_month_index = LAST_DAY_OF_MONTH_OPTION_INDEX;
    }
    else if (recurrenceGetPeriodType(r) == PERIOD_LAST_WEEKDAY)
    {
        day_of_month_index
        = LAST_DAY_OF_MONTH_OPTION_INDEX
          + g_date_get_weekday(&recurrence_date);
    }
    else if (recurrenceGetPeriodType(r) == PERIOD_NTH_WEEKDAY)
    {
        week = day_of_month_index / 7 > 3 ? 3 : day_of_month_index / 7;
        day_of_month_index = LAST_DAY_OF_MONTH_OPTION_INDEX + 7 +
                             g_date_get_weekday(&recurrence_date) + 7 * week;


    }
    /* else { default value } */
    return day_of_month_index;
}


void
gnc_frequency_setup_recurrence(GncFrequency *gf, GList *recurrences, const GDate *start_date)
{
    gnc_frequency_setup(gf, recurrences, start_date);
}


void
gnc_frequency_setup(GncFrequency *gf, GList *recurrences, const GDate *start_date)
{
    gboolean made_changes = FALSE;

    // setup start-date, if present
    if (start_date != NULL
            && g_date_valid(start_date))
    {
        gnc_date_edit_set_gdate(gf->startDate, start_date);
        made_changes = TRUE;
    }

    if (recurrences == NULL)
    {
        goto maybe_signal;
        // return...
    }

    if (g_list_length(recurrences) > 1)
    {
        if (recurrenceListIsWeeklyMultiple(recurrences))
        {
            for (; recurrences != NULL; recurrences = recurrences->next)
            {
                _setup_weekly_recurrence(gf, (Recurrence*)recurrences->data);
            }

            gtk_notebook_set_current_page(gf->nb, PAGE_WEEKLY);
            gtk_combo_box_set_active(gf->freqComboBox, PAGE_WEEKLY);
        }
        else if (recurrenceListIsSemiMonthly(recurrences))
        {
            Recurrence *first, *second;
            GtkWidget *multiplier_spin;
            GtkWidget *dom_combobox;

            first = (Recurrence*)g_list_nth_data(recurrences, 0);
            second = (Recurrence*)g_list_nth_data(recurrences, 1);

            multiplier_spin = GTK_WIDGET(gtk_builder_get_object (gf->builder, "semimonthly_spin"));
            gtk_spin_button_set_value(GTK_SPIN_BUTTON(multiplier_spin), recurrenceGetMultiplier(first));
            dom_combobox = GTK_WIDGET(gtk_builder_get_object (gf->builder, "semimonthly_first"));
            gtk_combo_box_set_active(GTK_COMBO_BOX(dom_combobox), _get_monthly_combobox_index(first));
            dom_combobox = GTK_WIDGET(gtk_builder_get_object (gf->builder, "semimonthly_first_weekend"));
            gtk_combo_box_set_active(GTK_COMBO_BOX(dom_combobox), recurrenceGetWeekendAdjust(first));
            dom_combobox = GTK_WIDGET(gtk_builder_get_object (gf->builder, "semimonthly_second"));
            gtk_combo_box_set_active(GTK_COMBO_BOX(dom_combobox), _get_monthly_combobox_index(second));
            dom_combobox = GTK_WIDGET(gtk_builder_get_object (gf->builder, "semimonthly_second_weekend"));
            gtk_combo_box_set_active(GTK_COMBO_BOX(dom_combobox), recurrenceGetWeekendAdjust(second));

            gtk_notebook_set_current_page(gf->nb, PAGE_SEMI_MONTHLY);
            gtk_combo_box_set_active(gf->freqComboBox, PAGE_SEMI_MONTHLY);
        }
        else
        {
            g_error("unknown composite recurrence with [%d] entries", g_list_length(recurrences));
        }
    }
    else
    {
        Recurrence *r = (Recurrence*)recurrences->data;
        g_debug("recurrence period [%d]", recurrenceGetPeriodType(r));
        switch (recurrenceGetPeriodType(r))
        {
        case PERIOD_ONCE:
        {
            GDate recurrence_date = recurrenceGetDate(r);
            if (g_date_compare(start_date, &recurrence_date) != 0)
            {
                char start_date_str[128], recur_date_str[128];
                g_date_strftime(start_date_str, 127, "%x", start_date);
                g_date_strftime(recur_date_str, 127, "%x", &recurrence_date);
                g_critical("start_date [%s] != recurrence_date [%s]", start_date_str, recur_date_str);
            }

            gtk_notebook_set_current_page(gf->nb, PAGE_ONCE);
            gtk_combo_box_set_active(gf->freqComboBox, PAGE_ONCE);
        }
        break;
        case PERIOD_DAY:
        {
            guint multiplier;
            GtkWidget *spin_button;

            multiplier = recurrenceGetMultiplier(r);
            spin_button = GTK_WIDGET(gtk_builder_get_object (gf->builder, "daily_spin"));
            gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_button), multiplier);
            made_changes = TRUE;

            gtk_notebook_set_current_page(gf->nb, PAGE_DAILY);
            gtk_combo_box_set_active(gf->freqComboBox, PAGE_DAILY);
        }
        break;
        case PERIOD_WEEK:
        {
            _setup_weekly_recurrence(gf, r);
            gtk_notebook_set_current_page(gf->nb, PAGE_WEEKLY);
            gtk_combo_box_set_active(gf->freqComboBox, PAGE_WEEKLY);
        }
        break;
        case PERIOD_END_OF_MONTH:
        case PERIOD_MONTH:
        case PERIOD_YEAR:
        case PERIOD_LAST_WEEKDAY:
        case PERIOD_NTH_WEEKDAY:
        {
            guint multiplier;
            GtkWidget *multipler_spin, *day_of_month, *weekend_mode;

            multipler_spin = GTK_WIDGET(gtk_builder_get_object (gf->builder, "monthly_spin"));
            multiplier = recurrenceGetMultiplier(r);
            if (recurrenceGetPeriodType(r) == PERIOD_YEAR)
                multiplier *= 12;
            gtk_spin_button_set_value(GTK_SPIN_BUTTON(multipler_spin), multiplier);

            day_of_month = GTK_WIDGET(gtk_builder_get_object (gf->builder, "monthly_day"));
            gtk_combo_box_set_active(GTK_COMBO_BOX(day_of_month), _get_monthly_combobox_index(r));
            weekend_mode = GTK_WIDGET(gtk_builder_get_object (gf->builder, "monthly_weekend"));
            gtk_combo_box_set_active(GTK_COMBO_BOX(weekend_mode), recurrenceGetWeekendAdjust(r));

            gtk_notebook_set_current_page(gf->nb, PAGE_MONTHLY);
            gtk_combo_box_set_active(gf->freqComboBox, PAGE_MONTHLY);
        }
        break;
        default:
            g_error("unknown recurrence period type [%d]", recurrenceGetPeriodType(r));
            break;
        }
    }

maybe_signal:
    if (made_changes)
        g_signal_emit_by_name(gf, "changed");
}


static gint
_get_multiplier_from_widget(GncFrequency *gf, char *widget_name)
{
    GtkWidget *multiplier_spin;
    multiplier_spin = GTK_WIDGET(gtk_builder_get_object (gf->builder, widget_name));
    return gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(multiplier_spin));
}


static Recurrence*
_get_day_of_month_recurrence(GncFrequency *gf, GDate *start_date, int multiplier, char *combo_name, char *combo_weekend_name)
{
    Recurrence *r;
    GtkWidget *day_of_month_combo = GTK_WIDGET(gtk_builder_get_object (gf->builder, combo_name));
    int day_of_month_index = gtk_combo_box_get_active(GTK_COMBO_BOX(day_of_month_combo));
    GtkWidget *weekend_adjust_combo = GTK_WIDGET(gtk_builder_get_object (gf->builder, combo_weekend_name));
    int weekend_adjust = gtk_combo_box_get_active(GTK_COMBO_BOX(weekend_adjust_combo));
    GDateWeekday selected_day_of_week;
    GDate *day_of_week_date;
    int selected_index, selected_week;
    r = g_new0(Recurrence, 1);
    if (day_of_month_index > LAST_DAY_OF_MONTH_OPTION_INDEX + 7)
    {
        selected_index = day_of_month_index - LAST_DAY_OF_MONTH_OPTION_INDEX - 7;
        day_of_week_date = g_date_new_julian(g_date_get_julian(start_date));
        selected_week = (selected_index - 1) / 7 == 4 ? 3 : (selected_index - 1) / 7;
        selected_day_of_week = selected_index - 7 * selected_week;
        g_date_set_day(day_of_week_date, 1);
        while (g_date_get_weekday(day_of_week_date) != selected_day_of_week)
            g_date_add_days(day_of_week_date, 1);
        g_date_add_days(day_of_week_date, 7 * selected_week);
        recurrenceSet(r, multiplier, PERIOD_NTH_WEEKDAY, day_of_week_date, WEEKEND_ADJ_NONE);
    }
    else if (day_of_month_index > LAST_DAY_OF_MONTH_OPTION_INDEX)
    {
        day_of_week_date = g_date_new_julian(g_date_get_julian(start_date));
        selected_day_of_week = day_of_month_index - LAST_DAY_OF_MONTH_OPTION_INDEX;
        // increment until we align on the DOW, but stay inside the month
        g_date_set_day(day_of_week_date, 1);
        while (g_date_get_weekday(day_of_week_date) != selected_day_of_week)
            g_date_add_days(day_of_week_date, 1);
        recurrenceSet(r, multiplier, PERIOD_LAST_WEEKDAY, day_of_week_date, weekend_adjust);
    }
    else if (day_of_month_index == LAST_DAY_OF_MONTH_OPTION_INDEX)
    {
        GDate *day_of_month = g_date_new_julian(g_date_get_julian(start_date));
        recurrenceSet(r, multiplier, PERIOD_END_OF_MONTH, day_of_month, weekend_adjust);
    }
    else
    {
        int allowable_date = -1;
        GDate *day_of_month = g_date_new_julian(g_date_get_julian(start_date));
        allowable_date = MIN(day_of_month_index + 1,
                             g_date_get_days_in_month(g_date_get_month(day_of_month),
                                     g_date_get_year(day_of_month)));
        g_date_set_day(day_of_month, allowable_date);
        recurrenceSet(r, multiplier, PERIOD_MONTH, day_of_month, weekend_adjust);
    }
    return r;
}


void
gnc_frequency_save_to_recurrence(GncFrequency *gf, GList **recurrences, GDate *out_start_date)
{
    GDate start_date;
    gint page_index;

    gnc_date_edit_get_gdate(gf->startDate, &start_date);
    if (out_start_date != NULL)
        *out_start_date = start_date;

    if (recurrences == NULL)
        return;

    page_index = gtk_notebook_get_current_page(gf->nb);

    switch (page_index)
    {
    case PAGE_NONE:
    {
        // empty-recurrence list ~~ none.
    } break;
    case PAGE_ONCE:
    {
        Recurrence *r = g_new0(Recurrence, 1);
        recurrenceSet(r, 1, PERIOD_ONCE, &start_date, WEEKEND_ADJ_NONE);
        *recurrences = g_list_append(*recurrences, r);
    }
    break;
    case PAGE_DAILY:
    {
        gint multiplier = _get_multiplier_from_widget(gf, "daily_spin");
        Recurrence *r = g_new0(Recurrence, 1);
        recurrenceSet(r, multiplier, PERIOD_DAY, &start_date, WEEKEND_ADJ_NONE);
        *recurrences = g_list_append(*recurrences, r);
    }
    break;
    case PAGE_WEEKLY:
    {
        int multiplier = _get_multiplier_from_widget(gf, "weekly_spin");
        int checkbox_idx;
        for (checkbox_idx = 0; CHECKBOX_NAMES[checkbox_idx] != NULL; checkbox_idx++)
        {
            GDate *day_of_week_aligned_date;
            Recurrence *r;
            const char *day_widget_name = CHECKBOX_NAMES[checkbox_idx];
            GtkWidget *weekday_checkbox = GTK_WIDGET(gtk_builder_get_object (gf->builder, day_widget_name));

            if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(weekday_checkbox)))
                continue;

            day_of_week_aligned_date = g_date_new_julian(g_date_get_julian(&start_date));
            // increment until we align on the DOW.
            while ((g_date_get_weekday(day_of_week_aligned_date) % 7) != checkbox_idx)
                g_date_add_days(day_of_week_aligned_date, 1);

            r = g_new0(Recurrence, 1);
            recurrenceSet(r, multiplier, PERIOD_WEEK, day_of_week_aligned_date, WEEKEND_ADJ_NONE);

            *recurrences = g_list_append(*recurrences, r);
        }
    }
    break;
    case PAGE_SEMI_MONTHLY:
    {
        int multiplier = _get_multiplier_from_widget(gf, "semimonthly_spin");
        *recurrences = g_list_append(*recurrences, _get_day_of_month_recurrence(gf, &start_date, multiplier, "semimonthly_first", "semimonthly_first_weekend"));
        *recurrences = g_list_append(*recurrences, _get_day_of_month_recurrence(gf, &start_date, multiplier, "semimonthly_second", "semimonthly_second_weekend"));
    }
    break;
    case PAGE_MONTHLY:
    {
        int multiplier = _get_multiplier_from_widget(gf, "monthly_spin");
        Recurrence *r = _get_day_of_month_recurrence(gf, &start_date, multiplier, "monthly_day", "monthly_weekend");
        *recurrences = g_list_append(*recurrences, r);
    }
    break;
    default:
        g_error("unknown page index [%d]", page_index);
        break;
    }
}
