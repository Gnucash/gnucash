/* gnc-recurrence.c:
 *
 */

/* Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gnome.h>
#include <glade/glade.h>

#include "dialog-utils.h"
#include "gnc-recurrence.h"
#include "Recurrence.h"
#include "gnc-engine.h"
#include "gnc-gdate-utils.h"

static QofLogModule log_module = GNC_MOD_GUI;

struct _GncRecurrence {
    GtkVBox widget;

    GnomeDateEdit *gde_start;
    GtkComboBox *gcb_period;
    GtkCheckButton *gcb_eom;
    GtkSpinButton *gsb_mult;
    GtkCheckButton *nth_weekday;
    GladeXML *xml;

    Recurrence recurrence;
};

typedef struct {
    GtkVBoxClass parent_class;
    void (*changed) (GncRecurrence *gr);
} GncRecurrenceClass;

typedef enum {
    GNCRECURRENCE_CHANGED,
    LAST_SIGNAL
} GNCR_Signals;

typedef enum {
    GNCR_DAY,
    GNCR_WEEK,
    GNCR_MONTH,
    GNCR_YEAR,
} UIPeriodType;

static GObjectClass *parent_class = NULL;

static UIPeriodType get_pt_ui(GncRecurrence *gr)
{
    return (gtk_combo_box_get_active(gr->gcb_period));
}

static void set_pt_ui(GncRecurrence *gr, PeriodType pt)
{
    UIPeriodType idx;
    switch (pt) {
    case PERIOD_DAY:
        idx = 0; break;
    case PERIOD_WEEK:
        idx = 1; break;
    case PERIOD_MONTH:
    case PERIOD_END_OF_MONTH:
    case PERIOD_NTH_WEEKDAY:
    case PERIOD_LAST_WEEKDAY:
        idx = 2; break;
    case PERIOD_YEAR:
        idx = 3; break;
    default: return;
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
    time_t t;
    gboolean show_last, use_wd;
    GncRecurrence *gr = GNC_RECURRENCE(d);


    pt = get_pt_ui(gr);
    t = gnome_date_edit_get_time(gr->gde_start);
    g_date_set_time_t(&start, t);

    if (pt == GNCR_MONTH)
        g_object_set(G_OBJECT(gr->nth_weekday), "visible", TRUE, NULL);
    else {
        g_object_set(G_OBJECT(gr->nth_weekday), "visible", FALSE, NULL);
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(gr->nth_weekday), FALSE);
    }
    use_wd = gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(gr->nth_weekday));
    //TODO: change label

    /* The case under which we show the "end of month" flag is very
       narrow, because we can almost always DTRT without it. */
    if (pt == GNCR_MONTH) {
        if (use_wd)
            show_last = is_ambiguous_relative(&start);
        else
            show_last = is_ambiguous_absolute(&start);
    } else {
        show_last = FALSE;
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gr->gcb_eom), FALSE);
    }
    g_object_set(G_OBJECT(gr->gcb_eom), "visible", show_last, NULL);

    g_signal_emit_by_name(d, "changed");
}

static void
gnc_recurrence_init( GncRecurrence *gr )
{
    GtkVBox *vb;

    recurrenceSet(&gr->recurrence, 1, PERIOD_MONTH, NULL, WEEKEND_ADJ_NONE);

    gr->xml = gnc_glade_xml_new("budget.glade", "RecurrenceEntryVBox");
    vb = GTK_VBOX(glade_xml_get_widget(gr->xml, "RecurrenceEntryVBox"));
    gr->gde_start = GNOME_DATE_EDIT(glade_xml_get_widget(gr->xml,
                                                         "GDE_StartDate"));
    gtk_widget_set_no_show_all(GTK_WIDGET(gr->gde_start), TRUE);
    gr->gcb_period = GTK_COMBO_BOX(glade_xml_get_widget(gr->xml,
                                                        "GCB_PeriodType"));
    gr->gsb_mult = GTK_SPIN_BUTTON(glade_xml_get_widget(gr->xml, "GSB_Mult"));
    gr->gcb_eom = GTK_CHECK_BUTTON(glade_xml_get_widget(gr->xml,
                                                        "GCB_EndOfMonth"));
    gr->nth_weekday = GTK_CHECK_BUTTON(glade_xml_get_widget(gr->xml,
                                                            "GCB_NthWeekday"));
    gtk_widget_set_no_show_all(GTK_WIDGET(gr->gcb_eom), TRUE);
    gtk_widget_set_no_show_all(GTK_WIDGET(gr->nth_weekday), TRUE);


    gtk_container_add( GTK_CONTAINER(&gr->widget), GTK_WIDGET(vb) );

    gnc_recurrence_set(gr, &gr->recurrence);
    something_changed( GTK_WIDGET(gr), gr);

    /* respond to changes */
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
        time_t t;
        t = gnc_timet_get_day_start_gdate (&start);
        gnome_date_edit_set_time(gr->gde_start, t);
    }

    set_pt_ui(gr, pt);
}

const Recurrence *
gnc_recurrence_get(GncRecurrence *gr)
{
    time_t t;
    guint mult;
    UIPeriodType period;
    PeriodType pt;
    GDate start;
    gboolean use_eom = FALSE, rel;

    mult = (guint) gtk_spin_button_get_value_as_int(gr->gsb_mult);
    t = gnome_date_edit_get_time(gr->gde_start);
    g_date_set_time_t(&start, t);
    period = get_pt_ui(gr);

    switch (period) {
    case GNCR_DAY:
        pt = PERIOD_DAY; break;
    case GNCR_WEEK:
        pt = PERIOD_WEEK; break;
    case GNCR_MONTH:
        rel = gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(gr->nth_weekday));
        if (rel) {
            if (is_ambiguous_relative(&start)) {
                use_eom = gtk_toggle_button_get_active(
                    GTK_TOGGLE_BUTTON(gr->gcb_eom));
            } else {
                GDateDay d;
                d = g_date_get_day(&start);

                use_eom = ((d - 1) / 7 == 4);
            }
            if (use_eom)
                pt = PERIOD_LAST_WEEKDAY;
            else pt = PERIOD_NTH_WEEKDAY;
        } else {
            if (g_date_is_last_of_month(&start) &&
                (g_date_get_day(&start) < 31)) {
                // ambiguous, need to examine the checkbox
                use_eom = gtk_toggle_button_get_active(
                    GTK_TOGGLE_BUTTON(gr->gcb_eom));
            } else {
                // if it's the last dom, use eom anyway because it's the 31st.
                use_eom = g_date_is_last_of_month(&start);
            }
            if (use_eom)
                pt = PERIOD_END_OF_MONTH;
            else pt = PERIOD_MONTH;
        }
        break;
    case GNCR_YEAR:
        pt = PERIOD_YEAR; break;
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
        G_OBJECT_CLASS (parent_class)->finalize (o);
}

static void
gnc_recurrence_class_init( GncRecurrenceClass *klass )
{
    GObjectClass *object_class;
    static gint signals[LAST_SIGNAL] = { 0 };

    object_class = G_OBJECT_CLASS (klass);
    signals[GNCRECURRENCE_CHANGED] =
        g_signal_new ("changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GncRecurrenceClass, changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    parent_class = g_type_class_peek_parent (klass);
    object_class->finalize = gnc_recurrence_finalize;
}

GType
gnc_recurrence_get_type()
{
    static GType type = 0;
    if (type == 0) {
        static GTypeInfo typeinfo = {
            sizeof(GncRecurrenceClass),
            NULL, NULL,
            (GClassInitFunc)gnc_recurrence_class_init,
            NULL, NULL,
            sizeof(GncRecurrence),
            0,
            (GInstanceInitFunc)gnc_recurrence_init
        };

        type = g_type_register_static (GTK_TYPE_VBOX, "GncRecurrence",
                                       &typeinfo, 0);
    }
    return type;
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

/* TODO: Maybe this stuff should go into another file.
 *
 */

struct _GncRecurrenceComp {
    GtkScrolledWindow widget;

    GtkVBox *vbox;
    GtkHBox *hbox;
    GtkHButtonBox *hbb;
    gint num_rec;
    GtkButton *buttRemove;
    GtkButton *buttAdd;

    GList *rlist;
};

typedef struct {
    GtkScrolledWindowClass parent_class;
    void (*changed) (GncRecurrenceComp *gr);
} GncRecurrenceCompClass;

typedef enum {
    GNCRECURRENCECOMP_CHANGED,
    GNCRC_LAST_SIGNAL
} GNCRC_Signals;

static void grc_changed(GtkWidget *w, gpointer data)
{
    g_signal_emit_by_name(data, "changed");
}
static void addRecurrence(GncRecurrenceComp *grc, GncRecurrence *gr)
{

    gtk_box_pack_start(GTK_BOX(grc->vbox), GTK_WIDGET(gr),
                       FALSE, FALSE, 3);
    g_signal_connect( G_OBJECT(gr), "changed",
                      G_CALLBACK(grc_changed), grc );
    grc->num_rec++;

    gtk_widget_set_sensitive(GTK_WIDGET(grc->buttRemove), (grc->num_rec > 1));
    g_signal_emit_by_name(G_OBJECT(grc), "changed");


}
static void removeRecurrence(GncRecurrenceComp *grc)
{
    GList *children, *last;

    grc->num_rec--;

    children = gtk_container_get_children(GTK_CONTAINER(grc->vbox));
    last = g_list_last(children);
    gtk_widget_destroy(GTK_WIDGET(last->data));
    g_list_free(children);
    g_signal_emit_by_name(G_OBJECT(grc), "changed");


    gtk_widget_set_sensitive(GTK_WIDGET(grc->buttRemove), (grc->num_rec > 1));

}

static void addClicked(GtkButton *b, gpointer data)
{
    GncRecurrenceComp *grc = data;
    GncRecurrence *gr;

    gr = GNC_RECURRENCE(gnc_recurrence_new());
    addRecurrence(grc, gr);
}

static void removeClicked(GtkButton *b, gpointer data)
{
    GncRecurrenceComp *grc = data;

    if (grc->num_rec > 1)
        removeRecurrence(grc);
}

void
gnc_recurrence_comp_set_list(GncRecurrenceComp *grc, const GList *rlist)
{
    const GList *iter;

    g_return_if_fail(grc);

    while (grc->num_rec > 0)
        removeRecurrence(grc);

    for (iter = rlist; iter; iter = iter->next) {
        GncRecurrence *gr = GNC_RECURRENCE(gnc_recurrence_new());

        gnc_recurrence_set(gr, (Recurrence *)iter->data);
        addRecurrence(grc, gr);
    }
}

GList *
gnc_recurrence_comp_get_list(GncRecurrenceComp *grc)
{
    GList *rlist = NULL, *children;
    gint i;


    children = gtk_container_get_children(GTK_CONTAINER(grc->vbox));
    for (i = 0; i < g_list_length(children); i++) {
        GncRecurrence *gr;
        const Recurrence *r;
        gr = GNC_RECURRENCE(g_list_nth_data(children, i));
        r = gnc_recurrence_get(gr);
        rlist = g_list_append(rlist, (gpointer)r);
    }
    g_list_free(children);
    return rlist;
}


static void
gnc_recurrence_comp_init(GncRecurrenceComp *grc)
{
    GtkWidget *vb;

    grc->hbb = GTK_HBUTTON_BOX(gtk_hbutton_box_new());
    grc->vbox = GTK_VBOX(gtk_vbox_new(FALSE, 1));
    grc->rlist = NULL;

    grc->buttAdd = GTK_BUTTON(gtk_button_new_from_stock(GTK_STOCK_ADD));
    g_signal_connect(G_OBJECT(grc->buttAdd), "clicked",
                     G_CALLBACK(addClicked), grc);
    grc->buttRemove = GTK_BUTTON(gtk_button_new_from_stock(GTK_STOCK_REMOVE));
    g_signal_connect(G_OBJECT(grc->buttRemove), "clicked",
                     G_CALLBACK(removeClicked), grc);

    gtk_box_pack_start(GTK_BOX(grc->hbb), GTK_WIDGET(grc->buttAdd),
                     FALSE, FALSE, 3);
    gtk_box_pack_start(GTK_BOX(grc->hbb), GTK_WIDGET(grc->buttRemove),
                     FALSE, FALSE, 3);

    vb = gtk_vbox_new(FALSE, 1);
    gtk_box_pack_start(GTK_BOX(vb), GTK_WIDGET(grc->hbb),
                       FALSE, FALSE, 3);
    gtk_box_pack_start(GTK_BOX(vb), GTK_WIDGET(grc->vbox),
                       FALSE, FALSE, 3);

    gtk_scrolled_window_add_with_viewport(
        GTK_SCROLLED_WINDOW(grc), GTK_WIDGET(vb));
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(grc),
                                   GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

    grc->num_rec = 0;
    gtk_widget_show_all(GTK_WIDGET(grc));
    addClicked(NULL, grc);
}

static void
gnc_recurrence_comp_class_init( GncRecurrenceCompClass *klass )
{
    GObjectClass *object_class;
    static gint signals[GNCRC_LAST_SIGNAL] = { 0 };

    object_class = G_OBJECT_CLASS (klass);
    signals[GNCRECURRENCECOMP_CHANGED] =
        g_signal_new ("changed",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GncRecurrenceCompClass, changed),
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);

    //parent_class = g_type_class_peek_parent (klass);
    //object_class->finalize = gnc_recurrence_finalize;
}

GType
gnc_recurrence_comp_get_type()
{
    static GType type = 0;
    if (type == 0) {
        static GTypeInfo typeinfo = {
            sizeof(GncRecurrenceCompClass),
            NULL, NULL,
            (GClassInitFunc)gnc_recurrence_comp_class_init,
            NULL, NULL,
            sizeof(GncRecurrenceComp),
            0,
            (GInstanceInitFunc)gnc_recurrence_comp_init
        };

        type = g_type_register_static (GTK_TYPE_SCROLLED_WINDOW,
                                       "GncRecurrenceComp", &typeinfo, 0);
    }
    return type;
}

GtkWidget *
gnc_recurrence_comp_new()
{
    GncRecurrenceComp *grc;
    grc = g_object_new(gnc_recurrence_comp_get_type(), NULL);
    return GTK_WIDGET(grc);
}

/* ========================= END OF FILE =========================== */
