/* Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
 * This file is free software.  See COPYING for details. */

/* test-gnc-recurrence.c:
 *
 *     When you close the window, a text description of the
 * recurrence is printed.
 *
 */

#include "config.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include "gnc-recurrence.h"
#include "Recurrence.h"

static GtkWidget * mainwin;
static GncRecurrence *rw;
static GncRecurrenceComp *grc;

static void get_list(GtkWidget *w)
{
    gchar *s;
    GList *rlist;
    rlist = gnc_recurrence_comp_get_list(grc);
    s = recurrenceListToString(rlist);
    printf("%s\n", s);

    g_free(s);
    g_list_free(rlist);
}

static void changed(GtkWidget *widget)
{
    gchar *s;
    const Recurrence *r;

    r = gnc_recurrence_get(rw);
    s = recurrenceToString(r);
    printf("%s\n", s);
    g_free(s);
}

static void die(GtkWidget *widget)
{
    gtk_main_quit();
}

static void show_gnc_recurrence()
{
    GDate d;
    Recurrence *r;
    GList *rl = NULL;

    rw = GNC_RECURRENCE(gnc_recurrence_new());

    r = g_new(Recurrence, 1);
    rl = g_list_append(rl, r);
    g_date_set_dmy(&d, 17, 4, 2005);
    recurrenceSet(r, 1, PERIOD_WEEK, &d, WEEKEND_ADJ_NONE);

    gnc_recurrence_set(rw, r);
    g_free(r);

    gtk_container_add(GTK_CONTAINER(mainwin), GTK_WIDGET(rw));
    g_signal_connect(rw, "changed", G_CALLBACK(changed), NULL);
}

static void show_gnc_recurrence_comp()
{
    GList *rlist = NULL;
    Recurrence r[2];

    grc = (GncRecurrenceComp *)gnc_recurrence_comp_new();

    gtk_container_add(GTK_CONTAINER(mainwin), GTK_WIDGET(grc));

    recurrenceSet(&r[0], 1, PERIOD_MONTH, NULL, WEEKEND_ADJ_NONE);
    rlist = g_list_append(rlist, &r[0]);
    recurrenceSet(&r[1], 1, PERIOD_YEAR, NULL, WEEKEND_ADJ_NONE);
    rlist = g_list_append(rlist, &r[1]);

    gnc_recurrence_comp_set_list(grc, rlist);
    g_list_free(rlist);

    g_signal_connect(grc, "changed", G_CALLBACK(get_list), NULL);
    //rlist = gnc_recurrence_comp_get_list(grc);
}


int main (int argc, char ** argv)
{
    gtk_init(&argc, &argv);

    mainwin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    g_signal_connect(mainwin, "delete-event", G_CALLBACK(die), NULL);

    if (argc > 1)
        show_gnc_recurrence();
    else
        show_gnc_recurrence_comp();

    gtk_widget_show_all(mainwin);
    gtk_main();
    return 0;
}
