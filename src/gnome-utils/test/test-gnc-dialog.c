/* Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
 * This file is free software.  See COPYING for details. */

#include "config.h"

#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include "gnc-recurrence.h"
#include "Recurrence.h"
#include "qof.h"
#include "gnc-dialog.h"
#include "gnc-tree-view-account.h"

//static GtkWidget * mainwin;
static GncDialog *pw;
//static QofBook *book;


static gboolean apply_cb (GncDialog *pw, gpointer _n)
{
    gchar *s;
    gdouble d;
    gpointer p;
    gboolean b;
    gint i;

    s = gnc_dialog_get_custom(pw, "SampleEntry");
    printf("Entry: %s\n", s);
    s = gnc_dialog_get_string(pw, "SampleLabel");
    printf("Label: %s\n", s);
    g_free(s);

    p = gnc_dialog_get_custom(pw, "SampleSpinButton");
    d = *(double *)p;
    printf("SpinButton: %f\n", d);

    b = gnc_dialog_get_boolean(pw, "SampleToggleButton");
    printf("ToggleButton: %s\n", b?"true":"false");

    i = gnc_dialog_get_index(pw, "SampleComboBox");
    printf("ComboBox: %d\n", i);

    /*
    gnc_dialog_get(pw, "SampleEntry", &s);
    gnc_dialog_get(pw, "SampleLabel", &s);
    printf("Label: %s\n", s);
    gnc_dialog_get(pw, "SampleSpinButton", &d);
    printf("SpinButton: %f\n", *d);
    gnc_dialog_get(pw, "SampleToggleButton", &b);
    printf("ToggleButton: %s\n", *b?"true":"false");
    gnc_dialog_get(pw, "SampleComboBox", &i);
    printf("ComboBox: %d\n", *i);
    */

    //r = gnc_dialog_get(pw, "SampleCustomGncRecurrence");
    //s = recurrenceToString(r);
    //printf("Recurrence: %s\n", s);
    //g_free(s);
    return TRUE;

}

static gboolean close_cb (GncDialog *pw, gpointer _n)
{
    gtk_widget_destroy(GTK_WIDGET(pw));
    gtk_main_quit();
    return TRUE;
}

static void test_setters(GncDialog *pw)
{
    gdouble d = 3.0;
    //GDate date;
    //gboolean b = TRUE;
    //gint i = 2;
    //Recurrence *r;
    //GList *rl;

    gnc_dialog_set_custom(pw, "SampleEntry", "entrytest");
    //gnc_dialog_set(pw, "SampleLabel", "labeltest");
    gnc_dialog_set_custom(pw, "SampleSpinButton", &d);
    //gnc_dialog_set(pw, "SampleToggleButton", &b);
    //gnc_dialog_set(pw, "SampleComboBox", &i);

    /*
    r = recurrenceNew();
    entry = recurrenceEntryNew();
    recurrenceAddEntry(r, entry);
    g_date_set_dmy(&date, 17, 4, 2005);
    recurrenceEntrySet(entry, 2, PERIOD_WEEKLY, &date);
    gnc_dialog_set(pw, "SampleCustomGncRecurrence", r);
    g_free(r);
    */
}

static void init_widgets(GncDialog *pw)
{
    //GtkComboBox *cbox;
    GtkListStore *ls;
    GtkTreeIter iter;
    GtkCellRenderer *cell;
    int i;

    ls = gtk_list_store_new(1, G_TYPE_STRING);
    for (i = 0; i<5; i++) {
        gtk_list_store_append(ls, &iter);
        gtk_list_store_set(ls, &iter, 0, "item", -1);
    }

    //cbox = GTK_COMBO_BOX(gnc_dialog_get_widget(pw, "SampleComboBox"));
    //gtk_combo_box_set_model(cbox, GTK_TREE_MODEL(ls));

    cell = gtk_cell_renderer_text_new();
    //gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(cbox), cell, TRUE);
    //gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(cbox), cell, "text", 0);

    {
        /*
          GncTreeViewAccount *account_view;

          account_view = GNC_TREE_VIEW_ACCOUNT(gnc_dialog_get_widget(pw, "SampleCustomTreeViewAccount"));

          gnc_tree_view_account_configure_columns (account_view, "name", NULL);
        */
    }
}

int main (int argc, char ** argv)
{
 // g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

  gtk_init(&argc, &argv);

  g_type_init();
  pw = gnc_dialog_new("budget.glade", "SampleOptions");
  gnc_dialog_set_cb(pw, apply_cb, close_cb, NULL, NULL);

  gnc_dialog_register_testing_types();
  init_widgets(pw);

  test_setters(pw);
  gtk_widget_show_all(GTK_WIDGET(pw));

  gtk_main();
  return 0;

}
