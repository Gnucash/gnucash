#include "config.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <time.h>

#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "gnc-date.h"
#include "gnc-tree-view.h"
#include "gnc-ui-util.h"

#include "gnc-tree-model-transaction.h"
#include "gnc-tree-view-transaction.h"
#include "Group.h"
#include "Transaction.h"
#include "qof.h"
#include "gnc-engine.h"
#include "gnc-session.h"

static QofSession *session;
static QofBook *book;
static GtkWidget *mainwin;


static void
die(void)
{
    qof_session_destroy (session);
    gtk_main_quit();
}


static QofQuery *
make_query (QofBook *book)
{
    QofQuery *q;

    q = qof_query_create_for(GNC_ID_SPLIT);
    qof_query_set_book (q, book);
    return q;
}
static void
just_get_one(QofEntity *ent, gpointer data)
{
    if (ent)
        *(gpointer *)data = ent;
}

int main (int argc, char ** argv)
{
    GtkTreeView *tv;
    GncTreeModelTransaction *tm;
    QofQuery *q;
    GtkWidget *scroll;
    Account *acct1, *acct2;
    Transaction *trans;
    Split *split;
    QofCollection *coll;

    srand(0);
    g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

    gnc_engine_init(argc, argv);
    gtk_init(&argc, &argv);
    gnc_log_default();
    qof_log_set_level(GNC_MOD_GUI, QOF_LOG_DEBUG);
    qof_log_set_level(GNC_MOD_ENGINE, QOF_LOG_DEBUG);

    g_type_init();

    session = get_random_session ();
    gnc_set_current_session(session);
    book = qof_session_get_book (session);
    add_random_transactions_to_book (book, 5);
    coll = qof_book_get_collection(book, GNC_ID_TRANS);
    qof_collection_foreach(coll, just_get_one, &trans);
    split = xaccTransGetSplit(trans, 0);
    acct1 = xaccSplitGetAccount(split);
    split = xaccTransGetSplit(trans, 1);
    acct2 = xaccSplitGetAccount(split);

    q = make_query(book);

    mainwin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(mainwin), xaccAccountGetName(acct1));
    g_signal_connect(mainwin, "delete-event", G_CALLBACK(die), NULL);

    tm = gnc_tree_model_transaction_new_from_account(acct1);
    if (0) {
        GtkTreeViewColumn *col;
        GtkTreeModel *s_model;

        tv = GTK_TREE_VIEW(g_object_new(gnc_tree_view_transaction_get_type(),
                                        NULL));

        s_model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(tm));
        //tv = GTK_TREE_VIEW(gtk_tree_view_new_with_model(s_model));
        gnc_tree_view_set_model(GNC_TREE_VIEW(tv), GTK_TREE_MODEL(s_model));
        g_object_unref(G_OBJECT(s_model));
        col = gnc_tree_view_add_text_column(
            GNC_TREE_VIEW(tv), "num", "num_pref",
            NULL, "xxx", GNC_TREE_MODEL_TRANSACTION_COL_NUM,
            GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, NULL);
        col = gnc_tree_view_add_text_column(
            GNC_TREE_VIEW(tv), "desc", "desc_pref",
            NULL, "xxx", GNC_TREE_MODEL_TRANSACTION_COL_DESCRIPTION,
            GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS, NULL);
        gnc_tree_view_configure_columns(GNC_TREE_VIEW(tv));

    } else {
        tv = GTK_TREE_VIEW(gnc_tree_view_transaction_new_with_model(tm));
    }
    g_object_set(G_OBJECT(tv), "gconf-section",
                 "gnome-utils/test/register",
                 "show-column-menu", TRUE, NULL);
    g_object_unref(tm);

    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(tv));
    gtk_container_add(GTK_CONTAINER(mainwin), scroll);
    gtk_window_set_default_size(GTK_WINDOW(mainwin), 890, 400);
    gtk_widget_show_all(mainwin);

    mainwin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(mainwin), xaccAccountGetName(acct2));
    g_signal_connect(mainwin, "delete-event", G_CALLBACK(die), NULL);

    tm = gnc_tree_model_transaction_new_from_account(acct2);
    tv = GTK_TREE_VIEW(gnc_tree_view_transaction_new_with_model(tm));
    g_object_unref(tm);

    g_object_set(G_OBJECT(tv), "gconf-section", "gnome-utils/test/register",
                 "show-column-menu", TRUE, NULL);
    scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_container_add(GTK_CONTAINER(scroll),  GTK_WIDGET(tv));
    gtk_container_add(GTK_CONTAINER(mainwin), scroll);
    gtk_window_set_default_size(GTK_WINDOW(mainwin), 890, 400);

    gtk_widget_show_all(mainwin);
    gtk_main();
    return 0;
}
