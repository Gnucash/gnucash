/* File: plot-test.c */

#include <stdio.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <plot.h>

#include <Account.h>
#include <LedgerUtils.h>
#include <FileIO.h>

static int
graph_balance(Split **splits) {
  
  FILE *child = popen("graph -T X", "w");
  int i = 0;

  assert(child != NULL);

  while(*splits) {
    const double split_balance = xaccGetBalance(*splits);
    fprintf(child, "%d %f\n", i, split_balance);
    i++;
    splits++;
  }  
  pclose(child);
  return 0;
}

void
destroy() {
  gtk_main_quit ();
}

static void
graph_account_balance_per_transaction(gpointer item, gpointer user_data) {
  Account *acct = (Account *) 
    gtk_object_get_data(GTK_OBJECT(item), "acct_ptr");
  
  fprintf(stderr, "%s\n", acct->accountName);  
  
  xaccRecomputeBalance(acct);
  {
    Account *list[] = { acct, NULL };
    Split **splits = accListGetSortedSplits(list);
    graph_balance(splits);
  }
}

static void
plot(GtkButton *button, gpointer data) {
  GtkWidget *list = GTK_WIDGET(data);
  GList *selection = GTK_LIST(list)->selection;
  g_list_foreach(selection, mangle_account, NULL);
}

static GtkWidget *
make_main_window(GtkWidget **list) {

  GtkWidget *window;
  GtkWidget *vbox;
  GtkWidget *quit_button;
  GtkWidget *plot_button;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  *list = gtk_list_new();

  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC (destroy), NULL);
  gtk_container_border_width (GTK_CONTAINER (window), 3);

  quit_button = gtk_button_new_with_label ("Quit");

  gtk_signal_connect_object (GTK_OBJECT (quit_button), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (window));
  
  plot_button = gtk_button_new_with_label ("Plot");

  gtk_signal_connect (GTK_OBJECT (plot_button), "clicked",
                      GTK_SIGNAL_FUNC (plot), *list);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add (GTK_CONTAINER(window), vbox);
  
  gtk_box_pack_start(GTK_BOX(vbox), *list, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(vbox), quit_button, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(vbox), plot_button, FALSE, FALSE, 0);

  gtk_widget_show (*list);
  gtk_widget_show (plot_button);
  gtk_widget_show (quit_button);
  gtk_widget_show(vbox);
  gtk_widget_show (window);

  return(window);
}

static void
add_accounts_to_list(GtkWidget *list, const char *filename) {
  AccountGroup *db = xaccReadAccountGroup(filename);
  if(!db) {
    fprintf(stderr, "db: %p error: %d\n", db,  xaccGetFileIOError());
    exit(1);
  }  

  {
    int count = xaccGetNumAccounts(db);
    int i;
    GtkWidget *item;
      
    for(i=0; i<count; i++) {
      Account *acc = getAccount(db, i);
      item = gtk_list_item_new_with_label(acc->accountName);
      gtk_object_set_data(GTK_OBJECT(item), "acct_ptr", acc); 
      gtk_container_add(GTK_CONTAINER(list), item);
      gtk_widget_show(item);
    }
  }
}

int
main(int argc, char *argv[]) {
  
  int handle;
  char *filename;
  GtkWidget *main_win;
  GtkWidget *display_area;
  GtkWidget *list = NULL;

  
  gtk_init(&argc, &argv);
  
  if(argc != 2) {
    fprintf(stderr, "usage: plot-test filename\n");
    exit(1);
  }
  filename = argv[1];

  main_win = make_main_window(&list);
  add_accounts_to_list(list, filename);

  gtk_main();
  
  return(0);
}
