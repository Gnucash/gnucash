// Dialogs.h

struct _add_account_dialog 
{
  GtkWidget 	*add_account_dialog;
  GtkWidget 	*main_vbox;
  GtkWidget 	*box2;
  GtkWidget	*box3;
  GtkWidget	*box4;
  GtkWidget 	*frame;
  
  GSList 	*group;
  
  GList		*parent_accounts;
  
  GtkWidget 	*label;
  GtkWidget 	*textbox_name;
  GtkWidget	*textbox_description;
  GtkWidget	*button;
  GtkWidget 	*okButton;
  GtkWidget 	*cancelButton;
  GtkWidget	*separator;
};

typedef struct _add_account_dialog add_account_dialog;

GtkWidget *create_add_account_dialog ( void );
gpointer *add_account_dialog_init ( void );


