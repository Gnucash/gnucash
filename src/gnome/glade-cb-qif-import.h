#include <gnome.h>


void
gnc_ui_qif_import_select_loaded_file_cb
                                        (GtkList         *list,
                                        GtkWidget       *widget,
                                        gpointer         user_data);

void
gnc_ui_qif_import_select_file_cb       (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_qif_import_load_file_cb         (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_qif_import_account_line_select_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
gnc_ui_qif_import_category_line_select_cb
                                        (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
gnc_ui_qif_import_ok_cb                (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_qif_import_cancel_cb            (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_qif_import_help_cb              (GtkButton       *button,
                                        gpointer         user_data);
