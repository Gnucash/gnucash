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

void
gnc_ui_account_picker_select_cb        (GtkTree         *tree,
                                        GtkWidget       *widget,
                                        gpointer         user_data);

void
gnc_ui_account_picker_ok_cb            (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_account_picker_cancel_cb        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_preview_OK_cb             (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_dialog_select_printer_cb  (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_dialog_select_paper_cb    (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_dialog_ok_cb              (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_dialog_preview_cb         (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_dialog_cancel_cb          (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_dialog_help_cb            (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_paper_dialog_ok_cb              (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_paper_dialog_cancel_cb          (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_check_dialog_ok_cb        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_check_dialog_cancel_cb    (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_print_check_dialog_help_cb      (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_early_date_select_cb
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_late_date_select_cb
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_search_type_cb
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_ok_cb  (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_cancel_cb
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_find_transactions_dialog_help_cb
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_select_date_dialog_today_cb     (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_select_date_dialog_ok_cb        (GtkButton       *button,
                                        gpointer         user_data);

void
gnc_ui_select_date_dialog_cancel_cb    (GtkButton       *button,
                                        gpointer         user_data);

void
on_Budget_Dialog_destroy               (GtkObject       *object,
                                        gpointer         user_data);
