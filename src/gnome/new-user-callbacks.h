#include <gnome.h>


gboolean
on_newUserStartPage_next               (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

gboolean
on_chooseAccountTypesPage_next         (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
on_newUserDruidFinishPage_finish       (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);

void
on_accountChooseDruidPage_cancel       (GnomeDruid      *gnomedruid,
                                        gpointer         user_data);

void
on_newAccountRunAgain_toggled          (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_newAccountCancelDialog_OKButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
on_newAccountCancelDialog_ApplyButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data);

void
on_newAccountCancelDialog_CancelButton_clicked
                                        (GtkButton       *button,
                                        gpointer         user_data);
