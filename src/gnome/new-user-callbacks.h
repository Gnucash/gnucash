#ifndef _NEW_USER_CALLBACKS_H_
#define _NEW_USER_CALLBACKS_H_

#include <gnome.h>
#include <glib.h>

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
on_newAccountCurrencyChoosePage_prepare
                                        (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                         gpointer         user_data);

void
on_chooseAccountTypesPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);


#endif /* _NEW_USER_CALLBACKS_H_ */    

void
on_newUserDruidFinishPage_prepare      (GnomeDruidPage  *gnomedruidpage,
                                        gpointer         arg1,
                                        gpointer         user_data);
