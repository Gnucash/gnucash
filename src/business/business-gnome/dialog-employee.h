/*
 * dialog-employee.h -- Dialog(s) for Employee search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_EMPLOYEE_H_
#define GNC_DIALOG_EMPLOYEE_H_

/* Functions to create and edit employees */
GncEmployee * gnc_employee_new (GtkWidget *parent, GncBusiness *bus);
void gnc_employee_edit (GtkWidget *parent, GncEmployee *employee);

/* Callbacks to select a employee that match the necessary functions
 * for use with the gnc_general_select widget.
 *
 * new_select provides a selection and the ability to create and edit
 *	employees.
 * new_edit provides only the ability to edit the current selection
 */
gpointer        gnc_employee_edit_new_select (gpointer bus, gpointer c,
					      GtkWidget *toplevel);
gpointer	gnc_employee_edit_new_edit (gpointer bus, gpointer employee,
					    GtkWidget *toplevel);

#endif /* GNC_DIALOG_EMPLOYEE_H_ */
