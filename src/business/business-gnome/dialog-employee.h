/*
 * dialog-employee.h -- Dialog(s) for Employee search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_EMPLOYEE_H_
#define GNC_DIALOG_EMPLOYEE_H_

typedef struct _employee_window EmployeeWindow;

#include "gncEmployee.h"
#include "dialog-search.h"

/* Functions to edit and create employees */
EmployeeWindow * gnc_ui_employee_edit (GncEmployee *employee);
EmployeeWindow * gnc_ui_employee_new (GNCBook *book);

/* Search for an employee */
GNCSearchWindow * gnc_employee_search (GncEmployee *start, GNCBook *book);

/* Callbacks to select a employee that match the necessary functions
 * for use with the gnc_general_select widget.
 *
 * new_select provides a selection and the ability to create and edit
 *	employees.
 * new_edit provides only the ability to edit the current selection
 */
gpointer        gnc_employee_edit_new_select (gpointer book, gpointer c,
					      GtkWidget *toplevel);
gpointer	gnc_employee_edit_new_edit (gpointer book, gpointer employee,
					    GtkWidget *toplevel);

#endif /* GNC_DIALOG_EMPLOYEE_H_ */
