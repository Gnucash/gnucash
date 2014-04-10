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
EmployeeWindow * gnc_ui_employee_new (QofBook *book);

/* Search for an employee */
GNCSearchWindow * gnc_employee_search (GncEmployee *start, QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing vendor for editing and returns NULL.
 */
GNCSearchWindow * gnc_employee_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_employee_search_edit (gpointer start, gpointer book);

#endif /* GNC_DIALOG_EMPLOYEE_H_ */
