/*
 * dialog-employee.h -- Dialog(s) for Employee search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */


#ifndef GNC_DIALOG_EMPLOYEE_H_
#define GNC_DIALOG_EMPLOYEE_H_

typedef struct _employee_window EmployeeWindow;

#include "gncEmployee.h"
#include "dialog-search.h"

/* Functions to edit and create employees */
EmployeeWindow * gnc_ui_employee_edit (GtkWindow *parent, GncEmployee *employee);
EmployeeWindow * gnc_ui_employee_new (GtkWindow *parent, QofBook *book);

/* Search for an employee */
GNCSearchWindow * gnc_employee_search (GtkWindow *parent, GncEmployee *start, QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing vendor for editing and returns NULL.
 */
GNCSearchWindow * gnc_employee_search_select (GtkWindow *parent, gpointer start, gpointer book);
GNCSearchWindow * gnc_employee_search_edit (GtkWindow *parent, gpointer start, gpointer book);

#endif /* GNC_DIALOG_EMPLOYEE_H_ */
