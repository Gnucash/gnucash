/*
 * dialog-daterange.h --
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

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file dialog-daterange.h
 * @brief Dialog for date range entry
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef DIALOG_DATERANGE_H
#define DIALOG_DATERANGE_H

#include <gtk/gtk.h>

#include "qof.h"

G_BEGIN_DECLS

/**
 * Show a dialog to pick a time frame using a sensible set of default options.
 *
 * @param parent Widget to use as parent, may be NULL
 * @param heading Descriptive text showed at the top, may be NULL
 * @param from_date Location to read from the initial and write to the final
 * value of the from date entry
 * @param last_retv_date Location to read from whether the caller knows the last
 * retrieval date and write to whether the corresponding button has been chosen
 * @param first_possible_date Location to write to whether the earliest possible
 * date button has been chosen
 * @param to_date Location to read from the initial and write to the final value
 * of the to date entry
 * @param to_now Location to write to whether the to now button has been chosen
 */
gboolean gnc_ab_enter_daterange(GtkWidget *parent,
                                const char *heading,
                                Timespec *from_date,
                                gboolean *last_retv_date,
                                gboolean *first_possible_date,
                                Timespec *to_date,
                                gboolean *to_now);

G_END_DECLS

#endif /* DIALOG_DATERANGE_H */
