/********************************************************************\
 * dialog-totd.h : dialog to display a "tip of the day"             *
 *                                                                  *
 * Initial copyright not recorded.                                  *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
\********************************************************************/

#ifndef DIALOG_TOTD_H
#define DIALOG_TOTD_H

void gnc_totd_dialog (GtkWindow *parent, gboolean startup);
void gnc_totd_dialog_reparent (void);

#endif
