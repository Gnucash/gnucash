/********************************************************************\
 * dialog-custom-report.h -- dialog for managing custom reports     *
 *                                                                  *
 *                                                                  *
 * Copyright (C) 2009                                               *
 *                                                                  *
 * Andrew Sackville-West                                            *
 * (andrew@swclan.homelinux.org)                                    *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef DIALOG_CUSTOM_REPORT_H
#define DIALOG_CUSTOM_REPORT_H

#include "gnc-main-window.h"

/** @addtogroup Reports
    @{ */
/** @file dialog-custom-report.h
 *
 *  This file contains the functions to present a GUI to manage custom
 *  reports
 */

void
gnc_ui_custom_report(GncMainWindow * window);

/** @} */

#endif /* DIALOG_CUSTOM_REPORT_H */




