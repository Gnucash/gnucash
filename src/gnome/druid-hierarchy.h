/********************************************************************\
 * druid-hierarchy.h -- account hierarchy creation functionality    *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
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

#ifndef DRUID_HIERARCHY_H
#define DRUID_HIERARCHY_H

/**
 * A callback (provided by the caller) to be invoked when the druid
 * completes successfully.  I.e., the new-user druid can finish the GnuCash
 * New-User Experience, create an account plugin-page, &c.
 **/

typedef void (*GncHierarchyDruidFinishedCallback)(void);

GtkWidget* gnc_ui_hierarchy_druid (gboolean use_defaults);
GtkWidget* gnc_ui_hierarchy_druid_with_callback(gboolean use_defaults, GncHierarchyDruidFinishedCallback when_finished);

void gnc_ui_hierarchy_druid_initialize (void);

#endif
