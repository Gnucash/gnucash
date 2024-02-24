/* gnc-recurrence.h:
 *
 *  GncRecurrence is a minimal GUI for specifying a Recurrence.
 *
 *  You see, small is _nice_.  :)
 *
 */

/* Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>  *
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
 * Boston, MA  02111-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_RECURRENCE_H
#define GNC_RECURRENCE_H

#include <glib.h>
#include "Recurrence.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_TYPE_RECURRENCE	  (gnc_recurrence_get_type())
G_DECLARE_FINAL_TYPE (GncRecurrence, gnc_recurrence, GNC, RECURRENCE, GtkBox)

GtkWidget * gnc_recurrence_new(void);

void gnc_recurrence_set(GncRecurrence *gr, const Recurrence *r);

/* The returned Recurrence is internally owned and is only valid as
   long as the GncRecurrence is around. */
const Recurrence * gnc_recurrence_get(GncRecurrence *gr);

#ifdef __cplusplus
}
#endif

#endif
