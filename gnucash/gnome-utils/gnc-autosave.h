/*
 * gnc-autosave.h -- Functions related to the auto-save feature.
 *
 * Copyright (C) 2007 Christian Stimming <stimming@tuhh.de>
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

#ifndef _GNC_AUTOSAVE_H
#define _GNC_AUTOSAVE_H

#include <gtk/gtk.h>
#include "qof.h"

/** Callback that is used to notify the autosave subsystem when the
    QofBook changed its dirty state. */
void gnc_autosave_dirty_handler (QofBook *book, gboolean dirty);

/** Removes any still existing autosave timer from the event loop. */
void gnc_autosave_remove_timer(QofBook *book);

#endif
