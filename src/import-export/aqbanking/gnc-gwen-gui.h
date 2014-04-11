/*
 * gnc-gwen-gui.h --
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
 * @file gnc-gwen-gui.h
 * @brief GUI callbacks for AqBanking
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_GWEN_GUI_H
#define GNC_GWEN_GUI_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

typedef struct _GncGWENGui GncGWENGui;

/**
 * Hook our logging into the gwenhywfar logging framework by creating a
 * minimalistic GWEN_GUI with only a callback for Gwen_Gui_LogHook().  This
 * function can be called more than once, it will unref and replace the
 * currently set GWEN_GUI though.
 */
void gnc_GWEN_Gui_log_init(void);

/**
 * When called for the first time, create a unique GncGWENGui object featuring a
 * GWEN_GUI with all necessary callbacks, which can serve as a user interface
 * for AqBanking jobs.  On later calls, return the object only when it is not
 * active and save to use.  Typically, you only need to call
 * gnc_GWEN_Gui_release() once your job has finished.
 *
 * @param parent Widget to set new dialogs transient for, may be NULL
 * @return The unique GncGWENGui object or NULL otherwise
 */
GncGWENGui *gnc_GWEN_Gui_get(GtkWidget *parent);

/**
 * Currently a no-op.  The GncGWENGui will not be freed and it is considered
 * finished once the first tracked progress has ended.
 *
 * @param gui The GncGwenGUI returned by gnc_GWEN_Gui_get()
 */
void gnc_GWEN_Gui_release(GncGWENGui *gui);

/**
 * Free all memory related to both the full-blown and minimalistic GUI objects.
 */
void gnc_GWEN_Gui_shutdown(void);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_GWEN_GUI_H */
