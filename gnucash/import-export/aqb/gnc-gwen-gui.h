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
#include <gwenhywfar/dialog.h>

G_BEGIN_DECLS

typedef struct _GncGWENGui GncGWENGui;

/* The work function is called on the serialized AqBanking worker. It may use
 * the prepared Gwen GUI callbacks, which marshal any GTK interaction back to
 * the GTK thread. The completion and destroy functions run on the GTK thread.
 */
typedef void (*GncGwenJobWork) (GncGWENGui *gui, gpointer user_data);
typedef void (*GncGwenJobComplete) (gpointer user_data);
typedef void (*GncGWENDialogDoneCallback) (gboolean accepted,
                                           gpointer user_data);

/**
 * Hook our logging into the gwenhywfar logging framework by creating a
 * minimalistic GWEN_GUI with only a callback for Gwen_Gui_LogHook().  This
 * function can be called more than once, it will unref and replace the
 * currently set GWEN_GUI though.
 */
void gnc_GWEN_Gui_log_init(void);

/**
 * Reserve a GncGWENGui object featuring a GWEN_GUI with all necessary
 * callbacks. Independent operations may reserve distinct objects; AqBanking
 * work is serialized by gnc_GWEN_Gui_run_job_async(). Release the reservation
 * after its final GTK-thread continuation has completed.
 *
 * @param parent Widget to set new dialogs transient for, may be NULL
 * @return A reserved GUI object, or NULL when called outside the GTK thread
 */
GncGWENGui *gnc_GWEN_Gui_get(GtkWidget *parent);

/** Run one AqBanking operation off the GTK thread. Only one operation may be
 * active at a time because AqBanking mutates shared bank/provider state.
 * @a work must restrict itself to the prepared backend job phase; GnuCash/QOF
 * continuations belong in @a completed.
 */
void gnc_GWEN_Gui_run_job_async (GncGWENGui *gui, GncGwenJobWork work,
                                 GncGwenJobComplete completed,
                                 gpointer user_data, GDestroyNotify destroy);

/** Open a Gwen dialog without a nested GTK loop. Call on GTK's main thread
 * with a reserved GUI. The callback runs exactly once on GTK's main thread
 * after CloseDialog and signal-handler restoration. The caller retains
 * ownership of @a dialog until the callback; parent destruction completes it
 * as rejected. Keep @a gui reserved until that callback. */
void gnc_GWEN_Gui_exec_dialog_async (GncGWENGui *gui, GWEN_DIALOG *dialog,
                                     GncGWENDialogDoneCallback completed,
                                     gpointer user_data);

/**
 * Release a reservation on the GTK thread. The object remains cached for
 * reuse and is freed by gnc_GWEN_Gui_shutdown() after shutdown barriers drain.
 *
 * @param gui The GncGwenGUI returned by gnc_GWEN_Gui_get()
 */
void gnc_GWEN_Gui_release(GncGWENGui *gui);

/**
 * Free all memory related to both the full-blown and minimalistic GUI objects.
 */
void gnc_GWEN_Gui_shutdown(void);

/**
 * Set "Close when finished" flag
 *
 * @param gboolean close_when_finished
 */
void gnc_GWEN_Gui_set_close_flag(gboolean close_when_finished);

/**
 * Get "Close when finished" flag
 *
 * @return gboolean close_when_finished
 */
gboolean gnc_GWEN_Gui_get_close_flag(void);

/**
 * Unhides Online Banking Connection Window (Make log visible)
 *
 * @return gboolean window is visible
 */
gboolean gnc_GWEN_Gui_show_dialog(void);

/**
 * Hides Online Banking Connection Window (Close log window)
 *
 */
void gnc_GWEN_Gui_hide_dialog(void);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_GWEN_GUI_H */
