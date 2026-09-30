/*
 * gnc-gwen-gui.c --
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
 * @internal
 * @file gnc-gwen-gui.c
 * @brief GUI callbacks for AqBanking
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include <ctype.h>
#include <stdint.h>
#include <glib/gi18n.h>
#include <gwenhywfar/gui_be.h>
#include <gwenhywfar/inherit.h>
#include <gwenhywfar/version.h>
#include <gwenhywfar/dialog_be.h>
#include <gwenhywfar/widget_be.h>

#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gnome-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-session.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-plugin-aqbanking.h"
#include "qof.h"

#include "gnc-flicker-gui.h"

# define GNC_GWENHYWFAR_CB GWENHYWFAR_CB

#define GWEN_GUI_CM_CLASS "dialog-hbcilog"
#define GNC_PREFS_GROUP_CONNECTION GNC_PREFS_GROUP_AQBANKING ".connection-dialog"
#define GNC_PREF_CLOSE_ON_FINISH   "close-on-finish"
#define GNC_PREF_REMEMBER_PIN      "remember-pin"

# include <gwen-gui-gtk3/gtk3_gui.h>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/* A unique full-blown GUI, featuring  */
static GncGWENGui *full_gui = NULL;
static GList *all_guis;
typedef struct _GncGWENAsyncDialog GncGWENAsyncDialog;

/* All widgets in the Gwen adapter belong to the GTK thread. Backend worker
 * callbacks use this context to marshal short GUI updates to that thread. */
static GThread *gtk_thread;
static GMainContext *gtk_context;
static GMutex aq_job_mutex;
static guint aq_shutdown_barrier;
static guint aq_active_jobs;
typedef struct { GSourceFunc finished; gpointer data; } AqShutdownWaiter;
static GList *aq_shutdown_waiters;
static guint aq_shutdown_poll_source;
static GWEN_GUI *gwen_gui_for_job (GncGWENGui *gui);
static gchar *strip_html (gchar *text);
static gboolean aq_gwen_shutdown_poll (gpointer unused);
static gboolean aq_gwen_has_leased_gui (void);
static gboolean gwen_async_dialog_finish_on_main (gpointer user_data);
static int GNC_GWENHYWFAR_CB gwen_exec_dialog_on_worker (
    GWEN_GUI *gwen_gui, GWEN_DIALOG *dialog, uint32_t guiid);
static GncGWENAsyncDialog *active_gwen_dialog;

static void
aq_gwen_finish_shutdown_waiters (void)
{
    GList *waiters = g_steal_pointer (&aq_shutdown_waiters);
    for (GList *link = waiters; link; link = link->next)
    {
        AqShutdownWaiter *waiter = link->data;
        waiter->finished (waiter->data);
        g_free (waiter);
    }
    g_list_free (waiters);
}

static gboolean
aq_gwen_shutdown_poll ([[maybe_unused]] gpointer unused)
{
    if (aq_active_jobs || aq_gwen_has_leased_gui () ||
        gnc_gui_session_operation_pending ())
    {
        return G_SOURCE_CONTINUE;
    }
    aq_shutdown_poll_source = 0;
    aq_gwen_finish_shutdown_waiters ();
    return G_SOURCE_REMOVE;
}

static void
aq_gwen_schedule_shutdown_poll (void)
{
    if (!aq_shutdown_poll_source)
    {
        GSource *source = g_timeout_source_new (25);
        g_source_set_callback (source, aq_gwen_shutdown_poll, NULL, NULL);
        aq_shutdown_poll_source = g_source_attach (source, gtk_context);
        g_source_unref (source);
    }
}

typedef struct
{
    GMutex mutex;
    GCond completed_cond;
    gboolean completed;
    GSourceFunc function;
    gpointer data;
} GncGwenMainCall;

typedef struct
{
    GncGWENGui *gui;
    GncGwenJobWork work;
    GncGwenJobComplete completed;
    gpointer user_data;
    GDestroyNotify destroy;
} GncGwenJob;

typedef struct
{
    GWEN_DIALOG *dialog;
    GWEN_DIALOG_SIGNALHANDLER old_handler;
    GWEN_DIALOG_SIGNALHANDLER2 old_handler2;
} GncGWENDialogHandler;

struct _GncGWENAsyncDialog
{
    GncGWENGui *gui;
    GWEN_DIALOG *dialog;
    GtkWidget *window;
    GList *handlers;
    GncGWENDialogDoneCallback completed;
    gpointer user_data;
    gulong destroy_handler;
    gulong delete_handler;
    gboolean opened;
    gboolean finishing;
    gboolean accepted;
    gboolean window_destroyed;
};

static gboolean
gwen_job_complete_on_gtk_thread (gpointer user_data)
{
    GncGwenJob *job = user_data;
    if (job->completed)
        job->completed (job->user_data);
    if (job->destroy)
        job->destroy (job->user_data);
    g_free (job);
    g_assert (aq_active_jobs > 0);
    --aq_active_jobs;
    if (aq_shutdown_waiters)
        aq_gwen_schedule_shutdown_poll ();
    return G_SOURCE_REMOVE;
}

static void
aq_gwen_shutdown_barrier ([[maybe_unused]] gpointer provider_data,
                          GSourceFunc finished,
                          gpointer finished_data)
{
    g_return_if_fail (g_thread_self () == gtk_thread);
    if (aq_active_jobs == 0 && !aq_gwen_has_leased_gui () &&
        !gnc_gui_session_operation_pending ())
    {
        finished (finished_data);
        return;
    }
    AqShutdownWaiter *waiter = g_new0 (AqShutdownWaiter, 1);
    waiter->finished = finished;
    waiter->data = finished_data;
    aq_shutdown_waiters = g_list_append (aq_shutdown_waiters, waiter);
    aq_gwen_schedule_shutdown_poll ();
}

static gpointer
gwen_job_worker (gpointer user_data)
{
    GncGwenJob *job = user_data;
    GSource *source;

    /* AqBanking's command executor changes job status/IDs, provider state,
     * and crypt-token bookkeeping. Serialize that phase for the shared API.
     */
    g_mutex_lock (&aq_job_mutex);
    GWEN_Gui_SetGui (gwen_gui_for_job (job->gui));
    job->work (job->gui, job->user_data);
    GWEN_Gui_SetGui (NULL);
    g_mutex_unlock (&aq_job_mutex);

    source = g_idle_source_new ();
    g_source_set_callback (source, gwen_job_complete_on_gtk_thread, job, NULL);
    g_source_attach (source, gtk_context);
    g_source_unref (source);
    return NULL;
}

void
gnc_GWEN_Gui_run_job_async (GncGWENGui *gui, GncGwenJobWork work,
                            GncGwenJobComplete completed,
                            gpointer user_data, GDestroyNotify destroy)
{
    GncGwenJob *job;

    g_return_if_fail (gui && g_list_find (all_guis, gui));
    g_return_if_fail (g_thread_self () == gtk_thread);
    g_return_if_fail (gwen_gui_for_job (gui) != NULL);
    g_return_if_fail (work != NULL);

    job = g_new0 (GncGwenJob, 1);
    job->gui = gui;
    job->work = work;
    job->completed = completed;
    job->user_data = user_data;
    job->destroy = destroy;
    ++aq_active_jobs;
    g_thread_unref (g_thread_new ("aqbanking-job", gwen_job_worker, job));
}

static gboolean
gwen_main_call_dispatch (gpointer user_data)
{
    GncGwenMainCall *call = user_data;
    call->function (call->data);
    g_mutex_lock (&call->mutex);
    call->completed = TRUE;
    g_cond_signal (&call->completed_cond);
    g_mutex_unlock (&call->mutex);
    return G_SOURCE_REMOVE;
}

/* This is deliberately a one-shot source, not g_main_context_invoke(): the
 * latter may call its function directly on the caller if it can acquire the
 * context, which would run GTK from the AqBanking worker. */
static void
gwen_call_on_gtk_thread (GSourceFunc function, gpointer data)
{
    GncGwenMainCall call = {0};
    GSource *source;

    if (g_thread_self () == gtk_thread)
    {
        function (data);
        return;
    }

    g_return_if_fail (gtk_context != NULL);
    g_mutex_init (&call.mutex);
    g_cond_init (&call.completed_cond);
    call.function = function;
    call.data = data;

    source = g_idle_source_new ();
    g_source_set_priority (source, G_PRIORITY_DEFAULT);
    g_source_set_callback (source, gwen_main_call_dispatch, &call, NULL);
    g_source_attach (source, gtk_context);
    g_source_unref (source);

    g_mutex_lock (&call.mutex);
    while (!call.completed)
        g_cond_wait (&call.completed_cond, &call.mutex);
    g_mutex_unlock (&call.mutex);
    g_cond_clear (&call.completed_cond);
    g_mutex_clear (&call.mutex);
}

/* A unique Gwenhywfar GUI for hooking our logging into the gwenhywfar logging
 * framework */
static GWEN_GUI *log_gwen_gui = NULL;

/* A mapping from gwenhywfar log levels to glib ones */
static GLogLevelFlags log_levels[] =
{
    G_LOG_LEVEL_ERROR,     /* GWEN_LoggerLevel_Emergency */
    G_LOG_LEVEL_ERROR,     /* GWEN_LoggerLevel_Alert */
    G_LOG_LEVEL_CRITICAL,  /* GWEN_LoggerLevel_Critical */
    G_LOG_LEVEL_CRITICAL,  /* GWEN_LoggerLevel_Error */
    G_LOG_LEVEL_WARNING,   /* GWEN_LoggerLevel_Warning */
    G_LOG_LEVEL_MESSAGE,   /* GWEN_LoggerLevel_Notice */
    G_LOG_LEVEL_INFO,      /* GWEN_LoggerLevel_Info */
    G_LOG_LEVEL_DEBUG,     /* GWEN_LoggerLevel_Debug */
    G_LOG_LEVEL_DEBUG      /* GWEN_LoggerLevel_Verbous */
};
static guint8 n_log_levels = G_N_ELEMENTS(log_levels);

/* Macros to determine the GncGWENGui* from a GWEN_GUI* */
GWEN_INHERIT(GWEN_GUI, GncGWENGui)
#define SETDATA_GUI(gwen_gui, gui) GWEN_INHERIT_SETDATA(GWEN_GUI, GncGWENGui, \
                                                        (gwen_gui), (gui), NULL)
#define GETDATA_GUI(gwen_gui) GWEN_INHERIT_GETDATA(GWEN_GUI, GncGWENGui, (gwen_gui))

#define OTHER_ENTRIES_ROW_OFFSET 3

typedef struct _Progress Progress;
typedef enum _GuiState GuiState;

static void register_callbacks(GncGWENGui *gui);
static void unregister_callbacks(GncGWENGui *gui);
static void setup_dialog(GncGWENGui *gui);
static void enable_password_cache(GncGWENGui *gui, gboolean enabled);
static void reset_dialog(GncGWENGui *gui);
static void set_finished(GncGWENGui *gui);
static void set_aborted(GncGWENGui *gui);
static void ggg_cancel_confirmation_done (GtkWindow *parent, gint response,
                                         gpointer user_data);
static void show_dialog(GncGWENGui *gui, gboolean clear_log);
static void hide_dialog(GncGWENGui *gui);
static gboolean show_progress_cb(gpointer user_data);
static void show_progress(GncGWENGui *gui, Progress *progress);
static void hide_progress(GncGWENGui *gui, Progress *progress);
static void free_progress(Progress *progress, gpointer unused);
static gboolean keep_alive(GncGWENGui *gui);
static void cm_close_handler(gpointer user_data);
static void erase_password(gchar *password);
static gchar *strip_html(gchar *text);
static void get_input(GncGWENGui *gui, guint32 flags, const gchar *title,
                      const gchar *text, const char *mimeType,
                      const char *pChallenge, uint32_t lChallenge,
                      gchar **input, gint min_len, gint max_len);
static gint GNC_GWENHYWFAR_CB messagebox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                          const gchar *text, const gchar *b1, const gchar *b2,
                          const gchar *b3, guint32 guiid);
static gint GNC_GWENHYWFAR_CB inputbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                        const gchar *text, gchar *buffer, gint min_len,
                        gint max_len, guint32 guiid);
static guint32 GNC_GWENHYWFAR_CB showbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                          const gchar *text, guint32 guiid);
static void GWENHYWFAR_CB hidebox_cb(GWEN_GUI *gwen_gui, guint32 id);
static guint32 GNC_GWENHYWFAR_CB progress_start_cb(GWEN_GUI *gwen_gui, uint32_t progressFlags,
                                 const char *title, const char *text,
                                 uint64_t total, uint32_t guiid);
static gint GNC_GWENHYWFAR_CB progress_advance_cb(GWEN_GUI *gwen_gui, uint32_t id,
                                uint64_t new_progress);
static gint GNC_GWENHYWFAR_CB progress_log_cb(GWEN_GUI *gwen_gui, guint32 id,
                            GWEN_LOGGER_LEVEL level, const gchar *text);
static gint GNC_GWENHYWFAR_CB progress_end_cb(GWEN_GUI *gwen_gui, guint32 id);
static gint GNC_GWENHYWFAR_CB getpassword_cb(GWEN_GUI *gwen_gui, guint32 flags,
                                             const gchar *token,
                                             const gchar *title,
                                             const gchar *text, gchar *buffer,
                                             gint min_len, gint max_len,
                                             GWEN_GUI_PASSWORD_METHOD methodId,
                                             GWEN_DB_NODE *methodParams,
                                             guint32 guiid);
static gint GNC_GWENHYWFAR_CB setpasswordstatus_cb(GWEN_GUI *gwen_gui, const gchar *token,
        const gchar *pin,
        GWEN_GUI_PASSWORD_STATUS status, guint32 guiid);
static gint GNC_GWENHYWFAR_CB loghook_cb(GWEN_GUI *gwen_gui, const gchar *log_domain,
        GWEN_LOGGER_LEVEL priority, const gchar *text);
typedef GWEN_SYNCIO GWEN_IO_LAYER;
static gint GNC_GWENHYWFAR_CB checkcert_cb(GWEN_GUI *gwen_gui, const GWEN_SSLCERTDESCR *cert,
        GWEN_IO_LAYER *io, guint32 guiid);

gboolean ggg_delete_event_cb(GtkWidget *widget, GdkEvent *event,
                             gpointer user_data);
void ggg_abort_clicked_cb(GtkButton *button, gpointer user_data);
void ggg_close_clicked_cb(GtkButton *button, gpointer user_data);
void ggg_close_toggled_cb(GtkToggleButton *button, gpointer user_data);

enum _GuiState
{
    INIT,
    RUNNING,
    FINISHED,
    ABORTED,
    HIDDEN
};

struct _GncGWENGui
{
    GWEN_GUI *gwen_gui;
    GWEN_GUI_EXEC_DIALOG_FN builtin_exec_dialog;
    GtkWidget *parent;
    GWeakRef parent_ref;
    gulong parent_destroy_handler;
    gboolean parent_destroyed;
    gboolean had_parent;
    GtkWidget *dialog;
    GtkWidget *active_input_dialog;
    GtkWidget *active_message_dialog;

    /* Progress bars */
    GtkWidget *entries_grid;
    GtkWidget *top_entry;
    GtkWidget *top_progress;
    GtkWidget *second_entry;
    GtkWidget *other_entries_box;

    /* Stack of nested Progresses */
    GList *progresses;

    /* Number of steps in top-level progress or -1 */
    guint64 max_actions;
    guint64 current_action;

    /* Log window */
    GtkWidget *log_text;

    /* Buttons */
    GtkWidget *abort_button;
    GtkWidget *close_button;
    GtkWidget *close_checkbutton;

    /* Flags to keep track on whether an HBCI action is running or not */
    gboolean keep_alive;
    GuiState state;
    gboolean cancel_confirmation_pending;
    gboolean leased;
    gboolean release_pending;

    /* Password caching */
    gboolean cache_passwords;
    GMutex password_mutex;
    GHashTable *passwords;

    /* Certificates handling */
    GHashTable *accepted_certs;
    GWEN_DB_NODE *permanently_accepted_certs;
    GWEN_GUI_CHECKCERT_FN builtin_checkcert;

    /* Dialogs */
    guint32 showbox_id;
    GHashTable *showbox_hash;
    GtkWidget *showbox_last;

    GncGWENAsyncDialog *active_exec_dialog;

    /* Cache the lowest loglevel, corresponding to the most serious warning */
    GWEN_LOGGER_LEVEL min_loglevel;
};

static void
gwen_async_dialog_schedule_finish (GncGWENAsyncDialog *request,
                                  gboolean accepted)
{
    GSource *source;
    if (!request || request->finishing)
    {
        if (request && !accepted)
            request->accepted = FALSE;
        return;
    }
    request->finishing = TRUE;
    request->accepted = accepted;
    source = g_idle_source_new ();
    g_source_set_callback (source,
        (GSourceFunc)gwen_async_dialog_finish_on_main, request, NULL);
    g_source_attach (source, gtk_context);
    g_source_unref (source);
}

static int GNC_GWENHYWFAR_CB
gwen_async_dialog_signal (GWEN_DIALOG *dialog, GWEN_DIALOG_EVENTTYPE type,
                          const char *sender, int int_arg,
                          const char *string_arg)
{
    GncGWENAsyncDialog *request = active_gwen_dialog;
    GncGWENDialogHandler *handler = NULL;
    int result = GWEN_DialogEvent_ResultNotHandled;
    if (!request)
        return GWEN_DialogEvent_ResultNotHandled;
    for (GList *node = request->handlers; node; node = node->next)
    {
        GncGWENDialogHandler *candidate = node->data;
        if (candidate->dialog == dialog)
        {
            handler = candidate;
            break;
        }
    }
    if (!handler)
        return GWEN_DialogEvent_ResultNotHandled;
    /* Ignore late user activation once completion has been scheduled. Apart
     * from risking a second response, forwarding it may mutate backend data
     * while the dialog is already being closed. Fini remains forwarded so
     * the Gwen backend can finish its cleanup. */
    if (request->finishing && type == GWEN_DialogEvent_TypeActivated)
        return GWEN_DialogEvent_ResultHandled;
    if (handler->old_handler2)
        result = handler->old_handler2 (dialog, type, sender, int_arg,
                                        string_arg);
    else if (handler->old_handler)
        result = handler->old_handler (dialog, type, sender);

    if (result == GWEN_DialogEvent_ResultAccept ||
        result == GWEN_DialogEvent_ResultReject)
    {
        if (type != GWEN_DialogEvent_TypeInit &&
            type != GWEN_DialogEvent_TypeFini)
        {
            if (request->finishing)
                return GWEN_DialogEvent_ResultHandled;
            gwen_async_dialog_schedule_finish (request,
                result == GWEN_DialogEvent_ResultAccept);
            /* GTK3 widget adapters call Gtk3Gui_Dialog_Leave on Accept or
             * Reject. Handled prevents access to synchronous-loop state. */
            return GWEN_DialogEvent_ResultHandled;
        }
    }
    if (type == GWEN_DialogEvent_TypeClose)
    {
        gwen_async_dialog_schedule_finish (request, FALSE);
        return GWEN_DialogEvent_ResultHandled;
    }
    return result;
}

static gboolean
gwen_async_dialog_delete_event ([[maybe_unused]] GtkWidget *window,
                                [[maybe_unused]] GdkEvent *event,
                                gpointer user_data)
{
    GncGWENAsyncDialog *request = user_data;
    gwen_async_dialog_schedule_finish (request, FALSE);
    return TRUE;
}

static void
gwen_async_dialog_window_destroyed ([[maybe_unused]] GtkWidget *window,
                                    gpointer user_data)
{
    GncGWENAsyncDialog *request = user_data;
    request->window_destroyed = TRUE;
    gwen_async_dialog_schedule_finish (request, FALSE);
}

static void
gwen_async_dialog_restore_handlers (GncGWENAsyncDialog *request)
{
    for (GList *node = request->handlers; node; node = node->next)
    {
        GncGWENDialogHandler *handler = node->data;
        GWEN_Dialog_SetSignalHandler2 (handler->dialog,
                                       handler->old_handler2);
        GWEN_Dialog_SetSignalHandler (handler->dialog,
                                     handler->old_handler);
    }
}

static gboolean
gwen_async_dialog_finish_on_main (gpointer user_data)
{
    GncGWENAsyncDialog *request = user_data;
    GncGWENGui *gui = request->gui;
    GncGWENDialogDoneCallback completed = request->completed;
    gpointer completed_data = request->user_data;
    gboolean accepted = request->accepted && !gui->parent_destroyed &&
        !request->window_destroyed;

    if (request->window)
    {
        if (request->destroy_handler)
            g_signal_handler_disconnect (request->window,
                                        request->destroy_handler);
        if (request->delete_handler)
            g_signal_handler_disconnect (request->window,
                                        request->delete_handler);
    }
    if (request->opened)
    {
        GWEN_Gui_SetGui (gui->gwen_gui);
        if (GWEN_Gui_CloseDialog (request->dialog) < 0)
            accepted = FALSE;
    }
    /* CloseDialog unextends the backend but leaves its GTK widgets allocated
     * until the caller frees GWEN_DIALOG. Destroy them here while the Gwen
     * objects and our signal guards are still alive, so late GTK teardown
     * signals cannot reference a freed GWEN_DIALOG. */
    if (request->window && !request->window_destroyed &&
        !gtk_widget_in_destruction (request->window))
        gtk_widget_destroy (request->window);
    gwen_async_dialog_restore_handlers (request);
    if (gui->active_exec_dialog == request)
        gui->active_exec_dialog = NULL;
    if (active_gwen_dialog == request)
        active_gwen_dialog = NULL;
    if (request->window)
        g_object_unref (request->window);
    g_list_free_full (request->handlers, g_free);
    g_free (request);
    completed (accepted, completed_data);
    return G_SOURCE_REMOVE;
}

static void
gwen_async_dialog_add_handler (GncGWENAsyncDialog *request,
                               GWEN_DIALOG *dialog)
{
    GncGWENDialogHandler *handler;
    if (!dialog)
        return;
    for (GList *node = request->handlers; node; node = node->next)
        if (((GncGWENDialogHandler *)node->data)->dialog == dialog)
            return;
    handler = g_new0 (GncGWENDialogHandler, 1);
    handler->dialog = dialog;
    handler->old_handler = GWEN_Dialog_SetSignalHandler (dialog, NULL);
    handler->old_handler2 = GWEN_Dialog_SetSignalHandler2 (
        dialog, gwen_async_dialog_signal);
    request->handlers = g_list_prepend (request->handlers, handler);
}

void
gnc_GWEN_Gui_exec_dialog_async (GncGWENGui *gui, GWEN_DIALOG *dialog,
                                GncGWENDialogDoneCallback completed,
                                gpointer user_data)
{
    GncGWENAsyncDialog *request;
    GWEN_WIDGET_TREE *tree;
    GWEN_WIDGET *widget;
    GWEN_DIALOG *widget_dialog;
    int rv;

    g_return_if_fail (gui && dialog && completed);
    g_return_if_fail (g_thread_self () == gtk_thread);
    g_return_if_fail (g_list_find (all_guis, gui) && gui->leased);
    if (gui->parent_destroyed || gui->active_exec_dialog || active_gwen_dialog)
    {
        /* The API promises a deferred, exactly-once callback even when the
         * parent disappeared or another Gwen dialog owns the GTK backend. */
        request = g_new0 (GncGWENAsyncDialog, 1);
        request->gui = gui;
        request->dialog = dialog;
        request->completed = completed;
        request->user_data = user_data;
        request->finishing = TRUE;
        request->accepted = FALSE;
        g_idle_add_full (G_PRIORITY_DEFAULT,
                         gwen_async_dialog_finish_on_main, request, NULL);
        return;
    }

    request = g_new0 (GncGWENAsyncDialog, 1);
    request->gui = gui;
    request->dialog = dialog;
    request->completed = completed;
    request->user_data = user_data;
    gui->active_exec_dialog = request;
    active_gwen_dialog = request;

    /* Gwen keeps each widget's owning dialog publically accessible. Include
     * subdialog handlers so signals from nested dialog sections are caught. */
    gwen_async_dialog_add_handler (request, dialog);
    tree = GWEN_Dialog_GetWidgets (dialog);
    for (widget = tree ? GWEN_Widget_Tree_GetFirst (tree) : NULL;
         widget; widget = GWEN_Widget_Tree_GetBelow (widget))
    {
        widget_dialog = GWEN_Widget_GetDialog (widget);
        gwen_async_dialog_add_handler (request, widget_dialog);
    }

    GWEN_Gui_SetGui (gui->gwen_gui);
    rv = GWEN_Gui_OpenDialog (dialog, 0);
    if (rv < 0)
    {
        gwen_async_dialog_schedule_finish (request, FALSE);
        return;
    }
    request->opened = TRUE;
    tree = GWEN_Dialog_GetWidgets (dialog);
    widget = tree ? GWEN_Widget_Tree_GetFirst (tree) : NULL;
    /* In the Gwen GTK3 implementation slot zero is the realized widget. This
     * public backend data accessor is needed only for GTK destroy/delete
     * lifecycle signals, which the Gwen abstract dialog API does not expose. */
    request->window = widget ? GWEN_Widget_GetImplData (widget, 0) : NULL;
    if (!request->window || !GTK_IS_WINDOW (request->window))
    {
        request->window = NULL;
        gwen_async_dialog_schedule_finish (request, FALSE);
        return;
    }
    g_object_ref (request->window);
    request->delete_handler = g_signal_connect (request->window,
        "delete-event", G_CALLBACK (gwen_async_dialog_delete_event), request);
    request->destroy_handler = g_signal_connect (request->window, "destroy",
        G_CALLBACK (gwen_async_dialog_window_destroyed), request);
    if (gui->parent_destroyed)
        gwen_async_dialog_schedule_finish (request, FALSE);
}

typedef struct
{
    GncGWENGui *gui;
    GWEN_DIALOG *dialog;
    GMutex mutex;
    GCond condition;
    gboolean finished;
    gboolean accepted;
} GncGWENExecWait;

static void
gwen_exec_dialog_finished (gboolean accepted, gpointer user_data)
{
    GncGWENExecWait *wait = user_data;
    g_mutex_lock (&wait->mutex);
    wait->accepted = accepted;
    wait->finished = TRUE;
    g_cond_signal (&wait->condition);
    g_mutex_unlock (&wait->mutex);
}

static gboolean
gwen_exec_dialog_start_on_main (gpointer user_data)
{
    GncGWENExecWait *wait = user_data;
    gnc_GWEN_Gui_exec_dialog_async (wait->gui, wait->dialog,
                                    gwen_exec_dialog_finished, wait);
    return G_SOURCE_REMOVE;
}

/* Gwen may request ExecDialog from a backend worker. Preserve its synchronous
 * contract there by waiting on this worker's condition while the main thread
 * runs the same callback-driven adapter as the frontend Setup wizard. */
static int GNC_GWENHYWFAR_CB
gwen_exec_dialog_on_worker (GWEN_GUI *gwen_gui, GWEN_DIALOG *dialog,
                            uint32_t guiid)
{
    GncGWENGui *gui = GETDATA_GUI (gwen_gui);
    GncGWENExecWait wait = {0};
    gboolean accepted;
    if (!gui || !dialog)
        return 0;
    /* AqBanking also invokes Gwen dialogs from GTK-thread certificate and
     * editor callbacks. Those are outside the async assistant path and still
     * require the stock GTK3 backend behavior. Keep the library callback as
     * the explicit compatibility boundary; worker requests use our async
     * adapter below and never enter Gwen's nested loop on the GTK thread. */
    if (g_thread_self () == gtk_thread)
        return gui->builtin_exec_dialog
            ? gui->builtin_exec_dialog (gwen_gui, dialog, guiid) : 0;
    wait.gui = gui;
    wait.dialog = dialog;
    g_mutex_init (&wait.mutex);
    g_cond_init (&wait.condition);
    gwen_call_on_gtk_thread (gwen_exec_dialog_start_on_main, &wait);
    g_mutex_lock (&wait.mutex);
    while (!wait.finished)
        g_cond_wait (&wait.condition, &wait.mutex);
    accepted = wait.accepted;
    g_mutex_unlock (&wait.mutex);
    g_cond_clear (&wait.condition);
    g_mutex_clear (&wait.mutex);
    return accepted ? 1 : 0;
}

static void
gwen_parent_destroyed ([[maybe_unused]] GtkWidget *parent, GncGWENGui *gui)
{
    gui->parent_destroyed = TRUE;
    gui->parent_destroy_handler = 0;
    gui->parent = NULL;
    if (gui->dialog && !gtk_widget_in_destruction (gui->dialog))
        gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), NULL);
    if (gui->active_input_dialog &&
        !gtk_widget_in_destruction (gui->active_input_dialog))
        gtk_widget_destroy (gui->active_input_dialog);
    if (gui->active_message_dialog &&
        !gtk_widget_in_destruction (gui->active_message_dialog))
        gtk_widget_destroy (gui->active_message_dialog);
    if (gui->active_exec_dialog)
        gwen_async_dialog_schedule_finish (gui->active_exec_dialog, FALSE);
}

static void
gwen_set_parent (GncGWENGui *gui, GtkWidget *parent)
{
    GtkWidget *old_parent = g_weak_ref_get (&gui->parent_ref);
    if (old_parent && gui->parent_destroy_handler)
        g_signal_handler_disconnect (old_parent, gui->parent_destroy_handler);
    g_clear_object (&old_parent);
    g_weak_ref_set (&gui->parent_ref, parent ? G_OBJECT (parent) : NULL);
    gui->parent = parent;
    gui->had_parent = parent != NULL;
    gui->parent_destroyed = FALSE;
    gui->parent_destroy_handler = parent
        ? g_signal_connect (parent, "destroy",
                            G_CALLBACK (gwen_parent_destroyed), gui) : 0;
}

struct _Progress
{
    GncGWENGui *gui;

    /* Title of the process */
    gchar *title;

    /* Event source id for showing delayed */
    guint source;
};

static gboolean
aq_gwen_has_leased_gui (void)
{
    for (GList *node = all_guis; node; node = node->next)
        if (((GncGWENGui *)node->data)->leased)
            return TRUE;
    return FALSE;
}

static GWEN_GUI *
gwen_gui_for_job (GncGWENGui *gui)
{
    return gui->gwen_gui;
}

typedef enum
{
    GWEN_SIMPLE_SHOWBOX,
    GWEN_SIMPLE_HIDEBOX,
    GWEN_SIMPLE_PROGRESS_START,
    GWEN_SIMPLE_PROGRESS_ADVANCE,
    GWEN_SIMPLE_PROGRESS_LOG,
    GWEN_SIMPLE_PROGRESS_END,
    GWEN_SIMPLE_PASSWORD_STATUS,
    GWEN_SIMPLE_CHECK_CERT
} GwenSimpleCallType;

typedef struct
{
    GwenSimpleCallType type;
    GWEN_GUI *gwen_gui;
    guint32 flags;
    guint32 id;
    guint64 amount;
    const gchar *title;
    const gchar *text;
    GWEN_LOGGER_LEVEL level;
    gint result;
    const GWEN_SSLCERTDESCR *cert;
    GWEN_IO_LAYER *io;
} GwenSimpleCall;

static gboolean
gwen_simple_call_on_main (gpointer user_data)
{
    GwenSimpleCall *call = user_data;
    switch (call->type)
    {
    case GWEN_SIMPLE_SHOWBOX:
        call->result = showbox_cb (call->gwen_gui, call->flags,
                                   call->title, call->text, call->id);
        break;
    case GWEN_SIMPLE_HIDEBOX:
        hidebox_cb (call->gwen_gui, call->id);
        break;
    case GWEN_SIMPLE_PROGRESS_START:
        call->result = progress_start_cb (call->gwen_gui, call->flags,
                                          call->title, call->text,
                                          call->amount, call->id);
        break;
    case GWEN_SIMPLE_PROGRESS_ADVANCE:
        call->result = progress_advance_cb (call->gwen_gui, call->id,
                                            call->amount);
        break;
    case GWEN_SIMPLE_PROGRESS_LOG:
        call->result = progress_log_cb (call->gwen_gui, call->id,
                                         call->level, call->text);
        break;
    case GWEN_SIMPLE_PROGRESS_END:
        call->result = progress_end_cb (call->gwen_gui, call->id);
        break;
    case GWEN_SIMPLE_PASSWORD_STATUS:
        call->result = setpasswordstatus_cb (call->gwen_gui, call->title,
                                              call->text,
                                              (GWEN_GUI_PASSWORD_STATUS)call->flags,
                                              call->id);
        break;
    case GWEN_SIMPLE_CHECK_CERT:
        call->result = checkcert_cb (call->gwen_gui, call->cert, call->io,
                                      call->id);
        break;
    }
    return G_SOURCE_REMOVE;
}

static gboolean
gwen_simple_call_if_worker (GwenSimpleCall *call)
{
    if (g_thread_self () == gtk_thread)
        return FALSE;
    gwen_call_on_gtk_thread (gwen_simple_call_on_main, call);
    return TRUE;
}

typedef struct
{
    GMutex mutex;
    GCond condition;
    gboolean complete;
    gchar *input;
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *entry;
    GtkWidget *confirm_entry;
    GtkWidget *heading;
    GtkWidget *remember;
    GncGWENGui *gui;
    gchar *title;
    gchar *text;
    gint min_len;
    gboolean confirm;
    gboolean is_tan;
    gint max_len;
    gchar *mime_type;
    guchar *challenge;
    guint32 challenge_len;
    guint32 flags;
    GncFlickerGui *flicker_gui;
} GwenInputRequest;

static void
gwen_input_finished (GwenInputRequest *request)
{
    g_mutex_lock (&request->mutex);
    if (!request->complete)
    {
        request->complete = TRUE;
        g_cond_signal (&request->condition);
    }
    g_mutex_unlock (&request->mutex);
}

static void
gwen_input_destroyed ([[maybe_unused]] GtkWidget *dialog, gpointer user_data)
{
    GwenInputRequest *request = user_data;
    if (request->builder)
    {
        g_object_unref (request->builder);
        request->builder = NULL;
    }
    request->dialog = NULL;
    if (request->gui->active_input_dialog == dialog)
        request->gui->active_input_dialog = NULL;
    if (request->entry)
        gtk_entry_set_text (GTK_ENTRY (request->entry), "");
    if (request->confirm_entry)
        gtk_entry_set_text (GTK_ENTRY (request->confirm_entry), "");
    g_free (request->mime_type);
    request->mime_type = NULL;
    g_free (request->challenge);
    request->challenge = NULL;
    if (request->flicker_gui)
    {
        g_slice_free (GncFlickerGui, request->flicker_gui);
        request->flicker_gui = NULL;
    }
    gwen_input_finished (request);
}

static void
gwen_input_response (GtkDialog *dialog, gint response, gpointer user_data)
{
    GwenInputRequest *request = user_data;
    const gchar *input;
    const gchar *confirmed;
    gchar *message;

    if (response != GTK_RESPONSE_OK)
    {
        gtk_widget_destroy (GTK_WIDGET (dialog));
        return;
    }

    input = gtk_entry_get_text (GTK_ENTRY (request->entry));
    if (!request->is_tan)
    {
        gboolean remember = gtk_toggle_button_get_active (
            GTK_TOGGLE_BUTTON (request->remember));
        enable_password_cache (request->gui, remember);
        gnc_prefs_set_bool (GNC_PREFS_GROUP_AQBANKING,
                            GNC_PREF_REMEMBER_PIN, remember);
    }
    if (strlen (input) < request->min_len)
    {
        message = g_strdup_printf (_("The PIN needs to be at least %d characters long."),
                                   request->min_len);
        gtk_label_set_text (GTK_LABEL (request->heading), message);
        g_free (message);
        gtk_widget_grab_focus (request->entry);
        return;
    }
    if (request->confirm)
    {
        confirmed = gtk_entry_get_text (GTK_ENTRY (request->confirm_entry));
        if (strcmp (input, confirmed))
        {
            gtk_label_set_text (GTK_LABEL (request->heading),
                                _("The entries do not match. Please try again."));
            gtk_widget_grab_focus (request->confirm_entry);
            return;
        }
    }

    /* Copy the secret before destroying its entry widgets. */
    request->input = g_strdup (input);
    gtk_widget_destroy (GTK_WIDGET (dialog));
}

static gboolean
gwen_input_show_on_main (gpointer user_data)
{
    GwenInputRequest *request = user_data;
    GtkWidget *heading_label, *confirm_label;
    GtkWidget *optical_challenge;
    GtkWidget *flicker_challenge, *flicker_marker, *flicker_hbox;
    GtkWidget *spin_barwidth, *spin_delay;

    if (request->gui->parent_destroyed)
    {
        gwen_input_finished (request);
        return G_SOURCE_REMOVE;
    }

    request->builder = gtk_builder_new ();
    gnc_builder_add_from_file (request->builder, "dialog-ab.glade",
                               "aqbanking_password_dialog");
    request->dialog = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "aqbanking_password_dialog"));
    request->gui->active_input_dialog = request->dialog;
    heading_label = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "heading_pw_label"));
    request->heading = heading_label;
    request->entry = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "input_entry"));
    request->confirm_entry = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "confirm_entry"));
    confirm_label = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "confirm_label"));
    request->remember = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "remember_pin"));
    flicker_challenge = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "flicker_challenge"));
    flicker_marker = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "flicker_marker"));
    flicker_hbox = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "flicker_hbox"));
    spin_barwidth = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "spin_barwidth"));
    spin_delay = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "spin_delay"));
    optical_challenge = GTK_WIDGET (gtk_builder_get_object (
        request->builder, "optical_challenge"));
    gtk_widget_hide (optical_challenge);
    gtk_widget_set_no_show_all (optical_challenge, TRUE);
    if (request->title)
        gtk_window_set_title (GTK_WINDOW (request->dialog), request->title);
    if (request->text)
    {
        gchar *raw_text = strip_html (g_strdup (request->text));
        gtk_label_set_text (GTK_LABEL (heading_label), raw_text);
        g_free (raw_text);
    }
    gtk_entry_set_max_length (GTK_ENTRY (request->entry), request->max_len);
    if (request->confirm)
        gtk_entry_set_max_length (GTK_ENTRY (request->confirm_entry),
                                  request->max_len);
    gtk_entry_set_activates_default (GTK_ENTRY (request->entry),
                                     !request->confirm);
    if (request->confirm)
        gtk_entry_set_activates_default (GTK_ENTRY (request->confirm_entry),
                                         TRUE);
    if (request->mime_type && request->challenge)
    {
        if (!g_strcmp0 (request->mime_type, "text/x-flickercode"))
        {
            request->flicker_gui = g_slice_new0 (GncFlickerGui);
            request->flicker_gui->dialog = request->dialog;
            request->flicker_gui->input_entry = request->entry;
            request->flicker_gui->flicker_challenge = flicker_challenge;
            request->flicker_gui->flicker_marker = flicker_marker;
            request->flicker_gui->flicker_hbox = flicker_hbox;
            request->flicker_gui->spin_barwidth = GTK_SPIN_BUTTON (spin_barwidth);
            request->flicker_gui->spin_delay = GTK_SPIN_BUTTON (spin_delay);
            ini_flicker_gui ((const gchar *)request->challenge,
                             request->flicker_gui);
            gtk_widget_show (flicker_challenge);
            gtk_widget_show (flicker_marker);
            gtk_widget_show (flicker_hbox);
            gtk_widget_show (spin_barwidth);
            gtk_widget_show (spin_delay);
        }
        else if (request->challenge_len)
        {
        GError *error = NULL;
        GdkPixbufLoader *loader = gdk_pixbuf_loader_new_with_mime_type (
            request->mime_type, &error);
        if (loader && gdk_pixbuf_loader_write (loader, request->challenge,
                                                request->challenge_len,
                                                &error) &&
            gdk_pixbuf_loader_close (loader, &error))
        {
            GdkPixbuf *pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
            if (pixbuf)
            {
                gtk_image_set_from_pixbuf (GTK_IMAGE (optical_challenge), pixbuf);
                gtk_widget_set_no_show_all (optical_challenge, FALSE);
                gtk_widget_show (optical_challenge);
            }
        }
        if (error)
        {
            g_warning ("Unable to display online banking challenge: %s",
                       error->message);
            g_error_free (error);
        }
        if (loader)
            g_object_unref (loader);
        }
    }
    if (request->gui->dialog)
        gtk_window_set_transient_for (GTK_WINDOW (request->dialog),
                                      GTK_WINDOW (request->gui->dialog));
    else if (request->gui->parent)
        gtk_window_set_transient_for (GTK_WINDOW (request->dialog),
                                      GTK_WINDOW (request->gui->parent));
    if (request->gui->had_parent)
        gtk_window_set_destroy_with_parent (GTK_WINDOW (request->dialog), TRUE);
    if (!request->confirm)
    {
        gtk_widget_hide (request->confirm_entry);
        gtk_widget_hide (confirm_label);
    }
    if (request->is_tan)
        gtk_widget_hide (request->remember);
    else
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (request->remember),
                                      request->gui->cache_passwords);
    gtk_dialog_set_default_response (GTK_DIALOG (request->dialog),
                                     GTK_RESPONSE_OK);
    if (request->flags & (GWEN_GUI_INPUT_FLAGS_TAN | GWEN_GUI_INPUT_FLAGS_SHOW))
        gtk_entry_set_visibility (GTK_ENTRY (request->entry), TRUE);
    g_signal_connect (request->dialog, "response",
                      G_CALLBACK (gwen_input_response), request);
    g_signal_connect (request->dialog, "destroy",
                      G_CALLBACK (gwen_input_destroyed), request);
    gtk_widget_show_all (request->dialog);
    if (!request->confirm)
    {
        gtk_widget_hide (request->confirm_entry);
        gtk_widget_hide (confirm_label);
    }
    if (request->is_tan)
        gtk_widget_hide (request->remember);
    gtk_widget_grab_focus (request->entry);
    return G_SOURCE_REMOVE;
}

static gchar *
gwen_get_input_from_worker (GncGWENGui *gui, guint32 flags,
                            const gchar *title, const gchar *text,
                            const gchar *mime_type, const gchar *challenge,
                            guint32 challenge_len, gint min_len, gint max_len)
{
    GwenInputRequest request = {0};
    g_return_val_if_fail (g_thread_self () != gtk_thread, NULL);
    request.gui = gui;
    request.title = g_strdup (title);
    request.text = g_strdup (text);
    request.min_len = min_len;
    request.max_len = max_len;
    request.confirm = (flags & GWEN_GUI_INPUT_FLAGS_CONFIRM) != 0;
    request.is_tan = (flags & GWEN_GUI_INPUT_FLAGS_TAN) != 0;
    request.mime_type = g_strdup (mime_type);
    request.challenge = challenge && challenge_len
        ? g_memdup2 (challenge, challenge_len) : NULL;
    if (request.challenge)
    {
        request.challenge = g_realloc (request.challenge, challenge_len + 1);
        request.challenge[challenge_len] = '\0';
    }
    request.challenge_len = challenge_len;
    request.flags = flags;
    g_mutex_init (&request.mutex);
    g_cond_init (&request.condition);
    gwen_call_on_gtk_thread (gwen_input_show_on_main, &request);
    g_mutex_lock (&request.mutex);
    while (!request.complete)
        g_cond_wait (&request.condition, &request.mutex);
    g_mutex_unlock (&request.mutex);
    g_cond_clear (&request.condition);
    g_mutex_clear (&request.mutex);
    g_free (request.title);
    g_free (request.text);
    return request.input;
}

void
gnc_GWEN_Gui_log_init(void)
{
    if (!gtk_thread)
    {
        gtk_thread = g_thread_self ();
        gtk_context = g_main_context_ref_thread_default ();
    }
    if (!aq_shutdown_barrier)
        aq_shutdown_barrier = gnc_gui_add_shutdown_barrier (
            aq_gwen_shutdown_barrier, NULL);

    if (!log_gwen_gui)
    {
        log_gwen_gui = Gtk3_Gui_new();

        /* Always use our own logging */
        GWEN_Gui_SetLogHookFn(log_gwen_gui, loghook_cb);

        /* Keep a reference so that the GWEN_GUI survives a GUI switch */
        GWEN_Gui_Attach(log_gwen_gui);
    }
    GWEN_Gui_SetGui(log_gwen_gui);
}

GncGWENGui *
gnc_GWEN_Gui_get(GtkWidget *parent)
{
    GncGWENGui *gui;

    ENTER("parent=%p", parent);

    if (!gtk_thread)
    {
        gtk_thread = g_thread_self ();
        gtk_context = g_main_context_ref_thread_default ();
    }
    g_return_val_if_fail (gtk_thread == g_thread_self (), NULL);

    for (GList *node = all_guis; node; node = node->next)
    {
        gui = node->data;
        if (!gui->leased)
        {
            gui->leased = TRUE;
            gwen_set_parent (gui, parent);
            reset_dialog (gui);
            register_callbacks (gui);
            full_gui = gui;
            LEAVE ("reused gui=%p", gui);
            return gui;
        }
    }

    gui = g_new0(GncGWENGui, 1);
    g_mutex_init (&gui->password_mutex);
    g_weak_ref_init (&gui->parent_ref, NULL);
    gui->leased = TRUE;
    gwen_set_parent (gui, parent);
    setup_dialog(gui);
    register_callbacks(gui);

    all_guis = g_list_append (all_guis, gui);
    full_gui = gui;

    LEAVE("new gui=%p", gui);
    return gui;
}

void
gnc_GWEN_Gui_release(GncGWENGui *gui)
{
    g_return_if_fail(gui && g_list_find (all_guis, gui));
    g_return_if_fail(g_thread_self () == gtk_thread);

    ENTER("gui=%p", gui);
    g_return_if_fail (gui->leased);
    if (gui->cancel_confirmation_pending)
    {
        gui->release_pending = TRUE;
        LEAVE ("deferred until cancel confirmation completes");
        return;
    }
    if (gui->gwen_gui && gui->state != RUNNING)
        unregister_callbacks (gui);
    gui->leased = FALSE;
    gui->release_pending = FALSE;
    LEAVE(" ");
}

void
gnc_GWEN_Gui_shutdown(void)
{
    GList *node;

    ENTER(" ");

    g_return_if_fail (g_thread_self () == gtk_thread);
    g_return_if_fail (aq_active_jobs == 0);
    g_return_if_fail (!aq_gwen_has_leased_gui ());
    if (aq_shutdown_barrier)
    {
        gnc_gui_remove_shutdown_barrier (aq_shutdown_barrier);
        aq_shutdown_barrier = 0;
    }
    for (node = all_guis; node; node = node->next)
    {
        GncGWENGui *gui = node->data;
        g_return_if_fail (!gui->leased);
        gwen_set_parent (gui, NULL);
        if (gui->gwen_gui)
            unregister_callbacks (gui);
        reset_dialog(gui);
        if (gui->passwords)
            g_hash_table_destroy(gui->passwords);
        if (gui->showbox_hash)
            g_hash_table_destroy(gui->showbox_hash);
        if (gui->permanently_accepted_certs)
            GWEN_DB_Group_free(gui->permanently_accepted_certs);
        if (gui->accepted_certs)
            g_hash_table_destroy(gui->accepted_certs);
        gtk_widget_destroy(gui->dialog);
        g_mutex_clear (&gui->password_mutex);
        g_weak_ref_clear (&gui->parent_ref);
        g_free(gui);
    }
    g_list_free (all_guis);
    all_guis = NULL;
    full_gui = NULL;
    if (log_gwen_gui)
    {
        GWEN_Gui_free(log_gwen_gui);
        log_gwen_gui = NULL;
    }
    GWEN_Gui_SetGui(NULL);

    LEAVE(" ");
}

void
gnc_GWEN_Gui_set_close_flag(gboolean close_when_finished)
{
    gnc_prefs_set_bool(
        GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH,
        close_when_finished);

    if (full_gui)
    {
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(full_gui->close_checkbutton))
                != close_when_finished)
        {
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(full_gui->close_checkbutton),
                close_when_finished);
        }
    }
}

gboolean
gnc_GWEN_Gui_get_close_flag()
{
    return gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH);
}

gboolean
gnc_GWEN_Gui_show_dialog()
{
    GncGWENGui *gui = full_gui;

    if (!gui)
        return FALSE;

    if (gui)
    {
        if (gui->state == HIDDEN)
        {
            gui->state = FINISHED;
        }
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(gui->close_checkbutton),
            gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH));

        gtk_widget_set_sensitive(gui->close_button, TRUE);

        show_dialog(gui, FALSE);

        return TRUE;
    }

    return FALSE;
}

void
gnc_GWEN_Gui_hide_dialog()
{
    GncGWENGui *gui = full_gui;

    if (gui)
    {
        hide_dialog(gui);
    }
}

static void
register_callbacks(GncGWENGui *gui)
{
    GWEN_GUI *gwen_gui;

    g_return_if_fail(gui && !gui->gwen_gui);

    ENTER("gui=%p", gui);

    gwen_gui = Gtk3_Gui_new();
    gui->gwen_gui = gwen_gui;

    GWEN_Gui_SetMessageBoxFn(gwen_gui, messagebox_cb);
    GWEN_Gui_SetInputBoxFn(gwen_gui, inputbox_cb);
    GWEN_Gui_SetShowBoxFn(gwen_gui, showbox_cb);
    GWEN_Gui_SetHideBoxFn(gwen_gui, hidebox_cb);
    GWEN_Gui_SetProgressStartFn(gwen_gui, progress_start_cb);
    GWEN_Gui_SetProgressAdvanceFn(gwen_gui, progress_advance_cb);
    GWEN_Gui_SetProgressLogFn(gwen_gui, progress_log_cb);
    GWEN_Gui_SetProgressEndFn(gwen_gui, progress_end_cb);
    GWEN_Gui_SetGetPasswordFn(gwen_gui, getpassword_cb);
    GWEN_Gui_SetSetPasswordStatusFn(gwen_gui, setpasswordstatus_cb);
    GWEN_Gui_SetLogHookFn(gwen_gui, loghook_cb);
    gui->builtin_checkcert = GWEN_Gui_SetCheckCertFn(gwen_gui, checkcert_cb);
    gui->builtin_exec_dialog =
        GWEN_Gui_SetExecDialogFn (gwen_gui, gwen_exec_dialog_on_worker);

    GWEN_Gui_SetGui(gwen_gui);
    SETDATA_GUI(gwen_gui, gui);

    LEAVE(" ");
}

static void
unregister_callbacks(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    if (!gui->gwen_gui)
    {
        LEAVE("already unregistered");
        return;
    }

    /* Switch to log_gwen_gui and free gui->gwen_gui */
    gnc_GWEN_Gui_log_init();

    gui->gwen_gui = NULL;

    LEAVE(" ");
}

static void
setup_dialog(GncGWENGui *gui)
{
    GtkBuilder *builder;
    gint component_id;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "aqbanking_connection_dialog");

    gui->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "aqbanking_connection_dialog"));

    gui->entries_grid = GTK_WIDGET(gtk_builder_get_object (builder, "entries_grid"));
    gui->top_entry = GTK_WIDGET(gtk_builder_get_object (builder, "top_entry"));
    gui->top_progress = GTK_WIDGET(gtk_builder_get_object (builder, "top_progress"));
    gui->second_entry = GTK_WIDGET(gtk_builder_get_object (builder, "second_entry"));
    gui->other_entries_box = NULL;
    gui->progresses = NULL;
    gui->log_text = GTK_WIDGET(gtk_builder_get_object (builder, "log_text"));
    gui->abort_button = GTK_WIDGET(gtk_builder_get_object (builder, "abort_button"));
    gui->close_button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
    gui->close_checkbutton = GTK_WIDGET(gtk_builder_get_object (builder, "close_checkbutton"));
    gui->accepted_certs = NULL;
    gui->permanently_accepted_certs = NULL;
    gui->showbox_hash = NULL;
    gui->showbox_id = 1;

    /* Connect the Signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(gui->close_checkbutton),
        gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH));

    component_id = gnc_register_gui_component(GWEN_GUI_CM_CLASS, NULL,
                   cm_close_handler, gui);
    gnc_gui_component_set_session(component_id, gnc_get_current_session());



    g_object_unref(G_OBJECT(builder));

    reset_dialog(gui);

    LEAVE(" ");
}

static void
enable_password_cache(GncGWENGui *gui, gboolean enabled)
{
    g_return_if_fail(gui);

    g_mutex_lock (&gui->password_mutex);
    if (enabled && !gui->passwords)
    {
        /* Remember passwords in memory, mapping tokens to passwords */
        gui->passwords = g_hash_table_new_full(
                             g_str_hash, g_str_equal, (GDestroyNotify) g_free,
                             (GDestroyNotify) erase_password);
    }
    else if (!enabled && gui->passwords)
    {
        /* Erase and free remembered passwords from memory */
        g_hash_table_destroy(gui->passwords);
        gui->passwords = NULL;
    }
    gui->cache_passwords = enabled;
    g_mutex_unlock (&gui->password_mutex);
}

static void
reset_dialog(GncGWENGui *gui)
{
    gboolean cache_passwords;
    GtkWidget *parent;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    gtk_entry_set_text(GTK_ENTRY(gui->top_entry), "");
    gtk_entry_set_text(GTK_ENTRY(gui->second_entry), "");
    g_list_foreach(gui->progresses, (GFunc) free_progress, NULL);
    g_list_free(gui->progresses);
    gui->progresses = NULL;

    if (gui->other_entries_box)
    {
        gtk_grid_remove_row (GTK_GRID(gui->entries_grid),
                             OTHER_ENTRIES_ROW_OFFSET);
        gtk_widget_destroy(gui->other_entries_box);
        gui->other_entries_box = NULL;
    }
    if (gui->showbox_hash)
    {
        g_hash_table_destroy(gui->showbox_hash);
        gui->showbox_hash = NULL;
    }
    gui->showbox_last = NULL;
    gui->showbox_hash = g_hash_table_new_full(
                            NULL, NULL, NULL, (GDestroyNotify) gtk_widget_destroy);

    parent = gui->parent_destroyed ? NULL : g_weak_ref_get (&gui->parent_ref);
    if (parent && !gtk_widget_in_destruction (parent) &&
        GTK_IS_WINDOW (parent))
    {
        gtk_window_set_transient_for (GTK_WINDOW (gui->dialog),
                                     GTK_WINDOW (parent));
        gnc_restore_window_size (GNC_PREFS_GROUP_CONNECTION,
                                 GTK_WINDOW (gui->dialog),
                                 GTK_WINDOW (parent));
    }
    else
        gtk_window_set_transient_for (GTK_WINDOW (gui->dialog), NULL);
    g_clear_object (&parent);

    gui->keep_alive = TRUE;
    gui->state = INIT;
    gui->min_loglevel = GWEN_LoggerLevel_Verbous;

    cache_passwords = gnc_prefs_get_bool(GNC_PREFS_GROUP_AQBANKING,
                                         GNC_PREF_REMEMBER_PIN);
    enable_password_cache(gui, cache_passwords);

    if (!gui->accepted_certs)
        gui->accepted_certs = g_hash_table_new_full(
                                  g_str_hash, g_str_equal, (GDestroyNotify) g_free, NULL);
    if (!gui->permanently_accepted_certs)
        gui->permanently_accepted_certs = gnc_ab_get_permanent_certs();

    LEAVE(" ");
}

static void
set_running(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    gui->state = RUNNING;
    gtk_widget_set_sensitive(gui->abort_button, TRUE);
    gtk_widget_set_sensitive(gui->close_button, FALSE);
    gui->keep_alive = TRUE;

    LEAVE(" ");
}

static void
set_finished(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    /* Do not serve as GUI anymore */
    gui->state = FINISHED;
    unregister_callbacks(gui);

    gtk_widget_set_sensitive(gui->abort_button, FALSE);
    gtk_widget_set_sensitive(gui->close_button, TRUE);
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gui->close_checkbutton)))
        hide_dialog(gui);

    LEAVE(" ");
}

static void
set_aborted(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    /* Do not serve as GUI anymore */
    gui->state = ABORTED;
    unregister_callbacks(gui);

    gtk_widget_set_sensitive(gui->abort_button, FALSE);
    gtk_widget_set_sensitive(gui->close_button, TRUE);
    gui->keep_alive = FALSE;

    LEAVE(" ");
}

static void
show_dialog(GncGWENGui *gui, gboolean clear_log)
{
    g_return_if_fail(gui);

    ENTER("gui=%p, clear_log=%d", gui, clear_log);

    gtk_widget_show(gui->dialog);

    gnc_plugin_aqbanking_set_logwindow_visible(TRUE);

    /* Clear the log window */
    if (clear_log)
    {
        gtk_text_buffer_set_text(
            gtk_text_view_get_buffer(GTK_TEXT_VIEW(gui->log_text)), "", 0);
    }

    LEAVE(" ");
}

static void
hide_dialog(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    /* Hide the dialog */
    gtk_widget_hide(gui->dialog);

    gnc_plugin_aqbanking_set_logwindow_visible(FALSE);

    /* Remember whether the dialog is to be closed when finished */
    gnc_prefs_set_bool(
        GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH,
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gui->close_checkbutton)));

    /* Remember size and position of the dialog */
    gnc_save_window_size(GNC_PREFS_GROUP_CONNECTION, GTK_WINDOW(gui->dialog));

    /* Do not serve as GUI anymore */
    gui->state = HIDDEN;
    unregister_callbacks(gui);

    LEAVE(" ");
}

static gboolean
show_progress_cb(gpointer user_data)
{
    Progress *progress = user_data;

    g_return_val_if_fail(progress, FALSE);

    ENTER("progress=%p", progress);

    show_progress(progress->gui, progress);

    LEAVE(" ");
    return FALSE;
}

/**
 * Show all processes down to and including @a progress.
 */
static void
show_progress(GncGWENGui *gui, Progress *progress)
{
    GList *item;
    Progress *current;

    g_return_if_fail(gui);

    ENTER("gui=%p, progress=%p", gui, progress);

    for (item = g_list_last(gui->progresses); item; item = item->prev)
    {
        current = (Progress*) item->data;

        if (!current->source
                && current != progress)
            /* Already showed */
            continue;

        /* Show it */
        if (!item->next)
        {
            /* Top-level progress */
            show_dialog(gui, TRUE);
            gtk_entry_set_text(GTK_ENTRY(gui->top_entry), current->title);
        }
        else if (!item->next->next)
        {
            /* Second-level progress */
            gtk_entry_set_text(GTK_ENTRY(gui->second_entry), current->title);
        }
        else
        {
            /* Other progress */
            GtkWidget *entry = gtk_entry_new();
            GtkWidget *box = gui->other_entries_box;
            gboolean new_box = box == NULL;

            gtk_entry_set_text(GTK_ENTRY(entry), current->title);
            if (new_box)
            {
                gui->other_entries_box = box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
                gtk_box_set_homogeneous (GTK_BOX (gui->other_entries_box), TRUE);
                gtk_box_set_homogeneous (GTK_BOX (box), TRUE);
            }

            gtk_box_pack_start(GTK_BOX(box), entry, TRUE, TRUE, 0);
            gtk_widget_show(entry);
            if (new_box)
            {
                gtk_grid_attach (GTK_GRID(gui->entries_grid), box,
                                 1, OTHER_ENTRIES_ROW_OFFSET, 1, 1);
                gtk_widget_show(box);
            }
        }

        if (current->source)
        {
            /* Stop delayed call */
            g_source_remove(current->source);
            current->source = 0;
        }

        if (current == progress)
            break;
    }

    LEAVE(" ");
}

/**
 * Hide all processes up to and including @a progress.
 */
static void
hide_progress(GncGWENGui *gui, Progress *progress)
{
    GList *item;
    Progress *current;

    g_return_if_fail(gui);

    ENTER("gui=%p, progress=%p", gui, progress);

    for (item = gui->progresses; item; item = item->next)
    {
        current = (Progress*) item->data;

        if (current->source)
        {
            /* Not yet showed */
            g_source_remove(current->source);
            current->source = 0;
            if (current == progress)
                break;
            else
                continue;
        }

        /* Hide it */
        if (!item->next)
        {
            /* Top-level progress */
            gtk_entry_set_text(GTK_ENTRY(gui->second_entry), "");
        }
        else if (!item->next->next)
        {
            /* Second-level progress */
            gtk_entry_set_text(GTK_ENTRY(gui->second_entry), "");
        }
        else
        {
            /* Other progress */
            GtkWidget *box = gui->other_entries_box;
            GList *entries;

            g_return_if_fail(box);
            entries = gtk_container_get_children(GTK_CONTAINER(box));
            g_return_if_fail(entries);
            if (entries->next)
            {
                /* Another progress is still to be showed */
                gtk_widget_destroy(GTK_WIDGET(g_list_last(entries)->data));
            }
            else
            {
                /* Last other progress to be hidden */
                gtk_grid_remove_row (GTK_GRID(gui->entries_grid),
                                     OTHER_ENTRIES_ROW_OFFSET);
                /* Box destroyed, Null the reference. */
                gui->other_entries_box = NULL;
            }
            g_list_free(entries);
        }

        if (current == progress)
            break;
    }

    LEAVE(" ");
}

static void
free_progress(Progress *progress, gpointer unused)
{
    if (progress->source)
        g_source_remove(progress->source);
    g_free(progress->title);
    g_free(progress);
}

static gboolean
keep_alive(GncGWENGui *gui)
{
    g_return_val_if_fail(gui, FALSE);

    ENTER("gui=%p", gui);

    LEAVE("alive=%d", gui->keep_alive);
    return gui->keep_alive;
}

static void
cm_close_handler(gpointer user_data)
{
    GncGWENGui *gui = user_data;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    /* FIXME */
    set_aborted(gui);

    LEAVE(" ");
}

static void
erase_password(gchar *password)
{
    g_return_if_fail(password);

    ENTER(" ");

    memset(password, 0, strlen(password));
    g_free(password);

    LEAVE(" ");
}

/**
 * Find first <[Hh][Tt][Mm][Ll]> and cut off the string there.
 */
static gchar *
strip_html(gchar *text)
{
    gchar *p, *q;

    if (!text)
        return NULL;

    p = text;
    while (strchr(p, '<'))
    {
        q = p + 1;
        if (*q && toupper(*q++) == 'H'
                && *q && toupper(*q++) == 'T'
                && *q && toupper(*q++) == 'M'
                && *q && toupper(*q) == 'L')
        {
            *p = '\0';
            return text;
        }
        p++;
    }
    return text;
}

static void
get_input(GncGWENGui *gui, guint32 flags, const gchar *title,
          const gchar *text, const char *mimeType, const char *pChallenge,
          uint32_t lChallenge, gchar **input, gint min_len, gint max_len)
{
    g_return_if_fail (input);
    g_return_if_fail (max_len >= min_len && max_len > 0);
    /* Gwenhywfar requires a synchronous result from this callback. The
     * serialized backend worker may wait; GTK remains signal-driven. */
    *input = gwen_get_input_from_worker (gui, flags, title, text,
                                         mimeType, pChallenge, lChallenge,
                                         min_len, max_len);
}
typedef struct
{
    GMutex mutex;
    GCond condition;
    gboolean complete;
    gint result;
    GncGWENGui *gui;
} GwenDialogWait;

static void
gwen_dialog_wait_complete (GwenDialogWait *wait, gint result)
{
    g_mutex_lock (&wait->mutex);
    if (!wait->complete)
    {
        wait->result = result;
        wait->complete = TRUE;
        g_cond_signal (&wait->condition);
    }
    g_mutex_unlock (&wait->mutex);
}

static void
gwen_dialog_wait_for_result (GwenDialogWait *wait)
{
    g_mutex_lock (&wait->mutex);
    while (!wait->complete)
        g_cond_wait (&wait->condition, &wait->mutex);
    g_mutex_unlock (&wait->mutex);
}

static void
gwen_messagebox_completed ([[maybe_unused]] GtkWindow *parent,
                           gint response, gpointer user_data)
{
    GwenDialogWait *wait = user_data;
    wait->gui->active_message_dialog = NULL;
    gwen_dialog_wait_complete (wait, response);
}

typedef struct
{
    GncGWENGui *gui;
    GwenDialogWait *wait;
    const gchar *title;
    const gchar *text;
    const gchar *button1;
    const gchar *button2;
    const gchar *button3;
} GwenMessageBoxRequest;

static gboolean
gwen_messagebox_show_on_main (gpointer user_data)
{
    GwenMessageBoxRequest *request = user_data;
    GtkWidget *dialog;
    GtkWidget *vbox;
    GtkWidget *label;
    gchar *raw_text;

    if (request->gui->parent_destroyed)
    {
        gwen_dialog_wait_complete (request->wait, 0);
        return G_SOURCE_REMOVE;
    }

    dialog = gtk_dialog_new_with_buttons (
        request->title, request->gui->parent
            ? GTK_WINDOW (request->gui->parent) : NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        request->button1, 1, request->button2, 2, request->button3, 3,
        (gchar *)NULL);
    request->gui->active_message_dialog = dialog;
    raw_text = strip_html (g_strdup (request->text));
    label = gtk_label_new (raw_text);
    g_free (raw_text);
    gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), TRUE);
    gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
    gtk_container_add (GTK_CONTAINER (vbox), label);
    gtk_container_set_border_width (GTK_CONTAINER (dialog), 5);
    gtk_container_add (GTK_CONTAINER (gtk_dialog_get_content_area (
                                       GTK_DIALOG (dialog))), vbox);
    gnc_gui_query_bind_dialog_response (GTK_DIALOG (dialog),
                                       gwen_messagebox_completed,
                                       request->wait);
    gtk_widget_show_all (dialog);
    return G_SOURCE_REMOVE;
}

static gint GNC_GWENHYWFAR_CB
messagebox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
              const gchar *text, const gchar *b1, const gchar *b2,
              const gchar *b3, [[maybe_unused]] guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GwenDialogWait wait = {0};
    GwenMessageBoxRequest request = {gui, &wait, title, text, b1, b2, b3};
    gint result;

    ENTER("gui=%p, flags=%d, title=%s, b1=%s, b2=%s, b3=%s", gui, flags,
          title ? title : "(null)", b1 ? b1 : "(null)", b2 ? b2 : "(null)",
          b3 ? b3 : "(null)");

    /* Gwen's callback ABI must return a button number. It may block its
     * worker, but GTK must keep dispatching on its own main thread. */
    g_return_val_if_fail (g_thread_self () != gtk_thread, 0);
    wait.gui = gui;
    g_mutex_init (&wait.mutex);
    g_cond_init (&wait.condition);
    gwen_call_on_gtk_thread (gwen_messagebox_show_on_main, &request);
    gwen_dialog_wait_for_result (&wait);
    result = wait.result;
    g_cond_clear (&wait.condition);
    g_mutex_clear (&wait.mutex);

    if (result < 1 || result > 3)
    {
        g_warning ("messagebox_cb: Bad result %d", result);
        result = 0;
    }
    LEAVE ("result=%d", result);
    return result;
}

static gint GNC_GWENHYWFAR_CB
inputbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
            const gchar *text, gchar *buffer, gint min_len, gint max_len,
            guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    gchar *input = NULL;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d", gui, flags);

    get_input(gui, flags, title, text, NULL, NULL, 0, &input, min_len, max_len);

    if (input)
    {
        /* Copy the input to the result buffer */
        strncpy(buffer, input, max_len);
        buffer[max_len-1] = '\0';
        erase_password (input);
    }

    LEAVE(" ");
    return input ? 0 : -1;
}

static guint32 GNC_GWENHYWFAR_CB
showbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
           const gchar *text, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkWidget *dialog;
    guint32 showbox_id;

    GwenSimpleCall call = {GWEN_SIMPLE_SHOWBOX, gwen_gui, flags, guiid,
                           0, title, text, 0, -1};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d, title=%s", gui, flags, title ? title : "(null)");

    dialog = gtk_message_dialog_new(
                 gui->parent ? GTK_WINDOW(gui->parent) : NULL, 0, GTK_MESSAGE_INFO,
                 GTK_BUTTONS_OK, "%s", text);

    if (title)
        gtk_window_set_title(GTK_WINDOW(dialog), title);

    g_signal_connect(dialog, "response", G_CALLBACK(gtk_widget_hide), NULL);
    gtk_widget_show_all(dialog);

    showbox_id = gui->showbox_id++;
    g_hash_table_insert(gui->showbox_hash, GUINT_TO_POINTER(showbox_id),
                        dialog);
    gui->showbox_last = dialog;

    /* Give it a change to be showed */
    if (!keep_alive(gui))
        showbox_id = 0;

    LEAVE("id=%" G_GUINT32_FORMAT, showbox_id);
    return showbox_id;
}

static void GNC_GWENHYWFAR_CB
hidebox_cb(GWEN_GUI *gwen_gui, guint32 id)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);

    GwenSimpleCall call = {GWEN_SIMPLE_HIDEBOX, gwen_gui, 0, id,
                           0, NULL, NULL, 0, 0};
    if (gwen_simple_call_if_worker (&call))
        return;

    g_return_if_fail(gui && gui->showbox_hash);

    ENTER("gui=%p, id=%d", gui, id);

    if (id == 0)
    {
        if (gui->showbox_last)
        {
            g_hash_table_remove(gui->showbox_hash,
                                GUINT_TO_POINTER(gui->showbox_id));
            gui->showbox_last = NULL;
        }
        else
        {
            g_warning("hidebox_cb: Last showed message box already destroyed");
        }
    }
    else
    {
        gpointer p_var;
        p_var = g_hash_table_lookup(gui->showbox_hash, GUINT_TO_POINTER(id));
        if (p_var)
        {
            g_hash_table_remove(gui->showbox_hash, GUINT_TO_POINTER(id));
            if (p_var == gui->showbox_last)
                gui->showbox_last = NULL;
        }
        else
        {
            g_warning("hidebox_cb: Message box %d could not been found", id);
        }
    }

    LEAVE(" ");
}

static guint32 GNC_GWENHYWFAR_CB
progress_start_cb(GWEN_GUI *gwen_gui, uint32_t progressFlags, const char *title,
                  const char *text, uint64_t total, uint32_t guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    Progress *progress;

    GwenSimpleCall call = {GWEN_SIMPLE_PROGRESS_START, gwen_gui,
                           progressFlags, guiid, total, title, text, 0, -1};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d, title=%s, total=%" G_GUINT64_FORMAT, gui,
          progressFlags, title ? title : "(null)", (guint64)total);

    if (!gui->progresses)
    {
        /* Top-level progress */
        if (progressFlags & GWEN_GUI_PROGRESS_SHOW_PROGRESS)
        {
            gtk_widget_set_sensitive(gui->top_progress, TRUE);
            gtk_progress_bar_set_fraction(
                GTK_PROGRESS_BAR(gui->top_progress), 0.0);
            gui->max_actions = total;
        }
        else
        {
            gtk_widget_set_sensitive(gui->top_progress, FALSE);
            gui->max_actions = -1;
        }
        set_running(gui);
    }

    /* Put progress onto the stack */
    progress = g_new0(Progress, 1);
    progress->gui = gui;
    progress->title = title ? g_strdup(title) : "";
    gui->progresses = g_list_prepend(gui->progresses, progress);

    if (progressFlags & GWEN_GUI_PROGRESS_DELAY)
    {
        /* Show progress later */
        progress->source = g_timeout_add(GWEN_GUI_DELAY_SECS * 1000,
                                         (GSourceFunc) show_progress_cb,
                                         progress);
    }
    else
    {
        /* Show it now */
        progress->source = 0;
        show_progress(gui, progress);
    }

    LEAVE(" ");
    return g_list_length(gui->progresses);
}

static gint GNC_GWENHYWFAR_CB
progress_advance_cb(GWEN_GUI *gwen_gui, uint32_t id, uint64_t progress)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);

    GwenSimpleCall call = {GWEN_SIMPLE_PROGRESS_ADVANCE, gwen_gui, 0, id,
                           progress, NULL, NULL, 0, -1};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, progress=%" G_GUINT64_FORMAT, gui, (guint64)progress);

    if (id == 1                                  /* top-level progress */
            && gui->max_actions > 0                  /* progressbar active */
            && progress != GWEN_GUI_PROGRESS_NONE)   /* progressbar update needed */
    {
        if (progress == GWEN_GUI_PROGRESS_ONE)
            gui->current_action++;
        else
            gui->current_action = progress;

        gtk_progress_bar_set_fraction(
            GTK_PROGRESS_BAR(gui->top_progress),
            ((gdouble) gui->current_action) / ((gdouble) gui->max_actions));
    }

    LEAVE(" ");
    return !keep_alive(gui);
}

static gint GNC_GWENHYWFAR_CB
progress_log_cb(GWEN_GUI *gwen_gui, guint32 id, GWEN_LOGGER_LEVEL level,
                const gchar *text)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkTextBuffer *tb;
    GtkTextView *tv;

    GwenSimpleCall call = {GWEN_SIMPLE_PROGRESS_LOG, gwen_gui, 0, id,
                           0, NULL, text, level, -1};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, text=%s", gui, text ? text : "(null)");

    tv = GTK_TEXT_VIEW(gui->log_text);
    tb = gtk_text_view_get_buffer(tv);
    gtk_text_buffer_insert_at_cursor(tb, text, -1);
    gtk_text_buffer_insert_at_cursor(tb, "\n", -1);

    /* Scroll to the end of the buffer */
    gtk_text_view_scroll_to_mark(tv, gtk_text_buffer_get_insert(tb),
                                 0.0, FALSE, 0.0, 0.0);

    /* Cache loglevel */
    if (level < gui->min_loglevel)
        gui->min_loglevel = level;

    LEAVE(" ");
    return !keep_alive(gui);
}

static gint GNC_GWENHYWFAR_CB
progress_end_cb(GWEN_GUI *gwen_gui, guint32 id)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    Progress *progress;

    GwenSimpleCall call = {GWEN_SIMPLE_PROGRESS_END, gwen_gui, 0, id,
                           0, NULL, NULL, 0, -1};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui, -1);
    g_return_val_if_fail(id == g_list_length(gui->progresses), -1);

    ENTER("gui=%p, id=%d", gui, id);

    if (gui->state != RUNNING)
    {
        /* Ignore finishes of progresses we do not track */
        LEAVE("not running anymore");
        return 0;
    }

    /* Hide progress */
    progress = (Progress*) gui->progresses->data;
    hide_progress(gui, progress);

    /* Remove progress from stack and free memory */
    gui->progresses = g_list_delete_link(gui->progresses, gui->progresses);
    free_progress(progress, NULL);

    if (!gui->progresses)
    {
        /* top-level progress finished */
        set_finished(gui);
    }

    LEAVE(" ");
    return 0;
}

static gint GNC_GWENHYWFAR_CB
getpassword_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *token,
               const gchar *title, const gchar *text, gchar *buffer,
               gint min_len, gint max_len, GWEN_GUI_PASSWORD_METHOD methodId,
               GWEN_DB_NODE *methodParams, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    gchar *password = NULL;
    gboolean is_tan = (flags & GWEN_GUI_INPUT_FLAGS_TAN) != 0;

    int opticalMethodId;
    const char *mimeType = NULL;
    const char *pChallenge = NULL;
    uint32_t lChallenge = 0;

    g_return_val_if_fail(gui, -1);

    // cf. https://www.aquamaniac.de/rdm/projects/aqbanking/wiki/ImplementTanMethods
    if(is_tan && methodId == GWEN_Gui_PasswordMethod_OpticalHHD)
    {
        /**
        * use GWEN_Gui_PasswordMethod_Mask to get the basic method id
        *  cf. gui/gui.h of gwenhywfar
        */
        opticalMethodId=GWEN_DB_GetIntValue(methodParams, "tanMethodId", 0, AB_BANKING_TANMETHOD_TEXT);
        switch(opticalMethodId)
        {
            case AB_BANKING_TANMETHOD_CHIPTAN:
                break;
            case AB_BANKING_TANMETHOD_CHIPTAN_OPTIC:
                mimeType = "text/x-flickercode";
                pChallenge = GWEN_DB_GetCharValue(methodParams, "challenge", 0, NULL);
                if ((pChallenge == NULL) || (pChallenge[0] == '\0'))
                {
                    /* empty flicker-data */
                    return GWEN_ERROR_NO_DATA;
                }
                break;
            case AB_BANKING_TANMETHOD_CHIPTAN_USB:
                /**
                 * ToDo: is this the same as CHIPTAN_OPTIC ?
                 */
                 break;
            case AB_BANKING_TANMETHOD_PHOTOTAN:
            case AB_BANKING_TANMETHOD_CHIPTAN_QR:
                /**
                 * image data is in methodParams
                 */
                mimeType=GWEN_DB_GetCharValue(methodParams, "mimeType", 0, NULL);
                pChallenge=(const char*) GWEN_DB_GetBinValue(methodParams, "imageData", 0, NULL, 0, &lChallenge);
                if (!(pChallenge && lChallenge))
                {
                    /* empty optical data */
                    return GWEN_ERROR_NO_DATA;
                }
                break;
            default:
                break;
        }
    }

    ENTER("gui=%p, flags=%d, token=%s", gui, flags, token ? token : "(null");

    /* Check remembered passwords, excluding TANs. Copy under the cache lock
     * so a preference change cannot erase the value while Gwen uses it. */
    if (!is_tan && token)
    {
        gpointer p_var;
        g_mutex_lock (&gui->password_mutex);
        if (gui->cache_passwords && gui->passwords &&
            (flags & GWEN_GUI_INPUT_FLAGS_RETRY))
            g_hash_table_remove (gui->passwords, token);
        else if (gui->cache_passwords && gui->passwords &&
                 g_hash_table_lookup_extended (gui->passwords, token, NULL,
                                               &p_var))
            password = g_strdup (p_var);
        g_mutex_unlock (&gui->password_mutex);
        if (password)
        {
            strncpy (buffer, password, max_len);
            buffer[max_len - 1] = '\0';
            erase_password (password);
            LEAVE ("chose remembered password");
            return 0;
        }
    }

    get_input(gui, flags, title, text, mimeType, pChallenge, lChallenge, &password, min_len, max_len);

    if (password)
    {
        /* Copy the password to the result buffer */
        strncpy(buffer, password, max_len);
        buffer[max_len-1] = '\0';

        if (!is_tan && token)
        {
            g_mutex_lock (&gui->password_mutex);
            if (gui->cache_passwords && gui->passwords)
            {
                /* Remember password */
                DEBUG("Remember password, token=%s", token);
                g_hash_table_insert(gui->passwords, g_strdup(token), password);
                g_mutex_unlock (&gui->password_mutex);
            }
            else
            {
                /* Remove the password from memory */
                DEBUG("Forget password, token=%s", token);
                g_mutex_unlock (&gui->password_mutex);
                erase_password(password);
            }
        }
    }

    LEAVE(" ");
    return password ? 0 : -1;
}

static gint GNC_GWENHYWFAR_CB
setpasswordstatus_cb(GWEN_GUI *gwen_gui, const gchar *token, const gchar *pin,
                     GWEN_GUI_PASSWORD_STATUS status, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);

    GwenSimpleCall call = {GWEN_SIMPLE_PASSWORD_STATUS, gwen_gui,
                           status, guiid, 0, token, pin, 0, -1};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, token=%s, status=%d", gui, token ? token : "(null)", status);

    if (gui->passwords && status != GWEN_Gui_PasswordStatus_Ok)
    {
        /* If remembered, remove password from memory */
        g_hash_table_remove(gui->passwords, token);
    }

    LEAVE(" ");
    return 0;
}

static gint GNC_GWENHYWFAR_CB
loghook_cb(GWEN_GUI *gwen_gui, const gchar *log_domain,
           GWEN_LOGGER_LEVEL priority, const gchar *text)
{
    if (G_LIKELY(priority < n_log_levels))
        g_log(log_domain, log_levels[priority], "%s", text);

    return 1;
}

static gint GNC_GWENHYWFAR_CB
checkcert_cb(GWEN_GUI *gwen_gui, const GWEN_SSLCERTDESCR *cert,
             GWEN_IO_LAYER *io, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    const gchar *hash, *status;
    GChecksum *gcheck = g_checksum_new (G_CHECKSUM_MD5);
    gchar cert_hash[16];
    gint retval;
    gsize hashlen = 0;

    GwenSimpleCall call = {GWEN_SIMPLE_CHECK_CERT, gwen_gui, 0, guiid,
                           0, NULL, NULL, 0, -1, cert, io};
    if (gwen_simple_call_if_worker (&call))
        return call.result;

    g_return_val_if_fail(gui && gui->accepted_certs, -1);

    ENTER("gui=%p, cert=%p", gui, cert);

    hash = GWEN_SslCertDescr_GetFingerPrint(cert);
    status = GWEN_SslCertDescr_GetStatusText(cert);

    g_checksum_update (gcheck, (const guchar *)hash, strlen (hash));
    g_checksum_update (gcheck, (const guchar *)status, strlen (status));

    /* Did we get the permanently accepted certs from AqBanking? */
    if (gui->permanently_accepted_certs)
    {
        /* Generate a hex string of the cert_hash for usage by AqBanking cert store */
        retval = GWEN_DB_GetIntValue(gui->permanently_accepted_certs,
				     g_checksum_get_string (gcheck), 0, -1);
        if (retval == 0)
        {
            /* Certificate is marked as accepted in AqBanking's cert store */
	    g_checksum_free (gcheck);
            LEAVE("Certificate accepted by AqBanking's permanent cert store");
            return 0;
        }
    }
    else
    {
        g_warning("Can't check permanently accepted certs from invalid AqBanking cert store.");
    }

    g_checksum_get_digest (gcheck, (guint8 *)cert_hash, &hashlen);
    g_checksum_free (gcheck);
    g_assert (hashlen <= sizeof (cert_hash));

    if (g_hash_table_lookup(gui->accepted_certs, cert_hash))
    {
        /* Certificate has been accepted by Gnucash before */
        LEAVE("Automatically accepting certificate");
        return 0;
    }

    retval = gui->builtin_checkcert(gwen_gui, cert, io, guiid);
    if (retval == 0)
    {
        /* Certificate has been accepted */
        g_hash_table_insert(gui->accepted_certs, g_strdup(cert_hash), cert_hash);
    }

    LEAVE("retval=%d", retval);
    return retval;
}

gboolean
ggg_delete_event_cb(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
    GncGWENGui *gui = user_data;

    g_return_val_if_fail(gui, FALSE);

    ENTER("gui=%p, state=%d", gui, gui->state);

    if (gui->state == RUNNING)
    {
        const char *still_running_msg =
            _("The Online Banking job is still running; are you "
              "sure you want to cancel?");
        if (!gui->cancel_confirmation_pending)
        {
            gui->cancel_confirmation_pending = TRUE;
            gnc_verify_dialog_async (GTK_WINDOW (gui->dialog), FALSE,
                                     ggg_cancel_confirmation_done, gui,
                                     "%s", still_running_msg);
        }
        return TRUE;
    }

    hide_dialog(gui);

    LEAVE(" ");
    return TRUE;
}

static void
ggg_cancel_confirmation_done (GtkWindow *parent, gint response,
                              gpointer user_data)
{
    GncGWENGui *gui = user_data;
    if (!gui || !g_list_find (all_guis, gui))
        return;
    gui->cancel_confirmation_pending = FALSE;
    if (gui->release_pending)
    {
        gnc_GWEN_Gui_release (gui);
        return;
    }
    if (parent && response == GTK_RESPONSE_YES && gui->state == RUNNING)
        set_aborted (gui);
}

void
ggg_abort_clicked_cb(GtkButton *button, gpointer user_data)
{
    GncGWENGui *gui = user_data;

    g_return_if_fail(gui && gui->state == RUNNING);

    ENTER("gui=%p", gui);

    set_aborted(gui);

    LEAVE(" ");
}

void
ggg_close_clicked_cb(GtkButton *button, gpointer user_data)
{
    GncGWENGui *gui = user_data;

    g_return_if_fail(gui);
    g_return_if_fail(gui->state == INIT || gui->state == FINISHED || gui->state == ABORTED);

    ENTER("gui=%p", gui);

    hide_dialog(gui);

    LEAVE(" ");
}

void
ggg_close_toggled_cb(GtkToggleButton *button, gpointer user_data)
{
    GncGWENGui *gui = user_data;

    g_return_if_fail(gui);
    g_return_if_fail(gui->parent);

    ENTER("gui=%p", gui);

    gnc_prefs_set_bool(
        GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH,
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button)));

    LEAVE(" ");
}
