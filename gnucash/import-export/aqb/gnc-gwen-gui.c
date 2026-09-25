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
#include <gwenhywfar/gwenhywfar.h>
#include <gwenhywfar/inherit.h>
#include <gwenhywfar/version.h>

#include "dialog-utils.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gtk-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-hooks.h"
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

# include <gwen-gui-gtk4/gtk4_gui.h>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/* A unique full-blown GUI, featuring  */
static GncGWENGui *full_gui = NULL;

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
typedef struct _GncGwenWait GncGwenWait;
typedef void (*GncGwenWaitCancelFunc) (GncGwenWait *wait);

struct _GncGwenWait
{
    GMainLoop *loop;
    GncGwenWaitCancelFunc cancel;
    gboolean registered;
};

typedef struct
{
    gboolean dispatched;
} GncGwenFinalizeSource;

static GList *active_waits = NULL;
static gboolean waits_shutting_down = FALSE;
static gboolean ui_shutdown_hook_registered = FALSE;
static guint active_abi_frames = 0;

static gboolean shutdown_pending = FALSE;
static gboolean gui_shutdown_finalized = FALSE;
static gboolean shutdown_finalizing = FALSE;
static gboolean gwen_library_fini_pending = FALSE;
static gboolean gwen_library_owned = FALSE;
static guint gwen_library_clients = 0;
static guint shutdown_finalize_source_id = 0;
static GApplication *shutdown_application = NULL;
static gulong shutdown_application_handler_id = 0;

static void register_callbacks(GncGWENGui *gui);
static void unregister_callbacks(GncGWENGui *gui);
static void setup_dialog(GncGWENGui *gui);
static void enable_password_cache(GncGWENGui *gui, gboolean enabled);
static void reset_dialog(GncGWENGui *gui);
static void set_finished(GncGWENGui *gui);
static void set_aborted(GncGWENGui *gui);
static void show_dialog(GncGWENGui *gui, gboolean clear_log);
static void hide_dialog(GncGWENGui *gui);
static gboolean show_progress_cb(gpointer user_data);
static void show_progress(GncGWENGui *gui, Progress *progress);
static void hide_progress(GncGWENGui *gui, Progress *progress);
static void free_progress(Progress *progress, gpointer unused);
static gboolean keep_alive(GncGWENGui *gui);
static void cm_close_handler(gpointer user_data);
static void gwen_shutdown_finish_now(void);
static void gwen_shutdown_schedule(void);
static void gwen_ui_shutdown_cb(gpointer hook_data, gpointer user_data);

#ifdef GNC_GWEN_GUI_TESTING
void gnc_gwen_gui_test_get_state (guint*, guint*, guint*, guint*, guint*,
                                  guint*, guint*, guint*, guint*, guint*,
                                  guint*, guint*, guint*, guint*, guint*,
                                  gulong*);
void gnc_gwen_gui_test_take_permanent_certs (GWEN_DB_NODE *certs);

static guint test_init_wrapper_calls = 0;
static guint test_fini_wrapper_calls = 0;
static guint test_raw_init_calls = 0;
static guint test_raw_fini_calls = 0;
static guint test_finalize_source_calls = 0;
static guint test_finalize_destroy_calls = 0;
static guint test_gui_finalize_calls = 0;
static guint test_component_register_calls = 0;
static guint test_component_unregister_calls = 0;
static guint test_application_barrier_connect_calls = 0;
static guint test_application_barrier_disconnect_calls = 0;
/* The lifecycle test transfers an empty certificate store before the GUI is
 * created.  Keeping this injection here prevents test setup from initializing
 * AqBanking's per-user configuration merely to obtain an otherwise unrelated
 * certificate store. */
static GWEN_DB_NODE *test_permanently_accepted_certs = NULL;
# define GNC_GWEN_TEST_COUNT(counter) ((counter)++)
#else
# define GNC_GWEN_TEST_COUNT(counter) ((void)0)
#endif

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
static gboolean showbox_close_request_cb(GtkWindow *window, gpointer user_data);
static void showbox_close_clicked_cb(GtkButton *button, gpointer user_data);
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

gboolean ggg_delete_event_cb(GtkWindow *window, gpointer user_data);
void ggg_abort_clicked_cb(GtkButton *button, gpointer user_data);
void ggg_close_clicked_cb(GtkButton *button, gpointer user_data);
void ggg_close_toggled_cb(GtkCheckButton *button, gpointer user_data);

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
    GtkWidget *parent;
    gint component_id;
    GtkWidget *dialog;

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
    GtkCheckButton *close_checkbutton;

    /* Flags to keep track on whether an HBCI action is running or not */
    gboolean keep_alive;
    GuiState state;

    /* Password caching */
    gboolean cache_passwords;
    GHashTable *passwords;

    /* Certificates handling */
    GHashTable *accepted_certs;
    GWEN_DB_NODE *permanently_accepted_certs;
    GWEN_GUI_CHECKCERT_FN builtin_checkcert;

    /* Dialogs */
    guint32 showbox_id;
    guint32 showbox_last_id;
    GHashTable *showbox_hash;
    GtkWidget *showbox_last;

    /* Cache the lowest loglevel, corresponding to the most serious warning */
    GWEN_LOGGER_LEVEL min_loglevel;
};

struct _Progress
{
    GncGWENGui *gui;

    /* Title of the process */
    gchar *title;

    /* Event source id for showing delayed */
    guint source;
};

static gboolean
gwen_wait_register (GncGwenWait *wait)
{
    g_return_val_if_fail (wait && wait->loop && wait->cancel, FALSE);

    if (waits_shutting_down)
        return FALSE;

    active_waits = g_list_prepend (active_waits, wait);
    wait->registered = TRUE;
    return TRUE;
}

static void
gwen_abi_frame_enter (void)
{
    active_abi_frames++;
}

static void
gwen_abi_frame_leave (void)
{
    g_assert (active_abi_frames > 0);
    active_abi_frames--;
    if (!active_abi_frames)
        gwen_shutdown_schedule ();
}

static void
gwen_wait_unregister (GncGwenWait *wait)
{
    if (!wait->registered)
        return;

    active_waits = g_list_remove (active_waits, wait);
    wait->registered = FALSE;

    if (!active_waits)
        gwen_shutdown_schedule ();
}

static void
gwen_gui_quiesce (GncGWENGui *gui)
{
    if (!gui)
        return;

    gui->state = ABORTED;
    gui->keep_alive = FALSE;
    for (GList *node = gui->progresses; node; node = node->next)
    {
        Progress *progress = node->data;

        if (progress->source)
        {
            g_source_remove (progress->source);
            progress->source = 0;
        }
    }
    if (gui->showbox_hash)
        g_hash_table_remove_all (gui->showbox_hash);
    gui->showbox_last = NULL;
    gui->showbox_last_id = 0;
    if (gui->dialog)
        gtk_widget_set_visible (gui->dialog, FALSE);
}

static void
gwen_waits_shutdown (void)
{
    GList *snapshot;

    waits_shutting_down = TRUE;
    snapshot = g_list_copy (active_waits);
    for (GList *node = snapshot; node; node = node->next)
    {
        GncGwenWait *wait = node->data;

        wait->cancel (wait);
    }
    g_list_free (snapshot);
    gwen_gui_quiesce (full_gui);
}

static void
gwen_shutdown_application_barrier_clear (void)
{
    g_assert ((shutdown_application == NULL) ==
              (shutdown_application_handler_id == 0));

    if (!shutdown_application)
        return;

    g_signal_handler_disconnect (shutdown_application,
                                 shutdown_application_handler_id);
    shutdown_application_handler_id = 0;
    GNC_GWEN_TEST_COUNT (test_application_barrier_disconnect_calls);
    g_clear_object (&shutdown_application);
}

static void
gwen_shutdown_application_shutdown_cb (GApplication *application,
                                       gpointer user_data)
{
    guint source_id;

    (void)user_data;
    g_assert (application == shutdown_application);
    g_assert (!active_waits && !active_abi_frames);

    source_id = shutdown_finalize_source_id;
    if (source_id && g_source_remove (source_id))
    {
        /* The source DestroyNotify finalizes synchronously and clears this
         * signal handler and its strong application reference. */
        return;
    }

    shutdown_finalize_source_id = 0;
    gwen_shutdown_finish_now ();
}

static void
gwen_shutdown_application_barrier_bind (void)
{
    GApplication *application;

    if (shutdown_application_handler_id)
        return;

    application = g_application_get_default ();
    if (!G_IS_APPLICATION (application))
        return;

    shutdown_application = g_object_ref (application);
    shutdown_application_handler_id = g_signal_connect (
        shutdown_application, "shutdown",
        G_CALLBACK (gwen_shutdown_application_shutdown_cb), NULL);
    GNC_GWEN_TEST_COUNT (test_application_barrier_connect_calls);
}

static void
gwen_shutdown_finish_now (void)
{
    GncGWENGui *gui = full_gui;

    if (!shutdown_pending || active_waits || active_abi_frames ||
        shutdown_finalizing)
        return;

    shutdown_finalizing = TRUE;
    shutdown_pending = FALSE;
    gwen_shutdown_application_barrier_clear ();

    if (!gui_shutdown_finalized)
    {
        GNC_GWEN_TEST_COUNT (test_gui_finalize_calls);

        if (gui && gui->component_id != NO_COMPONENT)
        {
            gnc_unregister_gui_component (gui->component_id);
            gui->component_id = NO_COMPONENT;
            GNC_GWEN_TEST_COUNT (test_component_unregister_calls);
        }
        if (gui)
        {
            unregister_callbacks (gui);
            g_signal_handlers_disconnect_by_data (gui->dialog, gui);
            g_signal_handlers_disconnect_by_data (gui->abort_button, gui);
            g_signal_handlers_disconnect_by_data (gui->close_button, gui);
            g_signal_handlers_disconnect_by_data (gui->close_checkbutton, gui);
        }
        if (ui_shutdown_hook_registered)
        {
            gnc_hook_remove_dangler (HOOK_UI_SHUTDOWN,
                                     (GFunc)gwen_ui_shutdown_cb);
            ui_shutdown_hook_registered = FALSE;
        }

        if (gui)
        {
            full_gui = NULL;
            gui->parent = NULL;
            gtk_widget_set_visible (gui->dialog, FALSE);
            gtk_window_destroy (GTK_WINDOW(gui->dialog));
            gui->dialog = NULL;
            g_list_foreach (gui->progresses, (GFunc)free_progress, NULL);
            g_list_free (gui->progresses);
            gui->progresses = NULL;
            if (gui->passwords)
                g_hash_table_destroy(gui->passwords);
            if (gui->showbox_hash)
                g_hash_table_destroy(gui->showbox_hash);
            if (gui->permanently_accepted_certs)
                GWEN_DB_Group_free(gui->permanently_accepted_certs);
            if (gui->accepted_certs)
                g_hash_table_destroy(gui->accepted_certs);
            g_free(gui);
        }
        GWEN_Gui_SetGui(NULL);
        if (log_gwen_gui)
        {
            GWEN_Gui_free(log_gwen_gui);
            log_gwen_gui = NULL;
        }
        gui_shutdown_finalized = TRUE;
    }

    if (gwen_library_fini_pending)
    {
        gwen_library_fini_pending = FALSE;
        if (gwen_library_owned)
        {
            GWEN_Logger_SetLevel (NULL, GWEN_LoggerLevel_Error);
            GWEN_Logger_SetLevel (GWEN_LOGDOMAIN, GWEN_LoggerLevel_Warning);
            GWEN_Logger_SetLevel (AQBANKING_LOGDOMAIN,
                                  GWEN_LoggerLevel_Warning);
            gwen_library_owned = FALSE;
            GNC_GWEN_TEST_COUNT (test_raw_fini_calls);
            GWEN_Fini();
        }
    }

    shutdown_finalizing = FALSE;
}

static gboolean
gwen_shutdown_finalize_source_cb (gpointer user_data)
{
    GncGwenFinalizeSource *source = user_data;

    source->dispatched = TRUE;
    shutdown_finalize_source_id = 0;
    GNC_GWEN_TEST_COUNT (test_finalize_source_calls);
    gwen_shutdown_finish_now ();
    return G_SOURCE_REMOVE;
}

static void
gwen_shutdown_finalize_source_destroyed (gpointer user_data)
{
    GncGwenFinalizeSource *source = user_data;

    if (!source->dispatched)
    {
        shutdown_finalize_source_id = 0;
        if (shutdown_pending && !active_waits && !active_abi_frames)
        {
            GNC_GWEN_TEST_COUNT (test_finalize_destroy_calls);
            gwen_shutdown_finish_now ();
        }
    }
    g_free (source);
}

static void
gwen_shutdown_schedule (void)
{
    GncGwenFinalizeSource *source;

    if (!shutdown_pending || active_waits || active_abi_frames ||
        shutdown_finalize_source_id)
        return;

    source = g_new0 (GncGwenFinalizeSource, 1);
    shutdown_finalize_source_id = g_idle_add_full (
        G_PRIORITY_HIGH_IDLE, gwen_shutdown_finalize_source_cb, source,
        gwen_shutdown_finalize_source_destroyed);
}

static void
gwen_shutdown_request (gboolean finalize_library)
{
    if (gui_shutdown_finalized && !finalize_library)
        return;

    waits_shutting_down = TRUE;
    shutdown_pending = TRUE;
    if (finalize_library)
        gwen_library_fini_pending = TRUE;

    gwen_waits_shutdown ();
    if (!active_waits && !active_abi_frames)
    {
        gwen_shutdown_finish_now ();
        return;
    }

    /* The idle source handles ordinary unwind. The application barrier owns
     * liveness when g_application_quit() prevents another idle dispatch. */
    gwen_shutdown_application_barrier_bind ();
    gwen_shutdown_schedule ();
}

static void
gwen_ui_shutdown_cb (gpointer hook_data, gpointer user_data)
{
    (void)hook_data;
    (void)user_data;
    gnc_GWEN_Gui_shutdown ();
}

static gint GNC_GWENHYWFAR_CB
messagebox_abi_cb (GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                   const gchar *text, const gchar *b1, const gchar *b2,
                   const gchar *b3, guint32 guiid)
{
    gint result;

    gwen_abi_frame_enter ();
    result = messagebox_cb (gwen_gui, flags, title, text, b1, b2, b3, guiid);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
inputbox_abi_cb (GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                 const gchar *text, gchar *buffer, gint min_len,
                 gint max_len, guint32 guiid)
{
    gint result;

    gwen_abi_frame_enter ();
    result = inputbox_cb (gwen_gui, flags, title, text, buffer, min_len,
                          max_len, guiid);
    gwen_abi_frame_leave ();
    return result;
}

static guint32 GNC_GWENHYWFAR_CB
showbox_abi_cb (GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                const gchar *text, guint32 guiid)
{
    guint32 result;

    gwen_abi_frame_enter ();
    result = showbox_cb (gwen_gui, flags, title, text, guiid);
    gwen_abi_frame_leave ();
    return result;
}

static void GNC_GWENHYWFAR_CB
hidebox_abi_cb (GWEN_GUI *gwen_gui, guint32 id)
{
    gwen_abi_frame_enter ();
    hidebox_cb (gwen_gui, id);
    gwen_abi_frame_leave ();
}

static guint32 GNC_GWENHYWFAR_CB
progress_start_abi_cb (GWEN_GUI *gwen_gui, uint32_t progress_flags,
                       const char *title, const char *text, uint64_t total,
                       uint32_t guiid)
{
    guint32 result;

    gwen_abi_frame_enter ();
    result = progress_start_cb (gwen_gui, progress_flags, title, text, total,
                                guiid);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
progress_advance_abi_cb (GWEN_GUI *gwen_gui, uint32_t id,
                         uint64_t progress)
{
    gint result;

    gwen_abi_frame_enter ();
    result = progress_advance_cb (gwen_gui, id, progress);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
progress_log_abi_cb (GWEN_GUI *gwen_gui, guint32 id,
                     GWEN_LOGGER_LEVEL level, const gchar *text)
{
    gint result;

    gwen_abi_frame_enter ();
    result = progress_log_cb (gwen_gui, id, level, text);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
progress_end_abi_cb (GWEN_GUI *gwen_gui, guint32 id)
{
    gint result;

    gwen_abi_frame_enter ();
    result = progress_end_cb (gwen_gui, id);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
getpassword_abi_cb (GWEN_GUI *gwen_gui, guint32 flags, const gchar *token,
                    const gchar *title, const gchar *text, gchar *buffer,
                    gint min_len, gint max_len,
                    GWEN_GUI_PASSWORD_METHOD method_id,
                    GWEN_DB_NODE *method_params, guint32 guiid)
{
    gint result;

    gwen_abi_frame_enter ();
    result = getpassword_cb (gwen_gui, flags, token, title, text, buffer,
                             min_len, max_len, method_id, method_params,
                             guiid);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
setpasswordstatus_abi_cb (GWEN_GUI *gwen_gui, const gchar *token,
                          const gchar *pin, GWEN_GUI_PASSWORD_STATUS status,
                          guint32 guiid)
{
    gint result;

    gwen_abi_frame_enter ();
    result = setpasswordstatus_cb (gwen_gui, token, pin, status, guiid);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
loghook_abi_cb (GWEN_GUI *gwen_gui, const gchar *log_domain,
                GWEN_LOGGER_LEVEL priority, const gchar *text)
{
    gint result;

    gwen_abi_frame_enter ();
    result = loghook_cb (gwen_gui, log_domain, priority, text);
    gwen_abi_frame_leave ();
    return result;
}

static gint GNC_GWENHYWFAR_CB
checkcert_abi_cb (GWEN_GUI *gwen_gui, const GWEN_SSLCERTDESCR *cert,
                  GWEN_IO_LAYER *io, guint32 guiid)
{
    gint result;

    gwen_abi_frame_enter ();
    result = checkcert_cb (gwen_gui, cert, io, guiid);
    gwen_abi_frame_leave ();
    return result;
}

void
gnc_GWEN_Init (void)
{
    gchar* gwen_logging;
    gchar* aqb_logging;

    GNC_GWEN_TEST_COUNT (test_init_wrapper_calls);
    if (waits_shutting_down)
    {
        g_warning ("Gwenhywfar cannot be reinitialized after UI shutdown");
        return;
    }

    gwen_library_clients++;
    if (gwen_library_owned)
        return;

    if (GWEN_Init() < 0)
    {
        gwen_library_clients--;
        g_warning ("Could not initialize Gwenhywfar");
        return;
    }
    gwen_library_owned = TRUE;
    GNC_GWEN_TEST_COUNT (test_raw_init_calls);

    gwen_logging = g_strdup (g_getenv ("GWEN_LOGLEVEL"));
    aqb_logging = g_strdup (g_getenv ("AQBANKING_LOGLEVEL"));
    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING, GNC_PREF_VERBOSE_DEBUG))
    {
        if (!gwen_logging)
        {
            GWEN_Logger_SetLevel (NULL, GWEN_LoggerLevel_Info);
            GWEN_Logger_SetLevel (GWEN_LOGDOMAIN, GWEN_LoggerLevel_Info);
        }
        if (!aqb_logging)
            GWEN_Logger_SetLevel (AQBANKING_LOGDOMAIN, GWEN_LoggerLevel_Debug);
    }
    else
    {
        if (!gwen_logging)
        {
            GWEN_Logger_SetLevel (NULL, GWEN_LoggerLevel_Error);
            GWEN_Logger_SetLevel (GWEN_LOGDOMAIN, GWEN_LoggerLevel_Error);
        }
        if (!aqb_logging)
            GWEN_Logger_SetLevel (AQBANKING_LOGDOMAIN, GWEN_LoggerLevel_Warning);
    }
    g_free (gwen_logging);
    g_free (aqb_logging);
    gnc_GWEN_Gui_log_init();
}

void
gnc_GWEN_Gui_log_init(void)
{
    if (!gwen_library_owned || waits_shutting_down)
        return;

    if (!log_gwen_gui)
    {
        if (!ui_shutdown_hook_registered)
        {
            gnc_hook_add_dangler (HOOK_UI_SHUTDOWN,
                                  (GFunc)gwen_ui_shutdown_cb, NULL, NULL);
            ui_shutdown_hook_registered = TRUE;
        }
        log_gwen_gui = Gtk4_Gui_new();

        /* Always use our own logging */
        GWEN_Gui_SetLogHookFn(log_gwen_gui, loghook_abi_cb);
    }

    GWEN_Gui_SetGui(log_gwen_gui);
}

GncGWENGui *
gnc_GWEN_Gui_get(GtkWidget *parent)
{
    GncGWENGui *gui;

    ENTER("parent=%p", parent);

    if (waits_shutting_down)
    {
        LEAVE("GUI shutdown is in progress");
        return NULL;
    }

    if (full_gui)
    {
        if (full_gui->state == INIT || full_gui->state == RUNNING)
        {
            LEAVE("full_gui in use, state=%d", full_gui->state);
            return NULL;
        }

        gui = full_gui;
        gui->parent = parent;
        reset_dialog(gui);
        register_callbacks(gui);

        LEAVE("gui=%p", gui);
        return gui;
    }

    gui = g_new0(GncGWENGui, 1);
    gui->parent = parent;
    setup_dialog(gui);
    register_callbacks(gui);

    full_gui = gui;

    LEAVE("new gui=%p", gui);
    return gui;
}

void
gnc_GWEN_Gui_release(GncGWENGui *gui)
{
    g_return_if_fail(gui && gui == full_gui);

    /* Currently a no-op */
    ENTER("gui=%p", gui);
    LEAVE(" ");
}

void
gnc_GWEN_Fini (void)
{
    GNC_GWEN_TEST_COUNT (test_fini_wrapper_calls);
    if (!gwen_library_clients)
        return;

    gwen_library_clients--;
    if (gwen_library_clients)
        return;

    if (!gwen_library_owned && !gwen_library_fini_pending)
        return;

    gwen_shutdown_request (TRUE);
}

void
gnc_GWEN_Gui_shutdown(void)
{
    ENTER(" ");

    gwen_shutdown_request (FALSE);
    if (active_waits)
        LEAVE("waiting for %u synchronous Gwen callback(s) to unwind",
              g_list_length (active_waits));
    else
        LEAVE("shutdown finalized");
}

void
gnc_GWEN_Gui_set_close_flag(gboolean close_when_finished)
{
    gnc_prefs_set_bool(
        GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH,
        close_when_finished);

    if (full_gui)
    {
        if (gtk_check_button_get_active(full_gui->close_checkbutton)
                != close_when_finished)
            gtk_check_button_set_active(full_gui->close_checkbutton,
                                        close_when_finished);
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

    if (waits_shutting_down)
        return FALSE;

    if (!gui)
    {
        gnc_GWEN_Gui_get(NULL);
        gui = full_gui;
    }

    if (gui)
    {
        if (gui->state == HIDDEN)
        {
            gui->state = FINISHED;
        }
        gtk_check_button_set_active(gui->close_checkbutton,
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

    gwen_gui = Gtk4_Gui_new();
    gui->gwen_gui = gwen_gui;

    GWEN_Gui_SetMessageBoxFn(gwen_gui, messagebox_abi_cb);
    GWEN_Gui_SetInputBoxFn(gwen_gui, inputbox_abi_cb);
    GWEN_Gui_SetShowBoxFn(gwen_gui, showbox_abi_cb);
    GWEN_Gui_SetHideBoxFn(gwen_gui, hidebox_abi_cb);
    GWEN_Gui_SetProgressStartFn(gwen_gui, progress_start_abi_cb);
    GWEN_Gui_SetProgressAdvanceFn(gwen_gui, progress_advance_abi_cb);
    GWEN_Gui_SetProgressLogFn(gwen_gui, progress_log_abi_cb);
    GWEN_Gui_SetProgressEndFn(gwen_gui, progress_end_abi_cb);
    GWEN_Gui_SetGetPasswordFn(gwen_gui, getpassword_abi_cb);
    GWEN_Gui_SetSetPasswordStatusFn(gwen_gui, setpasswordstatus_abi_cb);
    GWEN_Gui_SetLogHookFn(gwen_gui, loghook_abi_cb);
    gui->builtin_checkcert = GWEN_Gui_SetCheckCertFn(gwen_gui, checkcert_abi_cb);

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

    if (GWEN_Gui_GetGui() == gui->gwen_gui)
        GWEN_Gui_SetGui(log_gwen_gui);
    GWEN_Gui_free(gui->gwen_gui);
    gui->gwen_gui = NULL;

    LEAVE(" ");
}

static void
setup_dialog(GncGWENGui *gui)
{
    GtkBuilder *builder;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "aqbanking_connection_dialog");

    gui->component_id = NO_COMPONENT;
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
    gui->close_checkbutton = GTK_CHECK_BUTTON(
        gtk_builder_get_object (builder, "close_checkbutton"));
    gui->accepted_certs = NULL;
    gui->permanently_accepted_certs = NULL;
    gui->showbox_hash = NULL;
    gui->showbox_id = 1;

    /* Connect the Signals */
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gui);

    gtk_check_button_set_active(gui->close_checkbutton,
        gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH));

    gui->component_id = gnc_register_gui_component(GWEN_GUI_CM_CLASS, NULL,
                                                    cm_close_handler, gui);
    GNC_GWEN_TEST_COUNT (test_component_register_calls);
    gnc_gui_component_set_session(gui->component_id,
                                  gnc_get_current_session());



    g_object_unref(G_OBJECT(builder));

    reset_dialog(gui);

    LEAVE(" ");
}

static void
enable_password_cache(GncGWENGui *gui, gboolean enabled)
{
    g_return_if_fail(gui);

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
}

static void
reset_dialog(GncGWENGui *gui)
{
    gboolean cache_passwords;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    gnc_entry_set_text(GTK_ENTRY(gui->top_entry), "");
    gnc_entry_set_text(GTK_ENTRY(gui->second_entry), "");
    g_list_foreach(gui->progresses, (GFunc) free_progress, NULL);
    g_list_free(gui->progresses);
    gui->progresses = NULL;

    if (gui->other_entries_box)
    {
        gtk_grid_remove_row (GTK_GRID(gui->entries_grid),
                             OTHER_ENTRIES_ROW_OFFSET);
        gui->other_entries_box = NULL;
    }
    if (gui->showbox_hash)
        g_hash_table_destroy(gui->showbox_hash);
    gui->showbox_last = NULL;
    gui->showbox_last_id = 0;
    gui->showbox_hash = g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                              NULL, (GDestroyNotify)gtk_window_destroy);

    if (gui->parent)
        gtk_window_set_transient_for(GTK_WINDOW(gui->dialog),
                                     GTK_WINDOW(gui->parent));
    gnc_restore_window_size(GNC_PREFS_GROUP_CONNECTION,
                            GTK_WINDOW(gui->dialog), GTK_WINDOW(gui->parent));

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
#ifdef GNC_GWEN_GUI_TESTING
    {
        g_assert_nonnull (test_permanently_accepted_certs);
        gui->permanently_accepted_certs = test_permanently_accepted_certs;
        test_permanently_accepted_certs = NULL;
    }
#else
        gui->permanently_accepted_certs = gnc_ab_get_permanent_certs();
#endif

    LEAVE(" ");
}

static void
set_running(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    if (waits_shutting_down)
    {
        gui->keep_alive = FALSE;
        return;
    }

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
    if (gtk_check_button_get_active(gui->close_checkbutton))
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

    if (waits_shutting_down)
        return;

    ENTER("gui=%p, clear_log=%d", gui, clear_log);

    gtk_widget_set_visible (GTK_WIDGET(gui->dialog), TRUE);

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
    gtk_widget_set_visible (GTK_WIDGET(gui->dialog), FALSE);

    gnc_plugin_aqbanking_set_logwindow_visible(FALSE);

    /* Cache the close check button state for next time */
    gnc_prefs_set_bool(
        GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH,
        gtk_check_button_get_active(gui->close_checkbutton));

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

    if (waits_shutting_down)
        return G_SOURCE_REMOVE;

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

    if (waits_shutting_down)
        return;

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
            gnc_entry_set_text(GTK_ENTRY(gui->top_entry), current->title);
        }
        else if (!item->next->next)
        {
            /* Second-level progress */
            gnc_entry_set_text(GTK_ENTRY(gui->second_entry), current->title);
        }
        else
        {
            /* Other progress */
            GtkWidget *entry = gtk_entry_new();
            GtkWidget *box = gui->other_entries_box;
            gboolean new_box = box == NULL;

            gnc_entry_set_text(GTK_ENTRY(entry), current->title);
            if (new_box)
            {
                gui->other_entries_box = box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 6);
                gtk_box_set_homogeneous (GTK_BOX (gui->other_entries_box), TRUE);
                gtk_box_set_homogeneous (GTK_BOX (box), TRUE);
            }

            gtk_box_append (GTK_BOX(box), GTK_WIDGET(entry));
            gtk_widget_set_visible (GTK_WIDGET(entry), TRUE);
            if (new_box)
            {
                gtk_grid_attach (GTK_GRID(gui->entries_grid), box,
                                 1, OTHER_ENTRIES_ROW_OFFSET, 1, 1);
                gtk_widget_set_visible (GTK_WIDGET(box), TRUE);
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
            gnc_entry_set_text(GTK_ENTRY(gui->second_entry), "");
        }
        else if (!item->next->next)
        {
            /* Second-level progress */
            gnc_entry_set_text(GTK_ENTRY(gui->second_entry), "");
        }
        else
        {
            /* Other progress */
            GtkWidget *box = gui->other_entries_box;
            g_return_if_fail(box);
            GtkWidget *child = gtk_widget_get_last_child (box);

            if (child)
            {
                /* Another progress is still to be showed */
                gtk_box_remove (GTK_BOX(box), child);
            }
            if (!gtk_widget_get_first_child (box))
            {
                /* Last other progress to be hidden */
                gtk_grid_remove_row (GTK_GRID(gui->entries_grid),
                                     OTHER_ENTRIES_ROW_OFFSET);
                /* Box destroyed, Null the reference. */
                gui->other_entries_box = NULL;
            }
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

    /* Gwen drives progress through synchronous callbacks. Drain only pending
     * GTK work so the log window updates; this is not another main loop. */
    while (g_main_context_iteration(NULL, FALSE));

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

typedef struct
{
    GncGwenWait wait;
    GtkWindow *window;
    gint response;
    gboolean answered;
    gboolean destroyed;
} GncGwenWindowResponseState;

typedef struct
{
    GtkButton *button;
    gint response;
    GncGwenWindowResponseState *state;
    gulong handler;
} GncGwenResponseButton;

typedef struct
{
    GncGwenWait wait;
    GCancellable *cancellable;
    gint response;
    gboolean answered;
    gboolean parent_destroyed;
} GncGwenAlertResponseState;

static void gwen_window_wait_cancel (GncGwenWait *wait);

static void
gwen_window_response_finish (GncGwenWindowResponseState *state,
                             gint response)
{
    if (state->answered)
        return;

    state->response = response;
    state->answered = TRUE;
    g_main_loop_quit (state->wait.loop);
}

static void
gwen_window_wait_cancel (GncGwenWait *wait)
{
    GncGwenWindowResponseState *state = (GncGwenWindowResponseState *)wait;

    if (!state->destroyed)
        gtk_widget_set_visible (GTK_WIDGET (state->window), FALSE);
    else
        gwen_window_response_finish (state, GTK_RESPONSE_DELETE_EVENT);
}

static void
gwen_window_button_clicked_cb (GtkButton *button, gpointer user_data)
{
    GncGwenResponseButton *response_button = user_data;

    (void)button;
    gwen_window_response_finish (response_button->state,
                                 response_button->response);
}

static gboolean
gwen_window_close_request_cb (GtkWindow *window, gpointer user_data)
{
    (void)window;
    gwen_window_response_finish (user_data, GTK_RESPONSE_DELETE_EVENT);
    return TRUE;
}

static void
gwen_window_visibility_cb (GObject *object, GParamSpec *pspec,
                           gpointer user_data)
{
    GncGwenWindowResponseState *state = user_data;

    (void)pspec;
    if (gtk_widget_get_visible (GTK_WIDGET (object)))
        return;

    /* A logical GTK4 close hides the toplevel before dropping its owner refs.
     * Visibility is therefore the terminal event that can be observed while
     * this synchronous adapter still holds its lifetime ref. */
    state->destroyed = TRUE;
    gwen_window_response_finish (state, GTK_RESPONSE_DELETE_EVENT);
}

static gint
wait_for_window_response (GtkWindow *window, GncGwenResponseButton *buttons,
                          gsize n_buttons)
{
    GncGwenWindowResponseState state = { 0 };
    gulong close_handler;
    gulong visibility_handler;

    g_return_val_if_fail (GTK_IS_WINDOW (window), GTK_RESPONSE_DELETE_EVENT);
    g_return_val_if_fail (buttons && n_buttons, GTK_RESPONSE_DELETE_EVENT);

    for (gsize index = 0; index < n_buttons; index++)
        g_return_val_if_fail (GTK_IS_BUTTON (buttons[index].button),
                              GTK_RESPONSE_DELETE_EVENT);

    /* Gwen's C callback ABI needs a concrete response before it returns.
     * This and the alert helper below are the only nested-loop adapters;
     * the GtkWindow itself uses ordinary GTK4 button and close-request signals. */
    g_object_ref (window);
    state.wait.loop = g_main_loop_new (NULL, FALSE);
    state.wait.cancel = gwen_window_wait_cancel;
    state.window = window;
    if (!gwen_wait_register (&state.wait))
    {
        g_main_loop_unref (state.wait.loop);
        g_object_unref (window);
        return GTK_RESPONSE_DELETE_EVENT;
    }

    for (gsize index = 0; index < n_buttons; index++)
    {
        buttons[index].state = &state;
        buttons[index].handler = g_signal_connect (
            buttons[index].button, "clicked",
            G_CALLBACK (gwen_window_button_clicked_cb), &buttons[index]);
    }
    close_handler = g_signal_connect (window, "close-request",
                                      G_CALLBACK (gwen_window_close_request_cb),
                                      &state);
    visibility_handler = g_signal_connect (
        window, "notify::visible", G_CALLBACK (gwen_window_visibility_cb),
        &state);
    gtk_window_set_modal (window, TRUE);
    gtk_window_present (window);
    if (!state.answered)
        g_main_loop_run (state.wait.loop);
    gwen_wait_unregister (&state.wait);

    for (gsize index = 0; index < n_buttons; index++)
        g_signal_handler_disconnect (buttons[index].button,
                                     buttons[index].handler);
    g_signal_handler_disconnect (window, close_handler);
    g_signal_handler_disconnect (window, visibility_handler);
    g_main_loop_unref (state.wait.loop);
    g_object_unref (window);

    return state.answered && !state.destroyed ? state.response :
        GTK_RESPONSE_DELETE_EVENT;
}

static gint
wait_for_password_window_response (GtkWindow *window,
                                   GtkButton *cancel_button,
                                   GtkButton *ok_button)
{
    GncGwenResponseButton buttons[] =
    {
        { cancel_button, GTK_RESPONSE_CANCEL, NULL, 0 },
        { ok_button, GTK_RESPONSE_OK, NULL, 0 }
    };

    return wait_for_window_response (window, buttons, G_N_ELEMENTS (buttons));
}

static void
gwen_alert_wait_cancel (GncGwenWait *wait)
{
    GncGwenAlertResponseState *state = (GncGwenAlertResponseState *)wait;

    if (!g_cancellable_is_cancelled (state->cancellable))
        g_cancellable_cancel (state->cancellable);
}

static void
gwen_alert_response_cb (GObject *source, GAsyncResult *result,
                        gpointer user_data)
{
    GncGwenAlertResponseState *state = user_data;
    GError *error = NULL;
    gint response = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source),
                                                     result, &error);

    if (state->answered)
        return;

    if (error)
    {
        g_clear_error (&error);
        response = 0;
    }
    state->response = response;
    state->answered = TRUE;
    g_main_loop_quit (state->wait.loop);
}

static void
gwen_alert_parent_destroyed_cb (GtkWidget *widget, gpointer user_data)
{
    GncGwenAlertResponseState *state = user_data;

    (void)widget;
    state->parent_destroyed = TRUE;
    g_cancellable_cancel (state->cancellable);
}

static gint
wait_for_alert_response (GtkWindow *parent, const gchar *message,
                         gboolean yes_is_default)
{
    const char *buttons[] = { _("_No"), _("_Yes"), NULL };
    GncGwenAlertResponseState state = { 0 };
    GtkAlertDialog *alert;
    gulong parent_destroy_handler = 0;

    alert = gtk_alert_dialog_new ("%s", message);
    gtk_alert_dialog_set_buttons (alert, buttons);
    gtk_alert_dialog_set_cancel_button (alert, 0);
    gtk_alert_dialog_set_default_button (alert, yes_is_default ? 1 : 0);
    /* GtkAlertDialog completes asynchronously; wait only at this Gwen ABI
     * boundary and cancel the request if its parent disappears. */
    state.wait.loop = g_main_loop_new (NULL, FALSE);
    state.wait.cancel = gwen_alert_wait_cancel;
    state.cancellable = g_cancellable_new ();
    if (!gwen_wait_register (&state.wait))
    {
        g_object_unref (state.cancellable);
        g_main_loop_unref (state.wait.loop);
        g_object_unref (alert);
        return 0;
    }

    if (parent)
        parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (gwen_alert_parent_destroyed_cb),
            &state);
    gtk_alert_dialog_choose (alert, parent, state.cancellable,
                             gwen_alert_response_cb, &state);
    if (!state.answered)
        g_main_loop_run (state.wait.loop);
    gwen_wait_unregister (&state.wait);

    if (parent_destroy_handler && !state.parent_destroyed)
        g_signal_handler_disconnect (parent, parent_destroy_handler);
    g_object_unref (state.cancellable);
    g_main_loop_unref (state.wait.loop);
    g_object_unref (alert);

    return state.answered ? state.response : 0;
}

static gboolean
gwen_confirm (GtkWindow *parent, const gchar *message, gboolean yes_is_default)
{
    return wait_for_alert_response (parent, message, yes_is_default) == 1;
}

static GtkWindow *
gwen_message_window_new (GtkWindow *parent, const gchar *title,
                         const gchar *text, GtkBox **actions_out)
{
    GtkWindow *window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (window);
    GtkWidget *content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    GtkWidget *actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    GtkWidget *label;
    gchar *raw_text = strip_html (g_strdup (text));

    label = gtk_label_new (raw_text);
    g_free (raw_text);
    gtk_label_set_justify (GTK_LABEL (label), GTK_JUSTIFY_LEFT);
    gtk_label_set_wrap (GTK_LABEL (label), TRUE);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_widget_set_halign (actions, GTK_ALIGN_END);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_box_append (GTK_BOX (content), label);
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_child (window, content);
    if (parent)
        gtk_window_set_transient_for (window, parent);
    if (title)
        gtk_window_set_title (window, title);
    if (actions_out)
        *actions_out = GTK_BOX (actions);

    return window;
}

static void
get_input(GncGWENGui *gui, guint32 flags, const gchar *title,
                      const gchar *text, const char *mimeType,
                      const char *pChallenge, uint32_t lChallenge,
                      gchar **input, gint min_len, gint max_len)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *input_entry;
    GtkWidget *confirm_entry;
    GtkWidget *confirm_label;
    GtkCheckButton *remember_pin_checkbutton;
    GtkImage *optical_challenge;
    GtkButton *cancel_button;
    GtkButton *ok_button;

    static GncFlickerGui *flickergui = NULL;

    const gchar *internal_input, *internal_confirmed;
    gboolean confirm = (flags & GWEN_GUI_INPUT_FLAGS_CONFIRM) != 0;
    gboolean is_tan = (flags & GWEN_GUI_INPUT_FLAGS_TAN) != 0;

    g_return_if_fail(input);
    g_return_if_fail(max_len >= min_len && max_len > 0);

    ENTER(" ");

    /* Set up dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-ab.glade", "aqbanking_password_dialog");
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "aqbanking_password_dialog"));

    heading_label = GTK_WIDGET(gtk_builder_get_object (builder, "heading_pw_label"));
    input_entry = GTK_WIDGET(gtk_builder_get_object (builder, "input_entry"));
    confirm_entry = GTK_WIDGET(gtk_builder_get_object (builder, "confirm_entry"));
    confirm_label = GTK_WIDGET(gtk_builder_get_object (builder, "confirm_label"));
    remember_pin_checkbutton = GTK_CHECK_BUTTON(
        gtk_builder_get_object (builder, "remember_pin"));
    optical_challenge = GTK_IMAGE(gtk_builder_get_object (builder, "optical_challenge"));
    cancel_button = GTK_BUTTON(gtk_builder_get_object (builder, "cancelbutton2"));
    ok_button = GTK_BUTTON(gtk_builder_get_object (builder, "okbutton2"));
    gtk_widget_set_visible(GTK_WIDGET(optical_challenge), FALSE);

    flickergui = g_slice_new(GncFlickerGui);
    flickergui->flicker_challenge = GTK_WIDGET(gtk_builder_get_object(builder, "flicker_challenge"));
    flickergui->flicker_marker = GTK_WIDGET(gtk_builder_get_object(builder, "flicker_marker"));
    flickergui->flicker_hbox = GTK_WIDGET(gtk_builder_get_object(builder, "flicker_hbox"));
    flickergui->spin_barwidth = GTK_SPIN_BUTTON(gtk_builder_get_object(builder, "spin_barwidth"));
    flickergui->spin_delay = GTK_SPIN_BUTTON(gtk_builder_get_object(builder, "spin_delay"));

    gtk_widget_set_visible(GTK_WIDGET(flickergui->flicker_challenge), FALSE);
    gtk_widget_set_visible(GTK_WIDGET(flickergui->flicker_marker), FALSE);
    gtk_widget_set_visible(GTK_WIDGET(flickergui->flicker_hbox), FALSE);
    gtk_widget_set_visible(GTK_WIDGET(flickergui->spin_barwidth), FALSE);
    gtk_widget_set_visible(GTK_WIDGET(flickergui->spin_delay), FALSE);

    if (g_strcmp0(mimeType,"text/x-flickercode") == 0 && pChallenge != NULL)
    {
        /* Chiptan Optic (aka Flicker) */
        gtk_widget_set_visible(GTK_WIDGET(flickergui->flicker_challenge), TRUE);
        gtk_widget_set_visible(GTK_WIDGET(flickergui->flicker_marker), TRUE);
        gtk_widget_set_visible(GTK_WIDGET(flickergui->flicker_hbox), TRUE);
        gtk_widget_set_visible(GTK_WIDGET(flickergui->spin_barwidth), TRUE);
        gtk_widget_set_visible(GTK_WIDGET(flickergui->spin_delay), TRUE);
    }
    else if(mimeType != NULL && pChallenge != NULL && lChallenge > 0)
    {
        /* Phototan or Chiptan QR */
        gtk_widget_set_visible(GTK_WIDGET(optical_challenge), TRUE);
    }
    if (is_tan)
    {
        gtk_widget_set_visible (GTK_WIDGET (remember_pin_checkbutton), FALSE);
    }
    else
    {
        gtk_check_button_set_active(remember_pin_checkbutton,
                                    gui->cache_passwords);
    }

    /* Enable the normal input visibility for TAN and for the set SHOW flag */
    if ((flags & (GWEN_GUI_INPUT_FLAGS_TAN | GWEN_GUI_INPUT_FLAGS_SHOW)) != 0)
    {
        gtk_widget_set_visible(input_entry, TRUE);
        gtk_entry_set_visibility(GTK_ENTRY(input_entry), TRUE);
    }

    if (gui->dialog)
    {
        gtk_window_set_transient_for(GTK_WINDOW(dialog),
                                     GTK_WINDOW(gui->dialog));
    }
    else
    {
        if (gui->parent)
            gtk_window_set_transient_for(GTK_WINDOW(dialog),
                                         GTK_WINDOW(gui->parent));
    }
    if (title)
        gtk_window_set_title(GTK_WINDOW(dialog), title);

    if (text)
    {
        gchar *raw_text = strip_html(g_strdup(text));
        gtk_label_set_text(GTK_LABEL(heading_label), raw_text);
        g_free(raw_text);
    }

    /* Optical challenge. Flickercode sets the mimetype to
     * x-flickercode and doesn't set the challenge length */
    if (g_strcmp0(mimeType,"text/x-flickercode") == 0 && pChallenge != NULL)
    {
         /* Chiptan Optic (aka Flicker) */
         flickergui->dialog = dialog;
         flickergui->input_entry = input_entry;

         ini_flicker_gui(pChallenge, flickergui);
         g_slice_free(GncFlickerGui, flickergui);
    }
    /* While phototan has multiple mimetypes and does set the
     * challenge length. */
    else if(mimeType != NULL && pChallenge != NULL && lChallenge > 0)
    {
        /* Phototan or Chiptan QR */
        // convert PNG and load into widget
        // TBD: check mimeType?
        guchar *gudata = (guchar*)pChallenge;

        GError *error = NULL;
        GdkPixbufLoader *loader = gdk_pixbuf_loader_new_with_mime_type (mimeType,
                                                                          &error);

        if (!loader)
        {
            PERR ("Pixbuf loader not loaded: %s, perhaps MIME type %s isn't supported.",
                  error ? error->message : "unknown error", mimeType);
            g_clear_error (&error);
        }
        else if (gdk_pixbuf_loader_write (loader, gudata, lChallenge, &error) &&
                 gdk_pixbuf_loader_close (loader, &error))
        {
            GdkPixbuf *pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);

            if (pixbuf)
            {
                GdkTexture *texture = gnc_texture_new_from_pixbuf (pixbuf);

                if (texture)
                {
                    gtk_image_set_from_paintable (optical_challenge,
                                                   GDK_PAINTABLE (texture));
                    g_object_unref (texture);
                }
                else
                    gtk_image_clear (optical_challenge);
            }
            g_object_unref (loader);
        }
        else
        {
            PERR ("Could not load optical challenge: %s",
                  error ? error->message : "unknown error");
            g_clear_error (&error);
            g_object_unref (loader);
        }
    }

    if (*input)
    {
        gnc_entry_set_text(GTK_ENTRY(input_entry), *input);
        erase_password(*input);
        *input = NULL;
    }

    if (confirm)
    {
        gtk_entry_set_activates_default(GTK_ENTRY(input_entry), FALSE);
        gtk_entry_set_activates_default(GTK_ENTRY(confirm_entry), TRUE);
        gtk_entry_set_max_length(GTK_ENTRY(input_entry), max_len);
        gtk_entry_set_max_length(GTK_ENTRY(confirm_entry), max_len);
    }
    else
    {
        gtk_entry_set_activates_default(GTK_ENTRY(input_entry), TRUE);
        gtk_entry_set_max_length(GTK_ENTRY(input_entry), max_len);
        gtk_widget_set_visible (GTK_WIDGET(confirm_entry), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(confirm_label), FALSE);
    }
    gtk_window_set_default_widget (GTK_WINDOW(dialog), GTK_WIDGET(ok_button));

    /* Ask the user until he enters a valid input or cancels */
    while (TRUE)
    {
        gboolean remember_pin;

        if (wait_for_password_window_response (GTK_WINDOW(dialog), cancel_button,
                                               ok_button) != GTK_RESPONSE_OK)
            break;

        if (!is_tan)
        {
            /* Enable or disable the password cache */
            remember_pin = gtk_check_button_get_active(remember_pin_checkbutton);
            enable_password_cache(gui, remember_pin);
            gnc_prefs_set_bool(GNC_PREFS_GROUP_AQBANKING, GNC_PREF_REMEMBER_PIN,
                               remember_pin);
        }

        internal_input = gnc_entry_get_text(GTK_ENTRY(input_entry));
        if (strlen(internal_input) < min_len)
        {
            gboolean retval;
            gchar *msg = g_strdup_printf(
                             _("The PIN needs to be at least %d characters\n"
                               "long. Do you want to try again?"), min_len);
            retval = gwen_confirm (GTK_WINDOW (dialog), msg, TRUE);
            g_free(msg);
            if (!retval)
                break;
            continue;
        }

        if (!confirm)
        {
            *input = g_strdup(internal_input);
            break;
        }

        internal_confirmed = gnc_entry_get_text(GTK_ENTRY(confirm_entry));
        if (strcmp(internal_input, internal_confirmed) == 0)
        {
            *input = g_strdup(internal_input);
            break;
        }
    }

    /* Destroy while the builder still owns the dialog. An external destroy
     * may already have removed GTK's internal toplevel reference. */
    gtk_window_destroy (GTK_WINDOW(dialog));
    /* This trashes passwords in the entries' memory as well. */
    g_object_unref(G_OBJECT(builder));

    LEAVE("input %s", *input ? "non-NULL" : "NULL");
}

static gint GNC_GWENHYWFAR_CB
messagebox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
              const gchar *text, const gchar *b1, const gchar *b2,
              const gchar *b3, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    const gchar *labels[] = { b1, b2, b3 };
    GncGwenResponseButton buttons[3] = { 0 };
    GtkBox *actions;
    GtkWindow *window;
    guint n_buttons = 0;
    gint result;

    (void)flags;
    (void)guiid;
    if (!gui || waits_shutting_down)
        return 0;

    ENTER("gui=%p, flags=%d, title=%s, b1=%s, b2=%s, b3=%s", gui, flags,
          title ? title : "(null)", b1 ? b1 : "(null)", b2 ? b2 : "(null)",
          b3 ? b3 : "(null)");

    window = gwen_message_window_new (gui->parent ? GTK_WINDOW (gui->parent) : NULL,
                                      title, text, &actions);
    /* gtk_window_new() is transfer-none: GTK's toplevel list is the initial
     * owner. Keep the callback's own reference so an external destroy cannot
     * finalize the window before this Gwen ABI frame has unwound. */
    g_object_ref (window);
    gtk_window_set_destroy_with_parent (window, TRUE);
    for (guint index = 0; index < G_N_ELEMENTS (labels); index++)
    {
        GtkButton *button;

        if (!labels[index])
            continue;
        button = GTK_BUTTON (gtk_button_new_with_mnemonic (labels[index]));
        gtk_box_append (actions, GTK_WIDGET (button));
        buttons[n_buttons].button = button;
        buttons[n_buttons].response = index + 1;
        n_buttons++;
        if (n_buttons == 1)
            gtk_window_set_default_widget (window, GTK_WIDGET (button));
    }

    result = n_buttons ? wait_for_window_response (window, buttons, n_buttons) : 0;
    gtk_window_destroy (window);
    g_object_unref (window);
    if (result == GTK_RESPONSE_DELETE_EVENT)
        result = 0;
    else if (result < 1 || result > 3)
    {
        g_warning("messagebox_cb: Bad result %d", result);
        result = 0;
    }

    LEAVE("result=%d", result);
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

    if (waits_shutting_down)
        return -1;

    ENTER("gui=%p, flags=%d", gui, flags);

    get_input(gui, flags, title, text, NULL, NULL, 0, &input, min_len, max_len);

    if (input)
    {
        /* Copy the input to the result buffer */
        strncpy(buffer, input, max_len);
        buffer[max_len-1] = '\0';
    }

    LEAVE(" ");
    return input ? 0 : -1;
}

static void
showbox_close (GncGWENGui *gui, GtkWindow *window)
{
    guint32 showbox_id = GPOINTER_TO_UINT (g_object_get_data (
        G_OBJECT (window), "gnc-gwen-showbox-id"));

    if (!gui || !gui->showbox_hash ||
        !g_hash_table_remove (gui->showbox_hash,
                              GUINT_TO_POINTER (showbox_id)))
        gtk_window_destroy (window);
    if (gui && gui->showbox_last_id == showbox_id)
    {
        gui->showbox_last = NULL;
        gui->showbox_last_id = 0;
    }
}

static gboolean
showbox_close_request_cb (GtkWindow *window, gpointer user_data)
{
    showbox_close (user_data, window);
    return TRUE;
}

static void
showbox_close_clicked_cb (GtkButton *button, gpointer user_data)
{
    GtkRoot *root = gtk_widget_get_root (GTK_WIDGET (button));

    if (GTK_IS_WINDOW (root))
        showbox_close (user_data, GTK_WINDOW (root));
}

static guint32 GNC_GWENHYWFAR_CB
showbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
           const gchar *text, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkBox *actions;
    GtkButton *close_button;
    GtkWindow *window;
    guint32 showbox_id;

    (void)flags;
    (void)guiid;
    g_return_val_if_fail(gui, -1);

    if (waits_shutting_down)
        return 0;

    ENTER("gui=%p, flags=%d, title=%s", gui, flags, title ? title : "(null)");

    window = gwen_message_window_new (gui->parent ? GTK_WINDOW (gui->parent) : NULL,
                                      title, text, &actions);
    close_button = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_OK")));
    gtk_box_append (actions, GTK_WIDGET (close_button));

    showbox_id = gui->showbox_id++;
    g_hash_table_insert(gui->showbox_hash, GUINT_TO_POINTER(showbox_id), window);
    gui->showbox_last = GTK_WIDGET (window);
    gui->showbox_last_id = showbox_id;
    g_object_set_data(G_OBJECT(window), "gnc-gwen-showbox-id",
                      GUINT_TO_POINTER(showbox_id));
    g_signal_connect (window, "close-request",
                      G_CALLBACK (showbox_close_request_cb), gui);
    g_signal_connect (close_button, "clicked",
                      G_CALLBACK (showbox_close_clicked_cb), gui);
    gtk_window_present (window);

    /* Give it a chance to be shown. */
    if (!keep_alive(gui))
        showbox_id = 0;

    LEAVE("id=%" G_GUINT32_FORMAT, showbox_id);
    return showbox_id;
}

static void GNC_GWENHYWFAR_CB
hidebox_cb(GWEN_GUI *gwen_gui, guint32 id)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);

    if (waits_shutting_down)
        return;

    g_return_if_fail(gui && gui->showbox_hash);

    ENTER("gui=%p, id=%d", gui, id);

    if (id == 0)
    {
        if (gui->showbox_last)
        {
            g_hash_table_remove(gui->showbox_hash,
                                GUINT_TO_POINTER(gui->showbox_last_id));
            gui->showbox_last = NULL;
            gui->showbox_last_id = 0;
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
            {
                gui->showbox_last = NULL;
                gui->showbox_last_id = 0;
            }
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

    g_return_val_if_fail(gui, -1);

    if (waits_shutting_down)
        return 0;

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

    g_return_val_if_fail(gui, -1);

    if (waits_shutting_down)
        return 1;

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

    g_return_val_if_fail(gui, -1);

    if (waits_shutting_down)
        return 1;

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

    g_return_val_if_fail(gui, -1);
    if (waits_shutting_down)
        return 0;

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

    if (waits_shutting_down)
        return -1;

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

    /* Check remembered passwords, excluding TANs */
    if (!is_tan && gui->cache_passwords && gui->passwords && token)
    {
        if (flags & GWEN_GUI_INPUT_FLAGS_RETRY)
        {
            /* If remembered, remove password from memory */
            g_hash_table_remove(gui->passwords, token);
        }
        else
        {
            gpointer p_var;
            if (g_hash_table_lookup_extended(gui->passwords, token, NULL,
                                             &p_var))
            {
                /* Copy the password to the result buffer */
                password = p_var;
                strncpy(buffer, password, max_len);
                buffer[max_len-1] = '\0';

                LEAVE("chose remembered password");
                return 0;
            }
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
            if (gui->cache_passwords && gui->passwords)
            {
                /* Remember password */
                DEBUG("Remember password, token=%s", token);
                g_hash_table_insert(gui->passwords, g_strdup(token), password);
            }
            else
            {
                /* Remove the password from memory */
                DEBUG("Forget password, token=%s", token);
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

    g_return_val_if_fail(gui && gui->accepted_certs, -1);

    if (waits_shutting_down)
        return -1;

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
ggg_delete_event_cb(GtkWindow *window, gpointer user_data)
{
    GncGWENGui *gui = user_data;

    (void)window;
    g_return_val_if_fail(gui, FALSE);

    ENTER("gui=%p, state=%d", gui, gui->state);

    if (gui->state == RUNNING)
    {
        const char *still_running_msg =
            _("The Online Banking job is still running; are you "
              "sure you want to cancel?");
        if (!gwen_confirm (GTK_WINDOW (gui->dialog), still_running_msg, FALSE))
            return TRUE;

        set_aborted(gui);
    }

    hide_dialog(gui);

    LEAVE(" ");
    return TRUE;
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
ggg_close_toggled_cb(GtkCheckButton *button, gpointer user_data)
{
    GncGWENGui *gui = user_data;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    gnc_prefs_set_bool(
        GNC_PREFS_GROUP_AQBANKING, GNC_PREF_CLOSE_ON_FINISH,
        gtk_check_button_get_active(button));

    LEAVE(" ");
}

#ifdef GNC_GWEN_GUI_TESTING
void
gnc_gwen_gui_test_take_permanent_certs (GWEN_DB_NODE *certs)
{
    g_return_if_fail (certs);
    g_return_if_fail (!full_gui);
    g_return_if_fail (!test_permanently_accepted_certs);

    test_permanently_accepted_certs = certs;
}

void
gnc_gwen_gui_test_get_state (guint *init_wrapper_calls,
                              guint *fini_wrapper_calls,
                              guint *raw_init_calls,
                              guint *raw_fini_calls,
                              guint *library_clients,
                              guint *abi_frames,
                              guint *wait_count,
                              guint *finalize_source_calls,
                              guint *finalize_destroy_calls,
                              guint *gui_finalize_calls,
                              guint *component_register_calls,
                              guint *component_unregister_calls,
                              guint *finalize_source_id,
                              guint *application_barrier_connect_calls,
                              guint *application_barrier_disconnect_calls,
                              gulong *application_barrier_handler_id)
{
    *init_wrapper_calls = test_init_wrapper_calls;
    *fini_wrapper_calls = test_fini_wrapper_calls;
    *raw_init_calls = test_raw_init_calls;
    *raw_fini_calls = test_raw_fini_calls;
    *library_clients = gwen_library_clients;
    *abi_frames = active_abi_frames;
    *wait_count = g_list_length (active_waits);
    *finalize_source_calls = test_finalize_source_calls;
    *finalize_destroy_calls = test_finalize_destroy_calls;
    *gui_finalize_calls = test_gui_finalize_calls;
    *component_register_calls = test_component_register_calls;
    *component_unregister_calls = test_component_unregister_calls;
    *finalize_source_id = shutdown_finalize_source_id;
    *application_barrier_connect_calls = test_application_barrier_connect_calls;
    *application_barrier_disconnect_calls =
        test_application_barrier_disconnect_calls;
    *application_barrier_handler_id = shutdown_application_handler_id;
}
#endif
