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

#include "config.h"

#include <ctype.h>
#include <glib/gi18n.h>
#include <gwenhywfar/gui_be.h>
#include <gwenhywfar/inherit.h>

#include "dialog-utils.h"
#include "gnc-ab-utils.h"
#include "gnc-component-manager.h"
#include "gnc-gconf-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "md5.h"
#include "qof.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/* A unique full-blown GUI, featuring  */
static GncGWENGui *full_gui = NULL;

/* A unique Gwenhywfar GUI for hooking our logging into the gwenhywfar logging
 * framework */
static GWEN_GUI *log_gwen_gui = NULL;

/* A mapping from gwenhywfar log levels to glib ones */
static GLogLevelFlags log_levels[] = {
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

#define GWEN_GUI_CM_CLASS "dialog-hbcilog"
#define GCONF_SECTION_CONNECTION GCONF_SECTION_AQBANKING "/connection_dialog"
#define KEY_CLOSE_ON_FINISH "close_on_finish"
#define KEY_REMEMBER_PIN "remember_pin"

#define OTHER_ENTRIES_ROW_OFFSET 3

typedef struct _Progress Progress;
typedef enum _GuiState GuiState;

static void register_callbacks(GncGWENGui *gui);
static void unregister_callbacks(GncGWENGui *gui);
static void setup_dialog(GncGWENGui *gui);
static void enable_password_cache(GncGWENGui *gui, gboolean enabled);
static void reset_dialog(GncGWENGui *gui);
static void set_runing(GncGWENGui *gui);
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
static void erase_password(gchar *password);
static gchar *strip_html(gchar *text);
static void get_input(GncGWENGui *gui, guint32 flags, const gchar *title,
                      const gchar *text, gchar **input, gint min_len,
                      gint max_len);
static gint messagebox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                          const gchar *text, const gchar *b1,const gchar *b2,
                          const gchar *b3, guint32 guiid);
static gint inputbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                        const gchar *text, gchar *buffer, gint min_len,
                        gint max_len, guint32 guiid);
static guint32 showbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
                          const gchar *text, guint32 guiid);
static void hidebox_cb(GWEN_GUI *gwen_gui, guint32 id);
static guint32 progress_start_cb(GWEN_GUI *gwen_gui, guint32 progressFlags,
                                 const gchar *title, const gchar *text,
                                 guint64 total, guint32 guiid);
static gint progress_advance_cb(GWEN_GUI *gwen_gui, guint32 id,
                                guint64 new_progress);
static gint progress_log_cb(GWEN_GUI *gwen_gui, guint32 id,
                            GWEN_LOGGER_LEVEL level, const gchar *text);
static gint progress_end_cb(GWEN_GUI *gwen_gui, guint32 id);
static gint getpassword_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *token,
                           const gchar *title, const gchar *text, gchar *buffer,
                           gint min_len, gint max_len, guint32 guiid);
static gint setpasswordstatus_cb(GWEN_GUI *gwen_gui, const gchar *token,
                                 const gchar *pin,
                                 GWEN_GUI_PASSWORD_STATUS status, guint32 guiid);
static gint loghook_cb(GWEN_GUI *gwen_gui, const gchar *log_domain,
                       GWEN_LOGGER_LEVEL priority, const gchar *text);
static gint checkcert_cb(GWEN_GUI *gwen_gui, const GWEN_SSLCERTDESCR *cert,
                         GWEN_IO_LAYER *io, guint32 guiid);

gboolean ggg_delete_event_cb(GtkWidget *widget, GdkEvent *event,
                             gpointer user_data);
void ggg_abort_clicked_cb(GtkButton *button, gpointer user_data);
void ggg_close_clicked_cb(GtkButton *button, gpointer user_data);

enum _GuiState {
  INIT,
  RUNNING,
  FINISHED,
  ABORTED,
  HIDDEN
};

struct _GncGWENGui {
    GWEN_GUI *gwen_gui;
    GtkWidget *parent;
    GtkWidget *dialog;

    /* Progress bars */
    GtkWidget *entries_table;
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

    /* Password caching */
    gboolean cache_passwords;
    GHashTable *passwords;

    /* Certificates handling */
    GHashTable *accepted_certs;
    GWEN_GUI_CHECKCERT_FN builtin_checkcert;

    /* Dialogs */
    guint showbox_id;
    GHashTable *showbox_hash;
    GtkWidget *showbox_last;

    /* Cache the lowest loglevel, corresponding to the most serious warning */
    GWEN_LOGGER_LEVEL min_loglevel;
};

struct _Progress {
    GncGWENGui *gui;

    /* Title of the process */
    gchar *title;

    /* Event source id for showing delayed */
    guint source;
};

void
gnc_GWEN_Gui_log_init(void)
{
    if (!log_gwen_gui) {
        log_gwen_gui = GWEN_Gui_new();

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

    if (full_gui) {
        if (full_gui->state == INIT || full_gui->state == RUNNING) {
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
gnc_GWEN_Gui_shutdown(void)
{
    GncGWENGui *gui = full_gui;

    ENTER(" ");

    if (log_gwen_gui) {
        GWEN_Gui_free(log_gwen_gui);
        log_gwen_gui = NULL;
    }
    GWEN_Gui_SetGui(NULL);

    if (!gui)
        return;

    gui->parent = NULL;
    reset_dialog(gui);
    if (gui->passwords)
        g_hash_table_destroy(gui->passwords);
    if (gui->showbox_hash)
        g_hash_table_destroy(gui->showbox_hash);
    if (gui->accepted_certs)
        g_hash_table_destroy(gui->accepted_certs);
    gtk_widget_destroy(gui->dialog);
    g_free(gui);

    full_gui = NULL;

    LEAVE(" ");
}

static void
register_callbacks(GncGWENGui *gui)
{
    GWEN_GUI *gwen_gui;

    g_return_if_fail(gui && !gui->gwen_gui);

    ENTER("gui=%p", gui);

    gui->gwen_gui = gwen_gui = GWEN_Gui_new();

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

    GWEN_Gui_SetGui(gwen_gui);
    SETDATA_GUI(gwen_gui, gui);

    LEAVE(" ");
}

static void
unregister_callbacks(GncGWENGui *gui)
{
    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    if (!gui->gwen_gui) {
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
    GladeXML *xml;
    gint component_id;

    g_return_if_fail(gui);

    ENTER("gui=%p", gui);

    xml = gnc_glade_xml_new("aqbanking.glade", "Connection Dialog");

    gui->dialog = glade_xml_get_widget(xml, "Connection Dialog");
    g_object_set_data_full(G_OBJECT(gui->dialog), "xml", xml, g_object_unref);
    glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, gui);
    gui->entries_table = glade_xml_get_widget(xml, "entries_table");
    gui->top_entry = glade_xml_get_widget(xml, "top_entry");
    gui->top_progress = glade_xml_get_widget(xml, "top_progress");
    gui->second_entry = glade_xml_get_widget(xml, "second_entry");
    gui->other_entries_box = NULL;
    gui->progresses = NULL;
    gui->log_text = glade_xml_get_widget(xml, "log_text");
    gui->abort_button = glade_xml_get_widget(xml, "abort_button");
    gui->close_button = glade_xml_get_widget(xml, "close_button");
    gui->close_checkbutton = glade_xml_get_widget(xml, "close_checkbutton");
    gui->accepted_certs = NULL;
    gui->showbox_hash = NULL;
    gui->showbox_id = 1;

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(gui->close_checkbutton),
        gnc_gconf_get_bool(GCONF_SECTION_AQBANKING, KEY_CLOSE_ON_FINISH, NULL));

    component_id = gnc_register_gui_component(GWEN_GUI_CM_CLASS, NULL,
                                              cm_close_handler, gui);
    gnc_gui_component_set_session(component_id, gnc_get_current_session());

    reset_dialog(gui);

    LEAVE(" ");
}

static void
enable_password_cache(GncGWENGui *gui, gboolean enabled)
{
    g_return_if_fail(gui);

    if (enabled && !gui->passwords) {
        /* Remember passwords in memory, mapping tokens to passwords */
        gui->passwords = g_hash_table_new_full(
            g_str_hash, g_str_equal, (GDestroyNotify) g_free,
            (GDestroyNotify) erase_password);
    } else if (!enabled && gui->passwords) {
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

    gtk_entry_set_text(GTK_ENTRY(gui->top_entry), "");
    gtk_entry_set_text(GTK_ENTRY(gui->second_entry), "");
    g_list_foreach(gui->progresses, (GFunc) free_progress, NULL);
    g_list_free(gui->progresses);
    gui->progresses = NULL;

    if (gui->other_entries_box) {
        gtk_table_resize(GTK_TABLE(gui->entries_table),
                         OTHER_ENTRIES_ROW_OFFSET, 2);
        gtk_widget_destroy(gui->other_entries_box);
        gui->other_entries_box = NULL;
    }
    if (gui->showbox_hash)
        g_hash_table_destroy(gui->showbox_hash);
    gui->showbox_last = NULL;
    gui->showbox_hash = g_hash_table_new_full(
        NULL, NULL, NULL, (GDestroyNotify) gtk_widget_destroy);

    if (gui->parent)
        gtk_window_set_transient_for(GTK_WINDOW(gui->dialog),
                                     GTK_WINDOW(gui->parent));
    gnc_restore_window_size(GCONF_SECTION_CONNECTION, GTK_WINDOW(gui->dialog));

    gui->keep_alive = TRUE;
    gui->state = INIT;
    gui->min_loglevel = GWEN_LoggerLevel_Verbous;

    cache_passwords = gnc_gconf_get_bool(GCONF_SECTION_AQBANKING,
                                         KEY_REMEMBER_PIN, NULL);
    enable_password_cache(gui, cache_passwords);

    if (!gui->accepted_certs)
        gui->accepted_certs = g_hash_table_new_full(
            g_str_hash, g_str_equal, (GDestroyNotify) g_free, NULL);

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
    gboolean cache_pin;

    g_return_if_fail(gui);

    ENTER("gui=%p, clear_log=%d", gui, clear_log);

    gtk_widget_show(gui->dialog);

    /* Clear the log window */
    if (clear_log) {
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

    /* Remember whether the dialog is to be closed when finished */
    gnc_gconf_set_bool(
        GCONF_SECTION_AQBANKING, KEY_CLOSE_ON_FINISH,
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gui->close_checkbutton)),
        NULL);

    /* Remember size and position of the dialog */
    gnc_save_window_size(GCONF_SECTION_CONNECTION, GTK_WINDOW(gui->dialog));

    /* Do not serve as GUI anymore */
    gui->state = HIDDEN;
    unregister_callbacks(gui);

    LEAVE(" ");
}

static gboolean
show_progress_cb(gpointer user_data)
{
    Progress *progress = user_data;
    GncGWENGui *gui;
    GList *item;

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

    for (item = g_list_last(gui->progresses); item; item = item->prev) {
        current = (Progress*) item->data;

        if (!current->source
            && current != progress)
            /* Already showed */
            continue;

        /* Show it */
        if (!item->next) {
            /* Top-level progress */
            show_dialog(gui, TRUE);
            gtk_entry_set_text(GTK_ENTRY(gui->top_entry), current->title);
        } else if (!item->next->next) {
            /* Second-level progress */
            gtk_entry_set_text(GTK_ENTRY(gui->second_entry), current->title);
        } else {
            /* Other progress */
            GtkWidget *entry = gtk_entry_new();
            GtkWidget *box = gui->other_entries_box;
            gboolean new_box = box == NULL;

            gtk_entry_set_text(GTK_ENTRY(entry), current->title);
            if (new_box)
                gui->other_entries_box = box = gtk_vbox_new(TRUE, 6);
            gtk_box_pack_start_defaults(GTK_BOX(box), entry);
            gtk_widget_show(entry);
            if (new_box) {
                gtk_table_resize(GTK_TABLE(gui->entries_table),
                                 OTHER_ENTRIES_ROW_OFFSET + 1, 2);
                gtk_table_attach_defaults(
                    GTK_TABLE(gui->entries_table), box, 1, 2,
                    OTHER_ENTRIES_ROW_OFFSET, OTHER_ENTRIES_ROW_OFFSET + 1);
                gtk_widget_show(box);
            }
        }

        if (current->source) {
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

    for (item = gui->progresses; item; item = item->next) {
        current = (Progress*) item->data;

        if (current->source) {
            /* Not yet showed */
            g_source_remove(current->source);
            current->source = 0;
            if (current == progress)
                break;
            else
                continue;
        }

        /* Hide it */
        if (!item->next) {
            /* Top-level progress */
            gtk_entry_set_text(GTK_ENTRY(gui->second_entry), "");
        } else if (!item->next->next) {
            /* Second-level progress */
            gtk_entry_set_text(GTK_ENTRY(gui->second_entry), "");
        } else {
            /* Other progress */
            GtkWidget *box = gui->other_entries_box;
            GList *entries;

            g_return_if_fail(box);
            entries = gtk_container_get_children(GTK_CONTAINER(box));
            g_return_if_fail(entries);
            if (entries->next) {
                /* Another progress is still to be showed */
                gtk_widget_destroy(GTK_WIDGET(g_list_last(entries)->data));
            } else {
                /* Last other progress to be hided */
                gtk_table_resize(GTK_TABLE(gui->entries_table),
                                 OTHER_ENTRIES_ROW_OFFSET, 2);
                gtk_widget_destroy(box);
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

    /* Let the widgets be redrawn */
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
    while (strchr(p, '<')) {
        q = p + 1;
        if (*q && toupper(*q++) == 'H'
            && *q && toupper(*q++) == 'T'
            && *q && toupper(*q++) == 'M'
            && *q && toupper(*q) == 'L') {
            *p = '\0';
            return text;
        }
        p++;
    }
    return text;
}

static void
get_input(GncGWENGui *gui, guint32 flags, const gchar *title, const gchar *text,
          gchar **input, gint min_len, gint max_len)
{
    GladeXML *xml;
    GtkWidget *dialog;
    GtkWidget *heading_label;
    GtkWidget *input_entry;
    GtkWidget *confirm_entry;
    GtkWidget *confirm_label;
    GtkWidget *remember_pin_checkbutton;
    const gchar *internal_input, *internal_confirmed;
    gboolean confirm = (flags & GWEN_GUI_INPUT_FLAGS_CONFIRM) != 0;
    gboolean hidden = (flags & GWEN_GUI_INPUT_FLAGS_SHOW) == 0;
    gint retval;

    g_return_if_fail(input);
    g_return_if_fail(max_len >= min_len && max_len > 0);

    ENTER(" ");

    /* Set up dialog */
    xml = gnc_glade_xml_new("aqbanking.glade", "Password Dialog");
    dialog = glade_xml_get_widget(xml, "Password Dialog");
    g_object_set_data_full(G_OBJECT(dialog), "xml", xml, g_object_unref);

    heading_label = glade_xml_get_widget(xml, "heading_label");
    input_entry = glade_xml_get_widget(xml, "input_entry");
    confirm_entry = glade_xml_get_widget(xml, "confirm_entry");
    confirm_label = glade_xml_get_widget(xml, "confirm_label");
    remember_pin_checkbutton = glade_xml_get_widget(xml, "remember_pin");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(remember_pin_checkbutton),
                                 gui->cache_passwords);

    if (gui->parent)
        gtk_window_set_transient_for(GTK_WINDOW(dialog),
                                     GTK_WINDOW(gui->parent));
    if (title)
        gtk_window_set_title(GTK_WINDOW(dialog), title);

    if (text) {
        gchar *raw_text = strip_html(g_strdup(text));
        gtk_label_set_text(GTK_LABEL(heading_label), raw_text);
        g_free(raw_text);
    }

    if (*input) {
        gtk_entry_set_text(GTK_ENTRY(input_entry), *input);
        erase_password(*input);
        *input = NULL;
    }

    if (confirm) {
        gtk_entry_set_activates_default(GTK_ENTRY(input_entry), FALSE);
        gtk_entry_set_activates_default(GTK_ENTRY(confirm_entry), TRUE);
        gtk_entry_set_max_length(GTK_ENTRY(input_entry), max_len);
        gtk_entry_set_max_length(GTK_ENTRY(confirm_entry), max_len);
    } else {
        gtk_entry_set_activates_default(GTK_ENTRY(input_entry), TRUE);
        gtk_entry_set_max_length(GTK_ENTRY(input_entry), max_len);
        gtk_widget_hide(confirm_entry);
        gtk_widget_hide(confirm_label);
    }
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);

    /* Ask the user until he enters a valid input or cancels */
    while (TRUE) {
        gboolean remember_pin;

        if (gtk_dialog_run(GTK_DIALOG(dialog)) != GTK_RESPONSE_OK)
            break;

        /* Enable or disable the password cache */
        remember_pin = gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(remember_pin_checkbutton));
        enable_password_cache(gui, remember_pin);
        gnc_gconf_set_bool(GCONF_SECTION_AQBANKING, KEY_REMEMBER_PIN,
                           remember_pin, NULL);

        internal_input = gtk_entry_get_text(GTK_ENTRY(input_entry));
        if (strlen(internal_input) < min_len) {
            gboolean retval;
            gchar *msg = g_strdup_printf(
                _("The PIN needs to be at least %d characters \n"
                  "long. Do you want to try again?"), min_len);
            retval = gnc_verify_dialog(gui->parent, TRUE, "%s", msg);
            g_free(msg);
            if (!retval)
                break;
            continue;
        }

        if (!confirm) {
            *input = g_strdup(internal_input);
            break;
        }

        internal_confirmed = gtk_entry_get_text(GTK_ENTRY(confirm_entry));
        if (strcmp(internal_input, internal_confirmed) == 0) {
            *input = g_strdup(internal_input);
            break;
        }
    }

    /* This trashes passwords in the entries' memory as well */
    gtk_widget_destroy(dialog);

    LEAVE("input %s", *input ? "non-NULL" : "NULL");
}

static gint
messagebox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
              const gchar *text, const gchar *b1,const gchar *b2,
              const gchar *b3, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkWidget *dialog;
    GtkWidget *vbox;
    GtkWidget *label;
    gchar *raw_text;
    gint result;

    ENTER("gui=%p, flags=%d, title=%s, b1=%s, b2=%s, b3=%s", gui, flags,
          title ? title : "(null)", b1 ? b1 : "(null)", b2 ? b2 : "(null)",
          b3 ? b3 : "(null)");

    dialog = gtk_dialog_new_with_buttons(
        title, gui->parent ? GTK_WINDOW(gui->parent) : NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        b1, 1, b2, 2, b3, 3, (gchar*) NULL);

    raw_text = strip_html(g_strdup(text));
    label = gtk_label_new(raw_text);
    g_free(raw_text);
    gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
    vbox = gtk_vbox_new(TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(vbox), 5);
    gtk_container_add(GTK_CONTAINER(vbox), label);
    gtk_container_set_border_width(GTK_CONTAINER(dialog), 5);
    gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), vbox);
    gtk_widget_show_all(dialog);

    result = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);

    if (result<1 || result>3) {
        g_warning("messagebox_cb: Bad result %d", result);
        result = 0;
    }

    LEAVE("result=%d", result);
    return result;
}

static gint
inputbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
            const gchar *text, gchar *buffer, gint min_len, gint max_len,
            guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    gchar *input = NULL;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d", gui, flags);

    get_input(gui, flags, title, text, &input, min_len, max_len);

    if (input) {
        /* Copy the input to the result buffer */
        strncpy(buffer, input, max_len);
        buffer[max_len-1] = '\0';
    }

    LEAVE(" ");
    return input ? 0 : -1;
}

static guint32
showbox_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *title,
           const gchar *text, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkWidget *dialog;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d, title=%s", gui, flags, title ? title : "(null)");

    dialog = gtk_message_dialog_new(
        gui->parent ? GTK_WINDOW(gui->parent) : NULL, 0, GTK_MESSAGE_INFO,
        GTK_BUTTONS_OK, "%s", text);

    if (title)
        gtk_window_set_title(GTK_WINDOW(dialog), title);

    g_signal_connect(dialog, "response", G_CALLBACK(gtk_widget_hide), NULL);
    gtk_widget_show_all(dialog);

    g_hash_table_insert(gui->showbox_hash, GUINT_TO_POINTER(gui->showbox_id),
                        dialog);
    gui->showbox_id++;
    gui->showbox_last = dialog;

    LEAVE(" ");
    return 0;
}

static void
hidebox_cb(GWEN_GUI *gwen_gui, guint32 id)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkWidget *dialog;

    g_return_if_fail(gui && gui->showbox_hash);

    ENTER("gui=%p, id=%d", gui, id);

    if (id == 0) {
        if (gui->showbox_last) {
            g_hash_table_remove(gui->showbox_hash,
                                GUINT_TO_POINTER(gui->showbox_id));
            gui->showbox_last = NULL;
        } else {
            g_warning("hidebox_cb: Last showed message box already destroyed");
        }
    } else {
        gpointer p_var;
        p_var = g_hash_table_lookup(gui->showbox_hash, GUINT_TO_POINTER(id));
        if (p_var) {
            g_hash_table_remove(gui->showbox_hash, GUINT_TO_POINTER(id));
            if (p_var == gui->showbox_last)
                gui->showbox_last = NULL;
        } else {
            g_warning("hidebox_cb: Message box %d could not been found", id);
        }
    }

    LEAVE(" ");
}

static guint32
progress_start_cb(GWEN_GUI *gwen_gui, guint32 progressFlags, const gchar *title,
                  const gchar *text, guint64 total, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    Progress *progress;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d, title=%s, total=%" G_GUINT64_FORMAT, gui,
          progressFlags, title ? title : "(null)", total);

    if (!gui->progresses) {
        /* Top-level progress */
        if (progressFlags & GWEN_GUI_PROGRESS_SHOW_PROGRESS) {
            gtk_widget_set_sensitive(gui->top_progress, TRUE);
            gtk_progress_bar_set_fraction(
                GTK_PROGRESS_BAR(gui->top_progress), 0.0);
            gui->max_actions = total;
        } else {
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

    if (progressFlags & GWEN_GUI_PROGRESS_DELAY) {
        /* Show progress later */
        progress->source = g_timeout_add(GWEN_GUI_DELAY_SECS * 1000,
                                         (GSourceFunc) show_progress_cb,
                                         progress);
    } else {
        /* Show it now */
        progress->source = 0;
        show_progress(gui, progress);
    }

    LEAVE(" ");
    return g_list_length(gui->progresses);
}

static gint
progress_advance_cb(GWEN_GUI *gwen_gui, guint32 id, guint64 progress)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, progress=%" G_GUINT64_FORMAT, gui, progress);

    if (id == 1                                  /* top-level progress */
        && gui->max_actions > 0                  /* progressbar active */
        && progress != GWEN_GUI_PROGRESS_NONE) { /* progressbar update needed */
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

static gint
progress_log_cb(GWEN_GUI *gwen_gui, guint32 id, GWEN_LOGGER_LEVEL level,
                const gchar *text)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    GtkTextBuffer *tb;
    GtkTextView *tv;

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

static gint
progress_end_cb(GWEN_GUI *gwen_gui, guint32 id)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    Progress *progress;

    g_return_val_if_fail(gui, -1);
    g_return_val_if_fail(id == g_list_length(gui->progresses), -1);

    ENTER("gui=%p, id=%d", gui, id);

    if (gui->state != RUNNING) {
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

    if (!gui->progresses) {
        /* top-level progress finished */
        set_finished(gui);
    }

    LEAVE(" ");
    return 0;
}

static gint
getpassword_cb(GWEN_GUI *gwen_gui, guint32 flags, const gchar *token,
               const gchar *title, const gchar *text, gchar *buffer,
               gint min_len, gint max_len, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    gchar *password = NULL;

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, flags=%d, token=%s", gui, flags, token ? token : "(null");

    /* Check remembered passwords */
    if (gui->cache_passwords && gui->passwords && token) {
        if (flags & GWEN_GUI_INPUT_FLAGS_RETRY) {
            /* If remembered, remove password from memory */
            g_hash_table_remove(gui->passwords, token);
        } else {
            gpointer p_var;
            if (g_hash_table_lookup_extended(gui->passwords, token, NULL,
                                             &p_var)) {
                /* Copy the password to the result buffer */
                password = p_var;
                strncpy(buffer, password, max_len);
                buffer[max_len-1] = '\0';

                LEAVE("chose remembered password");
                return 0;
            }
        }
    }

    get_input(gui, flags, title, text, &password, min_len, max_len);

    if (password) {
        /* Copy the password to the result buffer */
        strncpy(buffer, password, max_len);
        buffer[max_len-1] = '\0';

        if (token) {
            if (gui->cache_passwords && gui->passwords) {
                /* Remember password */
                DEBUG("Remember password, token=%s", token);
                g_hash_table_insert(gui->passwords, g_strdup(token), password);
            } else {
                /* Remove the password from memory */
                DEBUG("Forget password, token=%s", token);
                erase_password(password);
            }
        }
    }

    LEAVE(" ");
    return password ? 0 : -1;
}

static gint
setpasswordstatus_cb(GWEN_GUI *gwen_gui, const gchar *token, const gchar *pin,
                     GWEN_GUI_PASSWORD_STATUS status, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);

    g_return_val_if_fail(gui, -1);

    ENTER("gui=%p, token=%s, status=%d", gui, token ? token : "(null)", status);

    if (gui->passwords && status != GWEN_Gui_PasswordStatus_Ok) {
        /* If remembered, remove password from memory */
        g_hash_table_remove(gui->passwords, token);
    }

    LEAVE(" ");
    return 0;
}

static gint
loghook_cb(GWEN_GUI *gwen_gui, const gchar *log_domain,
           GWEN_LOGGER_LEVEL priority, const gchar *text)
{
    if (G_LIKELY(priority < n_log_levels))
        g_log(log_domain, log_levels[priority], "%s", text);

    return 1;
}

static gint
checkcert_cb(GWEN_GUI *gwen_gui, const GWEN_SSLCERTDESCR *cert,
             GWEN_IO_LAYER *io, guint32 guiid)
{
    GncGWENGui *gui = GETDATA_GUI(gwen_gui);
    const gchar *hash, *status;
    struct md5_ctx md5_context;
    gchar cert_hash[16];
    gint retval;

    g_return_val_if_fail(gui && gui->accepted_certs, -1);

    ENTER("gui=%p, cert=%p", gui, cert);

    hash = GWEN_SslCertDescr_GetFingerPrint(cert);
    status = GWEN_SslCertDescr_GetStatusText(cert);

    /* Operate on an md5sum of the pair of hash and status */
    md5_init_ctx(&md5_context);
    md5_process_bytes(hash, strlen(hash), &md5_context);
    md5_process_bytes(status, strlen(status), &md5_context);
    md5_finish_ctx(&md5_context, cert_hash);

    if (g_hash_table_lookup(gui->accepted_certs, cert_hash)) {
        /* Certificate has been accepted before */
        LEAVE("Automatically accepting certificate");
        return 0;
    }

    retval = gui->builtin_checkcert(gwen_gui, cert, io, guiid);
    if (retval == 0) {
        /* Certificate has been accepted */
        g_hash_table_insert(gui->accepted_certs, g_strdup(cert_hash), cert_hash);
    }

    LEAVE("retval=%d", retval);
    return retval;
}

gboolean
ggg_delete_event_cb(GtkWidget *widget, GdkEvent *event, gpointer user_data){
    GncGWENGui *gui = user_data;

    g_return_val_if_fail(gui, FALSE);

    ENTER("gui=%p, state=%d", gui, gui->state);

    if (gui->state == RUNNING) {
        const char *still_running_msg =
            _("The Online Banking job is still running; are you "
              "sure you want to cancel?");
        if (!gnc_verify_dialog(gui->dialog, FALSE, "%s", still_running_msg))
            return FALSE;

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
    g_return_if_fail(gui->state == FINISHED || gui->state == ABORTED);

    ENTER("gui=%p", gui);

    hide_dialog(gui);

    LEAVE(" ");
}
