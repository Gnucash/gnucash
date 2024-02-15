/********************************************************************
 * gnc-html-litehtml.c -- gnucash report renderer using litehtml    *
 * Copyright (C) 2024 Bob Fewell                                    *
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
 ********************************************************************/

#include <config.h>

#include <platform.h>
#ifdef __MINGW32__
#define _GL_UNISTD_H //Deflect poisonous define of close in Guile's GnuLib
#endif
#include <libguile.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <gio/gio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <regex.h>

#include "Account.h"
#include "gnc-prefs.h"
#include "gnc-gui-query.h"
#include "gnc-engine.h"
#include "gnc-html.h"
#include "gnc-html-litehtml.h"
#include "gnc-html-history.h"
#include "print-session.h"


G_DEFINE_TYPE(GncHtmlLitehtml, gnc_html_litehtml, GNC_TYPE_HTML)

static void gnc_html_litehtml_dispose (GObject* obj);
static void gnc_html_litehtml_finalize (GObject* obj);

#define GNC_HTML_LITEHTML_GET_PRIVATE(o) (GNC_HTML_LITEHTML(o)->priv)

#include "gnc-html-litehtml-p.h"

/* indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_HTML;

/* hashes an HTML <object classid="ID"> classid to a handler function */
extern GHashTable* gnc_html_object_handlers;

/* hashes handlers for loading different URLType data */
extern GHashTable* gnc_html_stream_handlers;

/* hashes handlers for handling different URLType data */
extern GHashTable* gnc_html_url_handlers;

static char error_404_format[] = "<html><body><h3>%s</h3><p>%s</body></html>";
static char error_404_title[] = N_("Not found");
static char error_404_body[] = N_("The specified URL could not be loaded.");

#define BASE_URI_NAME "base-uri"
#define GNC_PREF_RPT_DFLT_ZOOM "default-zoom"

static gchar* handle_embedded_object (GncHtmlLitehtml* self, gchar* html_str);
static void impl_litehtml_show_url (GncHtml* self, URLType type,
                                    const gchar* location, const gchar* label,
                                    gboolean new_window_hint);
static void impl_litehtml_show_data (GncHtml* self, const gchar* data, int datalen);
static void impl_litehtml_reload (GncHtml* self, gboolean force_rebuild);
static void impl_litehtml_copy_to_clipboard (GncHtml* self);
static gboolean impl_litehtml_export_to_file (GncHtml* self, const gchar* filepath);
static void impl_litehtml_print (GncHtml* self,const gchar* jobname);
static void impl_litehtml_cancel (GncHtml* self);
static void impl_litehtml_set_parent (GncHtml* self, GtkWindow* parent);

static void gnc_html_open_scm (GncHtmlLitehtml* self, const gchar * location,
                               const gchar * label, int newwin);

static char *extract_base_name (URLType type, const gchar* path);

static gboolean load_to_stream (GncHtmlLitehtml* self, URLType type,
                                const gchar* location, const gchar* label);


static gboolean
button_release_event (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    GncHtmlLitehtml *ghlh = user_data;
    GncHtmlLitehtmlPrivate *priv = GNC_HTML_LITEHTML_GET_PRIVATE(ghlh);

g_print("%s called, self %p\n",__FUNCTION__, ghlh);

    const gchar *anchor = gnc_html_litehtml_get_anchor (priv->html_wrapped_widget, event);

    if (!anchor)
        return FALSE;

    GncHtml *self = (GncHtml*)ghlh;
    GncHTMLUrlCB url_handler;

    gchar *location = NULL, *label = NULL, *uri = NULL;
    const gchar *scheme = gnc_html_parse_url (self, anchor, &location, &label);
    URLType type = scheme;
    gboolean stream_loaded = FALSE;

    if (gnc_html_url_handlers)
        url_handler = (GncHTMLUrlCB)g_hash_table_lookup ((GHashTable*)gnc_html_url_handlers, type);
    else
        url_handler = NULL;

    if (url_handler)
    {
        GNCURLResult result;
        gboolean ok;

        result.load_to_stream = FALSE;
        result.url_type = type;
        result.location = NULL;
        result.label = NULL;
        result.base_type = URL_TYPE_FILE;
        result.base_location = NULL;
        result.error_message = NULL;
        result.parent = GTK_WINDOW(gnc_html_get_widget(self));
        result.parent = NULL;

        ok = url_handler (location, label, FALSE, &result);

        if (!ok)
        {
            if (result.error_message)
                gnc_error_dialog (GTK_WINDOW(priv->base.parent), "%s", result.error_message);
            else
            {
                /* %s is a URL (some location somewhere). */
                gnc_error_dialog (GTK_WINDOW(priv->base.parent),
                                  _("There was an error accessing %s."), location );
            }

            if (priv->base.load_cb)
            {
                priv->base.load_cb (GNC_HTML(self), result.url_type,
                                    location, label, priv->base.load_cb_data);
            }
        }
        else if (result.load_to_stream)
        {
            gnc_html_history_node *hnode;
            const char *new_location;
            const char *new_label;

            new_location = result.location ? result.location : location;
            new_label = result.label ? result.label : label;
            hnode = gnc_html_history_node_new (result.url_type, new_location, new_label);
            gnc_html_history_append (priv->base.history, hnode);

            g_free (priv->base.base_location);
            priv->base.base_type = result.base_type;
            priv->base.base_location = g_strdup (extract_base_name (result.base_type, new_location));

            DEBUG("resetting base location to %s",
                    priv->base.base_location ? priv->base.base_location : "(null)");

            stream_loaded = load_to_stream (GNC_HTML_LITEHTML(self),
                                            result.url_type,
                                            new_location, new_label);

            if (stream_loaded && priv->base.load_cb != NULL)
            {
                priv->base.load_cb (GNC_HTML(self), result.url_type,
                                    new_location, new_label, priv->base.load_cb_data);
            }
        }
        g_free (result.location);
        g_free (result.label);
        g_free (result.base_location);
        g_free (result.error_message);
    }
    g_free (uri);
    g_free (location);
    g_free (label);

    return TRUE;
}

static void
gnc_html_litehtml_init (GncHtmlLitehtml* self)
{
    GncHtmlLitehtmlPrivate* priv;
    GncHtmlLitehtmlPrivate* new_priv;
g_print("%s called, self %p\n",__FUNCTION__, self);

    ENTER("");

    new_priv = g_realloc (GNC_HTML(self)->priv, sizeof(GncHtmlLitehtmlPrivate));
    priv = self->priv = new_priv;
    GNC_HTML(self)->priv = (GncHtmlPrivate*)priv;

    priv->html_string = NULL;
    priv->html_wrapped_widget = gnc_html_litehtml_widget_new ();

    priv->web_view = gnc_html_litehtml_get_drawing_area (priv->html_wrapped_widget);

    g_signal_connect (G_OBJECT(priv->web_view), "button-release-event",
                      G_CALLBACK(button_release_event), (gpointer)self);

    gtk_container_add (GTK_CONTAINER(priv->base.container),
                       GTK_WIDGET(priv->web_view));

    g_object_ref_sink (priv->base.container);

    LEAVE("retval %p", self);
}

static void
gnc_html_litehtml_class_init (GncHtmlLitehtmlClass* klass)
{
    GObjectClass* gobject_class = G_OBJECT_CLASS(klass);
    GncHtmlClass* html_class = GNC_HTML_CLASS(klass);
g_print("%s called\n",__FUNCTION__);
    gobject_class->dispose = gnc_html_litehtml_dispose;
    gobject_class->finalize = gnc_html_litehtml_finalize;

    html_class->show_url = impl_litehtml_show_url;
    html_class->show_data = impl_litehtml_show_data;
    html_class->reload = impl_litehtml_reload;
    html_class->copy_to_clipboard = impl_litehtml_copy_to_clipboard;
    html_class->export_to_file = impl_litehtml_export_to_file;
    html_class->print = impl_litehtml_print;
    html_class->cancel = impl_litehtml_cancel;
    html_class->set_parent = impl_litehtml_set_parent;
}

static void
gnc_html_litehtml_dispose (GObject* obj)
{
    GncHtmlLitehtml* self = GNC_HTML_LITEHTML(obj);
    GncHtmlLitehtmlPrivate* priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);
g_print("%s called\n",__FUNCTION__);
    if (priv->web_view != NULL)
    {
        gtk_container_remove (GTK_CONTAINER(priv->base.container),
                              GTK_WIDGET(priv->web_view));

        priv->web_view = NULL;
    }

    if (priv->html_string != NULL)
    {
        g_free (priv->html_string);
        priv->html_string = NULL;
    }
    if (priv->html_wrapped_widget)
        gnc_html_litehtml_delete (priv->html_wrapped_widget);

    G_OBJECT_CLASS(gnc_html_litehtml_parent_class)->dispose (obj);
}

static void
gnc_html_litehtml_finalize (GObject* obj)
{
     GncHtmlLitehtml* self = GNC_HTML_LITEHTML(obj);
g_print("%s called\n",__FUNCTION__);

    self->priv = NULL;

    G_OBJECT_CLASS(gnc_html_litehtml_parent_class)->finalize (obj);
}

/*****************************************************************************/

static char*
extract_base_name (URLType type, const gchar* path)
{
    gchar       machine_rexp[] = "^(//[^/]*)/*(/.*)?$";
    gchar       path_rexp[] = "^/*(.*)/+([^/]*)$";
    regex_t     compiled_m, compiled_p;
    regmatch_t  match[4];
    gchar       * machine = NULL, * location = NULL, * base = NULL;
    gchar       * basename = NULL;
g_print("%s called, path '%s'\n",__FUNCTION__, path);
    DEBUG(" ");
    if (!path) return NULL;

    regcomp (&compiled_m, machine_rexp, REG_EXTENDED);
    regcomp (&compiled_p, path_rexp, REG_EXTENDED);

    if (!g_strcmp0 (type, URL_TYPE_HTTP) ||
        !g_strcmp0 (type, URL_TYPE_SECURE) ||
        !g_strcmp0 (type, URL_TYPE_FTP))
    {
        /* step 1: split the machine name away from the path
         * components */
        if (!regexec (&compiled_m, path, 4, match, 0))
        {
            /* $1 is the machine name */
            if (match[1].rm_so != -1)
            {
                machine = g_strndup (path + match[1].rm_so,
                                     match[1].rm_eo - match[1].rm_so);
            }
            /* $2 is the path */
            if (match[2].rm_so != -1)
            {
                location = g_strndup (path + match[2].rm_so,
                                      match[2].rm_eo - match[2].rm_so);
            }
        }
    }
    else
    {
         location = g_strdup (path);
    }
    /* step 2: split up the path into prefix and file components */
    if (location)
    {
        if (!regexec (&compiled_p, location, 4, match, 0))
        {
            if (match[1].rm_so != -1)
            {
                base = g_strndup (location + match[1].rm_so,
                                  match[1].rm_eo - match[1].rm_so);
            }
            else
            {
                base = NULL;
            }
        }
    }

    regfree (&compiled_m);
    regfree (&compiled_p);

    if (machine)
    {
        if (base && (strlen (base) > 0))
        {
            basename = g_strconcat (machine, "/", base, "/", NULL);
        }
        else
        {
            basename = g_strconcat (machine, "/", NULL);
        }
    }
    else
    {
        if (base && (strlen (base) > 0))
        {
            basename = g_strdup (base);
        }
        else
        {
            basename = NULL;
        }
    }

    g_free (machine);
    g_free (base);
    g_free (location);
    return basename;
}

static gboolean
http_allowed ()
{
g_print("%s called\n",__FUNCTION__);
     return TRUE;
}

static gboolean
https_allowed ()
{
g_print("%s called\n",__FUNCTION__);
     return TRUE;
}

static gchar*
handle_embedded_object (GncHtmlLitehtml* self, gchar* html_str)
{
    // Find the <object> tag and get the classid from it.  This will provide the correct
    // object callback handler.  Pass the <object> entity text to the handler.  What should
    // come back is embedded image information.
    gchar* remainder_str = html_str;
    gchar* object_tag;
    gchar* end_object_tag;
    gchar* object_contents;
    gchar* html_str_start = NULL;
    gchar* html_str_middle;
    gchar* html_str_result = NULL;
    gchar* classid_start;
    gchar* classid_end;
    gchar* classid_str;
    gchar* new_chunk;
    GncHTMLObjectCB h;
g_print("%s called\n",__FUNCTION__);
    object_tag = g_strstr_len (remainder_str, -1, "<object classid=");
    while (object_tag)
    {
        classid_start = object_tag + strlen ("<object classid=") + 1;
        classid_end = g_strstr_len (classid_start, -1, "\"");
        classid_str = g_strndup (classid_start, (classid_end - classid_start));

        end_object_tag = g_strstr_len (object_tag, -1, "</object>");
        if (end_object_tag == NULL)
        {
            /*  Hmmm... no object end tag
                Return the original html string because we can't properly parse it */
            g_free (classid_str);
            g_free (html_str_result);
            return g_strdup (html_str);
        }
        end_object_tag += strlen ( "</object>" );
        object_contents = g_strndup (object_tag, (end_object_tag - object_tag));

        h = g_hash_table_lookup (gnc_html_object_handlers, classid_str);
        if (h != NULL)
        {
             (void)h(GNC_HTML(self), object_contents, &html_str_middle);
        }
        else
        {
             html_str_middle = g_strdup_printf ( "No handler found for classid \"%s\"", classid_str);
        }

        html_str_start = html_str_result;
        new_chunk = g_strndup (remainder_str, (object_tag - remainder_str));
        if (!html_str_start)
            html_str_result = g_strconcat (new_chunk, html_str_middle, NULL);
        else
            html_str_result = g_strconcat (html_str_start, new_chunk, html_str_middle, NULL);

        g_free (html_str_start);
        g_free (new_chunk);
        g_free (html_str_middle);

        remainder_str = end_object_tag;
        object_tag = g_strstr_len (remainder_str, -1, "<object classid=");
    }

    if (html_str_result)
    {
        html_str_start =  html_str_result;
        html_str_result = g_strconcat (html_str_start, remainder_str, NULL);
        g_free (html_str_start);
    }
    else
        html_str_result = g_strdup (remainder_str);

    return html_str_result;
}

/********************************************************************
 * load_to_stream : actually do the work of loading the HTML
 * or binary data referenced by a URL and feeding it into the webkit
 * widget.
 ********************************************************************/

static gboolean
load_to_stream (GncHtmlLitehtml* self, URLType type,
                const gchar* location, const gchar* label)
{
    gchar* fdata = NULL;
    int fdata_len = 0;
    GncHtmlLitehtmlPrivate* priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);
g_print("%s called\n",__FUNCTION__);
    DEBUG("type %s, location %s, label %s", type ? type : "(null)",
          location ? location : "(null)", label ? label : "(null)");

    g_return_val_if_fail (self != NULL, FALSE);

    if (gnc_html_stream_handlers != NULL)
    {
        GncHTMLStreamCB stream_handler;

        stream_handler = g_hash_table_lookup (gnc_html_stream_handlers, type);
        if (stream_handler)
        {
            GncHtml *weak_html = GNC_HTML(self);
            gboolean ok;

            g_object_add_weak_pointer (G_OBJECT(self),
                                       (gpointer*)(&weak_html));
            ok = stream_handler (location, &fdata, &fdata_len);

            if (!weak_html) // will be NULL if self has been destroyed
            {
                g_free (fdata);
                return FALSE;
            }
            else
            {
                g_object_remove_weak_pointer (G_OBJECT(self),
                                              (gpointer*)(&weak_html));
            }

            if (ok)
            {
                fdata = fdata ? fdata : g_strdup ( "" );

                // Until webkitgtk supports download requests,
                // look for "<object classid=" indicating the
                // beginning of an embedded graph.  If found,
                // handle it
                if (g_strstr_len (fdata, -1, "<object classid=") != NULL)
                {
                     gchar* new_fdata;
                     new_fdata = handle_embedded_object (self, fdata);
                     g_free (fdata);
                     fdata = new_fdata;
                }

                // Save a copy for export purposes
                if (priv->html_string != NULL)
                {
                     g_free (priv->html_string);
                }
                priv->html_string = g_strdup (fdata);
                impl_litehtml_show_data (GNC_HTML(self), fdata, strlen(fdata));
            }
            else
            {
                fdata = fdata ? fdata :
                         g_strdup_printf (error_404_format,
                                          _(error_404_title), _(error_404_body));

g_print(" Error1: '%s', '%s'\n", fdata, BASE_URI_NAME);
            }

            g_free (fdata);

            if (label)
            {
                while (gtk_events_pending())
                {
                    gtk_main_iteration();
                }
                /* No action required: Webkit jumps to the anchor on its own. */
            }
            return TRUE;
        }
    }

    do
    {
        if (!g_strcmp0 (type, URL_TYPE_SECURE) ||
            !g_strcmp0 (type, URL_TYPE_HTTP))
        {

            if (!g_strcmp0 (type, URL_TYPE_SECURE))
            {
                if (!https_allowed())
                {
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                      _("Secure HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog."));
                    break;
                }
            }

            if (!http_allowed())
            {
                gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                  _("Network HTTP access is disabled. "
                                    "You can enable it in the Network section of "
                                    "the Preferences dialog."));
            }
            else
            {
                gnc_build_url (type, location, label);
            }
        }
        else
        {
            PWARN("load_to_stream for inappropriate type\n"
                  "\turl = '%s#%s'\n",
                   location ? location : "(null)",
                   label ? label : "(null)" );
            fdata = g_strdup_printf (error_404_format,
                                   _(error_404_title), _(error_404_body));

g_print(" Error2: '%s', '%s'\n", fdata, BASE_URI_NAME);

            g_free (fdata);
        }
    }
    while (FALSE);
    return TRUE;
}

/********************************************************************
 * gnc_html_open_scm
 * insert some scheme-generated HTML
 ********************************************************************/

static void
gnc_html_open_scm (GncHtmlLitehtml* self, const gchar * location,
                   const gchar * label, int newwin)
{
g_print("%s called\n",__FUNCTION__);
    PINFO("location='%s'", location ? location : "(null)");
}


/********************************************************************
 * impl_litehtml_show_data
 * display some HTML that the creator of the gnc-html got from
 * somewhere.
 ********************************************************************/

static void
impl_litehtml_show_data (GncHtml* self, const gchar* data, int datalen)
{
    GncHtmlLitehtmlPrivate* priv;
#define TEMPLATE_REPORT_FILE_NAME "gnc-report-XXXXXX.html"
    int fd;
    gchar* uri;
    gchar *filename;
g_print("%s called\n",__FUNCTION__);
    g_return_if_fail (self != NULL);
    g_return_if_fail (GNC_IS_HTML_LITEHTML(self));

    ENTER("datalen %d, data %20.20s", datalen, data);

    priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);

    /* Export the HTML to a file and load the file URI.   On Linux, this seems to get around some
       security problems (otherwise, it can complain that embedded images aren't permitted to be
       viewed because they are local resources).  On Windows, this allows the embedded images to
       be viewed (maybe for the same reason as on Linux, but I haven't found where it puts those
       messages. */
     filename = g_build_filename (g_get_tmp_dir(), TEMPLATE_REPORT_FILE_NAME, (gchar *)NULL);
     fd = g_mkstemp (filename);
     impl_litehtml_export_to_file (self, filename);
     close (fd);

#ifdef G_OS_WIN32
    uri = g_strdup_printf ("file:///%s", filename);
#else
    uri = g_strdup_printf ("file://%s", filename);
#endif

g_print("#### Do something with uri %p filename '%s' ####\n", priv->html_wrapped_widget, uri);

g_print("#### Do something with uri ####\n");

    gnc_html_litehtml_load_file (priv->html_wrapped_widget, uri);

    g_free (filename);
    g_free (uri);

    LEAVE("");
}

/********************************************************************
 * impl_litehtml_show_url
 *
 * open a URL.  This is called when the user clicks a link or
 * for the creator of the gnc_html window to explicitly request
 * a URL.
 ********************************************************************/

static void
impl_litehtml_show_url (GncHtml* self, URLType type,
                        const gchar* location, const gchar* label,
                        gboolean new_window_hint)
{
    GncHTMLUrlCB url_handler;
    gboolean new_window;
    GncHtmlLitehtmlPrivate* priv;
    gboolean stream_loaded = FALSE;
g_print("%s called\n",__FUNCTION__);
    g_return_if_fail (self != NULL );
    g_return_if_fail (GNC_IS_HTML_LITEHTML(self));
    g_return_if_fail (location != NULL );

    priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);

    /* make sure it's OK to show this URL type in this window */
    if (new_window_hint == 0)
    {
        if (priv->base.urltype_cb)
            new_window = !((priv->base.urltype_cb)(type));
        else
            new_window = FALSE;
    }
    else
        new_window = TRUE;

    if (!new_window)
        gnc_html_cancel (GNC_HTML(self));

    if (gnc_html_url_handlers)
        url_handler = g_hash_table_lookup (gnc_html_url_handlers, type);
    else
        url_handler = NULL;

    if (url_handler)
    {
        GNCURLResult result;
        gboolean ok;

        result.load_to_stream = FALSE;
        result.url_type = type;
        result.location = NULL;
        result.label = NULL;
        result.base_type = URL_TYPE_FILE;
        result.base_location = NULL;
        result.error_message = NULL;
        result.parent = GTK_WINDOW(priv->base.parent);

g_print(" result parent window is %p\n", GTK_WINDOW(priv->base.parent));

        ok = url_handler (location, label, new_window, &result);
g_print(" ok is %d\n", ok);
        if (!ok)
        {
            if (result.error_message)
                gnc_error_dialog (GTK_WINDOW(priv->base.parent), "%s", result.error_message);
            else
            {
                 /* %s is a URL (some location somewhere). */
                 gnc_error_dialog (GTK_WINDOW(priv->base.parent), _("There was an error accessing %s."), location);
            }

            if (priv->base.load_cb)
            {
                 priv->base.load_cb (GNC_HTML(self), result.url_type,
                                     location, label, priv->base.load_cb_data);
            }
        }
        else if (result.load_to_stream)
        {
            gnc_html_history_node *hnode;
            const char *new_location;
            const char *new_label;

            new_location = result.location ? result.location : location;
            new_label = result.label ? result.label : label;
            hnode = gnc_html_history_node_new (result.url_type, new_location, new_label);

            gnc_html_history_append (priv->base.history, hnode);

            g_free( priv->base.base_location );
            priv->base.base_type = result.base_type;
            priv->base.base_location = g_strdup (extract_base_name (result.base_type, new_location));
            DEBUG("resetting base location to %s",
                  priv->base.base_location ? priv->base.base_location : "(null)");

            stream_loaded = load_to_stream (GNC_HTML_LITEHTML(self),
                                            result.url_type,
                                            new_location, new_label);

            if (stream_loaded && priv->base.load_cb != NULL)
            {
                 priv->base.load_cb (GNC_HTML(self), result.url_type,
                                     new_location, new_label, priv->base.load_cb_data);
            }
         }
         g_free (result.location);
         g_free (result.label);
         g_free (result.base_location);
         g_free (result.error_message);
         return;
    }

    if (g_strcmp0 (type, URL_TYPE_SCHEME) == 0)
        gnc_html_open_scm (GNC_HTML_LITEHTML(self), location, label, new_window);
    else if (g_strcmp0 (type, URL_TYPE_JUMP) == 0)
    {
         /* Webkit jumps to the anchor on its own */
    }
    else if (g_strcmp0( type, URL_TYPE_SECURE ) == 0 ||
             g_strcmp0( type, URL_TYPE_HTTP ) == 0 ||
             g_strcmp0( type, URL_TYPE_FILE ) == 0)
    {
        do
        {
            if ( g_strcmp0 (type, URL_TYPE_SECURE) == 0)
            {
                if (!https_allowed())
                {
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                      _("Secure HTTP access is disabled. "
                                        "You can enable it in the Network section of "
                                        "the Preferences dialog."));
                    break;
                }
            }

            if (g_strcmp0 (type, URL_TYPE_HTTP) == 0)
            {
                if (!http_allowed())
                {
                    gnc_error_dialog (GTK_WINDOW (priv->base.parent), "%s",
                                       _("Network HTTP access is disabled. "
                                         "You can enable it in the Network section of "
                                         "the Preferences dialog."));
                    break;
                }
            }

            priv->base.base_type = type;

            if (priv->base.base_location != NULL)
                g_free (priv->base.base_location);
            priv->base.base_location = extract_base_name (type, location);

            /* FIXME : handle new_window = 1 */
            gnc_html_history_append (priv->base.history,
                                     gnc_html_history_node_new (type, location, label));
            stream_loaded = load_to_stream (GNC_HTML_LITEHTML(self),
                                            type, location, label);

        }
        while (FALSE);
    }
    else
        PERR("URLType %s not supported.", type);

    if (stream_loaded && priv->base.load_cb != NULL)
        (priv->base.load_cb)(GNC_HTML(self), type, location, label, priv->base.load_cb_data);
}


/********************************************************************
 * impl_litehtml_reload
 * reload the current page
 * if force_rebuild is TRUE, the report is recreated, if FALSE, report
 * is reloaded by browser
 ********************************************************************/

static void
impl_litehtml_reload (GncHtml* self, gboolean force_rebuild)
{
     GncHtmlLitehtmlPrivate* priv;
g_print("%s called, %d\n",__FUNCTION__, force_rebuild);

    g_return_if_fail (self != NULL);
    g_return_if_fail (GNC_IS_HTML_LITEHTML(self));

    priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);

    if (force_rebuild)
    {
        gnc_html_history_node *n = gnc_html_history_get_current (priv->base.history);
        if (n != NULL)
             gnc_html_show_url (self, n->type, n->location, n->label, 0);
    }
}


/********************************************************************
 * gnc_html_litehtml_new
 * create and set up a new browser widget.
 ********************************************************************/

GncHtml*
gnc_html_litehtml_new (void)
{
     GncHtmlLitehtml* self = g_object_new (GNC_TYPE_HTML_LITEHTML, NULL);
g_print("%s called\n",__FUNCTION__);
     return GNC_HTML(self);
}

/********************************************************************
 * impl_litehtml_cancel
 * cancel any outstanding HTML fetch requests.
 ********************************************************************/

static void
impl_litehtml_cancel (GncHtml* self)
{
g_print("%s called\n",__FUNCTION__);
}

static void
impl_litehtml_copy_to_clipboard (GncHtml* self)
{
g_print("%s called\n",__FUNCTION__);
}

/**************************************************************
 * impl_litehtml_export_to_file
 *
 * @param self GncHtmlLitehtml object
 * @param filepath Where to write the HTML
 * @return TRUE if successful, FALSE if unsuccessful
 **************************************************************/
static gboolean
impl_litehtml_export_to_file (GncHtml* self, const char *filepath)
{
    FILE *fh;
    GncHtmlLitehtmlPrivate* priv;
g_print("%s called\n",__FUNCTION__);
    g_return_val_if_fail (self != NULL, FALSE);
    g_return_val_if_fail (GNC_IS_HTML_LITEHTML(self), FALSE);
    g_return_val_if_fail (filepath != NULL, FALSE);

    priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);
    if (priv->html_string == NULL)
    {
        return FALSE;
    }
    fh = g_fopen (filepath, "w");
    if (fh != NULL)
    {
        gint written;
        gint len = strlen (priv->html_string);

        written = fwrite (priv->html_string, 1, len, fh);
        fclose (fh);

        if (written != len)
            return FALSE;

        return TRUE;
    }
    else
        return FALSE;
}

static void
impl_litehtml_print (GncHtml* self, const gchar* jobname)
{
//    GncHtmlLitehtmlPrivate* priv;
g_print("%s called\n",__FUNCTION__);
    g_return_if_fail (self != NULL );
    g_return_if_fail (GNC_IS_HTML_LITEHTML(self));

//    priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);

}

static void
impl_litehtml_set_parent (GncHtml* self, GtkWindow* parent)
{
    GncHtmlLitehtmlPrivate* priv;
g_print("%s called\n",__FUNCTION__);
    g_return_if_fail (self != NULL );
    g_return_if_fail (GNC_IS_HTML_LITEHTML(self));

    priv = GNC_HTML_LITEHTML_GET_PRIVATE(self);
    priv->base.parent = GTK_WIDGET(parent);
}
