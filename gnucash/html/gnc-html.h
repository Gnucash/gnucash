/********************************************************************
 * gnc-html.h -- display html with gnc special tags                 *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#ifndef GNC_HTML_H
#define GNC_HTML_H

/**
 * A GncHtml object is an abstract base for an html engine used to display reports and
 * charts in gnucash.  It must be overridden to create specific objects using specific
 * html engines (e.g. webkit).
 */

#include <glib-object.h>

G_BEGIN_DECLS

#define GNC_TYPE_HTML         (gnc_html_get_type())
#define GNC_HTML(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML, GncHtml))
#define GNC_HTML_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_HTML, GncHtmlClass))
#define GNC_IS_HTML(o)        (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HTML))
#define GNC_IS_HTML_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE((o), GNC_TYPE_HTML))
#define GNC_HTML_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS((o), GNC_TYPE_HTML, GncHtmlClass))

typedef struct _GncHtml GncHtml;
typedef struct _GncHtmlClass GncHtmlClass;
typedef struct _GncHtmlPrivate GncHtmlPrivate;

#ifdef __cplusplus
extern "C"
{
#endif

GType gnc_html_get_type(void);

#ifdef __cplusplus
}
#endif


#include "gnc-html-extras.h"

/* The result structure of url handlers. Strings should be g_malloc'd
 * by the handler and will be freed by gnc_html. */
typedef struct
{
    /* The following members are used if the handler succeeds (returns TRUE). */

    gboolean load_to_stream; /* If TRUE, the url should be loaded from
                            * a stream using the rest of the data in
                            * the struct into the original gnc_html
                            * object. If FALSE, the handler will
                            * perform all needed actions itself. */

    URLType url_type;        /* Defaults to original */
    gchar* location;         /* If nullptr, use original (nullptr is default) */
    gchar* label;            /* If nullptr, use original (nullptr is default) */

    URLType base_type;
    gchar* base_location;

    /* The window that triggered this URL request */
    GtkWindow *parent;

    /* The following members are used if the handler fails (returns FALSE). */
    gchar* error_message;
} GNCURLResult;

typedef gboolean (* GncHTMLObjectCB)(GncHtml* html, gpointer eb,
                                     gpointer data);
typedef gboolean (* GncHTMLStreamCB)(const gchar* location, gchar** data, int* datalen);
/** Function pointer type for loading a URL-like object into a web page.
 *
 * @param location: URL or other indicator to the resource. On multi-column reports this may be the index of the suberport.
 * @param label: The label to use for the notebook tab containing the item.
 * @param new_window: Open the tab in a new GncMainWindow if true
 * @param result: A GNCURLResult*
 * @returns true if the handler succeeded.
 */
typedef gboolean (* GncHTMLUrlCB)(const gchar* location, const gchar* label,
                                  gboolean new_window, GNCURLResult* result);

#ifdef __cplusplus
#define NOEXCEPT noexcept
extern "C"
{
#else
#define NOEXCEPT
#endif

/**
 * Registers a new URLType.
 * returns TRUE if successful, FALSE if type already exists.
 *
 * @param type New URL type
 * @param prococol Protocol - should be an empty string if there is no corresponding protocol.
 * @return TRUE if successful, FALSE if type already exists or protocol is nullptr.
 */
gboolean gnc_html_register_urltype( URLType type, const gchar* protocol ) NOEXCEPT;

/**
 * Initializes the html subsystem
 */
void gnc_html_initialize( void ) NOEXCEPT;

#ifdef __cplusplus
}
#endif

#include "gnc-html-history.h"

struct _GncHtmlClass
{
    GtkBinClass parent_class;

    /* Methods */
    void (*show_url)( GncHtml* html,
                      URLType type,
                      const gchar* location,
                      const gchar* label,
                      gboolean new_window );
    void (*show_data)( GncHtml* html, const gchar* data, int datalen );
    void (*reload)( GncHtml* html, gboolean force_rebuild );
    void (*copy_to_clipboard)( GncHtml* html );
    gboolean (*export_to_file)( GncHtml* html, const gchar* file );
    void (*print) (GncHtml* html, const gchar* jobname);
    void (*cancel)( GncHtml* html );
    URLType (*parse_url)( GncHtml* html, const gchar* url,
                          gchar** url_location, gchar** url_label );
    void (*set_parent)( GncHtml* html, GtkWindow* parent );
};

struct _GncHtml
{
    GtkBin parent_instance;

    /*< private >*/
    GncHtmlPrivate* priv;
};

typedef int  (* GncHTMLUrltypeCB)(URLType ut);
typedef void (* GncHTMLFlyoverCB)(GncHtml* html, const gchar* url,
                                  gpointer data);
typedef void (* GncHTMLLoadCB)(GncHtml* html, URLType type,
                               const gchar* location, const gchar* label,
                               gpointer data);
typedef int  (* GncHTMLButtonCB)(GncHtml* html, GdkEventButton* event,
                                 gpointer data);

#ifdef __cplusplus
extern "C"
{
#endif

/**
 * Destroys a GncHtml object.
 *
 * @param html GncHtml object to destroy
 */
void gnc_html_destroy( GncHtml* html ) NOEXCEPT;

/**
 * Displays a URL in a GncHtml object.
 *
 * @param html GncHtml object
 */
void gnc_html_show_url( GncHtml* html, URLType type, const gchar* location,
                        const gchar* label, gboolean new_window ) NOEXCEPT;

/**
 * Displays an HTML string in a GncHtml object.
 *
 * @param html GncHtml object
 */
void gnc_html_show_data( GncHtml* html, const gchar* data, int datalen ) NOEXCEPT;

/**
 * Reloads the current GncHtml object.
 *
 * @param html GncHtml object
 * @param view if TRUE, view is reloaded, if FALSE, report is recreated
 */
void gnc_html_reload( GncHtml* html, gboolean view ) NOEXCEPT;

/**
 * Copies the html to the clipboard
 *
 * @param html GncHtml object
 */
void gnc_html_copy_to_clipboard( GncHtml* html ) NOEXCEPT;

/**
 * Exports the html to an external file.
 *
 * @param html GncHtml object
 * @param filename External file name
 * @param TRUE if successful, FALSE if unsuccessful
 */
gboolean gnc_html_export_to_file( GncHtml* html, const gchar* filename ) NOEXCEPT;

/**
 * Prints the report.
 *
 * @param html GncHtml object
 */
void gnc_html_print (GncHtml* html, const char* jobname) NOEXCEPT;

/**
 * Cancels the current operation
 *
 * @param html GncHtml object
 */
void gnc_html_cancel( GncHtml* html ) NOEXCEPT;

/**
 * Parses a URL into URI and label
 *
 * @param html GncHtml object
 * @param url URL
 * @param url_location Pointer where to store address of string containing main URI
 * @param url_label Pointer where to store address of string containing label
 */
URLType gnc_html_parse_url( GncHtml* html, const gchar* url,
                            gchar** url_location, gchar** url_label ) NOEXCEPT;

/**
 * Returns the history for this html engine
 *
 * @param html GncHtml object
 * @return History
 */
gnc_html_history* gnc_html_get_history( GncHtml* html ) NOEXCEPT;

/**
 * Returns the main widget for this html engine
 *
 * @param html GncHtml object
 * @return Main widget
 */
GtkWidget* gnc_html_get_widget( GncHtml* html ) NOEXCEPT;

/**
 * Returns the webview widget for this html engine
 *
 * @param html GncHtml object
 * @return webview widget
 */
GtkWidget* gnc_html_get_webview( GncHtml* html ) NOEXCEPT;


/**
 * Sets the parent window for this html engine.  The engine will be embedded in this parent.
 *
 * @param html GncHtml object
 * @param parent Parent window
 */
void gnc_html_set_parent( GncHtml* html, GtkWindow* parent ) NOEXCEPT;

/* setting callbacks */
void gnc_html_set_urltype_cb( GncHtml* html, GncHTMLUrltypeCB urltype_cb ) NOEXCEPT;
void gnc_html_set_load_cb( GncHtml* html, GncHTMLLoadCB load_cb, gpointer data ) NOEXCEPT;
void gnc_html_set_flyover_cb( GncHtml* html, GncHTMLFlyoverCB newwin_cb, gpointer data ) NOEXCEPT;
void gnc_html_set_button_cb( GncHtml* html, GncHTMLButtonCB button_cb, gpointer data ) NOEXCEPT;

/* object handlers deal with <object classid="foo"> objects in HTML.
 * the handlers are looked up at object load time. */
void gnc_html_register_object_handler( const gchar* classid, GncHTMLObjectCB hand ) NOEXCEPT;
void gnc_html_unregister_object_handler( const gchar* classid ) NOEXCEPT;

/* stream handlers load data for particular URLTypes. */
void gnc_html_register_stream_handler( URLType url_type, GncHTMLStreamCB hand ) NOEXCEPT;
void gnc_html_unregister_stream_handler( URLType url_type ) NOEXCEPT;

/* handlers for particular URLTypes. */
void gnc_html_register_url_handler( URLType url_type, GncHTMLUrlCB hand ) NOEXCEPT;
void gnc_html_unregister_url_handler( URLType url_type ) NOEXCEPT;

const gchar* gnc_html_get_embedded_param( gpointer eb, const gchar* param_name ) NOEXCEPT;

#ifdef __cplusplus
}
#endif

G_END_DECLS
#endif
