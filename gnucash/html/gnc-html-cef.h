
#ifndef GNC_HTML_CEF_H
#define GNC_HTML_CEF_H

#include <glib-object.h>
#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

void cef_wrapper_init (int argc, char* argv[]);
void cef_wrapper_create_browser (const char* html_file);
void cef_wrapper_load_file(const char* html_file);
void cef_wrapper_shutdown();

#ifdef __cplusplus
}
#endif

#endif
