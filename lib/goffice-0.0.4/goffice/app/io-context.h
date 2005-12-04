#ifndef GO_IO_CONTEXT_H
#define GO_IO_CONTEXT_H

#include <goffice/app/goffice-app.h>
#include <glib-object.h>
#include <stdarg.h>

G_BEGIN_DECLS

typedef struct _IOContextClass IOContextClass;

#define TYPE_IO_CONTEXT    (io_context_get_type ())
#define IO_CONTEXT(obj)    (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_IO_CONTEXT, IOContext))
#define IS_IO_CONTEXT(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_IO_CONTEXT))

GType      io_context_get_type (void);
IOContext *gnumeric_io_context_new        (GOCmdContext *cc);

void       gnumeric_io_error_unknown      (IOContext *ioc);

void       gnumeric_io_error_info_set     (IOContext *ioc, ErrorInfo *error);
void       gnumeric_io_error_string       (IOContext *ioc, const gchar *str);
void       gnumeric_io_error_push         (IOContext *ioc, ErrorInfo *error);
void       gnumeric_io_error_clear        (IOContext *ioc);
void       gnumeric_io_error_display      (IOContext *ioc);

gboolean   gnumeric_io_error_occurred     (IOContext *ioc);
gboolean   gnumeric_io_warning_occurred   (IOContext *ioc);

void       io_progress_message      (IOContext *io_context, const gchar *msg);
void       io_progress_update       (IOContext *io_context, gdouble f);
void       io_progress_range_push   (IOContext *io_context, gdouble min, gdouble max);
void       io_progress_range_pop    (IOContext *io_context);

void       count_io_progress_set    (IOContext *io_context, gint total, gint step);
void       count_io_progress_update (IOContext *io_context, gint inc);

void       value_io_progress_set    (IOContext *io_context, gint total, gint step);
void       value_io_progress_update (IOContext *io_context, gint value);

void       io_progress_unset      (IOContext *io_context);

void gnm_io_context_set_num_files	(IOContext *ioc, guint count);
void gnm_io_context_processing_file	(IOContext *ioc, char const *uri);
void gnm_io_warning			(IOContext *ioc, char const *fmt, ...) G_GNUC_PRINTF (2, 3);
void gnm_io_warning_varargs		(IOContext *ioc, char const *fmt, va_list args);
void gnm_io_warning_unknown_font	(IOContext *ioc, char const *font_name);
void gnm_io_warning_unknown_function	(IOContext *ioc, char const *funct_name);
void gnm_io_warning_unsupported_feature	(IOContext *ioc, char const *feature);

G_END_DECLS

#endif /* GO_IO_CONTEXT_H */
