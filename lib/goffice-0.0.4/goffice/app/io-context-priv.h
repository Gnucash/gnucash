#ifndef GO_IO_CONTEXT_PRIV_H
#define GO_IO_CONTEXT_PRIV_H

#include <goffice/app/io-context.h>
#include <goffice/app/error-info.h>
#include <goffice/app/go-cmd-context-impl.h>

G_BEGIN_DECLS

#define IO_CONTEXT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_IO_CONTEXT, IOContextClass))
#define IS_IO_CONTEXT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_IO_CONTEXT))

typedef enum {
	GO_PROGRESS_HELPER_NONE,
	GO_PROGRESS_HELPER_COUNT,
	GO_PROGRESS_HELPER_VALUE,
	GO_PROGRESS_HELPER_LAST
} GOProgressHelperType;

typedef struct {
	GOProgressHelperType helper_type;
	union {
		struct {
			gchar *start;
			gint size;
		} mem;
		struct {
			gint total, last, current;
			gint step;
		} count;
		struct {
			gint total, last;
			gint step;
		} value;
		struct {
			gint n_elements, last, current;
			gint step;
		} workbook;
	} v;
} GOProgressHelper;

typedef struct {
	gfloat min, max;
} ProgressRange;

struct _IOContext {
	GObject base;

	GOCmdContext	*impl;
	ErrorInfo	*info;
	gboolean	 error_occurred;
	gboolean	 warning_occurred;

	GList	*progress_ranges;
	gfloat	 progress_min, progress_max;
	gdouble  last_progress;
	gdouble  last_time;
	GOProgressHelper helper;
};

struct _IOContextClass {
	GObjectClass base;
	void  (*set_num_files)   (IOContext *ioc, guint count);
	void  (*processing_file) (IOContext *ioc, char const *name);
};

G_END_DECLS

#endif /* GO_IO_CONTEXT_PRIV_H */
