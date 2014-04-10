#ifndef GNUMERIC_IO_CONTEXT_PRIV_H
#define GNUMERIC_IO_CONTEXT_PRIV_H

#include "gnumeric.h"
#include "io-context.h"
#include "error-info.h"
#include "command-context-priv.h"
#include <stdio.h>

#define IO_CONTEXT_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_IO_CONTEXT, IOContextClass))
#define IS_IO_CONTEXT_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_IO_CONTEXT))

typedef enum {
	GNM_PROGRESS_HELPER_NONE,
	GNM_PROGRESS_HELPER_FILE,
	GNM_PROGRESS_HELPER_MEM,
	GNM_PROGRESS_HELPER_COUNT,
	GNM_PROGRESS_HELPER_VALUE,
	GNM_PROGRESS_HELPER_WORKBOOK,
	GNM_PROGRESS_HELPER_LAST
} GnmProgressHelperType;

typedef struct {
	GnmProgressHelperType helper_type;
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
} GnmProgressHelper;

typedef struct {
	gfloat min, max;
} ProgressRange;

struct _IOContext {
	GObject base;

	GnmCmdContext	*impl;
	ErrorInfo	*info;
	gboolean	 error_occurred;
	gboolean	 warning_occurred;

	GList	*progress_ranges;
	gfloat	 progress_min, progress_max;
	gdouble  last_progress;
	gdouble  last_time;
	GnmProgressHelper helper;
};

struct _IOContextClass {
	GObjectClass base;
	void  (*set_num_files)   (IOContext *ioc, guint count);
	void  (*processing_file) (IOContext *ioc, char const *name);
};

#endif /* GNUMERIC_IO_CONTEXT_PRIV_H */
