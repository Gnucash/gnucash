#ifndef GO_FILE_PRIV_H
#define GO_FILE_PRIV_H

#include <goffice/app/goffice-app.h>

G_BEGIN_DECLS

/*
 * GOFileOpener
 */

#define GO_FILE_OPENER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GO_FILE_OPENER, GOFileOpenerClass))
#define IS_GO_FILE_OPENER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_GO_FILE_OPENER))

#define GO_FILE_OPENER_METHOD(obj,name) \
        ((GO_FILE_OPENER_CLASS (G_OBJECT_GET_CLASS (obj)))->name)

struct _GOFileOpenerClass {
	GObjectClass parent_class;

	gboolean  (*can_probe) (GOFileOpener const *fo,
				FileProbeLevel pl);
	gboolean  (*probe) (GOFileOpener const *fo,
	                    GsfInput *input,
	                    FileProbeLevel pl);
	void      (*open)  (GOFileOpener const *fo,
			    gchar const *opt_enc,
	                    IOContext *io_context,
	                    gpointer  fixme_fixme_workbook_view,
	                    GsfInput *input);
};

struct _GOFileOpener {
	GObject parent;

	gchar	*id;
	gchar	*description;
	GSList	*suffixes;
	GSList	*mimes;
	gboolean encoding_dependent;

	GOFileOpenerProbeFunc probe_func;
	GOFileOpenerOpenFunc  open_func;
};

void go_file_opener_setup (GOFileOpener *fo, const gchar *id,
			    const gchar *description,
			    GSList *suffixes,
			    GSList *mimes,
			    gboolean encoding_dependent,
			    GOFileOpenerProbeFunc probe_func,
			    GOFileOpenerOpenFunc open_func);

/*
 * GOFileSaver
 */

#define GO_FILE_SAVER_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_GO_FILE_SAVER, GOFileSaverClass))
#define IS_GO_FILE_SAVER_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_GO_FILE_SAVER))

#define GO_FILE_SAVER_METHOD(obj,name) \
        ((GO_FILE_SAVER_CLASS (G_OBJECT_GET_CLASS (obj)))->name)

struct _GOFileSaverClass {
	GObjectClass parent_class;

	void (*save) (GOFileSaver const *fs,
	              IOContext *io_context,
	              gconstpointer wbv,
	              GsfOutput *output);
};

struct _GOFileSaver {
	GObject parent;

	gchar                *id;
	const gchar          *mime_type;
	gchar                *extension;
	gchar                *description;
	gboolean              overwrite_files;
	FileFormatLevel               format_level;
	FileSaveScope                 save_scope;
	GOFileSaverSaveFunc         save_func;
};

void go_file_saver_setup (GOFileSaver *fs,
                            const gchar *id,
                            const gchar *extension,
                            const gchar *description,
                            FileFormatLevel level,
                            GOFileSaverSaveFunc save_func);

G_END_DECLS

#endif /* GO_FILE_PRIV_H */
