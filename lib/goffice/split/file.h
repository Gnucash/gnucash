#ifndef GNUMERIC_FILE_H
#define GNUMERIC_FILE_H

#include "gnumeric.h"
#include <glib-object.h>
#include <gsf/gsf.h>


/*
 * File format levels. They are ordered. When we save a file, we
 * remember the name, but not if we already have a name at a higher level.
 * When created, workbooks are assigned a name at level FILE_FL_NEW.
 */
typedef enum {
	FILE_FL_NONE,            /* No name assigned, won't happen */
	FILE_FL_WRITE_ONLY,      /* PostScript etc, won't be remembered */
	FILE_FL_NEW,             /* Wb just created */
	FILE_FL_MANUAL,          /* Save gets punted to save as */
	FILE_FL_MANUAL_REMEMBER, /* Ditto, but remember in history */
	FILE_FL_AUTO,            /* Save will save to this filename */
	FILE_FL_LAST
} FileFormatLevel;

/*
 * File probe level tells file opener (its probe method to be exact), how
 * hard it should try to recognize the type of the file. File openers may
 * ignore this or support only some probe levels, but if specifies
 * "reccomened" behaviour.
 * Before opening any file we detect its type by calling probe for
 * every registered file opener (in order of priority) and passing
 * FILE_PROBE_FILE_NAME as probe level. If none of them recogizes the file,
 * we increase probe level and try again...
 */
typedef enum {
	FILE_PROBE_FILE_NAME,	/* Test only file name, don't read file contents */
	FILE_PROBE_CONTENT,	/* Read the whole file if it's necessary */
	FILE_PROBE_LAST
} FileProbeLevel;

/*
 * FileSaveScope specifies what information file saver can save in a file.
 * Many savers can save the whole workbook (with all sheets), but others
 * save only current sheet, usually because of file format limitations.
 */
typedef enum {
	FILE_SAVE_WORKBOOK,
	FILE_SAVE_SHEET,
	FILE_SAVE_RANGE,
	FILE_SAVE_LAST
} FileSaveScope;

/*
 * GnmFileOpener
 */

typedef struct _GnmFileOpenerClass GnmFileOpenerClass;

#define TYPE_GNM_FILE_OPENER             (gnm_file_opener_get_type ())
#define GNM_FILE_OPENER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GNM_FILE_OPENER, GnmFileOpener))
#define IS_GNM_FILE_OPENER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GNM_FILE_OPENER))

typedef gboolean (*GnmFileOpenerProbeFunc) (GnmFileOpener const *fo,
					    GsfInput *input,
					    FileProbeLevel pl);
typedef void     (*GnmFileOpenerOpenFunc) (GnmFileOpener const *fo,
					   IOContext *io_context,
					   WorkbookView *wbv,
					   GsfInput *input);
typedef void     (*GnmFileOpenerOpenFuncWithEnc) (GnmFileOpener const *fo,
						  gchar const *enc,
						  IOContext *io_context,
						  WorkbookView *wbv,
						  GsfInput *input);

GType gnm_file_opener_get_type (void);

GnmFileOpener *gnm_file_opener_new (char const *id,
				    char const *description,
				    GSList *suffixes,
				    GSList *mimes,
				    GnmFileOpenerProbeFunc probe_func,
				    GnmFileOpenerOpenFunc open_func);
GnmFileOpener *gnm_file_opener_new_with_enc (char const *id,
					     char const *description,
					     GSList *suffixes,
					     GSList *mimes,
					     GnmFileOpenerProbeFunc probe_func,
					     GnmFileOpenerOpenFuncWithEnc open_func);


gboolean     gnm_file_opener_probe (GnmFileOpener const *fo, GsfInput *input,
				    FileProbeLevel pl);
void         gnm_file_opener_open (GnmFileOpener const *fo, gchar const *opt_enc,
				   IOContext *io_context,
				   WorkbookView *wbv, GsfInput *input);

char const *gnm_file_opener_get_id		  (GnmFileOpener const *fo);
char const *gnm_file_opener_get_description	  (GnmFileOpener const *fo);
gboolean    gnm_file_opener_is_encoding_dependent (GnmFileOpener const *fo);
gboolean    gnm_file_opener_can_probe		  (GnmFileOpener const *fo,
						   FileProbeLevel pl);
GSList const *gnm_file_opener_get_suffixes	  (GnmFileOpener const *fo);
GSList const *gnm_file_opener_get_mimes	  	  (GnmFileOpener const *fo);

/*
 * GnmFileSaver
 */

typedef struct _GnmFileSaverClass GnmFileSaverClass;

#define TYPE_GNM_FILE_SAVER             (gnm_file_saver_get_type ())
#define GNM_FILE_SAVER(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_GNM_FILE_SAVER, GnmFileSaver))
#define IS_GNM_FILE_SAVER(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_GNM_FILE_SAVER))

typedef void (*GnmFileSaverSaveFunc) (GnmFileSaver const *fs,
				      IOContext *io_context,
				      WorkbookView const *wbv,
				      GsfOutput *output);
GType gnm_file_saver_get_type (void);

GnmFileSaver *gnm_file_saver_new (char const *id,
				  char const *extension,
				  char const *description,
				  FileFormatLevel level,
				  GnmFileSaverSaveFunc save_func);

void          gnm_file_saver_set_save_scope (GnmFileSaver *fs, FileSaveScope scope);
FileSaveScope gnm_file_saver_get_save_scope (GnmFileSaver const *fs);

void         gnm_file_saver_save (GnmFileSaver const *fs, IOContext *io_context,
				  WorkbookView const *wbv, GsfOutput *output);
void         gnm_file_saver_set_overwrite_files	(GnmFileSaver *fs,
						 gboolean overwrite);
gboolean     gnm_vrfy_uri_ext (gchar const *std_ext,
			       gchar const *uri,
			       gchar **new_uri);
char const *gnm_file_saver_get_id	  	(GnmFileSaver const *fs);
char const *gnm_file_saver_get_extension	(GnmFileSaver const *fs);
char const *gnm_file_saver_get_mime_type	(GnmFileSaver const *fs);
char const *gnm_file_saver_get_description	(GnmFileSaver const *fs);
FileFormatLevel gnm_file_saver_get_format_level	(GnmFileSaver const *fs);

/*
 *
 */

GList *get_file_openers (void);
void		 gnm_file_opener_unregister (GnmFileOpener *fo);
void		 gnm_file_opener_register   (GnmFileOpener *fo, gint priority);
GnmFileOpener	*gnm_file_opener_for_id	    (char const *id);

GList *get_file_savers (void);
void gnm_file_saver_unregister	(GnmFileSaver *fs);
void gnm_file_saver_register	(GnmFileSaver *fs);
void gnm_file_saver_register_as_default (GnmFileSaver *fs, gint priority);

GnmFileSaver	*gnm_file_saver_get_default (void);
GnmFileSaver	*gnm_file_saver_for_mime_type	(char const *mime_type);
GnmFileSaver	*gnm_file_saver_for_file_name	(char const *file_name);
GnmFileSaver	*gnm_file_saver_for_id		(char const *id);

#endif /* GNUMERIC_FILE_H */
