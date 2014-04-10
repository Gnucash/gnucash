/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * file.c: File loading and saving routines
 *
 * Authors:
 *   Miguel de Icaza (miguel@kernel.org)
 *   Zbigniew Chyla (cyba@gnome.pl)
 */
#include <goffice/goffice-config.h>
#include <goffice/utils/go-file.h>
#include <goffice/utils/go-glib-extras.h>
#include <goffice/app/file.h>
#include <goffice/app/file-priv.h>
#include <goffice/app/error-info.h>
#include <goffice/app/io-context.h>

#include <gsf/gsf-input.h>
#include <gsf/gsf-output.h>
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-output-stdio.h>
#include <gsf/gsf-utils.h>
#include <string.h>
#include <glib/gi18n.h>

static void
go_file_opener_init (GOFileOpener *fo)
{
	fo->id = NULL;
	fo->description = NULL;
	fo->probe_func = NULL;
	fo->open_func = NULL;
}

static void
go_file_opener_finalize (GObject *obj)
{
	GOFileOpener *fo;

	g_return_if_fail (IS_GO_FILE_OPENER (obj));

	fo = GO_FILE_OPENER (obj);
	g_free (fo->id);
	g_free (fo->description);
	g_slist_foreach (fo->suffixes, (GFunc)g_free, NULL);
	g_slist_free (fo->suffixes);
	g_slist_foreach (fo->mimes, (GFunc)g_free, NULL);
	g_slist_free (fo->mimes);

	G_OBJECT_CLASS (g_type_class_peek (G_TYPE_OBJECT))->finalize (obj);
}

static gboolean
go_file_opener_can_probe_real (GOFileOpener const *fo, FileProbeLevel pl)
{
	return fo->probe_func != NULL;
}

static gboolean
go_file_opener_probe_real (GOFileOpener const *fo, GsfInput *input,
                             FileProbeLevel pl)
{
	gboolean ret = FALSE;

	if (fo->probe_func != NULL) {
		ret =  fo->probe_func (fo, input, pl);
		gsf_input_seek (input, 0, G_SEEK_SET);
	}
	return ret;
}

static void
go_file_opener_open_real (GOFileOpener const *fo, gchar const *opt_enc, 
			   IOContext *io_context,
			   gpointer FIXME_FIXME_workbook_view,
			   GsfInput *input)
{
	if (fo->open_func != NULL) {
		if (fo->encoding_dependent)
			((GOFileOpenerOpenFuncWithEnc)fo->open_func) 
				(fo, opt_enc, io_context, FIXME_FIXME_workbook_view, input);
		else
			fo->open_func (fo, io_context, FIXME_FIXME_workbook_view, input);
	} else
		gnumeric_io_error_unknown (io_context);
}

static void
go_file_opener_class_init (GOFileOpenerClass *klass)
{
	G_OBJECT_CLASS (klass)->finalize = go_file_opener_finalize;

	klass->can_probe = go_file_opener_can_probe_real;
	klass->probe = go_file_opener_probe_real;
	klass->open = go_file_opener_open_real;
}

GSF_CLASS (GOFileOpener, go_file_opener,
	   go_file_opener_class_init, go_file_opener_init,
	   G_TYPE_OBJECT)

/**
 * go_file_opener_setup:
 * @fo          : Newly created GOFileOpener object
 * @id          : Optional ID of the opener (or NULL)
 * @description : Description of supported file format
 * @encoding_dependent: whether the opener depends on an encoding sel.
 * @probe_func  : Optional pointer to "probe" function (or NULL)
 * @open_func   : Pointer to "open" function
 *
 * Sets up GOFileOpener object, newly created with g_object_new function.
 * This is intended to be used only by GOFileOpener derivates.
 * Use go_file_opener_new, if you want to create GOFileOpener object.
 */
void
go_file_opener_setup (GOFileOpener *fo, gchar const *id,
                        gchar const *description,
			GSList *suffixes,
			GSList *mimes,
		        gboolean encoding_dependent,
                        GOFileOpenerProbeFunc probe_func,
                        GOFileOpenerOpenFunc open_func)
{
	g_return_if_fail (IS_GO_FILE_OPENER (fo));

	fo->id = g_strdup (id);
	fo->description = g_strdup (description);
	fo->suffixes = suffixes;
	fo->mimes = mimes;

	fo->encoding_dependent = encoding_dependent;
	fo->probe_func = probe_func;
	fo->open_func = open_func;
}

/**
 * go_file_opener_new:
 * @id          : Optional ID of the opener (or NULL)
 * @description : Description of supported file format
 * @probe_func  : Optional pointer to "probe" function (or NULL)
 * @open_func   : Pointer to "open" function
 *
 * Creates new GOFileOpener object. Optional @id will be used
 * after registering it with go_file_opener_register function.
 *
 * Return value: newly created GOFileOpener object.
 */
GOFileOpener *
go_file_opener_new (gchar const *id,
                      gchar const *description,
		      GSList *suffixes,
		      GSList *mimes,
                      GOFileOpenerProbeFunc probe_func,
                      GOFileOpenerOpenFunc open_func)
{
	GOFileOpener *fo;

	fo = GO_FILE_OPENER (g_object_new (TYPE_GO_FILE_OPENER, NULL));
	go_file_opener_setup (fo, id, description, suffixes, mimes, FALSE,
			       probe_func, open_func);

	return fo;
}

/**
 * go_file_opener_new_with_enc:
 * @id          : Optional ID of the opener (or NULL)
 * @description : Description of supported file format 
 * @probe_func  : Optional pointer to "probe" function (or NULL)
 * @open_func   : Pointer to "open" function    
 *   
 * Creates new GOFileOpener object. Optional @id will be used
 * after registering it with go_file_opener_register function.
 *      
 * Return value: newly created GOFileOpener object.
 */

GOFileOpener *
go_file_opener_new_with_enc (gchar const *id,
		     gchar const *description,
		     GSList *suffixes,
		     GSList *mimes,
		     GOFileOpenerProbeFunc probe_func,
		     GOFileOpenerOpenFuncWithEnc open_func)
{
        GOFileOpener *fo;

        fo = GO_FILE_OPENER (g_object_new (TYPE_GO_FILE_OPENER, NULL));
        go_file_opener_setup (fo, id, description, suffixes, mimes, TRUE,
                               probe_func, (GOFileOpenerOpenFunc)open_func);
        return fo;
}




gchar const *
go_file_opener_get_id (GOFileOpener const *fo)
{
	g_return_val_if_fail (IS_GO_FILE_OPENER (fo), NULL);

	return fo->id;
}

gchar const *
go_file_opener_get_description (GOFileOpener const *fo)
{
	g_return_val_if_fail (IS_GO_FILE_OPENER (fo), NULL);

	return fo->description;
}

gboolean 
go_file_opener_is_encoding_dependent (GOFileOpener const *fo)
{
        g_return_val_if_fail (IS_GO_FILE_OPENER (fo), FALSE);
	
	return fo->encoding_dependent;
}

gboolean
go_file_opener_can_probe (GOFileOpener const *fo, FileProbeLevel pl)
{
	g_return_val_if_fail (IS_GO_FILE_OPENER (fo), FALSE);

	return GO_FILE_OPENER_METHOD (fo, can_probe) (fo, pl);
}

GSList const *
go_file_opener_get_suffixes (GOFileOpener const *fo)
{
	g_return_val_if_fail (IS_GO_FILE_OPENER (fo), NULL);
	return fo->suffixes;
}
GSList const *
go_file_opener_get_mimes (GOFileOpener const *fo)
{
	g_return_val_if_fail (IS_GO_FILE_OPENER (fo), NULL);
	return fo->mimes;
}


/**
 * go_file_opener_probe:
 * @fo      : GOFileOpener object
 * @input   : The input source
 *
 * Checks if a given file is supported by the opener.
 *
 * Return value: TRUE, if the opener can read given file and FALSE
 *               otherwise.
 */
gboolean
go_file_opener_probe (GOFileOpener const *fo, GsfInput *input, FileProbeLevel pl)
{
	g_return_val_if_fail (IS_GO_FILE_OPENER (fo), FALSE);
	g_return_val_if_fail (GSF_IS_INPUT (input), FALSE);

#if 0
	g_print ("Trying format %s at level %d...\n",
		 go_file_opener_get_id (fo),
		 (int)pl);
#endif
	return GO_FILE_OPENER_METHOD (fo, probe) (fo, input, pl);
}

/**
 * go_file_opener_open:
 * @fo          : GOFileOpener object
 * @opt_enc     : Optional encoding
 * @io_context  : Context for i/o operation
 * @wbv         : Workbook View
 * @input       : Gsf input stream
 *
 * Reads content of @file_name file into workbook @wbv is attached to.
 * Results are reported using @io_context object, use
 * gnumeric_io_error_occurred to find out if operation was successful.
 * The state of @wbv and its workbook is undefined if operation fails, you
 * should destroy them in that case.
 */
void
go_file_opener_open (GOFileOpener const *fo, gchar const *opt_enc,
		      IOContext *io_context,
		      gpointer FIXME_FIXME_workbook_view, GsfInput *input)
{
	g_return_if_fail (IS_GO_FILE_OPENER (fo));
	g_return_if_fail (GSF_IS_INPUT (input));

	GO_FILE_OPENER_METHOD (fo, open) (fo, opt_enc, io_context, FIXME_FIXME_workbook_view, input);
}

/*
 * GOFileSaver
 */

static void
go_file_saver_init (GOFileSaver *fs)
{
	fs->id = NULL;
	fs->extension = NULL;
	fs->mime_type = NULL;
	fs->description = NULL;
	fs->overwrite_files = TRUE;
	fs->format_level = FILE_FL_NEW;
	fs->save_scope = FILE_SAVE_WORKBOOK;
	fs->save_func = NULL;
}

static void
go_file_saver_finalize (GObject *obj)
{
	GOFileSaver *fs;

	g_return_if_fail (IS_GO_FILE_SAVER (obj));

	fs = GO_FILE_SAVER (obj);
	g_free (fs->id);
	g_free (fs->extension);
	g_free (fs->description);

	G_OBJECT_CLASS (g_type_class_peek (G_TYPE_OBJECT))->finalize (obj);
}

static void
go_file_saver_save_real (GOFileSaver const *fs, IOContext *io_context,
			  gconstpointer FIXME_FIXME_workbook_view, GsfOutput *output)
{
	if (fs->save_func == NULL) {
		gnumeric_io_error_unknown (io_context);
		return;
	}

	fs->save_func (fs, io_context, FIXME_FIXME_workbook_view, output);
}

static void
go_file_saver_class_init (GOFileSaverClass *klass)
{
	G_OBJECT_CLASS (klass)->finalize = go_file_saver_finalize;

	klass->save = go_file_saver_save_real;
}

GSF_CLASS (GOFileSaver, go_file_saver,
	   go_file_saver_class_init, go_file_saver_init,
	   G_TYPE_OBJECT)

/**
 * go_file_saver_setup:
 * @fs          : Newly created GOFileSaver object
 * @id          : Optional ID of the saver (or NULL)
 * @extension   : Optional default extension of saved files (or NULL)
 * @description : Description of supported file format
 * @level       : File format level
 * @save_func   : Pointer to "save" function
 *
 * Sets up GOFileSaver object, newly created with g_object_new function.
 * This is intended to be used only by GOFileSaver derivates.
 * Use go_file_saver_new, if you want to create GOFileSaver object.
 */
void
go_file_saver_setup (GOFileSaver *fs, gchar const *id,
                       gchar const *extension,
                       gchar const *description,
                       FileFormatLevel level,
                       GOFileSaverSaveFunc save_func)
{
	g_return_if_fail (IS_GO_FILE_SAVER (fs));

	fs->id = g_strdup (id);
	fs->mime_type = NULL;

//#warning mime disabled
#if 0
	gchar *tmp = g_strdup_printf ("SomeFile.%s", extension);
	gnome_mime_type_or_default (tmp,
	    "application/application/x-gnumeric");
	g_free (tmp);
#endif
	fs->extension = g_strdup (extension);
	fs->description = g_strdup (description);
	fs->format_level = level;
	fs->save_func = save_func;
}

/**
 * go_file_saver_new:
 * @id          : Optional ID of the saver (or NULL)
 * @extension   : Optional default extension of saved files (or NULL)
 * @description : Description of supported file format
 * @level       : File format level
 * @save_func   : Pointer to "save" function
 *
 * Creates new GOFileSaver object. Optional @id will be used
 * after registering it with go_file_saver_register or
 * go_file_saver_register_as_default function.
 *
 * Return value: newly created GOFileSaver object.
 */
GOFileSaver *
go_file_saver_new (gchar const *id,
                     gchar const *extension,
                     gchar const *description,
                     FileFormatLevel level,
                     GOFileSaverSaveFunc save_func)
{
	GOFileSaver *fs;

	fs = GO_FILE_SAVER (g_object_new (TYPE_GO_FILE_SAVER, NULL));
	go_file_saver_setup (fs, id, extension, description, level, save_func);

	return fs;
}

void
go_file_saver_set_save_scope (GOFileSaver *fs, FileSaveScope scope)
{
	g_return_if_fail (IS_GO_FILE_SAVER (fs));
	g_return_if_fail (scope < FILE_SAVE_LAST);

	fs->save_scope = scope;
}

FileSaveScope
go_file_saver_get_save_scope (GOFileSaver const *fs)
{
	g_return_val_if_fail (IS_GO_FILE_SAVER (fs), FILE_SAVE_WORKBOOK);

	return fs->save_scope;
}

gchar const *
go_file_saver_get_id (GOFileSaver const *fs)
{
	g_return_val_if_fail (IS_GO_FILE_SAVER (fs), NULL);

	return fs->id;
}

gchar const *
go_file_saver_get_mime_type (GOFileSaver const *fs)
{
	g_return_val_if_fail (IS_GO_FILE_SAVER (fs), NULL);

	return fs->mime_type;
}

gchar const *
go_file_saver_get_extension (GOFileSaver const *fs)
{
	g_return_val_if_fail (IS_GO_FILE_SAVER (fs), NULL);

	return fs->extension;
}

gchar const *
go_file_saver_get_description (GOFileSaver const *fs)
{
	g_return_val_if_fail (IS_GO_FILE_SAVER (fs), NULL);

	return fs->description;
}

FileFormatLevel
go_file_saver_get_format_level (GOFileSaver const *fs)
{
	g_return_val_if_fail (IS_GO_FILE_SAVER (fs), FILE_FL_NEW);

	return fs->format_level;
}

/**
 * go_file_saver_save:
 * @fs          : GOFileSaver object
 * @io_context  : Context for i/o operation
 * @wbv         : Workbook View
 * @output      : Output stream
 *
 * Saves @wbv and the workbook it is attached to into @output stream.
 * Results are reported using @io_context object, use
 * gnumeric_io_error_occurred to find out if operation was successful.
 * It's possible that @file_name is created and contain some data if
 * operation fails, you should remove the file in that case.
 */
void
go_file_saver_save (GOFileSaver const *fs, IOContext *io_context,
		     gconstpointer FIXME_FIXME_workbook_view,
		     GsfOutput *output)
{
	char *file_name;

	g_return_if_fail (IS_GO_FILE_SAVER (fs));
	g_return_if_fail (GSF_IS_OUTPUT (output));

	if (GSF_IS_OUTPUT_STDIO (output)) {
		file_name = (char *) gsf_output_name (output);

		if (file_name == NULL) {
			ErrorInfo *save_error = error_info_new_str(
				_("Not a valid UTF-8 filename."));
			gnumeric_io_error_info_set (io_context, save_error);
			return;
		}
		
		if (!fs->overwrite_files &&
		    g_file_test ((file_name), G_FILE_TEST_EXISTS)) {
			ErrorInfo *save_error;

			save_error = error_info_new_str_with_details (
				_("Saving over old files of this type is disabled for safety."),
				error_info_new_str (
					_("You can turn this safety feature off by editing appropriate plugin.xml file.")));
			gnumeric_io_error_info_set (io_context, save_error);
			return;
		}
	}

	GO_FILE_SAVER_METHOD (fs, save) (fs, io_context, FIXME_FIXME_workbook_view, output);
}

/**
 * go_file_saver_set_overwrite_files:
 * @fs          : GOFileSaver object
 * @overwrite   : A boolean value saying whether the saver should overwrite
 *                existing files.
 *
 * Changes behaviour of the saver when saving a file. If @overwrite is set
 * to TRUE, existing file will be overwritten. Otherwise, the saver will
 * report an error without saving anything.
 */
void
go_file_saver_set_overwrite_files (GOFileSaver *fs, gboolean overwrite)
{
	g_return_if_fail (IS_GO_FILE_SAVER (fs));

	fs->overwrite_files = overwrite;
}


/*
 * ------
 */

typedef struct {
	gint priority;
	GOFileSaver *saver;
} DefaultFileSaver;

static GHashTable *file_opener_id_hash = NULL,
                  *file_saver_id_hash = NULL;
static GList *file_opener_list = NULL, *file_opener_priority_list = NULL;
static GList *file_saver_list = NULL, *default_file_saver_list = NULL;

static gint
cmp_int_less_than (gconstpointer list_i, gconstpointer i)
{
	return !(GPOINTER_TO_INT (list_i) < GPOINTER_TO_INT (i));
}

/**
 * go_file_opener_register:
 * @fo          : GOFileOpener object
 * @priority    : Opener's priority
 *
 * Adds @fo opener to the list of available file openers, making it
 * available for Gnumeric i/o routines. The opener is registered with given
 * @priority. The priority is used to determine the order in which openers
 * will be tried when reading a file. The higher the priority, the sooner it
 * will be tried. Default XML-based Gnumeric file opener is registered at
 * priority 50. Recommended range for @priority is [0, 100].
 * Reference count for the opener is incremented inside the function, but
 * you don't have to (and shouldn't) call g_object_unref on it if it's
 * floating object (for example, when you pass object newly created with
 * go_file_opener_new and not referenced anywhere).
 */
void
go_file_opener_register (GOFileOpener *fo, gint priority)
{
	gint pos;
	gchar const *id;

	g_return_if_fail (IS_GO_FILE_OPENER (fo));
	g_return_if_fail (priority >=0 && priority <= 100);

	pos = go_list_index_custom (file_opener_priority_list,
	                           GINT_TO_POINTER (priority),
	                           cmp_int_less_than);
	file_opener_priority_list = g_list_insert (
	                            file_opener_priority_list,
	                            GINT_TO_POINTER (priority), pos);
	file_opener_list = g_list_insert (file_opener_list, fo, pos);
	g_object_ref (G_OBJECT (fo));

	id = go_file_opener_get_id (fo);
	if (id != NULL) {
		if (file_opener_id_hash	== NULL)
			file_opener_id_hash = g_hash_table_new (&g_str_hash, &g_str_equal);
		g_hash_table_insert (file_opener_id_hash, (gpointer) id, fo);
	}
}

/**
 * go_file_opener_unregister:
 * @fo          : GOFileOpener object previously registered using
 *                go_file_opener_register
 *
 * Removes @fo opener from list of available file openers. Reference count
 * for the opener is decremented inside the function.
 */
void
go_file_opener_unregister (GOFileOpener *fo)
{
	gint pos;
	GList *l;
	gchar const *id;

	g_return_if_fail (IS_GO_FILE_OPENER (fo));

	pos = g_list_index (file_opener_list, fo);
	g_return_if_fail (pos != -1);
	l = g_list_nth (file_opener_list, pos);
	file_opener_list = g_list_remove_link (file_opener_list, l);
	g_list_free_1 (l);
	l = g_list_nth (file_opener_priority_list, pos);
	file_opener_priority_list = g_list_remove_link (file_opener_priority_list, l);
	g_list_free_1 (l);

	id = go_file_opener_get_id (fo);
	if (id != NULL) {
		g_hash_table_remove (file_opener_id_hash, (gpointer) id);
		if (g_hash_table_size (file_opener_id_hash) == 0) {
			g_hash_table_destroy (file_opener_id_hash);
			file_opener_id_hash = NULL;
		}
	}

	g_object_unref (G_OBJECT (fo));
}

static gint
default_file_saver_cmp_priority (gconstpointer a, gconstpointer b)
{
	DefaultFileSaver const *dfs_a = a, *dfs_b = b;

	return dfs_b->priority - dfs_a->priority;
}

/**
 * go_file_saver_register:
 * @fs          : GOFileSaver object
 *
 * Adds @fs saver to the list of available file savers, making it
 * available for the user when selecting file format for save.
 */
void
go_file_saver_register (GOFileSaver *fs)
{
	gchar const *id;

	g_return_if_fail (IS_GO_FILE_SAVER (fs));

	file_saver_list = g_list_prepend (file_saver_list, fs);
	g_object_ref (G_OBJECT (fs));

	id = go_file_saver_get_id (fs);
	if (id != NULL) {
		if (file_saver_id_hash	== NULL)
			file_saver_id_hash = g_hash_table_new (&g_str_hash,
							       &g_str_equal);
		g_hash_table_insert (file_saver_id_hash, (gpointer) id, fs);
	}
}

/**
 * go_file_saver_register_as_default:
 * @fs          : GOFileSaver object
 * @priority    : Saver's priority
 *
 * Adds @fs saver to the list of available file savers, making it
 * available for the user when selecting file format for save.
 * The saver is also marked as default saver with given priority.
 * When Gnumeric needs default file saver, it chooses the one with the
 * highest priority. Recommended range for @priority is [0, 100].
 */
void
go_file_saver_register_as_default (GOFileSaver *fs, gint priority)
{
	DefaultFileSaver *dfs;

	g_return_if_fail (IS_GO_FILE_SAVER (fs));
	g_return_if_fail (priority >=0 && priority <= 100);

	go_file_saver_register (fs);

	dfs = g_new (DefaultFileSaver, 1);
	dfs->priority = priority;
	dfs->saver = fs;
	default_file_saver_list = g_list_insert_sorted (
	                          default_file_saver_list, dfs,
	                          default_file_saver_cmp_priority);
}

/**
 * go_file_saver_unregister:
 * @fs          : GOFileSaver object previously registered using
 *                go_file_saver_register or go_file_saver_register_as_default
 *
 * Removes @fs saver from list of available file savers. Reference count
 * for the saver is decremented inside the function.
 */
void
go_file_saver_unregister (GOFileSaver *fs)
{
	GList *l;
	gchar const *id;

	g_return_if_fail (IS_GO_FILE_SAVER (fs));

	l = g_list_find (file_saver_list, fs);
	g_return_if_fail (l != NULL);
	file_saver_list = g_list_remove_link (file_saver_list, l);
	g_list_free_1 (l);

	id = go_file_saver_get_id (fs);
	if (id != NULL) {
		g_hash_table_remove (file_saver_id_hash, (gpointer) id);
		if (g_hash_table_size (file_saver_id_hash) == 0) {
			g_hash_table_destroy (file_saver_id_hash);
			file_saver_id_hash = NULL;
		}
	}

	for (l = default_file_saver_list; l != NULL; l = l->next) {
		if (((DefaultFileSaver *) l->data)->saver == fs) {
			default_file_saver_list = g_list_remove_link (default_file_saver_list, l);
			g_free (l->data);
			g_list_free_1 (l);
			break;
		}
	}

	g_object_unref (G_OBJECT (fs));
}

/**
 * go_file_saver_get_default:
 *
 * Returns file saver registered as default saver with the highest priority.
 * Reference count for the saver is NOT incremented.
 *
 * Return value: GOFileSaver object or NULL if default saver is not
 *               available.
 */
GOFileSaver *
go_file_saver_get_default (void)
{
	if (default_file_saver_list == NULL)
		return NULL;

	return ((DefaultFileSaver *) default_file_saver_list->data)->saver;
}

/**
 * go_file_saver_for_mime_type:
 * @mime_type: A mime type
 *
 * Returns a file saver that claims to save files with given mime type.
 *
 * Return value: GOFileSaver object or NULL if no suitable file saver could
 *               be found.
 */
GOFileSaver *
go_file_saver_for_mime_type (gchar const *mime_type)
{
	GList *l;

	for (l = file_saver_list; l != NULL; l = l->next) {
		if (!strcmp (go_file_saver_get_mime_type (l->data), mime_type)) {
			return (l->data);
		}
	}
	return (NULL);
}

/**
 * go_file_saver_for_file_name :
 * @file_name :
 *
 * Searches for file opener with given @filename, registered using
 * go_file_opener_register
 *
 * Return value: GOFileOpener object or NULL if opener cannot be found.
 **/
GOFileSaver *
go_file_saver_for_file_name (char const *file_name)
{
	GList *l;
	char const *extension = gsf_extension_pointer (file_name);

	for (l = file_saver_list; l != NULL; l = l->next)
		if (!strcmp (go_file_saver_get_extension (l->data), extension))
			return l->data;
	return NULL;
}

/**
 * go_file_opener_for_id:
 * @id          : File opener's ID
 *
 * Searches for file opener with given @id, registered using
 * go_file_opener_register
 *
 * Return value: GOFileOpener object or NULL if opener cannot be found.
 */
GOFileOpener *
go_file_opener_for_id (gchar const *id)
{
	g_return_val_if_fail (id != NULL, NULL);

	if (file_opener_id_hash == NULL)
		return NULL;
	return GO_FILE_OPENER (g_hash_table_lookup (file_opener_id_hash, id));
}

/**
 * go_file_saver_for_id:
 * @id          : File saver's ID
 *
 * Searches for file saver with given @id, registered using
 * go_file_saver_register or register_file_opener_as_default.
 *
 * Return value: GOFileSaver object or NULL if saver cannot be found.
 */
GOFileSaver *
go_file_saver_for_id (gchar const *id)
{
	g_return_val_if_fail (id != NULL, NULL);

	if (file_saver_id_hash == NULL)
		return NULL;
	return GO_FILE_SAVER (g_hash_table_lookup (file_saver_id_hash, id));
}

/**
 * get_file_savers:
 *
 * Returns the list of registered file savers (using go_file_saver_register or
 * go_file_saver_register_as_default).
 *
 * Return value: list of GOFileSaver objects, which you shouldn't modify.
 */
GList *
get_file_savers (void)
{
	return file_saver_list;
}

/**
 * get_file_openers:
 *
 * Returns the list of registered file openers (using go_file_opener_register).
 *
 * Return value: list of GOFileOpener objects, which you shouldn't modify.
 */
GList *
get_file_openers (void)
{
	return file_opener_list;
}
