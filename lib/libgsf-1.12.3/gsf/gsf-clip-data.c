#include "gsf-config.h"
#include <glib/gi18n-lib.h>
#include "gsf-clip-data.h"
#include "gsf-utils.h"

/* Private part of the GsfClipData structure */
struct _GsfClipDataPrivate {
	GsfClipFormat format;
	GsfBlob *data_blob;
};

G_DEFINE_TYPE (GsfClipData, gsf_clip_data, G_TYPE_OBJECT);

static void gsf_clip_data_finalize (GObject *object);

static void
gsf_clip_data_class_init (GsfClipDataClass *class)
{
	GObjectClass *object_class;

	object_class = (GObjectClass *) class;

	object_class->finalize = gsf_clip_data_finalize;
}

static void
gsf_clip_data_init (GsfClipData *clip_data)
{
	GsfClipDataPrivate *priv;

	priv = g_new0 (GsfClipDataPrivate, 1);
	clip_data->priv = priv;
}

static void
gsf_clip_data_finalize (GObject *object)
{
	GsfClipData *clip_data;
	GsfClipDataPrivate *priv;

	clip_data = GSF_CLIP_DATA (object);
	priv = clip_data->priv;

	if (priv->data_blob)
		g_object_unref (priv->data_blob);

	G_OBJECT_CLASS (gsf_clip_data_parent_class)->finalize (object);
}

/**
 * gsf_clip_data_new:
 * @format: Format for the data inside the @data_blob
 * @data_blob: Object which holds the binary contents for the #GsfClipData
 *
 * Creates a new #GsfClipData object.  This function acquires a reference to the
 * @data_blob, so you should unref the blob on your own if you no longer need it
 * directly.
 *
 * Return value: A newly-created #GsfClipData.
 **/
GsfClipData *
gsf_clip_data_new (GsfClipFormat format, GsfBlob *data_blob)
{
	GsfClipData *clip_data;
	GsfClipDataPrivate *priv;

	g_return_val_if_fail (GSF_IS_BLOB (data_blob), NULL);

	clip_data = g_object_new (GSF_TYPE_CLIP_DATA,
				  NULL);
	priv = clip_data->priv;

	priv->format = format;
	priv->data_blob = g_object_ref (data_blob);

	return clip_data;
}

/**
 * gsf_clip_data_get_format:
 * @clip_data: A #GsfClipData.
 *
 * Queries the clipboard data format of a #GsfClipData.  The format refers to the data
 * blob inside the @clip_data; use gsf_clip_data_get_data_blob() to get that data blob.
 *
 * Return value: The format in which the #GsfClipData's data blob is stored.
 **/
GsfClipFormat
gsf_clip_data_get_format (GsfClipData *clip_data)
{
	GsfClipDataPrivate *priv;

	g_return_val_if_fail (GSF_IS_CLIP_DATA (clip_data), GSF_CLIP_FORMAT_UNKNOWN);

	priv = clip_data->priv;
	return priv->format;
}

/**
 * gsf_clip_data_get_data_blob:
 * @clip_data: A #GsfClipData.
 *
 * Queries the data blob that actually stores a #GsfClipData's binary data.
 *
 * Return value: A new reference to the #GsfBlob that stores this @clip_data's
 * binary data.  You must use g_object_unref() to dispose of that data blob when
 * you are done with it.
 **/
GsfBlob *
gsf_clip_data_get_data_blob (GsfClipData *clip_data)
{
	GsfClipDataPrivate *priv;

	g_return_val_if_fail (GSF_IS_CLIP_DATA (clip_data), NULL);

	priv = clip_data->priv;
	return g_object_ref (priv->data_blob);
}

static void
set_error_missing_clipboard_data (GError **error, const char *format_name, gsize at_least_size)
{
	g_set_error (error,
		     GSF_ERROR,
		     GSF_ERROR_INVALID_DATA,
		     _("The clip_data is in %s, but it is smaller than "
		       "at least %" G_GSIZE_FORMAT " bytes"),
		     format_name,
		     at_least_size);
}

static gsize
get_windows_clipboard_data_offset (GsfClipFormatWindows format)
{
	struct format_offset_pair {
		GsfClipFormatWindows format;
		gsize offset;
	};

	static const struct format_offset_pair pairs[] = {
		{ GSF_CLIP_FORMAT_WINDOWS_UNKNOWN, 4 },
		{ GSF_CLIP_FORMAT_WINDOWS_METAFILE, 12 },
		{ GSF_CLIP_FORMAT_WINDOWS_DIB, 4 },
		{ GSF_CLIP_FORMAT_WINDOWS_ENHANCED_METAFILE, 4 } /* FIXME: does this have a PACKEDMETA in front
								  * as well, similar to GSF_CLIP_FORMAT_WINDOWS_METAFILE? */
	};
	static const int num_pairs = G_N_ELEMENTS (pairs);

	int i;

	for (i = 0; i < num_pairs; i++)
		if (pairs[i].format == format)
			return pairs[i].offset;

	g_assert_not_reached ();
	return 0;
}

/* Checks that the specified blob size matches the expected size for the format.
 * Returns the same format if the size is correct, or
 * GSF_CLIP_FORMAT_WINDOWS_ERROR if the size is too small.
 */
static GsfClipFormatWindows
check_format_windows (GsfClipFormatWindows format, const char *format_name, gsize blob_size, GError **error)
{
	gsize offset;

	offset = get_windows_clipboard_data_offset (format);
	if (blob_size <= offset) {
		set_error_missing_clipboard_data (error, format_name, offset + 1);
		format = GSF_CLIP_FORMAT_WINDOWS_ERROR;
	}

	return format;
}

/**
 * gsf_clip_data_get_windows_clipboard_format:
 * @clip_data: A #GsfClipData.
 * @error: Location to store error, or %NULL
 *
 * Queries the Windows clipboard data format for a #GsfClipData.  The @clip_data must
 * have been created with #GSF_CLIP_FORMAT_WINDOWS_CLIPBOARD.
 *
 * Return value: A #GsfClipFormatWindows value.
 *
 * Possible errors: #GSF_ERROR_INVALID_DATA if the data blob in the @clip_data is
 * smaller than it should be; in this case GSF_CLIP_FORMAT_WINDOWS_ERROR will be returned.
 **/
GsfClipFormatWindows
gsf_clip_data_get_windows_clipboard_format (GsfClipData *clip_data, GError **error)
{
	GsfClipDataPrivate *priv;
	gsize size;
	guint32 value;
	gconstpointer data;
	GsfClipFormatWindows format;

	g_return_val_if_fail (GSF_IS_CLIP_DATA (clip_data), GSF_CLIP_FORMAT_WINDOWS_ERROR);
	g_return_val_if_fail (error == NULL || *error == NULL, GSF_CLIP_FORMAT_WINDOWS_ERROR);

	priv = clip_data->priv;
	g_return_val_if_fail (priv->format == GSF_CLIP_FORMAT_WINDOWS_CLIPBOARD, GSF_CLIP_FORMAT_WINDOWS_ERROR);

	size = gsf_blob_get_size (priv->data_blob);

	if (size < 4) {
		g_set_error (error,
			     GSF_ERROR,
			     GSF_ERROR_INVALID_DATA,
			     _("The clip_data is in Windows clipboard format, but it is smaller than "
			       "the required 4 bytes."));
		return GSF_CLIP_FORMAT_WINDOWS_ERROR;
	}

	data = gsf_blob_peek_data (priv->data_blob);

	value = GSF_LE_GET_GUINT32 (data);

	switch (value) {
	case GSF_CLIP_FORMAT_WINDOWS_METAFILE:
		format = check_format_windows (GSF_CLIP_FORMAT_WINDOWS_METAFILE, _("Windows Metafile format"),
					       size, error);
		break;

	case GSF_CLIP_FORMAT_WINDOWS_DIB:
	case 2: /* CF_BITMAP */
		format = check_format_windows (GSF_CLIP_FORMAT_WINDOWS_DIB, _("Windows DIB or BITMAP format"),
					       size, error);
		break;

	case GSF_CLIP_FORMAT_WINDOWS_ENHANCED_METAFILE:
		format = check_format_windows (GSF_CLIP_FORMAT_WINDOWS_ENHANCED_METAFILE, _("Windows Enhanced Metafile format"),
					       size, error);
		break;

	default:
		format = GSF_CLIP_FORMAT_WINDOWS_UNKNOWN;
		break;
	}

	return format;
}

/**
 * gsf_clip_data_peek_real_data:
 * @clip_data: A #GsfClipData.
 * @ret_size: Location to return the size of the returned data buffer.
 * @error: Location to store error, or %NULL.
 * 
 * Queries a pointer directly to the clipboard data of a #GsfClipData.  The
 * resulting pointer is not necessarily the same data pointer that was passed to
 * gsf_blob_new() prior to creating the @clip_data.  For example, if the data is
 * in #GSF_CLIP_FORMAT_WINDOWS_CLIPBOARD format, then it will have extra header
 * bytes in front of the actual metafile data.  This function will skip over
 * those header bytes if necessary and return a pointer to the "real" data.
 * 
 * Return value: Pointer to the real clipboard data.  The size in bytes of this
 * buffer is returned in the @ret_size argument.
 **/
gconstpointer
gsf_clip_data_peek_real_data (GsfClipData *clip_data, gsize *ret_size, GError **error)
{
	GsfClipDataPrivate *priv;
	gconstpointer data;
	gsize offset;

	g_return_val_if_fail (GSF_IS_CLIP_DATA (clip_data), NULL);
	g_return_val_if_fail (ret_size != NULL, NULL);
	g_return_val_if_fail (error == NULL || *error == NULL, NULL);

	priv = clip_data->priv;

	data = gsf_blob_peek_data (priv->data_blob);

	if (priv->format == GSF_CLIP_FORMAT_WINDOWS_CLIPBOARD) {
		GsfClipFormatWindows win_format;

		win_format = gsf_clip_data_get_windows_clipboard_format (clip_data, error);
		if (win_format == GSF_CLIP_FORMAT_WINDOWS_ERROR)
			return NULL;

		/* gsf_clip_data_get_windows_clipboard_format() already did the size checks for us,
		 * so we can jump to the offset right away without doing extra checks.
		 */

		offset = get_windows_clipboard_data_offset (win_format);
	} else
		offset = 0;

	*ret_size = gsf_blob_get_size (priv->data_blob) - offset;
	return (char *) data + offset; /* cast to avoid warning about void pointer arithmetic */
}
