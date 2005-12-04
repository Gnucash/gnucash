#include "gsf-config.h"
#include <glib/gi18n-lib.h>
#include <string.h>
#include "gsf-utils.h"
#include "gsf-blob.h"

/* Private part of the GsfBlob structure */
struct _GsfBlobPrivate {
	gsize size;
	gpointer data;
};

G_DEFINE_TYPE (GsfBlob, gsf_blob, G_TYPE_OBJECT);

static void gsf_blob_finalize (GObject *object);

static void
gsf_blob_class_init (GsfBlobClass *class)
{
	GObjectClass *object_class;

	object_class = (GObjectClass *) class;

	object_class->finalize = gsf_blob_finalize;
}

static void
gsf_blob_init (GsfBlob *blob)
{
	GsfBlobPrivate *priv;

	priv = g_new0 (GsfBlobPrivate, 1);
	blob->priv = priv;
}

static void
gsf_blob_finalize (GObject *object)
{
	GsfBlob *blob;
	GsfBlobPrivate *priv;

	blob = GSF_BLOB (object);
	priv = blob->priv;

	g_free (priv->data);
	g_free (priv);

	G_OBJECT_CLASS (gsf_blob_parent_class)->finalize (object);
}

/**
 * gsf_blob_new:
 * @size: Size of the data in bytes.
 * @data_to_copy: Data which will be copied into the blob, or %NULL if @size is zero.
 * @error: location to store error, or %NULL.
 * 
 * Creates a new #GsfBlob object to hold the specified data.  The blob can then
 * be used as a facility for reference-counting for the data.  The data is
 * copied internally, so the blob does not hold references to external chunks
 * of memory.
 * 
 * Return value: A newly-created #GsfBlob, or %NULL if the data could not be copied.
 *
 * Error domain: #GSF_ERROR
 *
 * Possible errors: #GSF_ERROR_OUT_OF_MEMORY if the @data_to_copy could not be copied.
 **/
GsfBlob *
gsf_blob_new (gsize size, gconstpointer data_to_copy, GError **error)
{
	GsfBlob *blob;
	GsfBlobPrivate *priv;
	gpointer data;

	g_return_val_if_fail ((size > 0 && data_to_copy != NULL) || (size == 0 && data_to_copy == NULL), NULL);
	g_return_val_if_fail (error == NULL || *error == NULL, NULL);

	if (data_to_copy) {
		data = g_try_malloc (size);
		if (!data) {
			g_set_error (error,
				     GSF_ERROR,
				     GSF_ERROR_OUT_OF_MEMORY,
				     _("Not enough memory to copy %" G_GSIZE_FORMAT " bytes of data"),
				     size);
			return NULL;
		}

		memcpy (data, data_to_copy, size);
	} else
		data = NULL;

	blob = g_object_new (GSF_TYPE_BLOB,
			     NULL);
	priv = blob->priv;

	priv->size = size;
	priv->data = data;

	return blob;
}

/**
 * gsf_blob_get_size:
 * @blob: A #GsfBlob.
 * 
 * Queries the size in bytes of the data stored in the blob.
 * 
 * Return value: Size in bytes, or 0 if the data is %NULL.
 **/
gsize
gsf_blob_get_size (GsfBlob *blob)
{
	GsfBlobPrivate *priv;

	g_return_val_if_fail (GSF_IS_BLOB (blob), 0);

	priv = blob->priv;
	return priv->size;
}

/**
 * gsf_blob_peek_data:
 * @blob: A #GsfBlob.
 * 
 * Queries a pointer to the data stored in the blob.  This does not copy the data
 * for you; it returns a pointer to the actual buffer which the blob uses internally,
 * so you should not free this buffer on your own.
 * 
 * Return value: Pointer to the data stored in the blob, or %NULL if the size
 * of the data is zero.
 **/
gconstpointer
gsf_blob_peek_data (GsfBlob *blob)
{
	GsfBlobPrivate *priv;

	g_return_val_if_fail (GSF_IS_BLOB (blob), NULL);

	priv = blob->priv;
	return priv->data;
}
