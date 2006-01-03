#include <glib-object.h>

#ifndef GSF_BLOB_H
#define GSF_BLOB_H

G_BEGIN_DECLS

#define GSF_TYPE_BLOB			(gsf_blob_get_type ())
#define GSF_BLOB(obj)		  	(G_TYPE_CHECK_INSTANCE_CAST ((obj), GSF_TYPE_BLOB, GsfBlob))
#define GSF_BLOB_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), GSF_TYPE_BLOB, GsfBlobClass))
#define GSF_IS_BLOB(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSF_TYPE_BLOB))
#define GSF_IS_BLOB_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GSF_TYPE_BLOB))
#define GSF_BLOB_GET_CLASS(obj)         (G_TYPE_INSTANCE_GET_CLASS ((obj), GSF_TYPE_BLOB, GsfBlobClass))

typedef struct _GsfBlob GsfBlob;
typedef struct _GsfBlobClass GsfBlobClass;
typedef struct _GsfBlobPrivate GsfBlobPrivate;

struct _GsfBlob {
	GObject object;
	GsfBlobPrivate *priv;
};

struct _GsfBlobClass {
	GObjectClass parent_class;
};

GType gsf_blob_get_type (void) G_GNUC_CONST;

GsfBlob *gsf_blob_new (gsize size,
		       gconstpointer data_to_copy,
		       GError **error);

gsize gsf_blob_get_size (GsfBlob *blob);

gconstpointer gsf_blob_peek_data (GsfBlob *blob);

G_END_DECLS

#endif
