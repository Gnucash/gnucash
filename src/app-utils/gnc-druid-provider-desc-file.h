

#ifndef GNC_DRUID_PROVIDER_DESC_FILE_H
#define GNC_DRUID_PROVIDER_DESC_FILE_H

#ifdef __cplusplus
//extern "C" {
#endif

#include <glib.h>
#include <glib-object.h>

#define G_TYPE_GNC_DRUID_PROVIDER_DESC_FILE	gnc_druid_provider_desc_file_get_type()
#define GNC_DRUID_PROVIDER_DESC_FILE(obj)	G_TYPE_CHECK_INSTANCE_CAST((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_FILE, GNCDruidProviderDescFile)
#define IS_GNC_DRUID_PROVIDER_DESC_FILE(obj)	G_TYPE_CHECK_INSTANCE_TYPE((obj), G_TYPE_GNC_DRUID_PROVIDER_DESC_FILE)

typedef struct _GNCDruidProviderDescFile GNCDruidProviderDescFile;
typedef struct _GNCDruidProviderDescFileClass GNCDruidProviderDescFileClass;

#include "gnc-druid.h"
#include "gnc-druid-provider-file-cb.h"

#define GNC_DRUID_PROVIDER_TYPE_FILE	"file"

struct _GNCDruidProviderDescFile
{
  GNCDruidProviderDesc parent;

  gchar* text;
  gchar* last_directory;
  void (*remove_file)(gpointer be_ctx, gpointer file_ctx);
};

struct _GNCDruidProviderDescFileClass
{
  GNCDruidProviderDescClass parent;
};

GType	gnc_druid_provider_desc_file_get_type(void);
GNCDruidProviderDescFile* gnc_druid_provider_desc_file_new(void);
GNCDruidProviderDescFile*
gnc_druid_provider_desc_file_new_with_data(const gchar* title,
					   const gchar* text,
					   const gchar* last_dir,
					   GNCDruidProviderCB next_cb,
					   void (*remove_file)(gpointer, gpointer));

void	gnc_druid_provider_desc_file_set_text(GNCDruidProviderDescFile*,
					       const gchar* text);
void	gnc_druid_provider_desc_file_set_last_dir(GNCDruidProviderDescFile*,
						  const gchar* last_dir);

/* methods */

#ifdef __cplusplus
//}
#endif

#endif /* GNC_DRUID_PROVIDER_DESC_FILE_H */
