#include "config.h"

#include "gnc-druid.h"
#include "gnc-druid-provider-desc-edge.h"
#include "gnc-druid-test.h"

#include <stdio.h>

static GList *
gnc_gd_test_build_providers(void)
{
  GList *list = NULL;

  /* Build the provider list in reverse order, because g_list_append is slow */
  list =
    g_list_prepend(list,
		   gnc_druid_provider_desc_edge_new_with_data(GNC_DPE_LAST,
							      "Test Druid B",
							      "Click finish to close the druid."));

  list =
    g_list_prepend(list,
		   gnc_druid_provider_desc_edge_new_with_data(GNC_DPE_FIRST,
							      "Test Druid A",
							      "This is a test druid.  It doesn't do much."));
  
  return list;
}

static gboolean
gnc_gd_test_finish_cb(gpointer ctx)
{
  fprintf(stderr, "finish callback called...\n");
  return TRUE;
}

static void
gnc_gd_test_cancel_cb(gpointer ctx)
{
  fprintf(stderr, "cancel callback called...\n");
}

GNCDruid *
gnc_druid_gnome_test(void)
{
  GNCDruid * druid;
  GList *providers;

  providers = gnc_gd_test_build_providers();

  druid = gnc_druid_new("This is a test druid title",
			providers, NULL,
			gnc_gd_test_finish_cb, gnc_gd_test_cancel_cb);
  return druid;
}
