#include "config.h"

#include "gnc-druid.h"
#include "gnc-druid-provider-desc-edge.h"
#include "gnc-druid-provider-desc-file.h"
#include "gnc-druid-provider-desc-multifile.h"
#include "gnc-import-desc-format.h"
#include "gnc-druid-test.h"

#include <stdio.h>

typedef struct _GncDruidTest
{
    GList *files;
} GncDruidTest;

static GList *
gnc_dt_test_get_files(gpointer ctx)
{
    GncDruidTest *gdt = ctx;
    return gdt->files;
}

static const gchar *
gnc_dt_test_get_filename(gpointer ctx, gpointer file)
{
    return file;
}

static void
gnc_dt_test_remove_file(gpointer ctx, gpointer file)
{
    GncDruidTest *gdt = ctx;
    gdt->files = g_list_remove(gdt->files, file);
    g_free(file);
}

static gboolean
gnc_dt_test_add_file(GNCDruidCB *cb)
{
    GNCDruidProviderFileCB *cb_f = GNC_DRUID_PROVIDER_FILE_CB(cb);
    GncDruidTest *gdt = cb->druid_ctx->be_ctx;

    gdt->files = g_list_prepend(gdt->files, g_strdup(cb_f->filename));
    cb_f->this_file = gdt->files->data;
    return TRUE;
}

static gboolean
gnc_dt_test_files_done(GNCDruidCB *cb)
{
    GncDruidTest *gdt = cb->druid_ctx->be_ctx;
    fprintf(stderr, "Done adding files: %d files..\n", g_list_length(gdt->files));
    return TRUE;
}

static GncImportFormat
gnc_dt_test_get_formats(GNCImportFormatCB *cb)
{
    return GNCIF_DATE_YMD | GNCIF_DATE_YDM;
}

static const gchar *
gnc_dt_test_get_sample(GNCImportFormatCB *cb)
{
    return "2004-10-11";
}

static gboolean
gnc_dt_test_formats_done(GNCDruidCB *cb)
{
    GNCImportFormatCB *cb_f = GNC_IMPORT_FORMAT_CB(cb);
    fprintf(stderr, "User selected format: %d\n", cb_f->format);
    return TRUE;
}

static GList *
gnc_dt_test_build_providers(void)
{
    GList *list = NULL;
    GNCDruidProviderDescFile *desc_file;
    GNCDruidProviderDescMultifile *desc_multifile;

    /* Build the provider list in reverse order, because g_list_append is slow */
    list =
        g_list_prepend(list,
                       gnc_druid_provider_desc_edge_new_with_data(GNC_DPE_LAST,
                               "Test Druid End",
                               "Click finish to close the druid."));

    desc_file =
        gnc_druid_provider_desc_file_new_with_data("Test Druid New File",
                "Choose a file",
                "test-druid-history",
                "/tmp",
                TRUE,
                gnc_dt_test_add_file,
                gnc_dt_test_remove_file);

    desc_multifile =
        gnc_druid_provider_desc_multifile_new_with_data("Test Druid Files",
                NULL,
                desc_file,
                gnc_dt_test_files_done,
                gnc_dt_test_get_files,
                gnc_dt_test_get_filename);
    desc_file->multifile_provider = desc_multifile;

    list = g_list_prepend(list, desc_multifile);
    list = g_list_prepend(list,
                          gnc_import_desc_format_new_with_data("Formats are Ambiguous",
                                  "Choose your format!",
                                  gnc_dt_test_formats_done,
                                  gnc_dt_test_get_formats,
                                  gnc_dt_test_get_sample));

    list = g_list_prepend(list, desc_file);
    list =
        g_list_prepend(list,
                       gnc_druid_provider_desc_edge_new_with_data(GNC_DPE_FIRST,
                               "Test Druid Start",
                               "This is a test druid.  It doesn't do much."));

    return list;
}

static gboolean
gnc_dt_test_finish_cb(gpointer ctx)
{
    fprintf(stderr, "finish callback called...\n");
    return TRUE;
}

static void
gnc_dt_test_cancel_cb(gpointer ctx)
{
    GncDruidTest *gdt = ctx;
    GList *node;

    fprintf(stderr, "cancel callback called...(%p)\n", gdt);

    for (node = gdt->files; node; node = node->next)
        g_free(node->data);

    g_list_free(gdt->files);
    g_free(gdt);
}

GNCDruid *
gnc_druid_gnome_test(void)
{
    GNCDruid * druid;
    GList *providers;
    GncDruidTest *gdt = g_new0(GncDruidTest, 1);

    providers = gnc_dt_test_build_providers();

    druid = gnc_druid_new("This is a test druid title",
                          providers, gdt,
                          gnc_dt_test_finish_cb, gnc_dt_test_cancel_cb);
    return druid;
}
