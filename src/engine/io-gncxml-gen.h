#ifndef __IO_GNCXML_GEN_H__
#define __IO_GNCXML_GEN_H__

#include <glib.h>
#include "sixtp.h"

typedef gboolean (*gxpf_callback)(const char *tag, gpointer globaldata,
                                  gpointer data);

struct gxpf_data_struct
{
    gxpf_callback cb;
    gpointer data;
};

typedef struct gxpf_data_struct gxpf_data;

gboolean
gnc_xml_parse_file(sixtp *top_parser, const char *filename,
                   gxpf_callback callback, gpointer data);

#endif /* __IO_GNCXML_GEN_H__ */
