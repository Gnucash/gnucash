#include "io-gncxml-gen.h"

gboolean
gnc_xml_parse_file(sixtp *top_parser, const char *filename,
                   gxpf_callback callback, gpointer data)
{
    gpointer parse_result = NULL;
    gxpf_data gpdata;

    gpdata.cb = callback;
    gpdata.data = data;
    
    return sixtp_parse_file(top_parser, filename, 
                            NULL, &gpdata, &parse_result);
}

