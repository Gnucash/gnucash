#include <stdlib.h>

#include "io-gncxml-v2.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

int
main(int argc, char **argv)
{
    do_test(
        gnc_is_xml_data_file_v2("test-files/xml2/Money95bank_fr.gml2"),
        "gnc_is_xml_data_file_v2" );

    print_test_results();
    exit(get_rv());
}
