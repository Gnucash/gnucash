#include "config.h"
#include <stdlib.h>
#include <string.h>

#include "io-gncxml-v2.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#define FILENAME "Money95bank_fr.gml2"

int
main(int argc, char **argv)
{
    const char *directory = g_getenv("GNC_TEST_FILES");
    char *filename;

    if (!directory)
    {
        directory = "../../../../accounts/C";
    }

    filename = malloc(strlen(directory) + 1 + strlen(FILENAME) + 1);
    sprintf(filename, "%s/%s", directory, FILENAME);
    do_test(gnc_is_xml_data_file_v2(filename, NULL), "gnc_is_xml_data_file_v2");

    print_test_results();
    exit(get_rv());
}
