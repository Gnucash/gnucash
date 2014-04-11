
#include "config.h"

#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "test-file-stuff.h"

#include <stdlib.h>

#include "sixtp-utils.h"
#include "sixtp-dom-generators.h"

int
main(int argc, char **argv)
{
    int i;

    for(i = 0; i < 20; i++)
    {
        Timespec *spec1;
        Timespec spec2;
        gchar *sec_str;
        gchar *nsec_str;
        
        spec1 = get_random_timespec();

        sec_str = timespec_sec_to_string(spec1);
        nsec_str = timespec_nsec_to_string(spec1);

        if(!string_to_timespec_secs(sec_str, &spec2))
        {
            failure_args("string_to_timespec_secs", __FILE__, __LINE__, 
                         "string is %s", sec_str);
        }

        else if(!string_to_timespec_nsecs(nsec_str, &spec2))
        {
            failure_args("string_to_timespec_nsecs", __FILE__, __LINE__, 
                         "string is %s", nsec_str);
        }

        else if(spec1->tv_sec != spec2.tv_sec)
        {
            failure_args("timespec_secs", __FILE__, __LINE__, 
                         "not equal ints are %" G_GINT64_FORMAT
                         " and %" G_GINT64_FORMAT "\n",
                         spec1->tv_sec, spec2.tv_sec);
        }

        else if(spec1->tv_nsec != spec2.tv_nsec)
        {
            failure_args("timespec_nsecs", __FILE__, __LINE__, 
                         "not equal ints are %ld and %ld\n",
                         spec1->tv_nsec, spec2.tv_nsec);
        }

        else
        {
            success("timespec");
        }

        g_free(spec1);
        g_free(sec_str);
        g_free(nsec_str);
    }
    print_test_results();
    exit(get_rv());
}
