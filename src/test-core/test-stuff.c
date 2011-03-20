/*
 * Created 20010320 by bstanley to hold only those
 * testing functions which are independent of the rest of
 * the GNUCash system.
 *
 * This allows me to compile simple test programs standalone...
 *
 */


#include "config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include "test-stuff.h"

void vsuccess_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    va_list ap);

void vfailure_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    va_list ap);

static guint successes;
static guint failures;
static gboolean success_should_print = FALSE;

void
success_call(
    const char *test_title,
    const char* file,
    int line )
{
    success_args( test_title, file, line, "" );
}

void
success_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    ... )
{
    va_list ap;
    va_start(ap, format);
    vsuccess_args( test_title, file, line, format, ap );
    va_end(ap);
}

void
vsuccess_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    va_list ap)
{
    if ( success_should_print )
    {
        printf("SUCCESS: %s, %s:%d ", test_title, file, line );
        vprintf(format, ap);
        printf("\n");
        fflush(stdout);
    }
    ++successes;
}

void
failure_call(
    const char *test_title,
    const char *file,
    int line)
{
    failure_args( test_title, file, line, "" );
}


void
failure_args(
    const char *test_title,
    const char *file,
    int line,
    const char *format,
    ... )
{
    va_list ap;
    va_start(ap, format);
    vfailure_args( test_title, file, line, format, ap );
    va_end(ap);
}
void
vfailure_args(
    const char *test_title,
    const char* file,
    int line,
    const char *format,
    va_list ap)
{
    printf("FAILURE %s %s:%d ", test_title, file, line );
    vprintf(format, ap);
    printf("\n");
    fflush(stdout);

    ++failures;
}

int
get_rv(void)
{
    return failures;
}

gboolean
do_test_call(gboolean result, const char* test_title, const char* filename,
             int line )
{
    if (result)
        success_args( test_title, filename, line, "" );
    else
        failure_args( test_title, filename, line, "" );

    return result;
}

gboolean
do_test_args(
    gboolean result,
    const char* test_title,
    const char* filename,
    int line,
    const char* format,
    ... )
{
    va_list ap;
    va_start(ap, format);

    if ( result )
    {
        vsuccess_args( test_title, filename, line, format, ap );
    }
    else
    {
        vfailure_args( test_title, filename, line, format, ap );
    }
    va_end(ap);

    return result;
}

void
print_test_results(void)
{
    guint total = successes + failures;
    if ( total == 1 )
    {
        printf( "Executed 1 test." );
    }
    else
    {
        printf("Executed %d tests.", successes + failures );
    }
    if ( failures )
    {
        if ( failures == 1 )
        {
            printf(" There was 1 failure." );
        }
        else
        {
            printf(" There were %d failures.", failures );
        }
    }
    else
    {
        printf(" All tests passed.");
    }
    printf("\n");
    fflush(stdout);
}

void
set_success_print( gboolean in_should_print )
{
    success_should_print = in_should_print;
}

gboolean
get_random_boolean(void)
{
    return get_random_int_in_range (0, 1);
}

gint
get_random_int_in_range(int start, int end)
{
    return CLAMP (start + (int)((double)(end - start + 1) * rand() /
                                (RAND_MAX + 1.0)),
                  start,
                  end);
}

static char *random_chars = NULL;

static char plain_chars[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "1234567890"
    " ";

static char funky_chars[] =
    ",.'\"`~!@#$%^*(){}[]/=?+-_\\|"
    "<>&"
    "\n\t";

static int rcend = 0;

void
random_character_include_funky_chars (gboolean use_funky_chars)
{
    g_free (random_chars);

    if (use_funky_chars)
        random_chars = g_strconcat (plain_chars, funky_chars, NULL);
    else
        random_chars = g_strdup (plain_chars);

    rcend = strlen (random_chars) - 1;
}

gchar
get_random_character(void)
{
    if (!rcend)
        random_character_include_funky_chars (TRUE);

    return random_chars[get_random_int_in_range(0, rcend)];
}

gchar *
get_random_string_length_in_range(int minlen, int maxlen)
{
    gchar *ret;
    int i, len = get_random_int_in_range(minlen, maxlen);

    ret = g_new0(gchar, len);

    for (i = 0; i < len - 1; i++)
        ret[i] = get_random_character ();

    return g_strstrip(ret);
}

gchar *
get_random_string_without(const char *exclude_chars)
{
    gchar *ret;
    int len;
    int i;

    switch (get_random_int_in_range(0, 9))
    {
        /*     case 0: */
        /*         return ""; */
        /*     case 1: */
        /*         return NULL; */
        /*     case 2: */
        /*         len = get_random_int_in_range(1000, 5000); */
        /*         break; */
    case 3:
        len = get_random_int_in_range(100, 500);
        break;
    default:
        len = get_random_int_in_range(5, 20);
        break;
    }
    ret = g_new0(gchar, len);

    for (i = 0; i < len - 1; i++)
    {
        char c;

        do
        {
            c = get_random_character ();
        }
        while (exclude_chars && strchr (exclude_chars, c));

        ret[i] = c;
    }

    return g_strstrip (ret);
}

gchar *
get_random_string(void)
{
    return get_random_string_without (NULL);
}

gint64
get_random_gint64(void)
{
    gint64 ret = 0;

    ret = rand();
    ret <<= 32;
    ret += rand();

    return ret;
}

double
get_random_double(void)
{
    double d;
    guint  i;

    i = (guint)get_random_int_in_range(8, 13);
    /* using 0.9 and 7 increases chances of getting lots of decimals */
    d = ((double)get_random_int_in_range(8, 999999) * i * 0.9 / 7);
    return d;
}

const char*
get_random_string_in_array(const char* str_list[])
{
    int num;

    /* count number of items in list */
    for (num = 0; str_list[num] != NULL; num++)
        ;

    num = get_random_int_in_range(0, num - 1);
    return str_list[num];
}
