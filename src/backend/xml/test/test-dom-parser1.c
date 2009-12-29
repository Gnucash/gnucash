#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "test-stuff.h"

#include "gnc-xml-helper.h"
#include "sixtp.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"

static void parse_file(char *filename, sixtp *parser);

static sixtp*
get_parser1_1_parser1(void)
{
    sixtp *ret;

    ret = sixtp_new();
    g_return_val_if_fail(ret, NULL);
    sixtp_set_chars(ret, allow_and_ignore_only_whitespace);

    sixtp_add_sub_parser(ret, "foobar",
                         sixtp_dom_parser_new(print_dom_tree, NULL, NULL));

    return ret;
}

static sixtp*
simple_parser(void)
{
    sixtp*ret;
    ret = sixtp_new();
    sixtp_set_chars(ret, allow_and_ignore_only_whitespace);
    return ret;
}

static sixtp*
get_parser1_1_parser2(void)
{
    sixtp *ret;
    sixtp *foobarer;

    ret = simple_parser();
    foobarer = simple_parser();

    sixtp_add_sub_parser(ret, "foobar", foobarer);
    sixtp_add_sub_parser(foobarer, "blah",
                         sixtp_dom_parser_new(print_dom_tree, NULL, NULL));
    sixtp_add_sub_parser(foobarer, "you",
                         sixtp_dom_parser_new(print_dom_tree, NULL, NULL));
    return ret;
}

int
main(int argc, char **argv)
{
    parse_file("test-dom-parser1-1.xml", get_parser1_1_parser1());
    parse_file("test-dom-parser1-1.xml", get_parser1_1_parser2());
    exit(get_rv());
}

static void
parse_file(char *filename, sixtp* parser)
{
    printf("STARTING: %s\n", filename);
    sixtp_parse_file(parser, filename, NULL, (gpointer)stdout, NULL);
    printf("\nENDING: %s\n", filename);
    sixtp_destroy(parser);
}
