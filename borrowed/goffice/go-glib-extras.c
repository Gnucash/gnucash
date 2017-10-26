/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * utils.c:  Various utility routines that should have been in glib.
 *
 * Authors:
 *    Miguel de Icaza (miguel@gnu.org)
 *    Jukka-Pekka Iivonen (iivonen@iki.fi)
 *    Zbigniew Chyla (cyba@gnome.pl)
 *    Morten Welinder (terra@gnome.org)
 */
#include <config.h>
#include "go-glib-extras.h"

#include <glib/gi18n-lib.h>
#include <libxml/encoding.h>

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <errno.h>

gint go_ascii_strcase_equal(gconstpointer v1, gconstpointer v2)
{
    return g_ascii_strcasecmp((char const *) v1, (char const *) v2) == 0;
}

/* a char* hash function from ASU */
guint go_ascii_strcase_hash(gconstpointer v)
{
    unsigned const char *s = (unsigned const char *) v;
    unsigned const char *p;
    guint h = 0, g;

    for (p = s; *p != '\0'; p += 1)
    {
        h = (h << 4) + g_ascii_tolower(*p);
        if ((g = h & 0xf0000000))
        {
            h = h ^ (g >> 24);
            h = h ^ g;
        }
    }

    return h /* % M */;
}

const char *
go_guess_encoding(const char *raw, size_t len, const char *user_guess,
        char **utf8_str)
{
    int try;
    gboolean debug = FALSE;

    g_return_val_if_fail(raw != NULL, NULL);

    for (try = 1; 1; try++)
    {
        char const *guess = NULL;
        GError *error = NULL;
        char *utf8_data;

        switch (try)
        {
        case 1:
            guess = user_guess;
            break;
        case 2:
            g_get_charset(&guess);
            break;
        case 3:
        {
            xmlCharEncoding enc = xmlDetectCharEncoding(
                    (const unsigned char*) raw, len);
            switch (enc)
            {
            case XML_CHAR_ENCODING_ERROR:
            case XML_CHAR_ENCODING_NONE:
                break;
            case XML_CHAR_ENCODING_UTF16LE:
                /* Default would give "UTF-16".  */
                guess = "UTF-16LE";
                break;
            case XML_CHAR_ENCODING_UTF16BE:
                /* Default would give "UTF-16".  */
                guess = "UTF-16BE";
                break;
            default:
                guess = xmlGetCharEncodingName(enc);
            }
            break;
        }
        case 4:
            guess = "ASCII";
            break;
        case 5:
            guess = "ISO-8859-1";
            break;
        case 6:
            guess = "UTF-8";
            break;
        default:
            return NULL;
        }

        if (!guess)
            continue;

        if (debug)
            g_print("Trying %s as encoding.\n", guess);

        utf8_data = g_convert(raw, len, "UTF-8", guess,
        NULL, NULL, &error);
        if (!error)
        {
            /*
             * We can actually fail this test when guess is UTF-8,
             * see #401588.
             */
            if (!g_utf8_validate(utf8_data, -1, NULL))
                continue;
            if (debug)
                g_print("Guessed %s as encoding.\n", guess);
            if (utf8_str)
                *utf8_str = utf8_data;
            else
                g_free(utf8_data);
            return guess;
        }

        g_error_free(error);
    }
}
