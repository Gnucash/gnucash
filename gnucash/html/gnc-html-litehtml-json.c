/*******************************************************************\
 * gnc-html-litehtml-json.c -- create chart from json string        *
 *                                                                  *
 * Copyright (C) 2024 Robert Fewell                                 *
 *                                                                  *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @file gnc-html-litehtml-json.c
    @brief Use json string to create chart
    @author Copyright (c) 2024 Robert Fewell
*/
#include <config.h>
#include <glib/gi18n.h>

#include "json-glib/json-glib.h"

#include "gnc-html-litehtml-json.h"

#include "cglib.h"

typedef struct general_data mygeneral_data;
typedef struct pie_data mypie_data;

typedef struct _chart_data
{
    gchar   *name;
    gdouble  value;
    gchar   *color;
} chart_data;


/* Replace 'target' with 'replacement' in the input string. */
static gchar *
string_replace (const gchar *input,
                const gchar *target,
                const gchar *replacement)
{
  gchar **pieces;
  gchar *output;

  pieces = g_strsplit (input, target, -1);
  output = g_strjoinv (replacement, pieces);
  g_strfreev (pieces);

  return output;
}

static const gchar *
get_label (JsonReader *reader, gint element)
{
    if (element > json_reader_count_elements (reader))
        return NULL;

    json_reader_read_element (reader, element);
    const char *label = json_reader_get_string_value (reader);
    json_reader_end_element (reader);

    return label;
}

static gdouble 
get_value (JsonReader *reader, gint element)
{
    json_reader_read_element (reader, 0);
    json_reader_read_member (reader, "data");

    if (element > json_reader_count_elements (reader))
        return 0;

    json_reader_read_element (reader, element);
    gdouble value = json_reader_get_double_value (reader);
    json_reader_end_element (reader);

    json_reader_end_member (reader); //data

    json_reader_end_element (reader); //element 0

    return value;
}

static const gchar *
get_color (JsonReader *reader, gint element)
{
    json_reader_read_element (reader, 0);
    json_reader_read_member (reader, "backgroundColor");

    if (element > json_reader_count_elements (reader))
        return NULL;

    json_reader_read_element (reader, element);
    const char *color = json_reader_get_string_value (reader);
    json_reader_end_element (reader);

    json_reader_end_member (reader); //backgroundColor

    json_reader_end_element (reader); //element 0

    return color;
}

void
gnc_html_litehtml_json_create_chart (const gchar* json_text, const gchar* file_name)
{
    JsonParser *parser = json_parser_new ();
    json_parser_load_from_data (parser, json_text, -1, NULL);
    JsonReader *reader = json_reader_new (json_parser_get_root (parser));

    const char *type = json_reader_get_string_value (reader);

    json_reader_end_member (reader);

//FIXME....
    if (g_strcmp0 (type, "pie") != 0)
    {
        g_object_unref (reader);
        g_object_unref (parser);
        return;
    }

    if (!json_reader_read_member (reader, "data"))
    {
        g_object_unref (reader);
        g_object_unref (parser);
        return;
    }
    json_reader_read_member (reader, "labels");
    gint count = json_reader_count_elements (reader);

    gint max_num_char = 0;
    gdouble total = 0;
    chart_data *cd = g_new0 (chart_data, count);

    for(int i = 0; i < count; i++)
    {
        cd[i].name = string_replace (get_label (reader, i), "&", "&amp;");
        gint len = strlen (cd[i].name);

        if (len > max_num_char) 
            max_num_char = len;
    }

    json_reader_end_member (reader); //labels

    json_reader_read_member (reader, "datasets");

    for(int i = 0; i < count; i++)
    {
        cd[i].value = get_value (reader, i);
        total = total + cd[i].value;
    }

    for(int i = 0; i < count; i++)
    {
        cd[i].color = g_strdup (get_color (reader, i));
    }

    json_reader_end_member (reader); //datasets

    json_reader_end_member (reader); //data

    mygeneral_data* general = g_new0 (mygeneral_data, 1);

    general->stroke_width = 1;
    general->margin = 40.0;    // 20 top and 20 bottom 
    general->viewport_x = 960; // image width size 680
    general->viewport_y = 360; // image height size 400
    general->font_size = 12;
    general->d_file = 0;

    // adjust width depending on max_num_char
    general->viewport_x = (general->viewport_y - general->margin) + 40 + (max_num_char * general->font_size) * 0.6;

    mypie_data* pd = g_new0 (mypie_data, 1);

    general->file_name = malloc(sizeof(char) * 30);
    strcpy(general->file_name, file_name);

    pd->general = general;
    pd->n_slices = count;
    pd->slices = malloc(sizeof(struct pie_slice) * pd->n_slices);

    pd->d_h1_font_size = 50;
    pd->d_h2_font_size = 20;

    for(int i = 0; i < pd->n_slices; i++)
    {
        pd->slices[i].name = malloc(60);
        strcpy(pd->slices[i].name, cd[i].name);
        pd->slices[i].percentage = cd[i].value / total;

        char c;
        int r, g, b;
        sscanf(cd[i].color, "%c%02x%02x%02x", &c, &r, &g, &b);

        pd->slices[i].color.r = r;
        pd->slices[i].color.g = g;
        pd->slices[i].color.b = b;
    }

    pie (pd);

    for(int i = 0; i < count; i++)
    {
        g_free (cd[i].name);
        g_free (cd[i].color);
    }
    g_free (cd);

    g_free (pd->slices);
    g_free (pd);

    g_free (general->file_name);
    g_free (general);

    g_object_unref (reader);
    g_object_unref (parser);
}
