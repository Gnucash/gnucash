/*
 * gsf-output-csv.c: a GsfOutput to write .csv style files.
 *
 * Copyright (C) 2005 Morten Welinder (terra@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-output-csv.h>
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <glib/gconvert.h>
#include <string.h>

static GObjectClass *parent_class;

enum {
	PROP_0,
	PROP_SINK,
	PROP_QUOTE,
	PROP_QUOTING_MODE,
	PROP_QUOTING_TRIGGERS,
	PROP_EOL,
	PROP_SEPARATOR
};

static void
gsf_output_csv_finalize (GObject *obj)
{
	GsfOutputCsv *csv = (GsfOutputCsv *)obj;

	if (csv->sink != NULL)
		g_object_unref (G_OBJECT (csv->sink));
	g_free (csv->quote);
	g_free (csv->quoting_triggers);
	g_free (csv->eol);
	g_free (csv->separator);
	g_string_free (csv->buf, TRUE);

	parent_class->finalize (obj);
}

static gboolean
gsf_output_csv_write (GsfOutput *output,
		      size_t num_bytes, guint8 const *data)
{
	GsfOutputCsv *csv = GSF_OUTPUT_CSV (output);
	return gsf_output_write (csv->sink, num_bytes, data);
}

static gboolean
gsf_output_csv_seek (GsfOutput *output,
		     gsf_off_t offset,
		     GSeekType whence)
{
	GsfOutputCsv *csv = GSF_OUTPUT_CSV (output);
	return gsf_output_seek (csv->sink, offset, whence);
}

static gboolean
gsf_output_csv_close (G_GNUC_UNUSED GsfOutput *output)
{
	return TRUE;
}

gboolean
gsf_output_csv_write_field (GsfOutputCsv *csv, char const *field, size_t len)
{
	gboolean quote;
	gboolean ok;
	char const *end;

	g_return_val_if_fail (GSF_IS_OUTPUT_CSV (csv), FALSE);
	g_return_val_if_fail (field != NULL, FALSE);

	if (len == (size_t)-1)
		len = strlen (field);
	end = field + len;

	if (csv->fields_on_line && csv->separator_len)
		g_string_append_len (csv->buf,
				     csv->separator,
				     csv->separator_len);
	csv->fields_on_line = TRUE;

	switch (csv->quoting_mode) {
	default:
	case GSF_OUTPUT_CSV_QUOTING_MODE_NEVER:
		quote = FALSE;
		break;
	case GSF_OUTPUT_CSV_QUOTING_MODE_ALWAYS:
		quote = TRUE;
		break;
	case GSF_OUTPUT_CSV_QUOTING_MODE_AUTO: {
		char const *p = field;
		quote = FALSE;
		while (p < end) {
			gunichar c = g_utf8_get_char (p);
			if (g_utf8_strchr (csv->quoting_triggers, -1, c)) {
				quote = TRUE;
				break;
			}
			p = g_utf8_next_char (p);
		}
		break;
	}
	}

	if (quote && csv->quote_len > 0) {
		g_string_append_len (csv->buf,
				     csv->quote,
				     csv->quote_len);

		while (field < end) {
			gunichar c = g_utf8_get_char (field);
			if (g_utf8_strchr (csv->quote, -1, c))
				g_string_append_len (csv->buf,
						     csv->quote,
						     csv->quote_len);
			g_string_append_unichar (csv->buf, c);
			field = g_utf8_next_char (field);
		}

		g_string_append_len (csv->buf,
				     csv->quote,
				     csv->quote_len);
	} else
		g_string_append_len (csv->buf, field, len);

	ok = gsf_output_write (csv->sink, csv->buf->len, csv->buf->str);

	g_string_truncate (csv->buf, 0);

	return ok;
}

/* ------------------------------------------------------------------------- */

gboolean
gsf_output_csv_write_eol (GsfOutputCsv *csv)
{
	g_return_val_if_fail (GSF_IS_OUTPUT_CSV (csv), FALSE);

	csv->fields_on_line = FALSE;
	return gsf_output_write (csv->sink, csv->eol_len, csv->eol);
}

/* ------------------------------------------------------------------------- */

GType
gsf_output_csv_quoting_mode_get_type (void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { GSF_OUTPUT_CSV_QUOTING_MODE_NEVER,  (char *)"GSF_OUTPUT_CSV_QUOTING_MODE_NEVER",  (char *)"never" },
      { GSF_OUTPUT_CSV_QUOTING_MODE_AUTO,   (char *)"GSF_OUTPUT_CSV_QUOTING_MODE_AUTO",   (char *)"auto" },
      { GSF_OUTPUT_CSV_QUOTING_MODE_ALWAYS, (char *)"GSF_OUTPUT_CSV_QUOTING_MODE_ALWAYS", (char *)"always" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static ("GsfOutputCsvQuotingMode", values);
  }
  return etype;
}

/* ------------------------------------------------------------------------- */

static void
gsf_output_csv_init (GObject *obj)
{
	GsfOutputCsv *csv = (GsfOutputCsv *)obj;
	csv->quoting_triggers = g_strdup ("");
	csv->eol = g_strdup ("\n");
	csv->buf = g_string_new (NULL);
}

static void
gsf_output_csv_get_property (GObject     *object,
			     guint        property_id,
			     GValue      *value,
			     GParamSpec  *pspec)
{
	GsfOutputCsv *csv = (GsfOutputCsv *)object;

	switch (property_id) {
	case PROP_SINK:
		g_value_set_object (value, csv->sink);
		break;
	case PROP_QUOTE:
		g_value_set_string (value, csv->quote);
		break;
	case PROP_QUOTING_MODE:
		g_value_set_enum (value, csv->quoting_mode);
		break;
	case PROP_QUOTING_TRIGGERS:
		g_value_set_string (value, csv->quoting_triggers);
		break;
	case PROP_EOL:
		g_value_set_string (value, csv->eol);
		break;
	case PROP_SEPARATOR:
		g_value_set_string (value, csv->separator);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_csv_set_sink (GsfOutputCsv *csv, GsfOutput *sink)
{
	g_return_if_fail (GSF_IS_OUTPUT (sink));
	g_object_ref (sink);
	if (csv->sink)
		g_object_unref (csv->sink);
	csv->sink = sink;
}

static void
gsf_output_csv_set_property (GObject      *object,
			       guint         property_id,
			       GValue const *value,
			       GParamSpec   *pspec)
{
	GsfOutputCsv *csv = (GsfOutputCsv *)object;
	char *scopy;

	switch (property_id) {
	case PROP_SINK:
		gsf_output_csv_set_sink (csv, g_value_get_object (value));
		break;
	case PROP_QUOTE:
		scopy = g_strdup (g_value_get_string (value));
		g_free (csv->quote);
		csv->quote = scopy;
		csv->quote_len = scopy ? strlen (scopy) : 0;
		break;
	case PROP_QUOTING_MODE:
		csv->quoting_mode = g_value_get_enum (value);
		break;
	case PROP_QUOTING_TRIGGERS:
		scopy = g_strdup (g_value_get_string (value));
		g_free (csv->quoting_triggers);
		csv->quoting_triggers = scopy ? scopy : g_strdup ("");
		if (*csv->quoting_triggers)
			csv->quoting_mode = GSF_OUTPUT_CSV_QUOTING_MODE_AUTO;
		break;
	case PROP_EOL:
		scopy = g_strdup (g_value_get_string (value));
		g_free (csv->eol);
		csv->eol = scopy ? scopy : g_strdup ("");
		csv->eol_len = strlen (csv->eol);
		break;
	case PROP_SEPARATOR:
		scopy = g_strdup (g_value_get_string (value));
		g_free (csv->separator);
		csv->separator = scopy;
		csv->separator_len = scopy ? strlen (scopy) : 0;
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_csv_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass *output_class = GSF_OUTPUT_CLASS (gobject_class);

	gobject_class->finalize     = gsf_output_csv_finalize;
	gobject_class->set_property = gsf_output_csv_set_property;
	gobject_class->get_property = gsf_output_csv_get_property;
	output_class->Write	    = gsf_output_csv_write;
	output_class->Seek	    = gsf_output_csv_seek;
	output_class->Close	    = gsf_output_csv_close;

	g_object_class_install_property
		(gobject_class,
		 PROP_SINK,
		 g_param_spec_object ("sink", "Sink",
				      "Where the compressed data is written.",
				      GSF_OUTPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE));
	g_object_class_install_property
		(gobject_class,
		 PROP_QUOTE,
		 g_param_spec_string ("quote", "Quote",
				      "The string used for quoting fields.",
				      "\"",
				      GSF_PARAM_STATIC |
				      G_PARAM_CONSTRUCT |
				      G_PARAM_READWRITE));
	g_object_class_install_property
		(gobject_class,
		 PROP_QUOTING_MODE,
		 g_param_spec_enum ("quoting-mode",
				    "Quoting Mode",
				    "When to quote fields.",
				    GSF_OUTPUT_CSV_QUOTING_MODE_TYPE,
				    GSF_OUTPUT_CSV_QUOTING_MODE_NEVER,
				    GSF_PARAM_STATIC |
				    G_PARAM_CONSTRUCT |
				    G_PARAM_READWRITE));
	g_object_class_install_property
		(gobject_class,
		 PROP_QUOTING_TRIGGERS,
		 g_param_spec_string ("quoting-triggers", "Quoting Triggers",
				      "Characters that cause field quoting.",
				      NULL,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE));
	g_object_class_install_property
		(gobject_class,
		 PROP_SEPARATOR,
		 g_param_spec_string ("separator", "Separator",
				      "The field separator.",
				      ",",
				      GSF_PARAM_STATIC |
				      G_PARAM_CONSTRUCT |
				      G_PARAM_READWRITE));
	g_object_class_install_property
		(gobject_class,
		 PROP_EOL,
		 g_param_spec_string ("eol", "eol",
				      "The end-of-line marker.",
				      "\n",
				      GSF_PARAM_STATIC |
				      G_PARAM_CONSTRUCT |
				      G_PARAM_READWRITE));

	parent_class = g_type_class_peek_parent (gobject_class);
}

GSF_CLASS (GsfOutputCsv, gsf_output_csv,
	   gsf_output_csv_class_init, gsf_output_csv_init, GSF_OUTPUT_TYPE)
