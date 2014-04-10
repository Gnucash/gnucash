#include <stdio.h>
#include <string.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-msole-utils.h>
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-output-stdio.h>

#define VBA_COMPRESSION_WINDOW 4096

#define HEADER_SIZE 3

/* Brute force and ugliness ! */
typedef struct {
	guint8  inblock[VBA_COMPRESSION_WINDOW];
	guint   length;
	guint   pos;
	guint8  shift;

	guint8     mask;
	GString   *outstr;

	GsfOutput *output;
} CompressBuf;

#define DEBUG

static char
byte_to_char (guint8 data)
{
	return data >= 0x20 && data < 126 ? data : '.';
}

static gint
get_shift (guint cur_pos)
{
	int shift;

	if (cur_pos <= 0x80) {
		if (cur_pos <= 0x20)
			shift = (cur_pos <= 0x10) ? 12 : 11;
		else
			shift = (cur_pos <= 0x40) ? 10 : 9;
	} else {
		if (cur_pos <= 0x200)
			shift = (cur_pos <= 0x100) ? 8 : 7;
		else if (cur_pos <= 0x800)
			shift = (cur_pos <= 0x400) ? 6 : 5;
		else
			shift = 4;
	}

	return shift;
}

static gint
find_match (CompressBuf *buf, guint pos, guint *len)
{
	gint i;
	guint max_match = (1 << get_shift (pos)) - 1;

	/* FIXME: the MS impl. does different to a linear search here
	   and is not very good at this either; is happy to get much
	   worse matches; perhaps some single-entry match lookup table ?
	   it seems to ~regularly truncate strings, and get earlier
	   / later matches of equivalent length with no predictability
	   ( hashing ? ).
	*/
	for (i = pos - 1; i >= 0; i--) {
		guint j;

		for (j = 0; j < buf->length - pos && j < pos; j++)
			if (buf->inblock[pos + j] != buf->inblock[i + j])
				break;

		if (j >= 3) {
			*len = MIN (j, max_match);
			return i;
		}
	}
	return -1;
}

static void
output_data (CompressBuf *buf, guint8 *data, gboolean compressed)
{
	if (compressed) {
		buf->mask |= 1 << buf->shift;
		g_string_append_c (buf->outstr, data [0]);
		g_string_append_c (buf->outstr, data [1]);
	} else
		g_string_append_c (buf->outstr, data [0]);

	buf->shift++;
	if (buf->shift == 8) {
		guint i;

		gsf_output_write (buf->output, 1, &buf->mask);
		gsf_output_write (buf->output, buf->outstr->len, buf->outstr->str);

#ifdef DEBUG		
		fprintf (stderr, "Block: 0x%x '", buf->mask);
		for (i = 0; i < buf->outstr->len; i++)
			fprintf (stderr, "%c", byte_to_char (buf->outstr->str[i]));
		fprintf (stderr, "'\n");
#endif

		buf->mask = 0;
		buf->shift = 0;
		g_string_set_size (buf->outstr, 0);
	}
}

static void
output_match (CompressBuf *buf, guint cur_pos, guint pos, guint len)
{
	int shift, token, distance;
	guint8 data[2];

	shift = get_shift (cur_pos);

	distance = cur_pos - pos - 1;

	/* Window size issue !? - get a better match later with a larger window !? */

	token = (distance << shift) + ((len - 3) & ((1<<(shift + 1))-1));
	data[0] = token & 0xff;
	data[1] = token >> 8;

#ifdef DEBUG		
	fprintf (stderr, "shift %d, [0x%x(%d) - %d], len %d, distance %d bytes %.2x %.2x\n",
		 shift, cur_pos, cur_pos, pos, len, distance,
		 data[0], data[1]);
	if (cur_pos + len >= (1u<<shift))
		fprintf (stderr, "Overlaps boundary\n");
#endif

	output_data (buf, data, TRUE);
}

static void
compress_block (CompressBuf *buf)
{
	guint pos, len;
	gint  match;

	for (pos = 0; pos < buf->length;) {
		if ((match = find_match (buf, pos, &len)) >= 0) {
			output_match (buf, pos, match, len);
			pos += len;
		} else
			output_data (buf, &(buf->inblock[pos++]), FALSE);
	}
}

static void
do_compress (GsfInput *input, GsfOutput *output)
{
	CompressBuf real_buf, *buf;
	GString *string;
	guint8   data[HEADER_SIZE];
	int      length;

	buf = &real_buf;
	memset (buf, 0, sizeof (CompressBuf));
	buf->output = output;
	buf->outstr = g_string_sized_new (20);

	data[0] = 0x01;
	data[1] = 0x00;
	data[2] = 0xb0;
	gsf_output_write (buf->output, 3, data); /* dummy padding */

	string = g_string_sized_new (64);

	while (gsf_input_remaining (input) > 0) {
		buf->length = MIN (gsf_input_remaining (input), VBA_COMPRESSION_WINDOW);
		if (!gsf_input_read (input, buf->length, buf->inblock))
			g_error ("Failed to read %d bytes\n", buf->length);
		compress_block (buf);
	}

       	if (buf->outstr->len) {
		gsf_output_write (buf->output, 1, &buf->mask);
		gsf_output_write (buf->output, buf->outstr->len, buf->outstr->str);
	}

	length = gsf_output_size (buf->output) - 3 - 1;
	if (length > 0x0c0c) /* TESTME: is this really right ? */
		length = 0x0c0c;
	data[1] = length & 0xff;
	data[2] |= (length >> 8);
	gsf_output_seek (output, 0, G_SEEK_SET);
	gsf_output_write (buf->output, 3, data); /* real data */
}

static void
do_decompress (GsfInput *input, GsfOutput *output)
{
	gboolean err = FALSE;
	guint8   data[HEADER_SIZE];
	GByteArray *decompressed;
	int      size, comp_len;

	err |= !gsf_input_read (input, HEADER_SIZE, data);
	if (data [0] != 0x01)
		fprintf (stderr, "Odd pre-amble byte 0x%x\n", data[0]);
	if ((data [2] & 0xf0) != 0xb0)
		fprintf (stderr, "Odd high nibble 0x%x\n", (data[2] & 0xf0));
	comp_len = ((data[2] & 0x0f) << 8) + data[1];
	if (comp_len + 1 != gsf_input_size (input) - 3)
		fprintf (stderr, "Size mismatch %d %d\n",
			 comp_len + 1, (int) (gsf_input_size (input) - 3));

	decompressed = gsf_msole_inflate (input, 3);
	if (!decompressed)
		fprintf (stderr, "Failed to decompress\n");

	size = decompressed->len;
	err |= !gsf_output_write (output, size,
		g_byte_array_free (decompressed, FALSE));

	if (err)
		fprintf (stderr, "I/O error\n");
}

#define MAP(a,b)

static void
decode_dir (GsfInput *input)
{
	gboolean err = FALSE;
	guint8 data[6];
	int    op_count = 0;

	while (gsf_input_remaining (input) && !err) {
		unsigned i;
		guint16 op;
		guint32 length;
		gboolean ascii = FALSE;
		gboolean unicode = FALSE;
		gboolean offset = FALSE;

		err |= !gsf_input_read (input, 6, data);

		op     = GSF_LE_GET_GUINT16 (&data[0]);
		length = GSF_LE_GET_GUINT32 (&data[2]);

		if (op == 9) {
			fprintf (stderr, "** Quirk fix **\n");
			length += 2;
		}
		
		/* Special nasties / up-stream bugs */
		switch (op) {
		case 0x4:
		case 0x16:
		case 0x19:
		case 0x1a:
			ascii = TRUE;
			break;
		case 0x32:
		case 0x3e:
		case 0x47:
			unicode = TRUE;
			break;
		case 0x31:
			offset = TRUE;
			break;
		default:
			break;
		}

		fprintf (stderr, "0x%.6x Op %3d 0x%.2x, length %3d: '",
			(int)gsf_input_tell (input), op_count, op, length);

		if (length > gsf_input_remaining (input)) {
			fprintf (stderr, "Broken - foo !\n");
			length = MIN (64, gsf_input_remaining (input));
			err = TRUE;
		}

		if (ascii || unicode) {
			int advance = ascii ? 1 : 2;
			/* quick and dirty for now */
			for (i = 0 ; i < length; i += advance) {
				guint8 ug;
				err |= !gsf_input_read (input, advance, &ug);
				fprintf (stderr, "%c", byte_to_char (ug));
			}
			fprintf (stderr, "' - '%s", ascii ? "Ascii" : "Unicode");
		} else if (offset) {
			gint8 data[4];
			guint32 offset;
			g_assert (length == 4);
			err |= !gsf_input_read (input, 4, data);
			offset = GSF_LE_GET_GUINT32 (data);
			fprintf (stderr, "0x%.8x' - 'Offset", offset);
		} else {
			GString *chars = g_string_new ("");

			for (i = 0 ; i < length; i++) {
				guint8 ug;
				err |= !gsf_input_read (input, 1, &ug);
				fprintf (stderr, "%.2x ", ug);
				g_string_append_printf (chars, "%c", byte_to_char (ug));
			}
			fprintf (stderr, "' - '%s", chars->str);
			g_string_free (chars, TRUE);
		}
		fprintf (stderr, "'\n");

		op_count++;
	}
}

int
main (int argc, char *argv[])
{
	int i;
	char const *src = NULL;
	char const *dest = NULL;
	GsfInput *input;
	GsfOutput *output;
	GError   *error = NULL;

	/* options */
	gboolean dir = FALSE;
	gboolean compress = FALSE;

	gsf_init ();

	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'c':
				compress = TRUE;
				break;
			case 'd':
				dir = TRUE;
				break;
			default:
				fprintf (stderr, "Unknown option '%s'\n", argv[i]);
				return 1;
				break;
			}
		} else if (!src)
			src = argv[i];
		else
			dest = argv[i];
	}

	if (!src || (!dir && !dest)) {
		fprintf (stderr, "%s: [-c(ompress)] <infile> <outfile>\n", argv[0]);
		fprintf (stderr, "%s: [-d(ecode dir)] <infile>\n", argv[0]);
		return 1;
	}

	input = gsf_input_stdio_new (src, &error);
	if (dir)
		decode_dir (input);
	else {
		output = gsf_output_stdio_new (dest, &error);
		if (!input || !output) {
			fprintf (stderr, "Failed to open input(%p)/output(%p): '%s'\n",
				 input, output, error ? error->message : "<NoMsg>");
			return 1;
		}
		
		if (compress)
			do_compress (input, output);
		else
			do_decompress (input, output);

		g_object_unref (output);
	}

	g_object_unref (input);

	gsf_shutdown ();

	return 0;
}
