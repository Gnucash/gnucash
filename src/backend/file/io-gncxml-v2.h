/********************************************************************\
 * io-gncxml-v2.h -- api for gnucash xml i/o                        *
 *                                                                  *
 * Copyright (c) 2001 Gnumatic Incorporated                         *
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
 *                                                                  *
\********************************************************************/

/**
 * @file io-gncxml-v2.h
 * @brief api for GnuCash version 2 XML-based file format
 * @author Initial code by James LewisMoss, 2001
 */

#ifndef IO_GNCXML_V2_H
#define IO_GNCXML_V2_H

#include <glib.h>

#include "gnc-engine.h"
#include "gnc-backend-file.h"

#include "sixtp.h"

typedef struct
{
    int accounts_total;
    int accounts_loaded;

    int books_total;
    int books_loaded;

    int commodities_total;
    int commodities_loaded;

    int transactions_total;
    int transactions_loaded;

    int prices_total;
    int prices_loaded;

    int schedXactions_total;
    int schedXactions_loaded;

    int budgets_total;
    int budgets_loaded;
} load_counter;

typedef struct sixtp_gdv2 sixtp_gdv2;
typedef void (*countCallbackFn)(sixtp_gdv2 *gd, const char *type);
struct sixtp_gdv2
{
    QofBook *book;
    load_counter counter;
    countCallbackFn countCallback;
    QofBePercentageFunc gui_display_fn;
    gboolean exporting;
};

/**
 * Struct used to pass in a new data type for XML storage.  This contains
 * the set of callbacks to read and write XML for new data objects..  New
 * types should register an instance of this object with the engine.
 *
 * The create_parser() method will create a new sixtp parser for this
 *   data type.
 *
 * The add_item() method takes a local state and a new object of this type
 *   and the method implementation should do whatever is necessary to cleanup
 *   the object and (maybe) add it into the book stored in the local-state.
 *
 * The get_count() method returns the number of items of this type.
 *
 * The write() method writes out all the objects of this particular type
 *   in the book and stores the XML in the FILE.
 *
 * The scrub() method will take a completed, parsed QofBook* and post process
 *   the data, allowing you to 'scrub' the data.
 *
 * The ns() method will output XML namespace information for the selected
 *   plug-in object.
 */
#define GNC_FILE_BACKEND	"gnc:file:2"
#define GNC_FILE_BACKEND_VERS	2
typedef struct
{
  int		version;	/* backend version number */
  const char *	type_name;	/* The XML tag for this type */

  sixtp *	(*create_parser) (void);
  gboolean	(*add_item)(sixtp_gdv2 *, gpointer obj);
  int	      (*get_count) (QofBook *);
  void		(*write) (FILE*, QofBook*);
  void		(*scrub) (QofBook *);
  void		(*ns) (FILE*);
} GncXmlDataType_t;

/**
 * Struct used to pass the account group/accounts and trasnactions in
 * the <gnc:template-transactions> section between the parser in
 * gnc-schedxactions-xml-v2.c and the add-to-book callback in
 * io-gncxml-v2.c.
 **/
typedef struct
{
	AccountList	*accts;
	TransList	*transactions;
	QofBook *book;
} gnc_template_xaction_data;

/** Call after loading each record */
void run_callback(sixtp_gdv2 *data, const char *type);

/** read in an account group from a file */
gboolean qof_session_load_from_xml_file_v2(FileBackend *, QofBook *);

/* write all book info to a file */
gboolean gnc_book_write_to_xml_filehandle_v2(QofBook *book, FILE *fh);
gboolean gnc_book_write_to_xml_file_v2(QofBook *book, const char *filename, gboolean compress);

/** write just the commodities and accounts to a file */
gboolean gnc_book_write_accounts_to_xml_filehandle_v2(QofBackend *be, QofBook *book, FILE *fh);
gboolean gnc_book_write_accounts_to_xml_file_v2(QofBackend * be, QofBook *book,
						const char *filename);

/** The is_gncxml_file() routine checks to see if the first few 
 * chars of the file look like gnc-xml data.
 */
gboolean gnc_is_xml_data_file_v2(const gchar *name, gboolean *with_encoding);

/** Write a name-space declaration for the provided namespace data type
 * within the GNC XML namespace at http://www.gnucash.org/XML.
 */
void gnc_xml2_write_namespace_decl (FILE *out, const char *namespace);


typedef struct {
  GQuark encoding;
  gchar *utf8_string;
} conv_type;

/** Read a file as plain byte stream to find words that are not completely ASCII.
 * On error, @unique, @ambiguous and @impossible will be filled up to that point,
 * @error may contain an io channel error, -1 will be returned.
 *
 * @param filename Name of the file to read.
 *
 * @param encodings List of encodings to check words for, each begin one a GQuark
 * in a pointer.
 *
 * @param unique Location used for a hash table for unique solutions, if not
 * NULL. The byte sequence is the key, successful_conversion the value.
 *
 * @param ambiguous Location used for a hash table for ambiguous byte sequences,
 * if not NULL. The byte sequences is the key, a list of successful_conversions
 * the value.
 *
 * @param impossible Location used for a list for undecodable byte sequences,
 * if not NULL.
 *
 * @param error Location to return an io channel error.
 *
 * @return Size of impossible, -1 on error.
 */
gint gnc_xml2_find_ambiguous(
    const gchar *filename, GList *encodings, GHashTable **unique,
    GHashTable **ambiguous, GList **impossible);

/** Parse a file in push mode, but replace byte sequences in the file given a
 * hash table of substitutions
 *
 * @param subst hash table with keys and values of type gchar*
 */
gboolean gnc_xml2_parse_with_subst (
    FileBackend *fbe, QofBook *book, GHashTable *subst);

#endif /* __IO_GNCXML_V2_H__ */
