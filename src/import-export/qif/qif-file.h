/* qif-import-p.h -- a QIF Importer module (private headers)
 *
 * Written By:	Derek Atkins <derek@ihtfp.com>
 *
 */

#ifndef QIF_FILE_H
#define QIF_FILE_H

struct _QifLine {
  char		type;
  gint		lineno;
  char *	line;
};

void qif_record_destroy(GList *record);

#endif /* QIF_FILE_H */
