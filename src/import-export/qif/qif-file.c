/*
 * qif-file.c -- parse a QIF File into its pieces
 *
 * Written by:  Derek Atkins  <derek@@ihtfp.com>
 *
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "gnc-engine-util.h"

#include "qif-import-p.h"

static short module = MOD_IMPORT;


static QifLine
qif_make_line(const char* buf, gint lineno)
{
  QifLine line;
  g_return_val_if_fail(buf && *buf, NULL);
  
  line = g_new0(struct _QifLine, 1);
  line->type = *buf;
  line->lineno = lineno;
  line->line = g_strdup(buf+1);

  return line;
}

void
qif_record_destroy(GList *record)
{
  GList *node;
  QifLine line;

  for (node = record; node; node = node->next) {
    line = node->data;
    g_free(line->line);
    g_free(line);
  }

  g_list_free(record);
}

/* This returns a record, which is a bunch of QifLines, ending
 * with a line with just a '^'.  If it finds a line that begins
 * with a !, then destroy the current record state, set the "found_bangtype",
 * and return NULL.
 */
static GList *
qif_make_record(QifContext ctx, char *buf, size_t bufsiz, gboolean *found_bangtype)
{
  GList *record = NULL;
  QifLine line;

  g_return_val_if_fail(ctx, NULL);
  g_return_val_if_fail(buf, NULL);
  g_return_val_if_fail(found_bangtype, NULL);

  *found_bangtype = FALSE;

  while (fgets(buf, bufsiz, ctx->fp) != NULL) {

    /* increment the line number */
    ctx->lineno++;

    /* strip start/end whitespace */
    g_strstrip(buf);

    /* if there is nothing left in the string, ignore it */
    if (strlen(buf) == 0)
      continue;

    /* If this is a bangline, then set the flag, clear our state, and return NULL */
    if (*buf == '!') {
      *found_bangtype = TRUE;
      break;
    }

    /* See if this is an End of Record marker */
    if (*buf == '^') {
      /* Yep.  If we've got a record then break and return ... */
      if (record)
	break;
      /* ... otherwise just continue reading (i.e. ignore empty records) */
      else
	continue;
    }

    /* otherwise, add the line to the list */
    line = qif_make_line(buf, ctx->lineno);
    if (line)
      record = g_list_prepend(record, line);

    /* and continue... */
  }

  /* If we found a bangtype, destroy anything we've collected */
  if (*found_bangtype) {
    if (record)
      PERR("error loading file: incomplete record at line %d", ctx->lineno);

    qif_record_destroy(record);
    record = NULL;
  }

  return g_list_reverse(record);
}

/* read a qif file and parse it, line by line
 * return FALSE on fatal error, TRUE otherwise
 */
QifError
qif_read_file(QifContext ctx, FILE *f)
{
  char buf[BUFSIZ];
  GList *record;
  gboolean found_bang;
  QifError err = QIF_E_OK;

  g_return_val_if_fail(ctx, FALSE);
  g_return_val_if_fail(f, FALSE);

  ctx->fp = f;
  ctx->lineno = -1;

  do {
    found_bang = FALSE;
    record = qif_make_record(ctx, buf, sizeof(buf), &found_bang);

    /* If we got a record, process it */
    if (record) {
      if (!ctx->handler || !ctx->handler->parse_record) {
	PERR("Trying to process QIF record without a handler at %d", ctx->lineno);
      } else {
	err = ctx->handler->parse_record(ctx, record);
      }

      /* Now destroy it; we don't need it anymore */
      qif_record_destroy(record);
    }

    /* if we found a bangtype, process that */
    if (found_bang) {
      g_assert(*buf == '!');

      /* First, process the end of the last handler.  This could possibly
       * merge items into the context or perform some other operation
       */
      if (ctx->handler && ctx->handler->end) {
	err = ctx->handler->end(ctx);
	if (err != QIF_E_OK)
	  break;
      }

      /* Now process the bangtype (stored in buf) to set the new handler */
      qif_parse_bangtype(ctx, buf);
    }

  } while ((record || found_bang) && err == QIF_E_OK);

  /* Make sure to run any end processor */
  if (err == QIF_E_OK && ctx->handler && ctx->handler->end)
    err = ctx->handler->end(ctx);

  return err;
}
