/*
 * FILE:
 * builder.c
 *
 * FUNCTION:
 * generic postgres backend query builder
 * compiles data types into sql queries
 *
 * Note: Postgres documentation states that the 
 * maximum length of a query is 8192 bytes, and that
 * longer queries are ignored ...
 *
 * TBD hack alert XXX FIXME:
 * -- check for buffer overflow at end of each setter
 * -- write escape functions so that no char strings 
 */

#define _GNU_SOURCE
#include <glib.h>
#include <string.h>

#include "date.h"
#include "builder.h"
#include "gnc-engine-util.h"

static short module = MOD_BACKEND;

/* ================================================ */

struct _builder {
   sqlBuild_QType qtype;

   /* pointers the the tail end of two different assembly areas */
   char * ptag;
   char * pval;

   /* sql needs commas to separate values */
   short  tag_need_comma;
   short  val_need_comma;
   short  where_need_and;

   /* pointers to the start of two different assembly areas. */
   char * tag_base;
   char * val_base;
   size_t buflen;

   /* pointer to temp memory used for escaping arguments */
   char * escape;
   size_t esc_buflen;
};

/* ================================================ */
/* escape single-quote marks and backslashes so that the 
 * database SQL parser doesn't puke on the query string 
 */

static const char *
sqlBuilder_escape (sqlBuilder *b, const char *str)
{
   const char *p, *src_head;
   char *dst_tail;
   size_t len, slen;
   
   /* if nothing to escape, just return */
   len = strlen (str);
   slen = strcspn (str, "\\\'");
   if (len == slen) return str;

   /* count to see how much space we'll need */
   p = str + slen + 1;
   while (*p)
   {
      len ++;
      p += 1 + strcspn (p, "\\\'");
   }

   /* get more space, if needed */
   if (len >= b->esc_buflen)
   {
      b->escape = g_realloc(b->escape, len+100);
      b->esc_buflen = len+100;
   }

   /* copy and escape */
   src_head = (char *) str;
   dst_tail = b->escape;
   p = src_head + strcspn (src_head, "\\\'");
   while (*p)
   {
      size_t cp_len = p - src_head;

      strncpy (dst_tail, src_head, cp_len);
      dst_tail += cp_len;
      *dst_tail = '\\';
      dst_tail ++;
      *dst_tail = *p;
      dst_tail ++;

      src_head = p+1;
      p = src_head + strcspn (src_head, "\\\'");
   }
   if (p != src_head)
   {
      size_t cp_len = p - src_head;

      strncpy (dst_tail, src_head, cp_len);
      dst_tail += cp_len;
   }
   *dst_tail = 0;

   g_warning (b->escape);

   return b->escape;
}

/* ================================================ */

#define INITIAL_BUFSZ 16300

sqlBuilder *
sqlBuilder_new (void)
{
   sqlBuilder *b = g_new (sqlBuilder, 1);

   b->qtype = SQL_INSERT;

   b->tag_base = g_malloc (INITIAL_BUFSZ);
   b->val_base = g_malloc (INITIAL_BUFSZ);
   b->buflen = INITIAL_BUFSZ;

   b->ptag = b->tag_base;
   b->pval = b->val_base;

   /* null terminated strings */
   *(b->ptag) = 0x0;
   *(b->pval) = 0x0;

   b->tag_need_comma = 0;
   b->val_need_comma = 0;
   b->where_need_and = 0;

   /* the escape area */
   b->escape = g_malloc (INITIAL_BUFSZ);
   b->esc_buflen = INITIAL_BUFSZ;
   return (b);
}

/* ================================================ */

void
sqlBuilder_destroy (sqlBuilder*b)
{
   if (!b) return;
   g_free (b->tag_base);   b->tag_base = NULL;
   g_free (b->val_base);   b->val_base = NULL;
   g_free (b->escape);     b->escape = NULL;
   g_free (b);
}

/* ================================================ */

void
sqlBuild_Table (sqlBuilder *b, const char *tablename, sqlBuild_QType qt)
{
   if (!b || !tablename) return;
   b->qtype = qt;

   b->ptag = b->tag_base;
   b->pval = b->val_base;

   /* null terminated strings */
   *(b->ptag) = 0x0;
   *(b->pval) = 0x0;

   b->tag_need_comma = 0;
   b->val_need_comma = 0;
   b->where_need_and = 0;

   switch (qt) 
   {
      case SQL_INSERT:
         b->ptag = stpcpy(b->ptag, "INSERT INTO ");
         b->ptag = stpcpy(b->ptag, tablename);
         b->ptag = stpcpy(b->ptag, " (");

         b->pval = stpcpy(b->pval, ") VALUES (");
         break;

      case SQL_UPDATE:
         b->ptag = stpcpy(b->ptag, "UPDATE ");
         b->ptag = stpcpy(b->ptag, tablename);
         b->ptag = stpcpy(b->ptag, " SET ");

         b->pval = stpcpy(b->pval, " WHERE ");
         break;

      case SQL_SELECT:
         b->ptag = stpcpy(b->ptag, "SELECT ");

         b->pval = stpcpy(b->pval, " FROM ");
         b->pval = stpcpy(b->pval, tablename);
         b->pval = stpcpy(b->pval, " WHERE ");
         break;
   };

}

/* ================================================ */
/* note that val may be NULL if a SELECT statement in being built */

void
sqlBuild_Set_Str (sqlBuilder *b, const char *tag, const char *val)
{
   if (!b || !tag) return;
   if (!val) val= "";

   val = sqlBuilder_escape (b, val);

   if (b->tag_need_comma) b->ptag = stpcpy(b->ptag, ", ");
   b->tag_need_comma = 1;

   switch (b->qtype) 
   {
      case SQL_INSERT:
         b->ptag = stpcpy(b->ptag, tag);

         if (b->val_need_comma) b->pval = stpcpy(b->pval, ", ");
         b->val_need_comma = 1;
         b->pval = stpcpy(b->pval, "'");
         b->pval = stpcpy(b->pval, val);
         b->pval = stpcpy(b->pval, "'");
         break;

      case SQL_UPDATE:
         b->ptag = stpcpy(b->ptag, tag);
         b->ptag = stpcpy(b->ptag, "='");
         b->ptag = stpcpy(b->ptag, val);
         b->ptag = stpcpy(b->ptag, "' ");
         break;

      case SQL_SELECT:
         b->ptag = stpcpy(b->ptag, tag);
         break;

      default:
         PERR ("mustn't happen");
   };
   
}

/* ================================================ */

void
sqlBuild_Set_Char (sqlBuilder *b, const char *tag, char val)
{
  char buf[2];
  buf[0] = val;
  buf[1] = 0x0;
  sqlBuild_Set_Str (b, tag, buf);
}

/* ================================================ */

void
sqlBuild_Set_GUID (sqlBuilder *b, const char *tag, const GUID *val)
{
  if (val) {
     char guid_str[GUID_ENCODING_LENGTH+1];
     guid_to_string_buff(val, guid_str);
     sqlBuild_Set_Str (b, tag, guid_str);
  } else {
     /* if a SELECT statement is being built, then val may be null */
     sqlBuild_Set_Str (b, tag, "");
  }
}

/* ================================================ */

void
sqlBuild_Set_Date (sqlBuilder *b, const char *tag, Timespec ts)
{
  char buf[120];
  gnc_timespec_to_iso8601_buff (ts, buf);
  sqlBuild_Set_Str (b, tag, buf);
}

/* ================================================ */

void
sqlBuild_Set_Double (sqlBuilder *b, const char *tag, double flt)
{
  char buf[120];
  snprintf (buf, 120, "%24.18g", flt);
  sqlBuild_Set_Str (b, tag, buf);
}

/* ================================================ */

void
sqlBuild_Set_Int64 (sqlBuilder *b, const char *tag, gint64 nval)
{
   char val[100];
   if (!b || !tag) return;

   snprintf (val, 100, "%lld", nval);
   if (b->tag_need_comma) b->ptag = stpcpy(b->ptag, ", ");
   b->tag_need_comma = 1;

   switch (b->qtype) 
   {
      case SQL_INSERT:
         b->ptag = stpcpy(b->ptag, tag);

         if (b->val_need_comma) b->pval = stpcpy(b->pval, ", ");
         b->val_need_comma = 1;
         b->pval = stpcpy(b->pval, val);
         break;

      case SQL_UPDATE:
         b->ptag = stpcpy(b->ptag, tag);
         b->ptag = stpcpy(b->ptag, "=");
         b->ptag = stpcpy(b->ptag, val);
         break;

      case SQL_SELECT:
         b->ptag = stpcpy(b->ptag, tag);
         break;

      default:
         PERR ("mustn't happen");
   };
}

/* ================================================ */

void
sqlBuild_Set_Int32 (sqlBuilder *b, const char *tag, gint32 nval)
{
   sqlBuild_Set_Int64 (b, tag, (gint64) nval);
}

/* ================================================ */

void
sqlBuild_Where_Str (sqlBuilder *b, const char *tag, const char *val)
{
   if (!b || !tag || !val) return;

   val = sqlBuilder_escape (b, val);
   switch (b->qtype) 
   {
      case SQL_INSERT:
         /* ther is no where clasue, so we do the set as a utility */
         sqlBuild_Set_Str (b, tag, val);
         break;

      case SQL_UPDATE:
      case SQL_SELECT:
         if (b->where_need_and) b->pval = stpcpy(b->pval, " AND ");
         b->where_need_and = 1;

         b->pval = stpcpy(b->pval, tag);
         b->pval = stpcpy(b->pval, "='");
         b->pval = stpcpy(b->pval, val);
         b->pval = stpcpy(b->pval, "'");

         break;


      default:
         PERR ("mustn't happen");
   };
   
}

/* ================================================ */

void
sqlBuild_Where_GUID (sqlBuilder *b, const char *tag, const GUID *val)
{
  char guid_str[GUID_ENCODING_LENGTH+1];
  guid_to_string_buff(val, guid_str);
  sqlBuild_Where_Str (b, tag, guid_str);
}

/* ================================================ */

void
sqlBuild_Where_Int32 (sqlBuilder *b, const char *tag, gint32 val)
{
  char str[40];
  snprintf (str, 40, "%d", val);
  sqlBuild_Where_Str (b, tag, str);
}

/* ================================================ */

const char *
sqlBuild_Query (sqlBuilder *b)
{
   if (!b) return NULL;

   switch (b->qtype) 
   {
      case SQL_INSERT:
         b->ptag = stpcpy(b->ptag, b->val_base);
         b->ptag = stpcpy(b->ptag, ");");
         break;

      case SQL_UPDATE:
      case SQL_SELECT:
         b->ptag = stpcpy(b->ptag, b->val_base);
         b->ptag = stpcpy(b->ptag, ";");
         break;

      default:
         PERR ("mustn't happen");
   };
   
   PINFO ("%s\n", b->tag_base);
   return b->tag_base;
}

/* ================ END OF FILE ==================== */
