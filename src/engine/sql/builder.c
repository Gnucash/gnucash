/*
 * FILE:
 * builder.c
 *
 * FUNCTION:
 * generic postgres backend query builder
 * compiles data types into sql queries

 * TBD hack alert XXX FIXME:
 * -- check for buffer overflow at end of each setter
 * -- write escape functions so that no char strings 
      have single quotes that mess up sql ...
 */

#define _GNU_SOURCE
#include <glib.h>
#include <string.h>

#include "builder.h"
#include "gnc-engine-util.h"

static short module = MOD_BACKEND;

struct _builder {
   sqlBuild_QType qtype;

   char * ptag;
   char * pval;

   short  tag_need_comma;
   short  val_need_comma;

   char * tag_base;
   char * val_base;
   size_t buflen;
};


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
   return (b);
}

/* ================================================ */

void
sqlBuilder_destroy (sqlBuilder*b)
{
   g_free (b->tag_base);
   g_free (b->val_base);
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
     char *guid;
     guid = guid_to_string(val);
     sqlBuild_Set_Str (b, tag, guid);
     g_free (guid);
  } else {
     /* if a SELECT statement is being built, then val may be null */
     sqlBuild_Set_Str (b, tag, "");
  }
}

/* ================================================ */

void
sqlBuild_Set_Date (sqlBuilder *b, const char *tag, Timespec ts)
{
  char buf[512];
  time_t tmp;
  struct tm parsed;
  tmp = ts.tv_sec;
  localtime_r(&tmp, &parsed);
  snprintf (buf, 512, "%4d-%02d-%02d %02d:%02d:%02d.%06ld %02ld",
       parsed.tm_year+1900, parsed.tm_mon+1, parsed.tm_mday, 
       parsed.tm_hour, parsed.tm_min, parsed.tm_sec,
       ts.tv_nsec/1000, timezone/3600);
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
sqlBuild_Where_Str (sqlBuilder *b, const char *tag, const char *val)
{
   if (!b || !tag || !val) return;

   switch (b->qtype) 
   {
      case SQL_INSERT:
         /* ther is no where clasue, so we do the set as a utility */
         sqlBuild_Set_Str (b, tag, val);
         break;

      case SQL_UPDATE:
      case SQL_SELECT:
         // hack alert -- implement multiple where's ...
         // if (b->tag_need_comma) b->ptag = stpcpy(b->ptag, ", ");
         // b->tag_need_comma = 1;

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
  char *guid;
  guid = guid_to_string(val);
  sqlBuild_Where_Str (b, tag, guid);
  g_free (guid);
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
