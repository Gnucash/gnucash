/********************************************************************
 * sixtp-xml-write-utils.c                                          *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#include "config.h"

#define _GNU_SOURCE
#define __EXTENSIONS__

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "sixtp-xml-write-utils.h"
#include "sixtp-utils.h"

#include "gnc-numeric.h"
#include "gnc-engine-util.h"

static short module = MOD_IO;

/* ============================================================== */

gboolean
xml_add_str(xmlNodePtr p, const char *tag, const char *str,
            gboolean include_if_empty) {
  xmlNodePtr child;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  if(!str && !include_if_empty) return(TRUE); 
  if((strlen(str) == 0)  && !include_if_empty) return(TRUE);

  child = xmlNewTextChild(p, NULL, tag, str);
  g_return_val_if_fail(child, FALSE);

  return(TRUE);
}

/* ============================================================== */

gboolean
xml_add_character(xmlNodePtr p, const char *tag, const char c) {
  char str[2];
  str[0] = c;
  str[1] = '\0';
  return(xml_add_str(p, tag, str, FALSE));
}

/* ============================================================== */

gboolean
xml_add_gint64(xmlNodePtr p, const char *tag, const gint64 value) {
  xmlNodePtr val_xml;
  char num_string[22];

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);

  g_snprintf(num_string, sizeof (num_string), "%lld", value);

  val_xml = xmlNewTextChild(p, NULL, tag, num_string);
  g_return_val_if_fail(val_xml, FALSE);

  return(TRUE);
}

/* ============================================================== */

gboolean
xml_add_gint32(xmlNodePtr p, const char *tag, const gint32 value) {
  xmlNodePtr val_xml;
  char num_string[22];

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);

  g_snprintf(num_string, sizeof (num_string), "%d", value);

  val_xml = xmlNewTextChild(p, NULL, tag, num_string);
  g_return_val_if_fail(val_xml, FALSE);

  return(TRUE);
}


/* ============================================================== */
/* 
   RLB writes:
   We have to use guile because AFAICT, libc, and C in general isn't
   smart enough to actually parse it's own output, especially not
   portably (big surprise).

   Linas writes:
   I don't understand the claim; I'm just going to use 
   atof or strtod to accomplish this.

 */

gboolean
xml_add_double(xmlNodePtr p, const char *tag, const double value) 
{
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);


#ifdef USE_GUILE_FOR_DOUBLE_CONVERSION 
  {
    /* FIXME: NOT THREAD SAFE - USES STATIC DATA */
    static SCM number_to_string;
    static gboolean ready = FALSE;
    const char *numstr;

    if(!ready) {
      number_to_string = gh_eval_str("number->string");
      scm_protect_object(number_to_string);
      ready = TRUE;
    }

    numstr = gh_scm2newstr(gh_call1(number_to_string, gh_double2scm(value)),
                           NULL);
    
    if(!numstr) {
      return(FALSE);
    } else {
      xmlNodePtr child = xmlNewTextChild(p, NULL, tag, numstr);
      free((void *) numstr);
      g_return_val_if_fail(child, FALSE);
    }
  }

#else /* don't USE_GUILE_FOR_DOUBLE_CONVERSION */
  {
    int len;
    char prtbuf[80];
    xmlNodePtr child;
  
    /* we're just going to use plain-old libc for the double conversion.
     * There was some question as to whether libc is accurate enough
     * in its printf function for doubles, but I don't understand
     * how it couldn't be ...
     */
    len = snprintf (prtbuf, 80, "%24.18g", value);
    if (80 <=len) return (FALSE);
    
    child = xmlNewTextChild(p, NULL, tag, prtbuf);
    g_return_val_if_fail(child, FALSE);
  }

#endif /* USE_GUILE_FOR_DOUBLE_CONVERSION */
  
  return(TRUE);
}

/* ============================================================== */

gboolean
xml_add_gnc_numeric(xmlNodePtr p, const char *tag, const gnc_numeric n) {
  char *numstr;
  xmlNodePtr child;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);

  /* fprintf(stderr, "WRITE GNUM S: %lld/%lld -> ", n.num, n.denom); */

  numstr = gnc_numeric_to_string(n);
  g_return_val_if_fail(numstr, FALSE);

  /* fprintf(stderr, "%s\n", numstr); */

  child = xmlNewTextChild(p, NULL, tag, numstr);
  g_free(numstr); numstr = FALSE;
  g_return_val_if_fail(child, FALSE);

  return(TRUE);
}

/* ============================================================== */

gboolean
xml_add_guid(xmlNodePtr p, const char *tag, const GUID *guid) {

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(guid, FALSE);

  {
    const char *guidstr;
    xmlNodePtr child;

    if(!guid) {
      guidstr = NULL;
    } else {
      guidstr = guid_to_string(guid);
      g_return_val_if_fail(guidstr, FALSE);
    }

    child = xmlNewTextChild(p, NULL, tag, guidstr);
    g_return_val_if_fail(child, FALSE);
    if(guidstr) free((void *) guidstr);
  }
  return(TRUE);
}

/* ============================================================== */

gboolean
xml_add_editable_timespec(xmlNodePtr p,
                          const char *tag,
                          const Timespec *ts,
                          gboolean include_if_zero) {
  xmlNodePtr timespec_xml;
  xmlNodePtr secs_xml;
  size_t num_written;
  struct tm parsed_time;
  time_t tmp_timet;
  char secs_str[512]; /* This should be way bigger than we need.
                         Still, it's bogus, we ought to have
                         astrftime... */
  
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(ts, FALSE); 
  if(!include_if_zero && (ts->tv_sec == 0) && (ts->tv_nsec == 0)) return TRUE;

  tmp_timet = ts->tv_sec;
  if(!localtime_r(&tmp_timet, &parsed_time)) return(FALSE);

  num_written = strftime(secs_str, sizeof(secs_str),
                         TIMESPEC_TIME_FORMAT,
                         &parsed_time);
  if(num_written == 0) return(FALSE);
  
  timespec_xml= xmlNewTextChild(p, NULL, tag, NULL);
  g_return_val_if_fail(timespec_xml, FALSE);

  secs_xml = xmlNewTextChild(timespec_xml, NULL, "s", secs_str);
  g_return_val_if_fail(secs_xml, FALSE);
  
  if(ts->tv_nsec) {
    xmlNodePtr nsec_xml;
    char num_string[22];

    g_snprintf(num_string, sizeof (num_string), "%ld", ts->tv_nsec);

    nsec_xml = xmlNewTextChild(timespec_xml, NULL, "ns", num_string);
    g_return_val_if_fail(nsec_xml, FALSE);
  }

  return(TRUE);
}


static gboolean
xml_add_binary(xmlNodePtr p,
               const char *tag,
               const gchar *format,
               const void *data,
               guint32 size) 
{

  xmlNodePtr value_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(format, FALSE);
  g_return_val_if_fail(data, FALSE);

  value_xml = xmlNewTextChild(p, NULL, tag, NULL);
  g_return_val_if_fail(value_xml, FALSE);
  
  if(size == 0) return(TRUE);

  if(0 == strcmp(format, "hex")) {
    /* Write out the chars as hex, buffering them in max 64 character
       lines.  I was going to use xmlNewTextChild, and xmlTextConcat,
       but that doesn't seem to work, and looking at the source,
       xmlNewTextChild doesn't set the node type to a type that
       xmlTextConcat will recognize and allow. */
    
    const guint max_line_len = 64;
    xmlNodePtr data_xml = NULL;
    GString *output;
    guint32 i;
    
    output = g_string_sized_new(max_line_len + 2);
    
    for(i = 0; i < size; i++) {
      g_string_sprintfa(output, "%x", (int) (((char *) data)[i]));
      if(((i + 1) % max_line_len) == 0) {
        data_xml = xmlNewTextChild(value_xml, NULL, "hex", output->str);
        if(!data_xml) {
          return(FALSE);
          g_string_free(output, TRUE);
        }
        g_string_truncate(output, 0);
      }
    }
    
    if(strlen(output->str) > 0) {
      data_xml = xmlNewTextChild(value_xml, NULL, "hex", output->str);
      if(!data_xml) {
        g_string_free(output, TRUE);
        return(FALSE);
      }
    }
    g_string_free(output, TRUE);

  } else {
    PERR("unknown output format %s.\n", format);
    return(FALSE);
  }
  return(TRUE);
}

/* ============================================================== */

static gboolean xml_add_kvp_value(xmlNodePtr p, kvp_value *val);

static gboolean
xml_add_kvp_glist(xmlNodePtr p, const char *tag, GList *lst) {
  xmlNodePtr list_xml;  
  GList *cursor;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(lst, FALSE);
  
  list_xml = xmlNewTextChild(p, NULL, tag, NULL);
  g_return_val_if_fail(list_xml, FALSE);

  for(cursor = lst; cursor; cursor = cursor->next) {
    kvp_value * val = (kvp_value *) cursor->data;
    if(!xml_add_kvp_value(list_xml, val)) {
      return(FALSE);
    }
  }
  return(TRUE);
}

/* ============================================================== */

gboolean
xml_add_kvp_frame(xmlNodePtr p, const char *tag,
                  const kvp_frame *kvpf,
                  gboolean add_if_empty);

static gboolean
xml_add_kvp_value(xmlNodePtr p, kvp_value *val) {

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(val, FALSE);
  
  switch(kvp_value_get_type(val)) {
  case KVP_TYPE_GINT64:
    return(xml_add_gint64(p, "gint64", kvp_value_get_gint64(val)));
    break;
  case KVP_TYPE_DOUBLE:
    return(xml_add_double(p, "double", kvp_value_get_double(val)));
    break;
  case KVP_TYPE_NUMERIC:
    return(xml_add_gnc_numeric(p, "numeric", kvp_value_get_numeric(val)));
    break;
  case KVP_TYPE_STRING:
    return(xml_add_str(p, "string", kvp_value_get_string(val), TRUE));
    break;
  case KVP_TYPE_GUID:
    return(xml_add_guid(p, "guid", kvp_value_get_guid(val)));
    break;
  case KVP_TYPE_BINARY:
    {
      guint64 size;
      void *binary_data = kvp_value_get_binary(val, &size);
      g_return_val_if_fail(binary_data, FALSE);
      return(xml_add_binary(p, "binary", "hex", binary_data, size));
    }
    break;
  case KVP_TYPE_GLIST:
    return(xml_add_kvp_glist(p, "glist", kvp_value_get_glist(val)));    
    break;
  case KVP_TYPE_FRAME:
    return(xml_add_kvp_frame(p, "frame", kvp_value_get_frame(val), TRUE));
    break;
  default:
    return(FALSE);
    break;
  };
  
  return(TRUE);
}

/* ============================================================== */

static gboolean
xml_add_kvp_slot(xmlNodePtr p, const char *key, kvp_value *val) {
  xmlNodePtr slot_xml;
  xmlNodePtr key_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(key, FALSE);
  g_return_val_if_fail(val, FALSE);

  slot_xml = xmlNewTextChild(p, NULL, "s", NULL);
  g_return_val_if_fail(slot_xml, FALSE);

  key_xml = xmlNewTextChild(slot_xml, NULL, "k", key);
  g_return_val_if_fail(key_xml, FALSE);

  return(xml_add_kvp_value(slot_xml, val));
}

/* ============================================================== */

typedef struct {
  xmlNodePtr node;
  gint64 keycount;
} kvp_value_foreach_info;

static void
xml_add_kvp_value_foreach_adapter(const char *key,
                                  kvp_value *value,
                                  gpointer data) {
  kvp_value_foreach_info *info = (kvp_value_foreach_info *) data;
  xml_add_kvp_slot(info->node, key, value);
  info->keycount++;
}

/* ============================================================== */

gboolean
xml_add_kvp_frame(xmlNodePtr p,
                  const char *tag,
                  const kvp_frame *kvpf,
                  gboolean add_if_empty) {

  xmlNodePtr kvp_xml;
  kvp_value_foreach_info info;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(kvpf, FALSE); 

  kvp_xml = xmlNewNode(NULL, tag);
  g_return_val_if_fail(kvp_xml, FALSE);

  info.node = kvp_xml;
  info.keycount = 0;
  kvp_frame_for_each_slot((kvp_frame *) kvpf,
                          xml_add_kvp_value_foreach_adapter,
                          &info); 
  if(add_if_empty || info.keycount) {
    xmlAddChild(p, kvp_xml);
  } else {
    xmlFreeNode(kvp_xml);
  }
  
  return(TRUE);
}
