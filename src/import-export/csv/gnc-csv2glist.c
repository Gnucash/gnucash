
/*
    Copyright 2004 Kevin dot Hammack at comcast dot net

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301 USA


 */

//#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>
#include <sys/types.h>
#include <fcntl.h>

//#include "file-utils.h"

#define TEST_CSV

/*
  <warlord> Yea, just returning a GList would be fine..  Or a GList of
  GLists, one per row..  
*/


/* 
   split_csv_list - Take a string, split on commas into a GList.  
   Tricky part: honor quotes.  
   Quotes aren't stripped.  
   This would have been both easier and cleaner in scheme, and / or
   using regular expressions.  
   I was thinking small and fast when I wrote it. :-P
*/

static GList *
split_csv_line(char *line) {
     GList *csvlist = NULL;
     gchar *begin;
     gchar *current;
     gchar *cell;
     gchar quote=0;
     gboolean eol = FALSE;

     current = line;
     begin = current;

     while (!eol) {
	  
	  if (quote && 
	      (*current == quote) && (*(current-1) != '\\') && 
	      (current != begin)) {

	       quote = 0;
	  }
	  else if (!quote && (*current == '"')) { quote = '"'; }
	  else if (!quote && (*current == '\'')) { quote = '\''; }

	  if (!quote && (*current == ',')) { *current = 0; }

	  if (*current == '\n') {
	       *current = 0;
	       eol = TRUE;
	  }

	  if (*current == 0) {
	       cell = g_strdup( begin );
	       csvlist = g_list_prepend(csvlist, cell);
	       current++;
	       begin = current;
	       quote = 0;
	  }
	  else {
	       current++;
	  }

     }

     return g_list_reverse(csvlist);
}


#if 1
gint64
gnc_getline (gchar **line, FILE *file)
{
  char str[BUFSIZ];
  gint64 len;
  GString *gs = g_string_new("");

  if (!line || !file) return 0;

  while (fgets(str, sizeof(str), file) != NULL) {
    g_string_append(gs, str);

    len = strlen(str);
    if (str[len-1] == '\n')
      break;
  }

  len = gs->len;
  *line = g_string_free(gs, FALSE);
  return len;
}
#endif


GList *
gnc_csv_parse (FILE *handle) 
{
     GList *csvlists = NULL;
     ssize_t bytes_read;
     char *line;
    
     while (bytes_read = gnc_getline (&line, handle) > 0) {
	  csvlists = g_list_prepend(csvlists, split_csv_line(line));
	  g_free(line);
     }

     return g_list_reverse(csvlists);
}



#ifdef TEST_CSV

static void print_glist_rec (GList *list) {

}
//     print_glist_rec(list);

static void print_glist(GList *list, gpointer dummy) {

     printf("%d: (", g_list_length(list));
     
     while (list != NULL) {  
	  if ((list->data != NULL) && ( *((char *) list->data) != 0)) {
	       printf( "%s",  list->data) ;
	  }
	  else {
	       printf( "\"\"" );
	  }
	  list = list->next;
	  if (list) {
	       printf(" ");
	  }
     }

     printf(")\n");
}
 
int main (int argc, char **argv) {

     FILE *fp;
     int result;
     GList *parsed_csv;
     GList *current;
     int dummy = 1;

     if (argc < 2) {
	  printf("usage:\n\tcsv2glist fname.csv\n");
     }

     fp = g_fopen (argv[1], "r");
     if (fp == NULL) return 1;

     parsed_csv = gnc_csv_parse(fp);

     g_list_foreach(parsed_csv, (GFunc)print_glist, &dummy);

}

#endif
