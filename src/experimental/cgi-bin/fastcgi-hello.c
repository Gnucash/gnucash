
#include "fcgi_stdio.h"
#include <stdlib.h>

extern char **environ;

int main (int argc, char *argv[])
{
   char * query_string;
   int count = 0;
   int i;

   while(FCGI_Accept() >= 0){
      printf("Content-type: text/html\r\n"
             "\r\n"
             "<title>FastCGI Hello!</title>"
             "<h1>FastCGI Hello!</h1>"
             "Request number %d running on host <i>%s</i>\n",
              ++count, getenv("SERVER_NAME"));

      printf("<p>If you have configured fastcgi correctly, then "
             "the request number should increment every time you "
             "hit reload on your browser.  You should also see "
             "\"%s\" (the name of this program) showing up in ps ax.\n",
             argv[0]);

      query_string = getenv ("QUERY_STRING");
      printf ("<p>QUERY_STRING is %s<p>\n", query_string);

      for (i=0; environ[i]; i++) 
      {
         printf ("<br>%s\n", environ[i]);
      }
   }
   return 0;
}
