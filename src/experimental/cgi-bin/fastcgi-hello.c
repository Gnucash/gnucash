/********************************************************************\
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


#include <fcgi_stdio.h>
#include <stdlib.h>
#include <string.h>

extern char **environ;

int main (int argc, char *argv[])
{
    char *query_string, *method, *len = 0x0;
    int count = 0;
    int i, ilen = 0;

    while (FCGI_Accept() >= 0)
    {
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
        printf ("<p>The QUERY_STRING environment vairable is %s\n"
                "The other environment variables are:<p>", query_string);

        for (i = 0; environ[i]; i++)
        {
            printf ("<br>%s\n", environ[i]);
        }

        method = getenv ("REQUEST_METHOD");
        if (!strcmp (method, "POST"))
        {
            char * bufp;
            ilen = atoi (getenv ("CONTENT_LENGTH"));
            printf ("<P>This is a method=post request, "
                    "with content length=%d<P>\n", ilen);
            bufp = (char *) malloc (ilen);
            fread (bufp, ilen, 1, stdin);

            printf ("The POST data is<P>%s\n", bufp);
            free (bufp);
        }

    }
    return 0;
}
