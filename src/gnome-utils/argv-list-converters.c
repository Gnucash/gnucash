/********************************************************************\
 * argv-list-converters.c                                           *
 * Copyright (C) 2000 Gnumatic, Inc                                 *
 * Copyright (C) 2000 James LewisMoss                               *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <libguile.h>

#include "argv-list-converters.h"


char**
gnc_scheme_list_to_nulltermcharpp(int prelen, const char **prepend, SCM list)
{
    SCM next = list;
    char **ret;
    int len = 0;
    int loc;

    if(SCM_CONSP(list))
    {
        int i;
        len = scm_ilength(list) + prelen;
        ret = g_new(char *, len + 1);
        ret[len] = NULL;
        for(i = 0; i < prelen; i++)
        {
            ret[i] = g_strdup(prepend[i]);
        }
    }
    else 
    {
        return NULL;
    }

    loc = prelen;
    while(SCM_CONSP(next)) 
    {
        SCM scm_string = SCM_CAR(next);
        next = SCM_CDR(next);
        if(SCM_STRINGP(scm_string))
        {
	    const gchar *onestr = SCM_STRING_CHARS(scm_string);
            ret[loc] = g_strdup (onestr);
        }
        else
        {
            int i;

            for (i = 0; i < loc; i++)
              g_free (ret[i]);
            g_free(ret);
            return NULL;
        }
        loc++;
    }

    return ret;
}

SCM
gnc_argvarr_to_scheme_list(int argc, const char** argv)
{
    int i;
    SCM ret = SCM_EOL;

    for(i = 0; i < argc; i++)
    {
      ret = scm_cons(scm_makfrom0str(argv[i]), ret);
    }

    return scm_reverse(ret);
}

void
gnc_free_argv(char** argv)
{
    char **now = argv;

    if(!argv)
    {
        return;
    }
    
    while(*now != 0)
    {
        g_free(*now);
        now++;
    }
    g_free(argv);
}

int
argv_length(char** nulltermlist)
{
    int ret = 0;

    if(!nulltermlist)
    {
        return 0;
    }
    
    while(nulltermlist[ret] != 0)
        ret++;
    return ret;
}
