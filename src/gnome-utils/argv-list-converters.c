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

#include <guile/gh.h>
#include <glib.h>

#include "argv-list-converters.h"


char**
gnc_scheme_list_to_nulltermcharpp(int prelen, const char **prepend, SCM list)
{
    SCM next = list;
    char **ret;
    int len = 0;
    int loc;

    if(gh_pair_p(list))
    {
        int i;
        len = gh_length(list) + prelen;
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
    while(gh_pair_p(next)) 
    {
        SCM scm_string = gh_car(next);
        next = gh_cdr(next);
        if(gh_string_p(scm_string))
        {
            char *onestr = gh_scm2newstr(scm_string, 0);
            ret[loc] = g_strdup (onestr);
            if (onestr)
              free (onestr);
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
      /* FIXME: when we drop support older guiles,
       * drop the (char *) coercion. */
      ret = gh_cons(gh_str02scm((char *) argv[i]), ret);
    }

    return gh_reverse(ret);
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
