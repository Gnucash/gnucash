/********************************************************************
 * File: gmtime_r.c
 * Renamed from: core-utils.h
 *
 * Copyright (C) 2001 Linux Developers Group
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 ********************************************************************/

#include "config.h"

#if !HAVE_GMTIME_R
#include <time.h>
#include <string.h>
#include "gmtime_r.h"

#if HAVE_PTHREAD_MUTEX_INIT
#include <pthread.h>
struct tm *
gmtime_r(const time_t *const timep, struct tm *p_tm)
{
    static pthread_mutex_t time_mutex;
    static int time_mutex_inited = 0;
    struct tm *tmp;

    if (!time_mutex_inited) {
        time_mutex_inited = 1;
        pthread_mutex_init(&time_mutex, NULL);
    }

    pthread_mutex_lock(&time_mutex);
    tmp = gmtime(timep);
    if (tmp) {
        memcpy(p_tm, tmp, sizeof(struct tm));
        tmp = p_tm;
    }
    pthread_mutex_unlock(&time_mutex);

    return tmp;
}
#else
struct tm *
gmtime_r(const time_t *const timep, struct tm *p_tm)
{
    static struct tm* tmp;
    tmp = gmtime(timep);
    if (tmp) {
        memcpy(p_tm, tmp, sizeof(struct tm));
        tmp = p_tm;
    }    
    return tmp;
}
#endif /* HAVE_PTHREAD_MUTEX_INIT */

#endif /* !HAVE_GMTIME_R */
