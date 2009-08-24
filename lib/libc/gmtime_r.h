/********************************************************************
 * File: gmtime_r.h
 *
 * Copyright (C) 2001 Linux Developers Group
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>
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

#ifndef __GMTIME_R_H__
#define __GMTIME_R_H__

#if !HAVE_GMTIME_R
#include <time.h>
/*
 * Version of "gmtime_r()", for the benefit of OSes that don't have it.
 */
extern struct tm *gmtime_r(const time_t *const timep, struct tm *p_tm);
#endif

#endif

