/********************************************************************\
 * escape.h : Escape SQL reserved characters in strings             *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * FILE:
 * escape.h
 *
 * FUNCTION:
 * Escape SQL reserved characters \ and ' from strings
 *
 * HISTORY:
 * Linas Vepstas January 2001
 */

#ifndef SQL_ESCAPE_H
#define SQL_ESCAPE_H

typedef struct _escape sqlEscape;

sqlEscape * sqlEscape_new(void);
void sqlEscape_destroy (sqlEscape *);

const char * sqlEscapeString (sqlEscape *, const char *orig_string);

#endif /* SQL_ESCAPE_H */

