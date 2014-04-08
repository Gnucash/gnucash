/*
 * FILE:
 * table-html.h
 *
 * FUNCTION:
 * This header defines the HTML specific functions
 * associated with the Table GUI.   This is just sample code. 
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/


#ifndef __XACC_TABLE_HTML_H__
#define __XACC_TABLE_HTML_H__

int xaccTableDumpHTML (Table * table, int fd);
int xaccTablePrintHTML (Table * table, char *filename);
void xaccTableWebServeHTML (Table * table, unsigned short port);


#endif __XACC_TABLE_HTML_H__
/* ================== end of file ======================= */
