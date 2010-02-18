/***************************************************************************
 *            test-dbi-stuff.h
 *
 *  Tests saving and loading to a dbi/sqlite3 db
 *
 *  Copyright (C) 2009  Phil Longstaff <plongstaff@rogers.com>
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */

#ifndef _TEST_DBI_STUFF_H_
#define _TEST_DBI_STUFF_H_

/**
 * Test storing a session contents to a db, reloading into a new session, then comparing the
 * two sessions.
 *
 * @param driver Driver name
 * @param session_1 Session to test
 * @param url Database URL
 */
void test_dbi_store_and_reload( const gchar* driver, QofSession* session_1, const gchar* url );

#endif
