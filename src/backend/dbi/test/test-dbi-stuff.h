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

typedef struct
{
    QofBook* book_1;
    QofBook* book_2;
    gboolean result;
} CompareInfoStruct;

void do_compare( QofBook* book_1, QofBook* book_2, const gchar* id, QofInstanceForeachCB cb, const gchar* msg );
/**
 * Test storing a session contents to a db, reloading into a new session, then comparing the
 * two sessions.
 *
 * @param driver Driver name
 * @param session_1 Session to test
 * @param url Database URL
 */
void test_dbi_store_and_reload( const gchar* driver, QofSession* session_1, const gchar* url );

/** Test the safe_save mechanism.  Beware that this test used on its
 * own doesn't ensure that the resave is done safely, only that the
 * database is intact and unchanged after the save. To observe the
 * safety one must run the test in a debugger and break after the
 * rename step of gnc_dbi_safe_sync, then examine the database in the
 * appropriate shell.
 */
void test_dbi_safe_save( const gchar* driver, const gchar* url );

/** Test the version control mechanism.
 */
void test_dbi_version_control( const gchar* driver,  const gchar* url );

#endif
