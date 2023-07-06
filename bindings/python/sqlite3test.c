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

#include <config.h>
#include "qofsession.h"
#define TESTFILE "/tmp/blah.gnucash"
int main()
{
    const char* testurl = "sqlite3://" TESTFILE;
    char * no_args[1] = { NULL };
    QofSession* s = NULL;

    qof_log_init();
    qof_init();
    gnc_engine_init(0, no_args);

    s = qof_session_new(NULL);
    qof_session_begin(s, testurl, SESSION_NEW_STORE);
    qof_session_load(s, NULL);
    qof_session_save(s, NULL);
    qof_session_end(s);
    qof_session_destroy(s);
    unlink(TESTFILE);
    return 0;
}
