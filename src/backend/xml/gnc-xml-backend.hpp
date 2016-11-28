/********************************************************************
 * gnc-xml-backend.hpp: Declare XML file backend.                 *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                   *
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

#ifndef __GNC_XML_BACKEND_HPP__
#define __GNC_XML_BACKEND_HPP__

extern "C"
{
#include <qof.h>
}

#include <string>
#include <qof-backend.hpp>

class GncXmlBackend : public QofBackend
{
public:
    GncXmlBackend() = default;
    GncXmlBackend(const GncXmlBackend&) = delete;
    GncXmlBackend operator=(const GncXmlBackend&) = delete;
    GncXmlBackend(const GncXmlBackend&&) = delete;
    GncXmlBackend operator=(const GncXmlBackend&&) = delete;
    ~GncXmlBackend() = default;
    void session_begin(QofSession* session, const char* book_id,
                       bool ignore_lock, bool create, bool force) override;
    void session_end() override;
    void load(QofBook* book, QofBackendLoadType loadType) override;
    /* The XML backend isn't able to do anything with individual instances. */
    void export_coa(QofBook*) override;
    void sync(QofBook* book) override;
    void safe_sync(QofBook* book) override { sync(book); } // XML sync is inherently safe.
    const char * get_filename() { return m_fullpath.c_str(); }
    QofBook* get_book() { return m_book; }

private:
    bool save_may_clobber_data();
    bool get_file_lock();
    bool link_or_make_backup(const std::string& orig, const std::string& bkup);
    bool backup_file();
    bool write_to_file(bool make_backup);
    void remove_old_files();
    void write_accounts(QofBook* book);
    bool check_path(const char* fullpath, bool create);

    std::string m_dirname;
    std::string m_lockfile;
    std::string m_linkfile;
    int m_lockfd;

    QofBook* m_book;  /* The primary, main open book */
};
#endif // __GNC_XML_BACKEND_HPP__
