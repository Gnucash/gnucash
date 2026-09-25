/*******************************************************************\
 * gnc-csv-preview-refresh.hpp -- Deferred CSV preview refresh      *
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
 * 51 Franklin Street, Fifth Floor    Fax:  +1-617-542-5942         *
 *                                                                  *
\********************************************************************/

#ifndef GNC_CSV_PREVIEW_REFRESH_HPP
#define GNC_CSV_PREVIEW_REFRESH_HPP

#include <glib.h>

/* Owns one deferred CSV preview refresh. It is intentionally local to the
 * CSV import implementation: its callback context must remain valid until
 * dispatch begins. */
class CsvPreviewRefreshIdle
{
public:
    using Callback = void (*) (gpointer user_data);

    CsvPreviewRefreshIdle (Callback callback, gpointer user_data) :
        m_callback {callback},
        m_user_data {user_data}
    {
    }

    ~CsvPreviewRefreshIdle ()
    {
        cancel ();
    }

    CsvPreviewRefreshIdle (const CsvPreviewRefreshIdle&) = delete;
    CsvPreviewRefreshIdle& operator= (const CsvPreviewRefreshIdle&) = delete;
    CsvPreviewRefreshIdle (CsvPreviewRefreshIdle&&) = delete;
    CsvPreviewRefreshIdle& operator= (CsvPreviewRefreshIdle&&) = delete;

    void queue ()
    {
        if (!m_source_id)
            m_source_id = g_idle_add (dispatch, this);
    }

    void cancel ()
    {
        if (m_source_id)
        {
            g_source_remove (m_source_id);
            m_source_id = 0;
        }
    }

private:
    static gboolean dispatch (gpointer user_data)
    {
        auto idle = static_cast<CsvPreviewRefreshIdle *> (user_data);

        /* Clear this first: the refresh may queue its required successor. */
        idle->m_source_id = 0;
        idle->m_callback (idle->m_user_data);

        /* The callback may have destroyed its owner and this helper. */
        return G_SOURCE_REMOVE;
    }

    Callback m_callback;
    gpointer m_user_data;
    guint m_source_id {0};
};

#endif
