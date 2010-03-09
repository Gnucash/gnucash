/*
 * RecentFileMenu.hpp
 * Copyright (C) 2010 Christian Stimming
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_RECENTFILEMENU_HPP
#define GNC_RECENTFILEMENU_HPP

#include <QtGui/QMenu>
#include <QtCore/QStringList>

class QAction;
class QSettings;

namespace gnc
{

/**
 * A menu that shows a list of recently opened files.
 */
class RecentFileMenu: public QMenu
{
    Q_OBJECT

public:
    RecentFileMenu(const QString& title, QWidget *parent = 0);
    ~RecentFileMenu();

    /**
     * Read the internal list from a QSettings array.
     * @param settings QSettings to read from
     * @param groupName name of the array for QSettings::beginReadArray().
     */
    void readSettings(QSettings *settings, const QString &groupName);

    /**
     * Write the internal list to a QSettings array.
     * @param settings QSettings to write to
     * @param groupName name of the array for QSettings::beginWriteArray().
     */
    void writeSettings(QSettings *settings, const QString &groupName);

public slots:
    /**
     * Record the given string as a filename that was (or is)
     * being used in the application.  As a result the given
     * filename will always become the new first menu entry.
     */
    void usingFile(const QString &fileName);

signals:
    /**
     * This signal is emitted whenever the user selects a file from the internally managed
     * menu (i.e. the user wants to open the given file).
     */
    void fileSelected(const QString &fileName);

private slots:
    void on_actionRecentFile();

private:
    void updateMenu();
    void createActions();

private:
    QStringList m_fileNames;

    enum { MaxRecentFiles = 5 };
    QAction *m_actionRecentFile[MaxRecentFiles];

};

} // END namespace gnc

#endif
