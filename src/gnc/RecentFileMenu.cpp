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


#include "RecentFileMenu.hpp"

#include <QMenu>
#include <QAction>
#include <QSettings>

namespace gnc
{

RecentFileMenu::RecentFileMenu(const QString& title, QWidget *parent)
        : QMenu(title, parent)
{
    createActions();
}


RecentFileMenu::~RecentFileMenu()
{
}


void RecentFileMenu::createActions()
{
    for (int i = 0; i < MaxRecentFiles; ++i)
    {
        QAction *newAct = new QAction(this);
        newAct->setVisible(false);
        connect(newAct, SIGNAL(triggered()),
                this, SLOT(on_actionRecentFile()));
        addAction(newAct);

        m_actionRecentFile[i] = newAct;
    }
}


void RecentFileMenu::usingFile(const QString& filename)
{
    if (filename.isEmpty())
        return;

    m_fileNames.removeAll(filename);
    while (m_fileNames.size() > MaxRecentFiles - 1)
        // remove last allowed position
        m_fileNames.removeAt(MaxRecentFiles - 1);
    m_fileNames.insert(0, filename);
    updateMenu();
}


void RecentFileMenu::updateMenu()
{
    for (int i = 0; i < std::min(int(MaxRecentFiles), m_fileNames.size()); ++i)
    {
        const QString& qs = m_fileNames.at(i);
        QAction *act = m_actionRecentFile[i];
        act->setVisible(true);
        act->setText(tr("&%1 %2").arg(i+1).arg(qs));
        act->setData(qs);
    }
    for (int i = m_fileNames.size(); i < MaxRecentFiles; ++i)
    {
        QAction *act = m_actionRecentFile[i];
        act->setVisible(false);
    }

    update();
}


void RecentFileMenu::readSettings(QSettings *settings, const QString &groupName)
{
    int size = settings->beginReadArray(groupName);
    for (int i = 0; i < size; i++)
    {
        settings->setArrayIndex(i);
        QString qs = settings->value("filename").toString();
        if (!qs.isEmpty())
        {
            m_fileNames << qs;
        }
    }
    settings->endArray();
    updateMenu();
}


void RecentFileMenu::writeSettings(QSettings *settings, const QString &groupName)
{
    settings->remove(groupName);
    settings->beginWriteArray(groupName);

    int numElements = m_fileNames.size();
    int j = 0;
    for (int i = 0; i < numElements; i++)
    {
        QString qs = m_fileNames.at(i);
        if (!qs.isEmpty())
        {
            settings->setArrayIndex(j++);
            settings->setValue("filename", qs);
        }
    }

    settings->endArray();
}


void RecentFileMenu::on_actionRecentFile()
{
    QAction *action = qobject_cast<QAction *>(sender());
    if (action)
    {
        QString str = action->data().toString();
        if (!str.isEmpty())
            emit fileSelected(str);
    }
}

} // END namespace gnc
