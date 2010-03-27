/*
 * mainwindow.cpp
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

#include <QtCore/QSettings>
#include <QtGui/QCloseEvent>
#include <QtGui/QFileDialog>
#include <QtGui/QMessageBox>
#include <QtGui/QToolBar>
#include <QtGui/QProgressBar>
#include <QtGui/QUndoStack>
#include <QDebug>

#include "config.h"
#include "mainwindow.hpp"
#include "ui_mainwindow.h"

// gnucash includes
#include <glib/gi18n.h>
extern "C"
{
#include "qof.h"
#include "engine/gnc-hooks.h"
#include "core-utils/gnc-uri-utils.h"
#include "engine/Account.h"
#include "engine/TransLog.h"
}

#include "gnc/Account.hpp"
#include "gnc/AccountItemModel.hpp"
#include "gnc/Book.hpp"
#include "gnc/Numeric.hpp"
#include "gnc/Split.hpp"
#include "gnc/SplitListModel.hpp"
#include "gnc/RecentFileMenu.hpp"
#include "gnc/SplitListView.hpp"

namespace gnc
{

inline QString errorToString(QofBackendError err)
{
    return errorToStringPair(err).first;
}
inline QString errorToDescription(QofBackendError err)
{
    return errorToStringPair(err).second;
}

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static const char* PROPERTY_TAB_LABEL = "tab_label";
static const char* PROPERTY_TAB_POSITION = "tab_position";
static const char* PROPERTY_TAB_ISCURRENT = "tab_iscurrent";
static const char* PROPERTY_TAB_PREVIOUSPOS = "tab_previouspos";

// ////////////////////////////////////////////////////////////

MainWindow::MainWindow()
        : ui(new Ui::MainWindow)
        , m_menuRecentFiles(new RecentFileMenu(tr("Open &Recent")))
        , m_undoStack(new QUndoStack(this))
{
    ui->setupUi(this);

    createActions();
    createToolBars();
    createStatusBar();

    readSettings();

    connect(m_undoStack, SIGNAL(cleanChanged(bool)),
            this, SLOT(documentCleanStateChanged(bool)));

    setWindowIcon(QIcon(":/pixmaps/gnucash-icon-32x32.png"));

    newFile();
    setUnifiedTitleAndToolBarOnMac(true);
}

MainWindow::~MainWindow()
{
    if (m_session.get())
    {
        qof_session_destroy(m_session.get());
        m_session.reset();
    }
}

// Auto-connected to ui->actionOpen's signal triggered()
void MainWindow::on_actionOpen_triggered()
{
    if (maybeSave())
    {
        QString fileName = QFileDialog::getOpenFileName(this);
        if (!fileName.isEmpty())
            loadFile(fileName);
    }
}

void MainWindow::loadFileMaybe(const QString &fileName)
{
    if (maybeSave())
    {
        loadFile(fileName);
    }
}


// Auto-connected to ui->actionSave's signal triggered()
bool MainWindow::on_actionSave_triggered()
{
    if (m_currentFilename.isEmpty())
    {
        return on_actionSave_as_triggered();
    }
    else
    {
        return saveFile();
    }
}

// Auto-connected to ui->actionSave_as's signal triggered()
bool MainWindow::on_actionSave_as_triggered()
{
    QString fileName = QFileDialog::getSaveFileName(this);
    if (fileName.isEmpty())
        return false;

    return saveFileAs(fileName);
}

// Auto-connected to ui->actionAbout's signal triggered()
void MainWindow::on_actionAbout_triggered()
{
    QMessageBox::about(this, tr("About Application"),
                       tr("This is a Gnucash C++ gui example, from the Qt4 Application example. It demonstrates how to "
                          "write modern GUI applications using Qt, with a menu bar, "
                          "toolbars, and a status bar."));
}

void MainWindow::documentWasModified()
{
    documentCleanStateChanged(false);
}

void MainWindow::documentCleanStateChanged(bool documentIsClean)
{
    bool unchanged = (documentIsClean == isWindowModified());

    setWindowModified(!documentIsClean);
    if (!unchanged)
        updateWindowTitle();
}

// Auto-connected to ui->textBrowser's signal anchorClicked()
void MainWindow::on_textBrowser_anchorClicked(const QUrl &url)
{
    QMessageBox::information(this, tr("Got you!"),
                             tr("Obviously you clicked the link with the URL %1.")
                             .arg(url.toString()));
}

void MainWindow::createActions()
{
    ui->actionNew->setShortcuts(QKeySequence::New);
    ui->actionOpen->setShortcuts(QKeySequence::Open);
    ui->actionSave->setShortcuts(QKeySequence::Save);
    ui->actionSave_as->setShortcuts(QKeySequence::SaveAs);

    m_actionRedo = m_undoStack->createRedoAction(ui->menuEdit, tr("&Redo"));
    m_actionRedo->setIcon(QIcon(":/gtk-icons/gtk-redo.png"));
    m_actionRedo->setShortcuts(QKeySequence::Redo);
    m_actionUndo = m_undoStack->createUndoAction(ui->menuEdit, tr("&Undo"));
    m_actionUndo->setIcon(QIcon(":/gtk-icons/gtk-undo.png"));
    m_actionUndo->setShortcuts(QKeySequence::Undo);
    ui->menuEdit->insertAction(ui->actionCut, m_actionUndo);
    ui->menuEdit->insertAction(ui->actionCut, m_actionRedo);
    ui->menuEdit->insertSeparator(ui->actionCut);

    ui->actionCut->setShortcuts(QKeySequence::Cut);
    ui->actionCopy->setShortcuts(QKeySequence::Copy);
    ui->actionPaste->setShortcuts(QKeySequence::Paste);

    ui->actionCloseTab->setShortcuts(QKeySequence::Close);

    connect(ui->actionNew, SIGNAL(triggered()), this, SLOT(newFile()));
    connect(ui->actionExit, SIGNAL(triggered()), this, SLOT(close()));
    connect(ui->actionAbout_Qt, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

    ui->actionCut->setEnabled(false);
    ui->actionCopy->setEnabled(false);

//     connect(ui->textBrowser, SIGNAL(copyAvailable(bool)),
//             ui->actionCut, SLOT(setEnabled(bool)));
    connect(ui->textBrowser, SIGNAL(copyAvailable(bool)),
            ui->actionCopy, SLOT(setEnabled(bool)));
    connect(ui->actionCopy, SIGNAL(triggered()),
            ui->textBrowser, SLOT(copy()));

    connect(ui->treeView, SIGNAL(activated(const QModelIndex &)),
            this, SLOT(accountItemActivated(const QModelIndex&)));
    connect(ui->tableView, SIGNAL(activated(const QModelIndex &)),
            this, SLOT(accountItemActivated(const QModelIndex&)));
}

void MainWindow::createToolBars()
{
    ui->menuFile->insertMenu(ui->actionSave, m_menuRecentFiles.data());
    connect(m_menuRecentFiles.data(), SIGNAL(fileSelected(const QString &)),
            this, SLOT(loadFileMaybe(const QString&)));

    m_fileToolBar = addToolBar(tr("File"));
    m_fileToolBar->addAction(ui->actionNew);
    m_fileToolBar->addAction(ui->actionOpen);
    m_fileToolBar->addAction(ui->actionSave);
    m_fileToolBar->addAction(ui->actionCloseTab);

    m_editToolBar = addToolBar(tr("Edit"));
    m_editToolBar->addAction(m_actionUndo);
    m_editToolBar->addAction(m_actionRedo);
    m_editToolBar->addAction(ui->actionCut);
    m_editToolBar->addAction(ui->actionCopy);
    m_editToolBar->addAction(ui->actionPaste);
}

void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
}

void MainWindow::readSettings()
{
    QSettings settings("gnucash.org", "Cutecash");
    QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
    QSize size = settings.value("size", QSize(400, 400)).toSize();
    resize(size);
    move(pos);
    m_menuRecentFiles->readSettings(&settings, "RecentFiles");
}

void MainWindow::writeSettings()
{
    QSettings settings("gnucash.org", "Cutecash");
    settings.setValue("pos", pos());
    settings.setValue("size", size());
    m_menuRecentFiles->writeSettings(&settings, "RecentFiles");
}

bool MainWindow::maybeSave()
{
    if (isWindowModified())
    {
        QMessageBox::StandardButton ret;
        ret = QMessageBox::warning(this, tr("Application"),
                                   tr("The document has been modified.\n"
                                      "Do you want to save your changes?"),
                                   QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        if (ret == QMessageBox::Save)
            return on_actionSave_triggered();
        else if (ret == QMessageBox::Cancel)
            return false;
    }
    return true;
}

void MainWindow::setCurrentFile(const QString &fileName)
{
    m_menuRecentFiles->usingFile(fileName);
    m_currentFilename = fileName;
    documentCleanStateChanged(true);

    updateWindowTitle();
}

void MainWindow::updateWindowTitle()
{
    QString shownName;
    if (m_currentFilename.isEmpty())
        shownName = "untitled.txt";
    else
        shownName = strippedName(m_currentFilename);

    setWindowTitle(tr("%1[*]%2 - %3").arg(shownName).arg(isWindowModified() ? "(*)" : "").arg(tr("Application")));
}

QString MainWindow::strippedName(const QString &fullFileName)
{
    return QFileInfo(fullFileName).fileName();
}


// ////////////////////////////////////////////////////////////

// Auto-connected to ui->actionCloseTab's signal triggered
void MainWindow::on_actionCloseTab_triggered()
{
    on_tabWidget_tabCloseRequested(ui->tabWidget->currentIndex());
}

// Auto-connected to ui->tabWidget's signal tabCloseRequested(int)
void MainWindow::on_tabWidget_tabCloseRequested(int index)
{
    QWidget *widget = ui->tabWidget->widget(index);
    if (widget == ui->textBrowserTab)
    {
        ui->actionViewWelcomepage->setChecked(false);
        reallyRemoveTab(index);
    }
    else if (widget == ui->treeViewTab)
    {
        ui->actionViewAccountTree->setChecked(false);
        reallyRemoveTab(index);
    }
    else if (widget == ui->tableViewTab)
    {
        ui->actionViewAccountList->setChecked(false);
        reallyRemoveTab(index);
    }
    else
    {
        QVariant prevPos = widget->property(PROPERTY_TAB_PREVIOUSPOS);
        if (prevPos.isValid())
            ui->tabWidget->setCurrentIndex(prevPos.toInt());
        ui->tabWidget->removeTab(index);
        delete widget;
    }
}

// Auto-connected to ui->actionViewAccountTree's signal triggered()
void MainWindow::on_actionViewAccountTree_triggered(bool checked)
{
    viewOrHideTab(checked, ui->treeViewTab);
}

// Auto-connected to ui->actionViewAccountList's signal triggered()
void MainWindow::on_actionViewAccountList_triggered(bool checked)
{
    viewOrHideTab(checked, ui->tableViewTab);
}

// Auto-connected to ui->actionViewWelcomepage's signal triggered()
void MainWindow::on_actionViewWelcomepage_triggered(bool checked)
{
    viewOrHideTab(checked, ui->textBrowserTab);
}

void MainWindow::viewOrHideTab(bool checked, QWidget *widget)
{
    if (checked)
    {
        QVariant tabLabel = widget->property(PROPERTY_TAB_LABEL);
        Q_ASSERT(tabLabel.isValid());
        QVariant tabPosition = widget->property(PROPERTY_TAB_POSITION);
        Q_ASSERT(tabPosition.isValid());
        QVariant tabIsCurrentV = widget->property(PROPERTY_TAB_ISCURRENT);
        Q_ASSERT(tabIsCurrentV.isValid());
        bool tabIsCurrent = tabIsCurrentV.toBool();

        if (tabIsCurrent)
            widget->setProperty(PROPERTY_TAB_PREVIOUSPOS, ui->tabWidget->currentIndex());

        ui->tabWidget->insertTab(tabPosition.toInt(), widget, tabLabel.toString());
        if (tabIsCurrent)
            ui->tabWidget->setCurrentWidget(widget);
    }
    else
    {
        on_tabWidget_tabCloseRequested(ui->tabWidget->indexOf(widget));
    }
}

void MainWindow::reallyRemoveTab(int index)
{
    QWidget *widget = ui->tabWidget->widget(index);
    widget->setProperty(PROPERTY_TAB_LABEL, ui->tabWidget->tabText(index));
    widget->setProperty(PROPERTY_TAB_POSITION, index);
    widget->setProperty(PROPERTY_TAB_ISCURRENT, (index == ui->tabWidget->currentIndex()));
    ui->tabWidget->removeTab(index);
    QVariant prevPos = widget->property(PROPERTY_TAB_PREVIOUSPOS);
    if (prevPos.isValid())
        ui->tabWidget->setCurrentIndex(prevPos.toInt());
}

// Auto-connected to ui->tabWidget's signal currentChanged(int)
void MainWindow::on_tabWidget_currentChanged(int index)
{
    QWidget *widget = ui->tabWidget->widget(index);
    bool tabWithAccounts = (widget != ui->textBrowserTab);
    ui->menuAccount->setEnabled(tabWithAccounts);
}

void MainWindow::accountItemActivated(const QModelIndex & index)
{
    if (index.model() != m_accountTreeModel
            && index.model() != m_accountListModel)
    {
        qDebug() << "Wrong model; row=" << (index.isValid()? index.row() : -1);
        return;
    }
    Account account(static_cast< ::Account*>(index.internalPointer()));
    if (!account)
    {
        qDebug() << "Account is null; why?";
        return;
    }

    // We create a new model for this account which will query it for
    // its splits, and also a view widget for this list.
    QTableView *tableView =
        new SplitListView(account, m_undoStack, ui->tabWidget);
    ui->actionCut->setEnabled(tableView->model()->rowCount() > 0);

    // Insert this as a new tab
    tableView->setProperty(PROPERTY_TAB_PREVIOUSPOS, ui->tabWidget->currentIndex());
    ui->tabWidget->addTab(tableView, account.getName());
    ui->tabWidget->setCurrentWidget(tableView);

    connect(tableView->selectionModel(), SIGNAL(selectionChanged(const QItemSelection &, const QItemSelection &)),
            this, SLOT(selectionChanged(const QItemSelection &, const QItemSelection & )));
}

void MainWindow::selectionChanged(const QItemSelection & selected, const QItemSelection & deselected )
{
    ui->actionCut->setEnabled(!selected.isEmpty());
    //ui->actionCopy->setEnabled(!selected.isEmpty());
}

// Auto-connected to actionCut's signal triggered()
void MainWindow::on_actionCut_triggered()
{
    QWidget *widget = ui->tabWidget->currentWidget();
    QTableView *tableView = qobject_cast<QTableView *>(widget);
    if (tableView)
    {
//         QModelIndexList selection = tableView->selectionModel()->selectedIndexes();
//         QSet<int> rows;
//         Q_FOREACH (QModelIndex index, selection)
//         {
//             rows.insert(index.row());
//         }
//         qDebug() << "Removing number of rows:" << rows.size();
        QModelIndex index = tableView->currentIndex();
        if (!index.isValid())
            return;
        QAbstractItemModel *model = tableView->model();
        Q_ASSERT(model);
        model->removeRow(index.row());
    }
}

// ////////////////////////////////////////////////////////////

void MainWindow::closeEvent(QCloseEvent *event)
{
    if (maybeSave())
    {
        writeSettings();
        event->accept();

        /* disable events; otherwise the mass deletion of accounts and
         * transactions during shutdown would cause massive redraws */
        qof_event_suspend ();

        qof_session_call_close_hooks(m_session.get());
        gnc_hook_run(HOOK_BOOK_CLOSED, m_session.get());

        qof_session_destroy(m_session.get());
        m_session.reset();

        qof_event_resume ();
    }
    else
    {
        event->ignore();
    }
}

void MainWindow::newFile()
{
    if (maybeSave())
    {

        if (m_session)
        {
            /* close any ongoing file sessions, and free the accounts.
             * disable events so we don't get spammed by redraws. */
            qof_event_suspend ();

            m_session.call_close_hooks();
            gnc_hook_run(HOOK_BOOK_CLOSED, m_session.get());

            qof_session_destroy(m_session.get());
            m_session.reset();
            qof_event_resume ();
        }

        /* start a new book */
        m_session.reset(qof_session_new());

        gnc_hook_run(HOOK_NEW_BOOK, NULL);

        /* Call this after re-enabling events. */
        gnc_hook_run(HOOK_BOOK_OPENED, m_session.get());

        setCurrentFile("");
    }
}

} // END namespace gnc
