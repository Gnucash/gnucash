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

#include "gnc/Cmd.hpp"

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

MainWindow::MainWindow()
        : ui(new Ui::MainWindow)
        , m_undoStack(new QUndoStack(this))
{
    ui->setupUi(this);

    createActions();
    createToolBars();
    createStatusBar();

    readSettings();

//     connect(ui->labelMain, SIGNAL(linkActivated(const QString&)),
//             this, SLOT(documentWasModified()));
    connect(m_undoStack, SIGNAL(cleanChanged(bool)),
            this, SLOT(documentCleanStateChanged(bool)));

    setWindowIcon(QIcon(":/pixmaps/gnucash-icon-32x32.png"));

    newFile();
    setUnifiedTitleAndToolBarOnMac(true);
}

MainWindow::~MainWindow()
{
    delete ui;
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
    if (curFile.isEmpty())
    {
        return on_actionSave_as_triggered();
    }
    else
    {
        return saveFile(curFile);
    }
}

// Auto-connected to ui->actionSave_as's signal triggered()
bool MainWindow::on_actionSave_as_triggered()
{
    QString fileName = QFileDialog::getSaveFileName(this);
    if (fileName.isEmpty())
        return false;

    return saveFile(fileName);
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
    setWindowModified(true);
}

void MainWindow::documentCleanStateChanged(bool clean)
{
    bool unchanged = (clean == isWindowModified());

    setWindowModified(!clean);
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

    QAction *redo = m_undoStack->createRedoAction(ui->menuEdit, tr("&Redo"));
    redo->setIcon(QIcon(":/gtk-icons/gtk-redo.png"));
    redo->setShortcuts(QKeySequence::Redo);
    QAction *undo = m_undoStack->createUndoAction(ui->menuEdit, tr("&Undo"));
    undo->setIcon(QIcon(":/gtk-icons/gtk-undo.png"));
    undo->setShortcuts(QKeySequence::Undo);
    ui->menuEdit->insertAction(ui->actionCut, undo);
    ui->menuEdit->insertAction(ui->actionCut, redo);
    ui->menuEdit->insertSeparator(ui->actionCut);

    ui->actionCut->setShortcuts(QKeySequence::Cut);
    ui->actionCopy->setShortcuts(QKeySequence::Copy);
    ui->actionPaste->setShortcuts(QKeySequence::Paste);

    ui->actionViewClose->setShortcuts(QKeySequence::Close);

    connect(ui->actionNew, SIGNAL(triggered()), this, SLOT(newFile()));
    connect(ui->actionExit, SIGNAL(triggered()), this, SLOT(close()));

//     connect(ui->actionCut, SIGNAL(triggered()), ui->textEdit, SLOT(cut()));
//     connect(ui->actionCopy, SIGNAL(triggered()), ui->textEdit, SLOT(copy()));
//     connect(ui->actionPaste, SIGNAL(triggered()), ui->textEdit, SLOT(paste()));

    connect(ui->actionAbout_Qt, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

    ui->actionCut->setEnabled(false);
    ui->actionCopy->setEnabled(false);

//     connect(ui->textEdit, SIGNAL(copyAvailable(bool)),
//             ui->actionCut, SLOT(setEnabled(bool)));
//     connect(ui->textEdit, SIGNAL(copyAvailable(bool)),
//             ui->actionCopy, SLOT(setEnabled(bool)));

    connect(ui->treeView, SIGNAL(activated(const QModelIndex &)),
            this, SLOT(accountItemActivated(const QModelIndex&)));
    connect(ui->tableView, SIGNAL(activated(const QModelIndex &)),
            this, SLOT(accountItemActivated(const QModelIndex&)));
}

void MainWindow::createToolBars()
{
    menuRecentFiles = new RecentFileMenu(tr("Open &Recent"));
    ui->menuFile->insertMenu(ui->actionSave, menuRecentFiles);
    connect(menuRecentFiles, SIGNAL(fileSelected(const QString &)),
            this, SLOT(loadFileMaybe(const QString&)));

    fileToolBar = addToolBar(tr("File"));
    fileToolBar->addAction(ui->actionNew);
    fileToolBar->addAction(ui->actionOpen);
    fileToolBar->addAction(ui->actionSave);

    editToolBar = addToolBar(tr("Edit"));
    editToolBar->addAction(ui->actionCut);
    editToolBar->addAction(ui->actionCopy);
    editToolBar->addAction(ui->actionPaste);
}

void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
}

void MainWindow::readSettings()
{
    QSettings settings("Trolltech", "Application Example");
    QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
    QSize size = settings.value("size", QSize(400, 400)).toSize();
    resize(size);
    move(pos);
    menuRecentFiles->readSettings(&settings, "RecentFiles");
}

void MainWindow::writeSettings()
{
    QSettings settings("Trolltech", "Application Example");
    settings.setValue("pos", pos());
    settings.setValue("size", size());
    menuRecentFiles->writeSettings(&settings, "RecentFiles");
}

bool MainWindow::maybeSave()
{
    if (false)//ui->textEdit->document()->isModified())
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
    menuRecentFiles->usingFile(fileName);
    curFile = fileName;
//     ui->textEdit->document()->setModified(false);
    setWindowModified(false);

    updateWindowTitle();
}

void MainWindow::updateWindowTitle()
{
    QString shownName;
    if (curFile.isEmpty())
        shownName = "untitled.txt";
    else
        shownName = strippedName(curFile);

    setWindowTitle(tr("%1[*]%2 - %3").arg(shownName).arg(isWindowModified() ? "(*)" : "").arg(tr("Application")));
}

QString MainWindow::strippedName(const QString &fullFileName)
{
    return QFileInfo(fullFileName).fileName();
}


// ////////////////////////////////////////////////////////////

// Auto-connected to ui->actionViewClose's signal triggered
void MainWindow::on_actionViewClose_triggered()
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
        QVariant tabLabel = widget->property("tab_label");
        Q_ASSERT(tabLabel.isValid());
        QVariant tabPosition = widget->property("tab_position");
        Q_ASSERT(tabPosition.isValid());
        QVariant tabIsCurrent = widget->property("tab_iscurrent");
        Q_ASSERT(tabIsCurrent.isValid());
        ui->tabWidget->insertTab(tabPosition.toInt(), widget, tabLabel.toString());
        if (tabIsCurrent.toBool())
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
    widget->setProperty("tab_label", ui->tabWidget->tabText(index));
    widget->setProperty("tab_position", index);
    widget->setProperty("tab_iscurrent", (index == ui->tabWidget->currentIndex()));
    ui->tabWidget->removeTab(index);
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
        qDebug() << "Wrong model";
        return;
    }
    Account account(static_cast< ::Account*>(index.internalPointer()));
    if (!account)
    {
        qDebug() << "Account is null; why?";
        return;
    }

    // We have an account, so obtains its list of splits now.
    ::SplitList* slist = account.getSplitList();

    // We create a new model for this list of splits and also a view
    // widget for this list.
    QTableView *tableView = new QTableView(ui->tabWidget); // FIXME: Is this parent correct?
    SplitListModel *smodel = new SplitListModel(Split::fromGList(slist), m_undoStack, tableView);
    tableView->setModel(smodel);
    tableView->setAlternatingRowColors(true);

    // Insert this as a new tab
    ui->tabWidget->addTab(tableView, account.getName());
    ui->tabWidget->setCurrentWidget(tableView);
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

static void
gnc_book_opened (Session& sess)
{
    gnc_hook_run(HOOK_BOOK_OPENED, sess.get());
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
        gnc_book_opened (m_session);

        setCurrentFile("");
    }
}

namespace
{
/** This is a workaround function object so that we can obtain a
 * QofPercentageFunc without extra boost::bind usage; obviously due to
 * the static member variable it will not work if multiple instances
 * are in use simultaneously */
class progress_functor
{
public:
    progress_functor(QProgressBar *progressBar)
    {
        m_progressBar = progressBar;
    }
    ~progress_functor()
    {
        m_progressBar = NULL;
    }
    static void static_func(const char *message, double percent)
    {
        assert(m_progressBar);
        m_progressBar->setValue(int(percent));
        // Give the Qt event loop some time
        qApp->processEvents();
    }
private:
    static QProgressBar *m_progressBar;
};
QProgressBar *progress_functor::m_progressBar = NULL;

} // END namespace anonymous


void MainWindow::loadFile(const QString &fileName)
{
    if (fileName.isEmpty())
        return;

    // copied from gnc_post_file_open, gnome-utils/gnc-file.c

    /* disable events while moving over to the new set of accounts;
     * the mass deletion of accounts and transactions during
     * switchover would otherwise cause excessive redraws. */
    qof_event_suspend ();

    /* Change the mouse to a busy cursor */
    //gnc_set_busy_cursor (NULL, TRUE);

    /* -------------- BEGIN CORE SESSION CODE ------------- */
    /* -- this code is almost identical in FileOpen and FileSaveAs -- */
    m_session.call_close_hooks();
    gnc_hook_run(HOOK_BOOK_CLOSED, m_session.get());
    qof_session_destroy(m_session.get());
    m_session.reset();

    /* load the accounts from the users datafile */
    /* but first, check to make sure we've got a session going. */
    QofSession *new_session = qof_session_new ();

    bool we_are_in_error = false;
    QByteArray newfile = fileName.toUtf8();
    qof_session_begin (new_session, newfile, FALSE, FALSE);
    QofBackendError io_err = qof_session_get_error (new_session);
    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        QString fmt1 = tr("GnuCash could not obtain the lock for %1.").arg(fileName);
        QString fmt2 =
            ((ERR_BACKEND_LOCKED == io_err)
             ? tr("That database may be in use by another user, "
                  "in which case you should not open the database. "
                  "What would you like to do? Open anyway? FIXME")
             : tr("That database may be on a read-only file system, "
                  "or you may not have write permission for the directory. "
                  "If you proceed you may not be able to save any changes. "
                  "What would you like to do? Open anyway? FIXME"));
        if (QMessageBox::question(this, fmt1, fmt2)
                == QMessageBox::Ok)
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE);
        }
        else
        {
            /* Can't use the given file, so just create a new
             * database so that the user will get a window that
             * they can click "Exit" on.
             */
            newFile();
        }
    }
    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_BACKEND_NO_SUCH_DB == io_err) ||
             (ERR_SQL_DB_TOO_OLD == io_err))
    {
        if (QMessageBox::question(this, tr("Create New File?"),
                                  tr("The file %1 does not exist. Do you want to create it?").arg(fileName))
                == QMessageBox::Ok)
        {
            /* user told us to create a new database. Do it. */
            qof_session_begin (new_session, newfile, FALSE, TRUE);
        }
    }

    QApplication::setOverrideCursor(Qt::WaitCursor);
    /* Check for errors again, since above may have cleared the lock.
     * If its still locked, still, doesn't exist, still too old, then
     * don't bother with the message, just die. */
    io_err = qof_session_get_error (new_session);
    if ((ERR_BACKEND_LOCKED == io_err) ||
            (ERR_BACKEND_READONLY == io_err) ||
            (ERR_BACKEND_NO_SUCH_DB == io_err) ||
            (ERR_SQL_DB_TOO_OLD == io_err))
    {
        we_are_in_error = true;
    }
    else
    {
        if (io_err != ERR_BACKEND_NO_ERR)
        {
            we_are_in_error = !(QMessageBox::question(this, tr("Open anyway?"),
                                tr("The file %1 has some errors: %2: %3. Open anyway?")
                                .arg(fileName)
                                .arg(errorToString(io_err))
                                .arg(errorToDescription(io_err)))
                                == QMessageBox::Ok);
        }
    }

    if (!we_are_in_error)
    {
        char * logpath = gnc_uri_get_path(newfile);
        PINFO ("logpath=%s", logpath ? logpath : "(null)");
        xaccLogSetBaseName (logpath);
        xaccLogDisable();

        {
            // Set up a progress bar in the statusBar()
            QProgressBar progressBar;
            progressBar.setMinimum(0);
            progressBar.setMaximum(100);
            statusBar()->showMessage(tr("Loading user data..."));
            statusBar()->addPermanentWidget(&progressBar);
            progressBar.show();
            // This local progress_functor is a workaround on how to
            // pass the suitable function pointer to session_load -
            // not very nice because of its static member, but it does
            // the trick for now.
            progress_functor functor(&progressBar);

            // Do the loading.
            qof_session_load (new_session, &progress_functor::static_func);

            // Remove the progress bar again from the status bar. No
            // explicit delete necessary because it is just a local
            // variable.
            statusBar()->removeWidget(&progressBar);
        }
        xaccLogEnable();

        /* check for i/o error, put up appropriate error dialog */
        io_err = qof_session_get_error (new_session);

        if (io_err != ERR_BACKEND_NO_ERR)
        {
            we_are_in_error = !(QMessageBox::question(this, tr("Error on Open"),
                                tr("There was an error opening the file %1: %2: %3. FIXME")
                                .arg(fileName)
                                .arg(errorToString(io_err))
                                .arg(errorToDescription(io_err)))
                                == QMessageBox::Ok);
        }
    }


    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
    if (m_session.get())
    {
        qof_session_destroy(m_session.get());
        m_session.reset();
    }
    m_session.reset(new_session);

    qof_event_resume ();

    /* Call this after re-enabling events. */
    gnc_book_opened (m_session);

    // ////////////////////////////////////////////////////////////
    // Some display about this file

    Account root (m_session.get_book().get_root_account());
    if (root)
    {
        m_accountListModel = new AccountListModel(root, this);
        ui->tableView->setModel(m_accountListModel);

        m_accountTreeModel = new AccountTreeModel(root, this);
        ui->treeView->setModel(m_accountTreeModel);

        ui->tabWidget->setCurrentWidget(ui->treeViewTab);
    }
    else
    {
        //ui->labelMain->setText(tr("No root account"));
    }

    // ////////////////////////////////////////////////////////////

    QApplication::restoreOverrideCursor();

    setCurrentFile(fileName);
    statusBar()->showMessage(tr("File loaded"), 5000);
}

bool MainWindow::saveFile(const QString &fileName)
{
    QFile file(fileName);
    if (!file.open(QFile::WriteOnly))
    {
        QMessageBox::warning(this, tr("Application"),
                             tr("Cannot write file %1:\n%2.")
                             .arg(fileName)
                             .arg(file.errorString()));
        return false;
    }
    file.close();
    QApplication::setOverrideCursor(Qt::WaitCursor);

    QofSession *session = m_session.get();
    /* Make sure all of the data from the old file is loaded */
    qof_session_ensure_all_data_loaded(session);

    /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */

    QByteArray newfile = fileName.toUtf8();
    xaccLogSetBaseName(newfile); //FIXME: This is premature.
    QofSession *new_session = qof_session_new ();
    qof_session_begin (new_session, newfile, FALSE, FALSE);

    QofBackendError io_err = qof_session_get_error (new_session);

    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        if (QMessageBox::question(this, tr("Ignore Lock?"),
                                  tr("The file %1 is locked. Should we ignore the lock?").arg(fileName))
                == QMessageBox::Ok)
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE);
        }
    }

    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_FILEIO_FILE_NOT_FOUND == io_err) ||
             (ERR_BACKEND_NO_SUCH_DB == io_err) ||
             (ERR_SQL_DB_TOO_OLD == io_err))
    {
        if (QMessageBox::question(this, tr("Create New File?"),
                                  tr("The file %1 does not exist. Should it be created?").arg(fileName)))
        {
            /* user told us to create a new database. Do it. */
            qof_session_begin (new_session, newfile, FALSE, TRUE);
        }
    }

    /* check again for session errors (since above dialog may have
     * cleared a file lock & moved things forward some more)
     * This time, errors will be fatal.
     */
    io_err = qof_session_get_error (new_session);
    if (ERR_BACKEND_NO_ERR != io_err)
    {
        QMessageBox::critical(this, tr("Still in Error"),
                              tr("The file %1 still cannot be written: %2: %3. FIXME")
                              .arg(fileName)
                              .arg(errorToString(io_err))
                              .arg(errorToDescription(io_err)));
        xaccLogDisable();
        qof_session_destroy (new_session);
        xaccLogEnable();
        return false;
    }

    /* Prevent race condition between swapping the contents of the two
     * sessions, and actually installing the new session as the current
     * one. Any event callbacks that occur in this interval will have
     * problems if they check for the current book. */
    qof_event_suspend();

    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
    qof_session_swap_data (session, new_session);
    qof_session_destroy(m_session.get());
    m_session.reset();
    session = NULL;

    /* XXX At this point, we should really mark the data in the new session
     * as being 'dirty', since we haven't saved it at all under the new
     * session. But I'm lazy...
     */
    m_session.reset(new_session);

    qof_event_resume();

    /* --------------- END CORE SESSION CODE -------------- */


    QApplication::restoreOverrideCursor();

    setCurrentFile(fileName);
    statusBar()->showMessage(tr("File saved"), 2000);
    return true;
}

} // END namespace gnc
