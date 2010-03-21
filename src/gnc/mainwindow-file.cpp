/*
 * mainwindow-file.cpp
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
#include <QtGui/QProgressBar>
#include <QtGui/QPushButton>
#include <QtGui/QToolBar>
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
static const char* PROPERTY_TAB_PREVIOUSPOS = "tab_previouspos";
static gint save_in_progress = 0;

// ////////////////////////////////////////////////////////////

bool MainWindow::show_session_error (QWidget *parent,
                                     ::QofBackendError io_error,
                                     const QString& filename,
                                     GNCFileDialogType type)
{
    bool should_abort = true;
    QString fmt;

    switch (io_error)
    {
    case ERR_BACKEND_NO_ERR:
        should_abort = FALSE;
        break;

    case ERR_BACKEND_NO_HANDLER:
        QMessageBox::critical(parent, tr("Error"),
                              tr("No suitable backend was found for %1.").arg(filename));
        break;

    case ERR_BACKEND_NO_BACKEND:
        QMessageBox::critical(parent, tr("Error"),
                              tr("The URL %1 is not supported by this version of GnuCash.").arg(filename));
        break;

    case ERR_BACKEND_BAD_URL:
        QMessageBox::critical(parent, tr("Error"),
                              tr("Can't parse the URL %1.").arg(filename));
        break;

    case ERR_BACKEND_CANT_CONNECT:
        QMessageBox::critical(parent, tr("Error"),
                              tr("Can't connect to %1. "
                                 "The host, username or password were incorrect.").arg(filename));
        break;

    case ERR_BACKEND_CONN_LOST:
        QMessageBox::critical(parent, tr("Error"),
                              tr("Can't connect to %1. "
                                 "Connection was lost, unable to send data.").arg(filename));
        break;

    case ERR_BACKEND_TOO_NEW:
        QMessageBox::critical(parent, tr("Error"),
                              tr("This file/URL appears to be from a newer version "
                                 "of GnuCash. You must upgrade your version of GnuCash "
                                 "to work with this data."));
        break;

    case ERR_BACKEND_NO_SUCH_DB:
        if (QMessageBox::question(parent, tr("Create New File?"),
                                  tr("The database %1 doesn't seem to exist. "
                                     "Do you want to create it?").arg(filename),
                                  QMessageBox::Ok | QMessageBox::Cancel)
                == QMessageBox::Ok)
        {
            should_abort = false;
        }
        break;

    case ERR_BACKEND_LOCKED:
        switch (type)
        {
        case GNC_FILE_DIALOG_OPEN:
        default:
            fmt = tr("GnuCash could not obtain the lock for %1. "
                     "That database may be in use by another user, "
                     "in which case you should not open the database. "
                     "Do you want to proceed with opening the database?");
            break;

        case GNC_FILE_DIALOG_IMPORT:
            fmt = tr("GnuCash could not obtain the lock for %1. "
                     "That database may be in use by another user, "
                     "in which case you should not import the database. "
                     "Do you want to proceed with importing the database?");
            break;

        case GNC_FILE_DIALOG_SAVE:
            fmt = tr("GnuCash could not obtain the lock for %1. "
                     "That database may be in use by another user, "
                     "in which case you should not save the database. "
                     "Do you want to proceed with saving the database?");
            break;

        case GNC_FILE_DIALOG_EXPORT:
            fmt = tr("GnuCash could not obtain the lock for %1. "
                     "That database may be in use by another user, "
                     "in which case you should not export the database. "
                     "Do you want to proceed with exporting the database?");
            break;
        }

        if (QMessageBox::question(parent, tr("Create New File?"),
                                  tr("The database %1 doesn't seem to exist. "
                                     "Do you want to create it?").arg(filename),
                                  QMessageBox::Yes | QMessageBox::No, QMessageBox::No)
                == QMessageBox::Yes)
        {
            should_abort = false;
        }
        break;

    case ERR_BACKEND_READONLY:
        QMessageBox::critical(parent, tr("Error"),
                              tr("GnuCash could not write to %1. "
                                 "That database may be on a read-only file system, "
                                 "or you may not have write permission for the directory.").arg(filename));
        break;

    case ERR_BACKEND_DATA_CORRUPT:
        QMessageBox::critical(parent, tr("Error"),
                              tr("The file/URL %1 "
                                 "does not contain GnuCash data or the data is corrupt.").arg(filename));
        break;

    case ERR_BACKEND_SERVER_ERR:
        QMessageBox::critical(parent, tr("Error"),
                              tr("The server at URL %1 "
                                 "experienced an error or encountered bad or corrupt data.").arg(filename));
        break;

    case ERR_BACKEND_PERM:
        QMessageBox::critical(parent, tr("Error"),
                              tr("You do not have permission to access %1.").arg(filename));
        break;

    case ERR_BACKEND_MISC:
        QMessageBox::critical(parent, tr("Error"),
                              tr("An error occurred while processing %1.").arg(filename));
        break;


    case ERR_FILEIO_FILE_BAD_READ:
        if (QMessageBox::question(parent, tr("Continue?"),
                                  tr("There was an error reading the file. "
                                     "Do you want to continue?"),
                                  QMessageBox::Ok | QMessageBox::Cancel)
                == QMessageBox::Ok)
        {
            should_abort = false;
        }
        break;

    case ERR_FILEIO_PARSE_ERROR:
        QMessageBox::critical(parent, tr("Error"),
                              tr("There was an error parsing the file %1.").arg(filename));
        break;

    case ERR_FILEIO_FILE_EMPTY:
        QMessageBox::critical(parent, tr("Error"),
                              tr("The file %1 is empty.").arg(filename));
        break;

    case ERR_FILEIO_FILE_NOT_FOUND:
        if (type == GNC_FILE_DIALOG_SAVE)
        {
            should_abort = false;
        }
        else
        {
            QMessageBox::critical(parent, tr("Error"),
                                  tr("The file %1 could not be found.").arg(filename));
        }
        break;

    case ERR_FILEIO_FILE_TOO_OLD:
        if (QMessageBox::question(parent, tr("Continue?"),
                                  tr("This file is from an older version of GnuCash. "
                                     "Do you want to continue?"),
                                  QMessageBox::Ok | QMessageBox::Cancel)
                == QMessageBox::Ok)
        {
            should_abort = false;
        }
        break;

    case ERR_FILEIO_UNKNOWN_FILE_TYPE:
        QMessageBox::critical(parent, tr("Error"),
                              tr("The file type of file %1 is unknown.").arg(filename));
        break;

    case ERR_FILEIO_BACKUP_ERROR:
        QMessageBox::critical(parent, tr("Error"),
                              tr("Could not make a backup of the file %1").arg(filename));
        break;

    case ERR_FILEIO_WRITE_ERROR:
        QMessageBox::critical(parent, tr("Error"),
                              tr("Could not write to file %1.  Check that you have "
                                 "permission to write to this file and that "
                                 "there is sufficient space to create it.").arg(filename));
        break;

    case ERR_FILEIO_FILE_EACCES:
        QMessageBox::critical(parent, tr("Error"),
                              tr("No read permission to read from file %1.").arg(filename));
        break;

    case ERR_SQL_DB_TOO_OLD:
        if (QMessageBox::question(parent, tr("Upgrade Database?"),
                                  tr("This database is from an older version of GnuCash. "
                                     "Do you want to want to upgrade the database "
                                     "to the current version?"),
                                  QMessageBox::Yes | QMessageBox::No, QMessageBox::No)
                == QMessageBox::Yes)
        {
            should_abort = false;
        }
        break;

    case ERR_SQL_DB_BUSY:
        QMessageBox::critical(parent, tr("Error"),
                              tr("The SQL database is in use by other users, "
                                 "and the upgrade cannot be performed until they logoff. "
                                 "If there are currently no other users, consult the  "
                                 "documentation to learn how to clear out dangling login "
                                 "sessions."));
        break;

    default:
        PERR("FIXME: Unhandled error %d", io_error);
        QMessageBox::critical(parent, tr("Error"),
                              tr("An unknown I/O error (%1) occurred.").arg(int(io_error)));
        break;
    }

    return should_abort;
}

// ////////////////////////////////////////////////////////////


static void
gnc_book_opened (Session& sess)
{
    gnc_hook_run(HOOK_BOOK_OPENED, sess.get());
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

    QString newfile_qstring =
        gchar_to_QString(gnc_uri_normalize_uri(fileName.toUtf8(), TRUE));
    if (newfile_qstring.isEmpty())
    {
        show_session_error (this, ERR_FILEIO_FILE_NOT_FOUND, fileName,
                            GNC_FILE_DIALOG_OPEN);
        return;
    }
    QByteArray newfile = newfile_qstring.toUtf8();

    /* disable events while moving over to the new set of accounts;
     * the mass deletion of accounts and transactions during
     * switchover would otherwise cause excessive redraws. */
    qof_event_suspend ();

    // Change the mouse to a busy cusor
    QApplication::setOverrideCursor(Qt::WaitCursor);

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
    qof_session_begin (new_session, newfile, FALSE, FALSE);
    QofBackendError io_err = qof_session_get_error (new_session);
    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        QString fmt1 = tr("GnuCash could not obtain the lock for %1. ").arg(fileName);
        QString fmt2 =
            ((ERR_BACKEND_LOCKED == io_err)
             ? tr("That database may be in use by another user, "
                  "in which case you should not open the database. "
                  "What would you like to do?")
             : tr("That database may be on a read-only file system, "
                  "or you may not have write permission for the directory. "
                  "If you proceed you may not be able to save any changes. "
                  "What would you like to do?"));
        QMessageBox msgBox(this);
        msgBox.setWindowTitle(tr("Could not obtain file lock"));
        msgBox.setText(fmt1 + fmt2);
        QPushButton *openAnyway = msgBox.addButton(tr("&Open Anyway"), QMessageBox::ActionRole);
        QPushButton *createNewFile = msgBox.addButton(tr("&Create New File"), QMessageBox::ActionRole);
        QPushButton *close = msgBox.addButton(QMessageBox::Close);
        msgBox.exec();
        if (msgBox.clickedButton() == openAnyway)
        {
            /* user told us to ignore locks. So ignore them. */
            qof_session_begin (new_session, newfile, TRUE, FALSE);
        }
        else if (msgBox.clickedButton() == createNewFile)
        {
            /* Can't use the given file, so just create a new
             * database so that the user will get a window that
             * they can click "Exit" on.
             */
            newFile();
            return;
        }
        else
        {
            qApp->quit();
            return;
        }
    }

    if (ERR_QSF_OPEN_NOT_MERGE == io_err)
    {
        we_are_in_error = true;
    }
    /* if the database doesn't exist, ask the user ... */
    else if ((ERR_BACKEND_NO_SUCH_DB == io_err) ||
             (ERR_SQL_DB_TOO_OLD == io_err))
    {
        if (false == show_session_error (this, io_err, newfile, GNC_FILE_DIALOG_OPEN))
        {
            /* user told us to create a new database. Do it. */
            qof_session_begin (new_session, newfile, FALSE, TRUE);
        }
    }

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
        we_are_in_error = show_session_error (this, io_err, newfile, GNC_FILE_DIALOG_OPEN);
    }

    if (!we_are_in_error)
    {
        gchar *logpath = NULL;
        if ( gnc_uri_is_file_uri ( newfile ) )
            logpath = gnc_uri_get_path(newfile);
        PINFO ("logpath=%s", logpath ? logpath : "(null)");
        xaccLogSetBaseName (logpath);
        g_free ( logpath );

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

            // Remove the progress bar again from the status bar.
            statusBar()->removeWidget(&progressBar);
        }
        xaccLogEnable();

        /* check for i/o error, put up appropriate error dialog */
        io_err = qof_session_get_error (new_session);

        we_are_in_error = show_session_error(this, io_err, newfile, GNC_FILE_DIALOG_OPEN);

        ::Account* new_root_account = gnc_book_get_root_account (qof_session_get_book (new_session));
        if (we_are_in_error) new_root_account = NULL;

        /* Umm, came up empty-handed, but no error:
         * The backend forgot to set an error. So make one up. */
        if (!we_are_in_error && !new_root_account)
        {
            we_are_in_error = show_session_error (this, ERR_BACKEND_MISC, newfile,
                                                  GNC_FILE_DIALOG_OPEN);
        }
    }

    QApplication::restoreOverrideCursor();

    /* going down -- abandon ship */
    if (we_are_in_error)
    {
        xaccLogDisable();
        qof_session_destroy (new_session);
        xaccLogEnable();

        /* well, no matter what, I think it's a good idea to have a root
         * account around.  For example, early in the gnucash startup
         * sequence, the user opens a file; if this open fails for any
         * reason, we don't want to leave them high & dry without a root
         * account, because if the user continues, then bad things will
         * happen. */
        new_session = qof_session_new ();
        m_session.reset(new_session);

        qof_event_resume ();
        return;
    }

    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
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

        ui->treeViewTab->setProperty(PROPERTY_TAB_PREVIOUSPOS, ui->tabWidget->currentIndex());
        ui->tabWidget->setCurrentWidget(ui->treeViewTab);
    }
    else
    {
        //ui->labelMain->setText(tr("No root account"));
    }

    // ////////////////////////////////////////////////////////////

    setCurrentFile(fileName);
    statusBar()->showMessage(tr("File loaded"), 5000);
}


// ////////////////////////////////////////////////////////////

bool MainWindow::saveFileAs(const QString &fileName)
{
    if (gnc_uri_is_file_uri(fileName.toUtf8()))
    {
        QFile file(gchar_to_QString(gnc_uri_get_path(fileName.toUtf8())));
        if (!file.open(QFile::WriteOnly))
        {
            QMessageBox::warning(this, tr("Application"),
                                 tr("Cannot write file %1:\n%2.")
                                 .arg(fileName)
                                 .arg(file.errorString()));
            return false;
        }
        file.close();
    }

    /* Check to see if the user specified the same file as the current
     * file. If so, then just do a simple save, instead of a full save as */
    /* FIXME Check if it is ok to have a password in the uri here */
    QString newfile_qstring =
        gchar_to_QString(gnc_uri_normalize_uri ( fileName.toUtf8(), TRUE ));
    if (newfile_qstring.isEmpty())
    {
        show_session_error (this, ERR_FILEIO_FILE_NOT_FOUND, fileName,
                            GNC_FILE_DIALOG_SAVE);
        return false;
    }
    QByteArray newfile = newfile_qstring.toUtf8();

    QofSession *session = m_session.get();
    if (m_session.get_url() == fileName)
    {
        return saveFile();
    }

    /* Make sure all of the data from the old file is loaded */
    qof_session_ensure_all_data_loaded(session);

    /* -- this session code is NOT identical in FileOpen and FileSaveAs -- */

    save_in_progress++;
    QofSession *new_session = qof_session_new ();
    qof_session_begin (new_session, newfile, FALSE, FALSE);

    QofBackendError io_err = qof_session_get_error (new_session);

    /* if file appears to be locked, ask the user ... */
    if (ERR_BACKEND_LOCKED == io_err || ERR_BACKEND_READONLY == io_err)
    {
        if (false == show_session_error (this, io_err, newfile, GNC_FILE_DIALOG_SAVE))
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
        if (false == show_session_error (this, io_err, newfile, GNC_FILE_DIALOG_SAVE))
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
        show_session_error (this, io_err, newfile, GNC_FILE_DIALOG_SAVE);
        xaccLogDisable();
        qof_session_destroy (new_session);
        xaccLogEnable();
        save_in_progress--;
        return false;
    }

    /* oops ... file already exists ... ask user what to do... */
    if (qof_session_save_may_clobber_data (new_session))
    {
        if (QMessageBox::question(this, tr("File Exists"),
                                  tr("The file %1 already exists. "
                                     "Are you sure you want to overwrite it?").arg(fileName),
                                  QMessageBox::Yes | QMessageBox::No)
                != QMessageBox::Yes)
        {
            /* if user says cancel, we should break out */
            xaccLogDisable();
            qof_session_destroy (new_session);
            xaccLogEnable();
            save_in_progress--;
            return false;
        }

        /* Whoa-ok. Blow away the previous file. */
    }

    /* XXX Would logging make sense for databases as well (mysql/postgres) ?
     * Currently the logpath is relative to the data file path.
     * Databases don't have a file path, so no logging will be
     * done for them in the current setup.
     */
    gchar *logpath = NULL;
    if ( gnc_uri_is_file_uri ( newfile ) )
        logpath = gnc_uri_get_path(newfile);
    PINFO ("logpath=%s", logpath ? logpath : "(null)");
    xaccLogSetBaseName (logpath);
    g_free ( logpath );


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

    bool r = saveFile();

    save_in_progress--;
    setCurrentFile(fileName);
    return r;
}

static bool been_here_before = false;

bool MainWindow::saveFile()
{
    /* hack alert -- Somehow make sure all in-progress edits get committed! */

    /* If we don't have a filename/path to save to get one. */
    if (m_session.get_url().isEmpty())
        return on_actionSave_as_triggered();

    /* use the current session to save to file */
    save_in_progress++;
    QApplication::setOverrideCursor(Qt::WaitCursor);

    {
        // Set up a progress bar in the statusBar()
        QProgressBar progressBar;
        progressBar.setMinimum(0);
        progressBar.setMaximum(100);
        statusBar()->showMessage(tr("Saving user data..."));
        statusBar()->addPermanentWidget(&progressBar);
        progressBar.show();
        // A local progress_functor to pass the suitable function
        // pointer to session_load
        progress_functor functor(&progressBar);

        // The actual saving
        qof_session_save (m_session.get(), &progress_functor::static_func);

        // Remove the progress bar again from the status bar.
        statusBar()->removeWidget(&progressBar);
    }

    // And saving is finished
    QApplication::restoreOverrideCursor();
    save_in_progress--;


    /* Make sure everything's OK - disk could be full, file could have
       become read-only etc. */
    QofBackendError io_err = m_session.get_error ();
    if (ERR_BACKEND_NO_ERR != io_err)
    {
        show_session_error (this, io_err, m_session.get_url(), GNC_FILE_DIALOG_SAVE);

        if (been_here_before)
            return true;
        been_here_before = true;
        on_actionSave_as_triggered();
        /* been_here prevents infinite recursion */
        been_here_before = false;
        return true;
    }

    xaccReopenLog();
    gnc_hook_run(HOOK_BOOK_SAVED, m_session.get());

    documentCleanStateChanged(true);
    statusBar()->showMessage(tr("File saved"), 5000);
    return true;
}

} // END namespace gnc
