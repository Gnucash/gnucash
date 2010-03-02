#include <QtGui>

#include "mainwindow.hpp"
#include "ui_mainwindow.h"

// gnucash includes
#include "config.h"
extern "C" {
#include <glib/gi18n.h>
#include "qof.h"
#include "engine/gnc-session.h"
#include "engine/gnc-hooks.h"
#include "engine/gnc-filepath-utils.h"
#include "engine/Account.h"
#include "engine/TransLog.h"
}

namespace gnc
{

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

MainWindow::MainWindow()
        : ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    createActions();
    createToolBars();
    createStatusBar();

    readSettings();

    connect(ui->textEdit->document(), SIGNAL(contentsChanged()),
            this, SLOT(documentWasModified()));

    setWindowIcon(QIcon(":/pixmaps/gnucash-icon-32x32.png"));

    setCurrentFile("");
    setUnifiedTitleAndToolBarOnMac(true);
}

MainWindow::~MainWindow()
{
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    if (maybeSave())
    {
        writeSettings();
        event->accept();

	QofSession *session = gnc_get_current_session ();

	/* disable events; otherwise the mass deletion of accounts and
	 * transactions during shutdown would cause massive redraws */
	qof_event_suspend ();

	qof_session_call_close_hooks(session);
	gnc_hook_run(HOOK_BOOK_CLOSED, session);
	gnc_clear_current_session();

	gnc_get_current_session ();

	qof_event_resume ();
    }
    else
    {
        event->ignore();
    }
}

void MainWindow::open()
{
    if (maybeSave())
    {
        QString fileName = QFileDialog::getOpenFileName(this);
        if (!fileName.isEmpty())
            loadFile(fileName);
    }
}

bool MainWindow::save()
{
    if (curFile.isEmpty())
    {
        return saveAs();
    }
    else
    {
        return saveFile(curFile);
    }
}

bool MainWindow::saveAs()
{
    QString fileName = QFileDialog::getSaveFileName(this);
    if (fileName.isEmpty())
        return false;

    return saveFile(fileName);
}

void MainWindow::about()
{
    QMessageBox::about(this, tr("About Application"),
                       tr("This is a Gnucash C++ gui example, from the Qt4 Application example. It demonstrates how to "
                          "write modern GUI applications using Qt, with a menu bar, "
                          "toolbars, and a status bar."));
}

void MainWindow::documentWasModified()
{
    setWindowModified(ui->textEdit->document()->isModified());
}

void MainWindow::createActions()
{
    ui->actionNew->setShortcuts(QKeySequence::New);
    ui->actionOpen->setShortcuts(QKeySequence::Open);
    ui->actionSave->setShortcuts(QKeySequence::Save);
    ui->actionSave_as->setShortcuts(QKeySequence::SaveAs);

    connect(ui->actionNew, SIGNAL(triggered()), this, SLOT(newFile()));
    connect(ui->actionOpen, SIGNAL(triggered()), this, SLOT(open()));

    connect(ui->actionSave, SIGNAL(triggered()), this, SLOT(save()));
    connect(ui->actionSave_as, SIGNAL(triggered()), this, SLOT(saveAs()));
    connect(ui->actionExit, SIGNAL(triggered()), this, SLOT(close()));

    connect(ui->actionCut, SIGNAL(triggered()), ui->textEdit, SLOT(cut()));
    connect(ui->actionCopy, SIGNAL(triggered()), ui->textEdit, SLOT(copy()));
    connect(ui->actionPaste, SIGNAL(triggered()), ui->textEdit, SLOT(paste()));

    connect(ui->actionAbout, SIGNAL(triggered()), this, SLOT(about()));
    connect(ui->actionAbout_Qt, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

    ui->actionCut->setEnabled(false);
    ui->actionCopy->setEnabled(false);
    connect(ui->textEdit, SIGNAL(copyAvailable(bool)),
            ui->actionCut, SLOT(setEnabled(bool)));
    connect(ui->textEdit, SIGNAL(copyAvailable(bool)),
            ui->actionCopy, SLOT(setEnabled(bool))); // why doesn't this work?!?
}

void MainWindow::createToolBars()
{
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
}

void MainWindow::writeSettings()
{
    QSettings settings("Trolltech", "Application Example");
    settings.setValue("pos", pos());
    settings.setValue("size", size());
}

bool MainWindow::maybeSave()
{
    if (ui->textEdit->document()->isModified())
    {
        QMessageBox::StandardButton ret;
        ret = QMessageBox::warning(this, tr("Application"),
                                   tr("The document has been modified.\n"
                                      "Do you want to save your changes?"),
                                   QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        if (ret == QMessageBox::Save)
            return save();
        else if (ret == QMessageBox::Cancel)
            return false;
    }
    return true;
}

// ////////////////////////////////////////////////////////////

static void
gnc_book_opened (void)
{
    gnc_hook_run(HOOK_BOOK_OPENED, gnc_get_current_session());
}

void MainWindow::newFile()
{
    if (maybeSave())
    {

	if (gnc_current_session_exist()) {
	    QofSession *session = gnc_get_current_session ();

	    /* close any ongoing file sessions, and free the accounts.
	     * disable events so we don't get spammed by redraws. */
	    qof_event_suspend ();

	    qof_session_call_close_hooks(session);
	    gnc_hook_run(HOOK_BOOK_CLOSED, session);

	    gnc_clear_current_session();
	    qof_event_resume ();
	}

	/* start a new book */
	gnc_get_current_session ();

	gnc_hook_run(HOOK_NEW_BOOK, NULL);

	//gnc_gui_refresh_all ();

	/* Call this after re-enabling events. */
	gnc_book_opened ();

        setCurrentFile("");
    }
}

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
    QofSession *current_session = gnc_get_current_session();
    qof_session_call_close_hooks(current_session);
    gnc_hook_run(HOOK_BOOK_CLOSED, current_session);
    gnc_clear_current_session();

    /* load the accounts from the users datafile */
    /* but first, check to make sure we've got a session going. */
    QofSession *new_session = qof_session_new ();

    bool uh_oh = false;
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
	uh_oh = true;
    }
    else
    {
        uh_oh = !(QMessageBox::question(this, tr("Open anyway?"),
					tr("The file %1 has some errors (FIXME). Open anyway?").arg(fileName))
		  == QMessageBox::Ok);
    }

    if (!uh_oh)
    {
	::Account *new_root;

	char * logpath = xaccResolveFilePath(newfile);
	PINFO ("logpath=%s", logpath ? logpath : "(null)");
	xaccLogSetBaseName (logpath);
	xaccLogDisable();

	statusBar()->showMessage(tr("Loading user data..."), 2000);
	qof_session_load (new_session, NULL);
	xaccLogEnable();

	/* check for i/o error, put up appropriate error dialog */
	io_err = qof_session_get_error (new_session);

        uh_oh = !(QMessageBox::question(this, tr("Error on Open"),
					tr("There was an error opening the file %1. FIXME").arg(fileName))
		  == QMessageBox::Ok);

	new_root = gnc_book_get_root_account (qof_session_get_book (new_session));
	if (uh_oh) new_root = NULL;
    }


    /* if we got to here, then we've successfully gotten a new session */
    /* close up the old file session (if any) */
    gnc_set_current_session(new_session);

    qof_event_resume ();

    /* Call this after re-enabling events. */
    gnc_book_opened ();

    QApplication::restoreOverrideCursor();

    setCurrentFile(fileName);
    statusBar()->showMessage(tr("File loaded"), 2000);
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

    QofSession *session = gnc_get_current_session ();
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
			      tr("The file %1 still cannot be written. FIXME").arg(fileName));
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
    gnc_clear_current_session();
    session = NULL;

    /* XXX At this point, we should really mark the data in the new session
     * as being 'dirty', since we haven't saved it at all under the new
     * session. But I'm lazy...
     */
    gnc_set_current_session(new_session);

    qof_event_resume();

    /* --------------- END CORE SESSION CODE -------------- */


    QApplication::restoreOverrideCursor();

    setCurrentFile(fileName);
    statusBar()->showMessage(tr("File saved"), 2000);
    return true;
}

void MainWindow::setCurrentFile(const QString &fileName)
{
    curFile = fileName;
    ui->textEdit->document()->setModified(false);
    setWindowModified(false);

    QString shownName;
    if (curFile.isEmpty())
        shownName = "untitled.txt";
    else
        shownName = strippedName(curFile);

    setWindowTitle(tr("%1[*] - %2").arg(shownName).arg(tr("Application")));
}

QString MainWindow::strippedName(const QString &fullFileName)
{
    return QFileInfo(fullFileName).fileName();
}

} // END namespace gnc
