#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QSharedPointer>
#include "gnc/Session.hpp"
#include "gnc/AccountItemModel.hpp"

class QAction;
class QMenu;
class QPlainTextEdit;
class QTextEdit;
class QTabWidget;

namespace Ui
{
class MainWindow;
}

namespace gnc
{

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow();
    ~MainWindow();

protected:
    void closeEvent(QCloseEvent *event);

private slots:
    void newFile();
    void open();
    bool save();
    bool saveAs();
    void about();
    void documentWasModified();

private:
    void createActions();
    void createToolBars();
    void createStatusBar();
    void readSettings();
    void writeSettings();
    bool maybeSave();
    void loadFile(const QString &fileName);
    bool saveFile(const QString &fileName);
    void setCurrentFile(const QString &fileName);
    QString strippedName(const QString &fullFileName);

    QSharedPointer<Ui::MainWindow> ui;

    QString curFile;

    QToolBar *fileToolBar;
    QToolBar *editToolBar;

    Session m_session;
    AccountItemModel *m_accountItemModel;
};

} // END namespace gnc

#endif
