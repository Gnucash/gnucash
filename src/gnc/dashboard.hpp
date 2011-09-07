#ifndef DASHBOARD_HPP
#define DASHBOARD_HPP

#include <QMainWindow>

namespace Ui {
    class Dashboard;
}

class Dashboard : public QMainWindow
{
    Q_OBJECT

public:
    explicit Dashboard(QWidget *parent = 0);
    ~Dashboard();

private:
    Ui::Dashboard *ui;
};

#endif // DASHBOARD_HPP
