#ifndef CONV_HPP
#define CONV_HPP

#include <QDate>
#include <QString>
#include <glibmm/date.h>
#include <glibmm/ustring.h>

namespace gnc
{

/// Convert the given Glib::Date into a QDate object
inline QDate g2q(const Glib::Date& d)
{
    return QDate(d.get_year(), d.get_month(), d.get_day());
}

/// Convert the given QDate into a Glib::Date object
inline Glib::Date q2g(const QDate& d)
{
    return Glib::Date(d.day(), Glib::Date::Month(d.month()), d.year());
}

/// Convert the given Glib::ustring into a QString object
inline QString g2q(const Glib::ustring& s)
{
    return QString::fromUtf8(s.c_str());
}

/// Convert the given QString into a Glib::ustring object
inline Glib::ustring q2g(const QString& s)
{
    return Glib::ustring(s.toUtf8());
}
}

#endif // CONV_HPP
