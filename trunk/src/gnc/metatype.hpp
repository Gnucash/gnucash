#ifndef METATYPE_HPP
#define METATYPE_HPP

#include <QMetaType>

#include "config.h"
#include <gncmm/Account.hpp>

// The macros to declare those types for inclusion in QVariant
Q_DECLARE_METATYPE(::Account*);

#endif // METATYPE_HPP
