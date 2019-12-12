/********************************************************************\
 * gnc-features.h -- manage GnuCash features table                  *
 * Copyright (C) 2011 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Geert Janssens <geert@kobaltwit.be>           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

/** @addtogroup Utils Utility functions
    @{ */
/** @addtogroup UtilFeature Features
 * @{ */
/** @file gnc-features.h
 *  @brief  Utility functions for file access
 *  @author Copyright (C) 2011 Derek Atkins <derek@ihtfp.com>
 *  @author Copyright (C) 2012 Geert Janssens <geert@kobaltwit.be>
 *
 *  These functions help you to manage features that GnuCash supports.
 *  This is mainly used to prevent older GnuCash versions from opening
 *  book with data they aren't capable of processing properly.
 */

#ifndef GNC_FEATURES_H
#define GNC_FEATURES_H

#include "qof.h"

#ifdef __cplusplus
extern "C" {
#endif

/** @name Defined features
@{
 */
#define GNC_FEATURE_CREDIT_NOTES "Credit Notes"
#define GNC_FEATURE_NUM_FIELD_SOURCE "Number Field Source"
#define GNC_FEATURE_KVP_EXTRA_DATA "Extra data in addresses, jobs or invoice entries"
#define GNC_FEATURE_BOOK_CURRENCY "Use a Book-Currency"
#define GNC_FEATURE_GUID_BAYESIAN "Account GUID based Bayesian data"
#define GNC_FEATURE_GUID_FLAT_BAYESIAN "Account GUID based bayesian with flat KVP"
#define GNC_FEATURE_SQLITE3_ISO_DATES "ISO-8601 formatted date strings in SQLite3 databases."
#define GNC_FEATURE_REG_SORT_FILTER "Register sort and filter settings stored in .gcm file"
#define GNC_FEATURE_BUDGET_UNREVERSED "Use natural signs in budget amounts"
#define GNC_FEATURE_BUDGET_SHOW_EXTRA_ACCOUNT_COLS "Show extra account columns in the Budget View"

/** @} */

/**
 * Test if the current book relies on features only introduced in a more
 * recent version of GnuCash.
 *
 * Returns a message to display if we found unknown features, NULL if we're okay.
 */
gchar *gnc_features_test_unknown (QofBook *book);

/**
 * Indicate that the current book uses the given feature. This will prevent
 * older versions of GnuCash that don't support this feature to refuse to load
 * this book.
 */
void gnc_features_set_used (QofBook *book, const gchar *feature);

/*
 * Returns true if the specified feature is used.
 */
gboolean gnc_features_check_used (QofBook *, char const * feature);

#ifdef __cplusplus
} /* extern "C" */
#endif /*__cplusplus*/
#endif /* GNC_FEATURES_H */
/** @} */
/** @} */

