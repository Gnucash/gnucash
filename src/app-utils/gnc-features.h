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
 *  datasets with data they aren't capable of processing properly.
 */

#ifndef GNC_FEATURES_H
#define GNC_FEATURES_H


/**
 * Test if the current session relies on features we don't know.
 *
 * Returns a message to display if we found unknown features, NULL if we're okay.
 */
gchar *test_unknown_features(QofSession* new_session);

#endif /* GNC_FEATURES_H */
/** @} */
/** @} */

