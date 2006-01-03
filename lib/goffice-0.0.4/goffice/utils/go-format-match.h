/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-format-match.h : 
 *
 * Copyright (C) 2005 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */
#ifndef GO_FORMAT_MATCH_H
#define GO_FORMAT_MATCH_H

#include <goffice/utils/goffice-utils.h>

G_BEGIN_DECLS

typedef enum {
	MATCH_DAY_FULL 		  = 1,
	MATCH_DAY_NUMBER	  = 2,
	MATCH_MONTH_FULL	  = 3,
	MATCH_MONTH_SHORT	  = 4,
	MATCH_MONTH_NUMBER	  = 5,
	MATCH_YEAR_FULL		  = 6,
	MATCH_YEAR_SHORT	  = 7,
	MATCH_HOUR		  = 8,
	MATCH_MINUTE		  = 9,
	MATCH_SECOND		  = 10,
	MATCH_AMPM		  = 11,
	MATCH_NUMBER		  = 12,
	MATCH_NUMBER_DECIMALS	  = 13,
	MATCH_PERCENT		  = 14,
	MATCH_SKIP		  = 15,
	MATCH_STRING_CONSTANT	  = 16,
	MATCH_CUMMULATIVE_HOURS	  = 17,
	MATCH_CUMMULATIVE_MINUTES = 18,
	MATCH_CUMMULATIVE_SECONDS = 19,
	MATCH_NUMERATOR           = 20,
	MATCH_DENOMINATOR         = 21
} MatchType;

gboolean format_match_create  (GOFormatElement *elem);
void	 format_match_release (GOFormatElement *elem);

G_END_DECLS

#endif /* GO_FORMAT_MATCH_H */
