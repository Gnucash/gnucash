/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
#ifndef GNUMERIC_XML_IO_VERSION_H
#define GNUMERIC_XML_IO_VERSION_H

typedef enum
{
	GNUM_XML_UNKNOWN = -1,
	GNUM_XML_V1,
	GNUM_XML_V2,
	GNUM_XML_V3,	/* >= 0.52 */
	GNUM_XML_V4,	/* >= 0.57 */
	GNUM_XML_V5,	/* >= 0.58 */
	GNUM_XML_V6,	/* >= 0.62 */
	GNUM_XML_V7,	/* >= 0.66 */
	GNUM_XML_V8,	/* >= 0.71 */
	GNUM_XML_V9,	/* >= 0.73 add print scaling */
	GNUM_XML_V10,	/* >= 1.03 remove useless Content node in cells */

	/* NOTE : Keep this up to date (and in sync with the schema) */
	GNUM_XML_LATEST = GNUM_XML_V10
} GnumericXMLVersion;

#endif /* GNUMERIC_XML_IO_VERSION_H */
