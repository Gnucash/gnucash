/*
 * dates.c: Include the string definitions for the date names
 *
 * Author:
 *    Miguel de Icaza (miguel@kernel.org)
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "dates.h"

/* FIXME : use nl_langinfo */
const char *day_short [] =
{
	N_("*Sun"),
	N_("*Mon"),
	N_("*Tue"),
	N_("*Wed"),
	N_("*Thu"),
	N_("*Fri"),
	N_("*Sat"),
	NULL,
};

const char *day_long [] =
{
	N_("Sunday"),
	N_("Monday"),
	N_("Tuesday"),
	N_("Wednesday"),
	N_("Thursday"),
	N_("Friday"),
	N_("Saturday"),
	NULL
};

const char *month_short [] =
{
	N_("*Jan"),
	N_("*Feb"),
	N_("*Mar"),
	N_("*Apr"),
	N_("*May"),
	N_("*Jun"),
	N_("*Jul"),
	N_("*Aug"),
	N_("*Sep"),
	N_("*Oct"),
	N_("*Nov"),
	N_("*Dec"),
	NULL
};

const char *month_long [] =
{
	N_("January"),
	N_("February"),
	N_("March"),
	N_("April"),
	N_("May"),
	N_("June"),
	N_("July"),
	N_("August"),
	N_("September"),
	N_("October"),
	N_("November"),
	N_("December"),
	NULL
};

