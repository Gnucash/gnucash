/********************************************************************\
 * date.h -- utility functions to handle the date (adjusting, get   * 
 *           current date, etc.) for xacc (X-Accountant)            *
 * Copyright (C) 1997 Robin D. Clark (rclark@cs.hmc.edu)            *
 * Copyright (C) 1998, 1999 Linas Vepstas                           *
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
\********************************************************************/

/* 
 * hack alert -- the scan and print routines should probably be moved 
 * to somewhere else ... the engine really isn't involved with things 
 * like printing formats.  This is needed mostl;y by the GUI and so on.
 * If a file-io thing needs date handling, it should do it itself, instead
 * of depending on the routines here ...
 */


/**
 * printDate
 *    Convert a date as day / month / year integers into a localized string
 *    representation
 *
 * Args:   buff - pointer to previously allocated character array; its size
 *                must be at lease MAX_DATE_LENTH bytes.
 *         day - day of the month as 1 ... 31
 *         month - month of the year as 1 ... 12
 *         year - year (4-digit)
 *
 * Return: nothing
 *
 * Globals: global dateFormat value
 */

/**
 * scanDate
 *    Convert a string into  day / month / year integers according to
 *    the current dateFormat value.
 *
 * Args:   buff - pointer to date string
 *         day -  will store day of the month as 1 ... 31
 *         month - will store month of the year as 1 ... 12
 *         year - will store the year (4-digit)
 *
 * Return: 0 if conversion was successful, 1 otherwise
 *
 * Globals: global dateFormat value
 */
/**
 * dateSeparator
 *    Return the field separator for the current date format
 *
 * Args:   none
 *
 * Return: date character
 *
 * Globals: global dateFormat value
 */


#ifndef __XACC_DATE_H__
#define __XACC_DATE_H__

#include "config.h"
#include "Transaction.h"

typedef enum
{
  DATE_FORMAT_US,       /* United states: mm/dd/yyyy */
  DATE_FORMAT_UK,       /* Britain: dd/mm/yyyy */
  DATE_FORMAT_CE,       /* Continental Europe: dd.mm.yyyy */
  DATE_FORMAT_ISO,      /* ISO: yyyy-mm-dd */
  DATE_FORMAT_LOCALE    /* Take from locale information */
} DateFormat;

#define DATE_FORMAT_FIRST DATE_FORMAT_US
#define DATE_FORMAT_LAST  DATE_FORMAT_LOCALE
/* the maximum length of a string created by sprtDate() */
#define MAX_DATE_LENGTH 11

/** PROTOTYPES ******************************************************/
void setDateFormat(DateFormat df);
void printDate (char * buff, int day, int month, int year);
void printDateSecs (char * buff, time_t secs);

char * xaccPrintDateSecs (time_t secs);

void scanDate (const char *buff, int *day, int *monty, int *year);
char dateSeparator(void);

char * xaccTransGetDateStr (Transaction *trans);
void   xaccTransSetDateStr (Transaction *trans, char *str);

time_t xaccDMYToSec (int day, int month, int year);
time_t xaccScanDateS (const char *buff);

#endif /* __XACC_DATE_H__ */
