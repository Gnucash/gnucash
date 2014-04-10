#ifndef __LOCALTIME_R_H__
#define __LOCALTIME_R_H__
#include <time.h>
/*
 * Version of "localtime_r()", for the benefit of OSes that don't have it.
 */
extern struct tm *localtime_r(const time_t *const timep, struct tm *p_tm);

#endif

