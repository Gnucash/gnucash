/********************************************************************\
 * SX-ttinfo.h -- Template Transaction manipulation functions       *
 *               for scheduled transactions                         *
 * Copyright (C) 2001 Robert Merkel <rgmerk@mira.net>               *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_SX_TTINFO_H
#define GNC_SX_TTINFO_H

#include "config.h"

#include <glib.h>

#include "SchedXaction.h"
#include "Account.h"
#include "gnc-numeric.h"
#include "gnc-commodity.h"

/* kvp_frame policy? */
typedef struct 
{
  /* FIXME add notes field */
  char *description; /* owned by us */
  char *num;         /* owned  by us */
  gnc_commodity *common_currency; /* not freed */
  
  GList *splits; /* list of template splits, owned by us */
} TTInfo;

typedef struct
{
  char *action; /* owned by us */
  /* FIXME: What about the split's kvp_frame */
  char *memo; /* owned by us */
  char *credit_formula, *debit_formula; /* owned by us */
  Account *acc; 
} TTSplitInfo;

TTInfo *gnc_ttinfo_malloc(void);

void gnc_ttinfo_free(TTInfo *info);

/* these two deep-copy their arguments */
void gnc_ttinfo_set_description(TTInfo *tti, const char *description);
void gnc_ttinfo_set_num(TTInfo *tti, const char *num);


/* this one points to a persistent pointer so ownership isn't relevant */

void gnc_ttinfo_set_currency(TTInfo *tti, gnc_commodity *common_currency);


/* no deep copy occurs - if you want a deep copy make one yourself ! */
void gnc_ttinfo_set_template_splits(TTInfo *tti, GList *splits);

const char    * gnc_ttinfo_get_description(TTInfo *tti);
const char    * gnc_ttinfo_get_num(TTInfo *tti);
gnc_commodity * gnc_ttinfo_get_currency(TTInfo *tti);
GList         * gnc_ttinfo_get_template_splits(TTInfo *tti);

/* split_i IS NOT deep copied and becomes owned by TTI */
void gnc_ttinfo_append_template_split(TTInfo *tti, TTSplitInfo *split_i);

TTSplitInfo * gnc_ttsplitinfo_malloc(void);
void gnc_ttsplitinfo_free(TTSplitInfo *split_i);

void gnc_ttsplitinfo_set_action(TTSplitInfo *split_i, const char *action);
const char * gnc_ttsplitinfo_get_action(TTSplitInfo *split_i);

void gnc_ttsplitinfo_set_memo(TTSplitInfo *split_i, const char *memo);
const char *gnc_ttsplitinfo_get_memo(TTSplitInfo *split_i);

void gnc_ttsplitinfo_set_credit_formula(TTSplitInfo *split_i,
                                        const char *credit_formula);
void gnc_ttsplitinfo_set_credit_formula_numeric(TTSplitInfo *split_i,
                                                gnc_numeric credit_formula);
const char *gnc_ttsplitinfo_get_credit_formula(TTSplitInfo *split_i);

void gnc_ttsplitinfo_set_debit_formula(TTSplitInfo *split_i,
                                       const char *debit_formula);
void gnc_ttsplitinfo_set_debit_formula_numeric(TTSplitInfo *split_i,
                                               gnc_numeric debit_formula);
const char *gnc_ttsplitinfo_get_debit_formula(TTSplitInfo *split_i);

void gnc_ttsplitinfo_set_account(TTSplitInfo *split_i, Account *acc);
Account *gnc_ttsplitinfo_get_account(TTSplitInfo *split_i);

#endif
