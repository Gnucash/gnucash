/*
 * dialog-search.h -- Search Dialog
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _GNC_DIALOG_SEARCH_H
#define _GNC_DIALOG_SEARCH_H

#include "GNCId.h"

typedef struct _GNCSearchWindow GNCSearchWindow;

typedef gpointer (*GNCSearchCallback)(gpointer obj, gpointer user_data);

typedef struct {
  const char *		label;
  GNCSearchCallback	cb_fcn;
} GNCSearchCallbackButton;

typedef enum {
  GNC_SEARCH_MATCH_ALL = 0,
  GNC_SEARCH_MATCH_ANY = 1
} GNCSearchType;

void gnc_search_dialog_destroy (GNCSearchWindow *sw);
GNCSearchWindow * gnc_search_dialog_create (GNCIdTypeConst obj_type,
					    GNCSearchCallbackButton *callbacks,
					    gpointer user_data);
void gnc_search_dialog_test (void);

#endif
