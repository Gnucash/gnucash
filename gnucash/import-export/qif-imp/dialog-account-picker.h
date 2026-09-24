/********************************************************************\
 * dialog-account-picker.h -- account picker for the QIF importer  *
 *                                                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2026 GnuCash Contributors                          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#ifndef DIALOG_ACCOUNT_PICKER_H
#define DIALOG_ACCOUNT_PICKER_H

#include <gtk/gtk.h>
#include <libguile.h>

#include "assistant-qif-import.h"

typedef struct _accountpickerdialog QIFAccountPickerDialog;

typedef void (*QIFAccountPickerCallback) (gboolean accepted,
                                          gpointer user_data);

void qif_account_picker_dialog_async (GtkWindow *parent,
                                      QIFImportWindow *qif_wind,
                                      SCM map_entry,
                                      QIFAccountPickerCallback callback,
                                      gpointer user_data);

#endif
