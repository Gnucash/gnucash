/*
 * gnucash-guile-bootstrap.h -- Keep the application lifecycle in Guile mode
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#ifndef GNUCASH_GUILE_BOOTSTRAP_H
#define GNUCASH_GUILE_BOOTSTRAP_H

#include <glib.h>

G_BEGIN_DECLS

typedef int (*GncGuileMainFunc) (int argc, char **argv, void *user_data);

G_GNUC_NORETURN void gnc_run_with_guile (int argc,
                                          char **argv,
                                          GncGuileMainFunc main_func,
                                          void *user_data);

G_END_DECLS

#endif /* GNUCASH_GUILE_BOOTSTRAP_H */
