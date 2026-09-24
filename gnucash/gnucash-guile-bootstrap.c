/*
 * gnucash-guile-bootstrap.c -- Keep the application lifecycle in Guile mode
 * Copyright (C) 2026 GnuCash Developers
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include "gnucash-guile-bootstrap.h"

#include <libguile.h>
#include <stdlib.h>

typedef struct
{
    GncGuileMainFunc main_func;
    void *user_data;
} GncGuileMainContext;

static void
gnc_guile_main (void *data, int argc, char **argv)
{
    GncGuileMainContext *context = data;

    /* scm_boot_guile deliberately never returns. Exiting from its callback is
     * the documented way to preserve the application's exit status while all
     * callbacks and exit handlers remain in Guile mode. */
    exit (context->main_func (argc, argv, context->user_data));
}

void
gnc_run_with_guile (int argc,
                    char **argv,
                    GncGuileMainFunc main_func,
                    void *user_data)
{
    GncGuileMainContext context = { main_func, user_data };

    if (main_func == NULL)
        abort ();
    scm_boot_guile (argc, argv, gnc_guile_main, &context);
    abort ();
}
