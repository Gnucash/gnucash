/*
 * test-sx-variable-fixture.h -- scheduled transaction variable test data
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#ifndef GNC_TEST_SX_VARIABLE_FIXTURE_H
#define GNC_TEST_SX_VARIABLE_FIXTURE_H

#include <glib.h>

typedef struct _SchedXaction SchedXaction;

G_BEGIN_DECLS

SchedXaction *add_daily_sx_with_variable (const gchar *name,
                                          const GDate *start);

G_END_DECLS

#endif /* GNC_TEST_SX_VARIABLE_FIXTURE_H */
