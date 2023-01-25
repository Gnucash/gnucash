/********************************************************************\
 * gtest-qofevent.cpp -- Unit tests for qofevent.cpp                *
 *                                                                  *
 * Copyright 2022 Christopher Lam                                   *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 \ *********************************************************************/

#include <config.h>
#include <glib.h>
#include "../test-core/test-engine-stuff.h"
#include "../qofevent.h"
#include "../qofevent-p.h"
#include <gtest/gtest.h>

static void
easy_handler (QofInstance *ent,  QofEventId event_type,
            gpointer handler_data, gpointer event_data)
{
    int *data = static_cast<int*>(handler_data);
    int increment = GPOINTER_TO_INT(event_data);
    *data = *data + increment;
}

TEST (qofevent, events)
{
    QofInstance entity;         // qofevents needs a non-null entity.
    int data = 1;

    // initial setup. register first handler. id is 1. data is initialized at 1.
    int id1 = qof_event_register_handler (easy_handler, &data);
    EXPECT_EQ (id1, 1);
    EXPECT_EQ (data, 1);

    // call events. this increment data by 2. data is now 3.
    qof_event_gen (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 3);

    // entity is NULL, call events. data is unchanged.
    qof_event_gen (NULL, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 3);

    // event is QOF_EVENT_NONE, call events. data is unchanged.
    qof_event_gen (&entity, QOF_EVENT_NONE, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 3);

    // call events. this increment data by 2. data is now 5.
    qof_event_gen (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 5);

    // suspend, try calling events. data unchanged at 5.
    qof_event_suspend ();
    qof_event_gen (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 5);

    // although suspended, force events. data changed to 7.
    qof_event_force (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 7);

    // resume, call events. data incremented by 2 to 9.
    qof_event_resume ();
    qof_event_gen (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 9);

    qof_event_unregister_handler (id1);

    // no more handler. running events means data is no longer updated.
    qof_event_gen (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 9);

    // test handler id is incremented to 2
    int id2 = qof_event_register_handler (easy_handler, &data);
    EXPECT_EQ (id2, 2);

    // test handler id is incremented to 3
    int id3 = qof_event_register_handler (easy_handler, &data);
    EXPECT_EQ (id3, 3);
    qof_event_unregister_handler (id3);
    qof_event_unregister_handler (id2);

    // handler_id 1 to 3 are now unregistered, the next handler_id is still 4.
    int id4 = qof_event_register_handler (easy_handler, &data);
    EXPECT_EQ (id4, 4);
    qof_event_unregister_handler (id4);
}

static void
compound_handler (QofInstance *ent,  QofEventId event_type,
                  gpointer handler_data, gpointer event_data)
{
    int *data = static_cast<int*>(handler_data);
    int increment = GPOINTER_TO_INT(event_data);

    int id = qof_event_register_handler (easy_handler, &data);
    // this handler is immediately deregistered but is not deleted
    // until all handlers have run
    qof_event_unregister_handler (id);

    *data = *data + increment;
}

TEST (qofevent, compound_events)
{
    QofInstance entity;         // qofevents needs a non-null entity.
    int data = 1;

    // initial setup. first handler_id is 1. data is initialized at 1.
    int id5 = qof_event_register_handler (compound_handler, &data);
    EXPECT_EQ (id5, 5);
    EXPECT_EQ (data, 1);

    // call handlers. this increments data by 2. calling
    // compound_handler will cause the deletion to be effected after
    // all the handlers have completed.
    qof_event_gen (&entity, QOF_EVENT_ALL, GINT_TO_POINTER(2));
    EXPECT_EQ (data, 3);

    qof_event_unregister_handler (id5);
}

