/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later
 */
#ifndef GNC_TEST_LOGGING_HPP
#define GNC_TEST_LOGGING_HPP

#include <glib.h>
#include <gtest/gtest.h>
#include <cstddef>
#include <string>

namespace gnc::test
{
inline GLogWriterOutput log_writer (GLogLevelFlags level,
                                    const GLogField *fields, gsize count,
                                    gpointer data)
{
    if (level & G_LOG_LEVEL_CRITICAL)
    {
        std::string domain{"GLib"};
        std::string message{"Critical log without a message"};
        for (std::size_t index = 0; index < count; ++index)
        {
            const auto &field = fields[index];
            if (!field.value)
                continue;
            auto destination = g_strcmp0 (field.key, "MESSAGE") == 0 ? &message :
                g_strcmp0 (field.key, "GLIB_DOMAIN") == 0 ? &domain : nullptr;
            if (destination)
            {
                const auto value = static_cast<const char *> (field.value);
                *destination = field.length < 0 ? std::string{value} :
                    std::string{value, static_cast<std::size_t> (field.length)};
            }
        }
        ADD_FAILURE () << domain << ": " << message;
    }
    return g_log_writer_default (level, fields, count, data);
}

inline void initialize_logging ()
{
    // GLib's expected-message facility intercepts explicitly expected logs
    // before the writer. Warnings remain diagnostic; unexpected criticals
    // fail the current GoogleTest case without preventing its cleanup.
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (G_LOG_FATAL_MASK));
    // The default legacy handler forwards to the structured writer too.
    g_log_set_default_handler (g_log_default_handler, nullptr);
    g_log_set_writer_func (log_writer, nullptr, nullptr);
}
}
#endif
