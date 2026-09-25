/********************************************************************\
 * gnc-register-input.h -- GTK-neutral register keyboard input     *
 *                                                                  *
 * Copyright 2026 GnuCash Contributors                              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#ifndef GNC_REGISTER_INPUT_H
#define GNC_REGISTER_INPUT_H

#include <glib.h>

G_BEGIN_DECLS

typedef enum
{
    GNC_REGISTER_KEY_OTHER,
    GNC_REGISTER_KEY_ESCAPE,
    GNC_REGISTER_KEY_RETURN,
    GNC_REGISTER_KEY_KEYPAD_ENTER,
    GNC_REGISTER_KEY_KEYPAD_DECIMAL,
    GNC_REGISTER_KEY_SLASH,
    GNC_REGISTER_KEY_TAB,
    GNC_REGISTER_KEY_LEFT_TAB,
    GNC_REGISTER_KEY_PLUS,
    GNC_REGISTER_KEY_KEYPAD_ADD,
    GNC_REGISTER_KEY_EQUAL,
    GNC_REGISTER_KEY_SEMICOLON,
    GNC_REGISTER_KEY_MINUS,
    GNC_REGISTER_KEY_KEYPAD_SUBTRACT,
    GNC_REGISTER_KEY_UNDERSCORE,
    GNC_REGISTER_KEY_RIGHT_BRACKET,
    GNC_REGISTER_KEY_RIGHT_BRACE,
    GNC_REGISTER_KEY_LEFT_BRACKET,
    GNC_REGISTER_KEY_LEFT_BRACE
} GncRegisterKey;

typedef enum
{
    GNC_REGISTER_MODIFIER_NONE = 0,
    GNC_REGISTER_MODIFIER_SHIFT = 1 << 0,
    GNC_REGISTER_MODIFIER_CONTROL = 1 << 1,
    GNC_REGISTER_MODIFIER_ALT = 1 << 2,
    GNC_REGISTER_MODIFIER_DEFAULT = 1 << 3
} GncRegisterModifiers;

typedef struct
{
    GncRegisterKey key;
    gunichar unicode_value;
    GncRegisterModifiers modifiers;
    gboolean pressed;
} GncRegisterInput;

G_END_DECLS

#endif /* GNC_REGISTER_INPUT_H */
