/********************************************************************\
 * gnc-register-input.c -- GTK adapter for register keyboard input *
 *                                                                  *
 * Copyright 2026 GnuCash Contributors                              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>

#include "gnc-register-input.h"
#include "dialog-utils.h"

static GncRegisterKey
gnc_register_key_from_keyval (guint keyval)
{
    switch (keyval)
    {
    case GDK_KEY_Escape:
        return GNC_REGISTER_KEY_ESCAPE;
    case GDK_KEY_Return:
        return GNC_REGISTER_KEY_RETURN;
    case GDK_KEY_KP_Enter:
        return GNC_REGISTER_KEY_KEYPAD_ENTER;
    case GDK_KEY_KP_Decimal:
        return GNC_REGISTER_KEY_KEYPAD_DECIMAL;
    case GDK_KEY_slash:
        return GNC_REGISTER_KEY_SLASH;
    case GDK_KEY_Tab:
        return GNC_REGISTER_KEY_TAB;
    case GDK_KEY_ISO_Left_Tab:
        return GNC_REGISTER_KEY_LEFT_TAB;
    case GDK_KEY_plus:
        return GNC_REGISTER_KEY_PLUS;
    case GDK_KEY_KP_Add:
        return GNC_REGISTER_KEY_KEYPAD_ADD;
    case GDK_KEY_equal:
        return GNC_REGISTER_KEY_EQUAL;
    case GDK_KEY_semicolon:
        return GNC_REGISTER_KEY_SEMICOLON;
    case GDK_KEY_minus:
        return GNC_REGISTER_KEY_MINUS;
    case GDK_KEY_KP_Subtract:
        return GNC_REGISTER_KEY_KEYPAD_SUBTRACT;
    case GDK_KEY_underscore:
        return GNC_REGISTER_KEY_UNDERSCORE;
    case GDK_KEY_bracketright:
        return GNC_REGISTER_KEY_RIGHT_BRACKET;
    case GDK_KEY_braceright:
        return GNC_REGISTER_KEY_RIGHT_BRACE;
    case GDK_KEY_bracketleft:
        return GNC_REGISTER_KEY_LEFT_BRACKET;
    case GDK_KEY_braceleft:
        return GNC_REGISTER_KEY_LEFT_BRACE;
    default:
        return GNC_REGISTER_KEY_OTHER;
    }
}

void
gnc_register_input_from_keyval (GncRegisterInput *input,
                                 guint keyval,
                                 GdkModifierType state)
{
    g_return_if_fail (input != NULL);

    input->key = gnc_register_key_from_keyval (keyval);
    input->unicode_value = gdk_keyval_to_unicode (keyval);
    input->modifiers = GNC_REGISTER_MODIFIER_NONE;
    input->pressed = TRUE;

    if (state & GDK_SHIFT_MASK)
        input->modifiers |= GNC_REGISTER_MODIFIER_SHIFT;
    if (state & GDK_CONTROL_MASK)
        input->modifiers |= GNC_REGISTER_MODIFIER_CONTROL;
    if (state & GDK_ALT_MASK)
        input->modifiers |= GNC_REGISTER_MODIFIER_ALT;
    if (state & gtk_accelerator_get_default_mod_mask ())
        input->modifiers |= GNC_REGISTER_MODIFIER_DEFAULT;
}
