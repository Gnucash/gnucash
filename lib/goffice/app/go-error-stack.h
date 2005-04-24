/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-error-stack.h : A tree of errors
 *
 * Copyright (C) 2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#ifndef GO_ERROR_STACK_H
#define GO_ERROR_STACK_H

#include <glib-object.h>

GOErrorStack *gog_error_stack_new (GOErrorStack *child,
				   char const *msg, ...) G_GNUC_PRINTF (2, 3);
void go_error_stack_add_child	  (GOErrorStack *estack, GOErrorStack *child);
void go_error_stack_dump	  (GOErrorStack *estack);
void go_error_stack_free	  (GOErrorStack *estack);

#endif /* GO_ERROR_STACK_H */
