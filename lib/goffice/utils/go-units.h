/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-units.h : 
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
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
#ifndef GO_UNITS_H
#define GO_UNITS_H

#include <glib.h>

G_BEGIN_DECLS

/* Conversion factors */
/* The following number is the least common multiplier of 254 (1/10mm), 72(pt), 100000, and 576 */
/* This way inch, pt, and mm are all integer multipliers (in fact, a nanometer is.) */
/* (Of course that is only true because we use the lobotomized pt size so that
   1inch is exactly 72pt.)  */
#define PT_PER_IN 72
#define CM_PER_IN 254
#define EMU_PER_IN 914400

#define UN_PER_IN 228600000
#define UN_PER_EMU (UN_PER_IN/EMU_PER_IN)
#define UN_PER_PT (UN_PER_IN/PT_PER_IN)
#define UN_PER_CM (UN_PER_IN/CM_PER_IN)

#define GO_IN_TO_UN(inch)	((inch)*UN_PER_IN)
#define GO_IN_TO_PT(inch)	((inch)*PT_PER_IN)
#define GO_IN_TO_CM(inch)	((inch)*CM_PER_IN/100)
#define GO_IN_TO_EMU(inch)	((inch)*EMU_PER_IN)

#define GO_UN_TO_IN(unit)	((unit)/UN_PER_IN)
#define GO_UN_TO_PT(unit)	((unit)/UN_PER_PT)
#define GO_UN_TO_CM(unit)	((unit)/UN_PER_CM/100)
#define GO_UN_TO_EMU(unit)	((unit)/UN_PER_EMU)

#define GO_PT_TO_UN(pt)		((pt)* UN_PER_PT)
#define GO_PT_TO_IN(pt)		((pt)           /PT_PER_IN)
#define GO_PT_TO_CM(pt)		((pt)* CM_PER_IN/PT_PER_IN/100)
#define GO_PT_TO_EMU(pt)	((pt)*EMU_PER_IN/PT_PER_IN)

#define GO_CM_TO_UN(cm)		((cm)*100*UN_PER_CM)
#define GO_CM_TO_IN(cm)		((cm)*100          /CM_PER_IN)
#define GO_CM_TO_PT(cm)		((cm)*100*PT_PER_IN/CM_PER_IN)
#define GO_CM_TO_EMU(cm)	((cm)*100*PT_PER_IN/EMU_PER_IN)

#define GO_EMU_TO_UN(emu)	((emu)*UN_PER_EMU)
#define GO_EMU_TO_IN(emu)	((emu)          /EMU_PER_IN)
#define GO_EMU_TO_PT(emu)	((emu)*PT_PER_IN/EMU_PER_IN)
#define GO_EMU_TO_CM(emu)	((emu)*CM_PER_IN/EMU_PER_IN/100)

typedef gint64 go_unit_t;
#define GO_UNIT_T_FORMAT G_GINT64_FORMAT 

typedef struct {
	go_unit_t x;
	go_unit_t y;
} GoPoint;

typedef struct {
	go_unit_t top;
	go_unit_t left;
	go_unit_t bottom;
	go_unit_t right;
} GoRect;

G_END_DECLS

#endif /* GO_UNITS_H */
