# Generated automatically from Makefile.in by configure.
#
######################################################################
#********************************************************************
#* Makefile -- makefile for xacc/src                                *
#* Copyright (C) 1997 Robin Clark                                   *
#*                                                                  *
#* This program is free software; you can redistribute it and/or    *
#* modify it under the terms of the GNU General Public License as   *
#* published by the Free Software Foundation; either version 2 of   *
#* the License, or (at your option) any later version.              *
#*                                                                  *
#* This program is distributed in the hope that it will be useful,  *
#* but WITHOUT ANY WARRANTY; without even the implied warranty of   *
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
#* GNU General Public License for more details.                     *
#*                                                                  *
#* You should have received a copy of the GNU General Public License*
#* along with this program; if not, write to the Free Software      *
#* Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
#*                                                                  *
#*   Author: Robin Clark                                            *
#* Internet: rclark@rush.aero.org                                   *
#*  Address: 609 8th Street                                         *
#*           Huntington Beach, CA 92648-4632                        *
#********************************************************************

srcdir   = .
CC       = gcc
INCLPATH = -I/usr/include \
           -I.. \
           -I./register \
           -I/usr/local/include \
           -I/usr/X11R6/include/. \
           -I./../include \
           -I./../lib/ComboBox-1.33 \
           -I./../lib/XmHTML-1.1.0/src \
           -I./../lib/Xbae-4.6.2-linas 

# libhtmlw eliminated due to license restrictions 
# and general brokenness
#           -I./../lib/libhtmlw 

CFLAGS   = -g -DCELL_WIDGETS=1
LFLAGS   = -g
LIBS     = -lXpm -lXm -lXmu -lXt -lXext  -lSM -lICE -lX11  -lpng -ljpeg -lz -lm
LIBPATH  = -L/lib -L/usr/lib -L/usr/local/lib -L/usr/X11R6/lib/.
TARGET   = ../xacc.bin
STATIC   = ../xacc-static.bin

# LIBHTMLW = ../lib/libhtmlw/libhtmlw.a
LIBXMHTML= ../lib/XmHTML-1.1.0/src/libXmHTML.a
LIBXBAE  = ../lib/Xbae-4.6.2-linas/libXbae.a
LIBCOMBO = ../lib/ComboBox-1.33/libComboBox.a
LIBREG   = libregister.a
######################################################################
SRCS = AccWindow.c Account.c AccountMenu.c AdjBWindow.c    \
       BuildMenu.c Data.c Destroy.c FileBox.c FileIO.c HelpWindow.c \
       Ledger.c LedgerUtils.c MainWindow.c QIFIO.c  \
       RecnWindow.c RegWindow.c Reports.c TextBox.c Transaction.c   \
       XferWindow.c date.c main.c util.c xtutil.c
# OBJS = ${SRCS:.c=.o} $(LIBHTMLW) $(LIBXBAE) $(LIBCOMBO)
OBJS = ${SRCS:.c=.o} $(LIBREG) $(LIBXMHTML) $(LIBXBAE) $(LIBCOMBO)
######################################################################

default: $(TARGET)

$(TARGET): $(OBJS)
	@echo "++++++"
	$(CC) $(LFLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $@

static: $(STATIC)

$(STATIC): $(OBJS)
	@echo "++++++"
	$(CC) $(LFLAGS) $(OBJS) $(LIBPATH) $(LIBS) -o $@ -static

.c.o:
	@echo "+++"
	$(CC) -c $(CFLAGS) $(INCLPATH) $<

depend:
	makedepend -- $(INCLPATH) $(DEFN) -- $(SRCS)

clean:
	rm -f *.o *~ *.bak

distclean: clean
	rm -f $(TARGET) $(STATIC) Makefile Makefile.bak config.h

# DO NOT DELETE THIS LINE -- make depend depends on it.

AccWindow.o: /usr/include/string.h /usr/include/features.h
AccWindow.o: /usr/include/sys/cdefs.h
AccWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
AccWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
AccWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
AccWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
AccWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
AccWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
AccWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
AccWindow.o: /usr/include/X11/X11/X11/Xosdefs.h /usr/include/X11/X11/Xutil.h
AccWindow.o: /usr/include/X11/X11/Xresource.h /usr/include/X11/X11/Core.h
AccWindow.o: /usr/include/X11/X11/Composite.h
AccWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
AccWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
AccWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
AccWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
AccWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
AccWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
AccWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
AccWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
AccWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
AccWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
AccWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
AccWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
AccWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
AccWindow.o: /usr/include/Xm/DialogS.h /usr/include/Xm/Form.h
AccWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/Frame.h
AccWindow.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/LabelGP.h
AccWindow.o: /usr/include/Xm/LabelG.h /usr/include/Xm/GadgetP.h
AccWindow.o: /usr/include/Xm/XmP.h /usr/include/X11/IntrinsicP.h
AccWindow.o: /usr/include/X11/X11/CoreP.h /usr/include/X11/X11/CompositeP.h
AccWindow.o: /usr/include/X11/X11/ConstrainP.h /usr/include/X11/X11/ObjectP.h
AccWindow.o: /usr/include/X11/X11/RectObjP.h /usr/include/Xm/ColorP.h
AccWindow.o: /usr/include/Xm/AccColorT.h /usr/include/Xm/BaseClassP.h
AccWindow.o: /usr/include/Xm/ExtObjectP.h /usr/include/Xm/ToggleB.h
AccWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/Text.h
AccWindow.o: /usr/include/stdio.h /usr/include/libio.h
AccWindow.o: /usr/include/_G_config.h ../config.h ./../include/Account.h
AccWindow.o: ./../include/Transaction.h ./../include/date.h
AccWindow.o: ./../include/AccountMenu.h ./../include/Data.h
AccWindow.o: ./../include/messages.h ./../include/MainWindow.h
AccWindow.o: ./../include/RegWindow.h ./../include/TextBox.h
AccWindow.o: ./../include/util.h /usr/include/stdlib.h /usr/include/errno.h
AccWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
AccWindow.o: /usr/include/alloca.h ./../include/xtutil.h
Account.o: ../config.h ./../include/Account.h /usr/include/Xm/Xm.h
Account.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
Account.o: /usr/include/sys/types.h /usr/include/linux/types.h
Account.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
Account.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
Account.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/X11/Xfuncproto.h
Account.o: /usr/include/X11/X11/X11/Xosdefs.h
Account.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Account.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Account.o: /usr/include/string.h /usr/include/features.h
Account.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
Account.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
Account.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Account.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Account.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Account.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Account.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Account.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Account.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Account.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Account.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Account.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Account.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Account.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Account.o: /usr/include/Xm/XmIm.h ./../include/Transaction.h
Account.o: ./../include/date.h ./../include/Data.h ./../include/util.h
Account.o: /usr/include/stdlib.h /usr/include/errno.h
Account.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
Account.o: /usr/include/alloca.h /usr/include/stdio.h /usr/include/libio.h
Account.o: /usr/include/_G_config.h
AccountMenu.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
AccountMenu.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
AccountMenu.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
AccountMenu.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
AccountMenu.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
AccountMenu.o: /usr/include/X11/X11/X11/Xfuncproto.h
AccountMenu.o: /usr/include/X11/X11/X11/Xosdefs.h
AccountMenu.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
AccountMenu.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
AccountMenu.o: /usr/include/string.h /usr/include/features.h
AccountMenu.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
AccountMenu.o: /usr/include/X11/X11/Composite.h
AccountMenu.o: /usr/include/X11/X11/Constraint.h
AccountMenu.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
AccountMenu.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
AccountMenu.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
AccountMenu.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
AccountMenu.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
AccountMenu.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
AccountMenu.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
AccountMenu.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
AccountMenu.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
AccountMenu.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
AccountMenu.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
AccountMenu.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
AccountMenu.o: /usr/include/Xm/XmIm.h /usr/include/Xm/RowColumn.h ../config.h
AccountMenu.o: ./../include/Account.h ./../include/Transaction.h
AccountMenu.o: ./../include/date.h ./../include/BuildMenu.h
AccountMenu.o: /usr/include/Xm/PushB.h /usr/include/Xm/Separator.h
AccountMenu.o: ./../include/Data.h ./../include/messages.h
AccountMenu.o: ./../include/util.h /usr/include/stdlib.h /usr/include/errno.h
AccountMenu.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
AccountMenu.o: /usr/include/alloca.h /usr/include/stdio.h
AccountMenu.o: /usr/include/libio.h /usr/include/_G_config.h
AdjBWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
AdjBWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
AdjBWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
AdjBWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
AdjBWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
AdjBWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
AdjBWindow.o: /usr/include/X11/X11/X11/Xosdefs.h
AdjBWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
AdjBWindow.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
AdjBWindow.o: /usr/include/string.h /usr/include/features.h
AdjBWindow.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
AdjBWindow.o: /usr/include/X11/X11/Composite.h
AdjBWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
AdjBWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
AdjBWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
AdjBWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
AdjBWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
AdjBWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
AdjBWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
AdjBWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
AdjBWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
AdjBWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
AdjBWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
AdjBWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
AdjBWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
AdjBWindow.o: /usr/include/Xm/Form.h /usr/include/Xm/BulletinB.h
AdjBWindow.o: /usr/include/Xm/Text.h /usr/include/stdio.h
AdjBWindow.o: /usr/include/libio.h /usr/include/_G_config.h
AdjBWindow.o: /usr/include/Xm/DialogS.h /usr/include/Xm/PanedW.h
AdjBWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/LabelGP.h
AdjBWindow.o: /usr/include/Xm/LabelG.h /usr/include/Xm/GadgetP.h
AdjBWindow.o: /usr/include/Xm/XmP.h /usr/include/X11/IntrinsicP.h
AdjBWindow.o: /usr/include/X11/X11/CoreP.h /usr/include/X11/X11/CompositeP.h
AdjBWindow.o: /usr/include/X11/X11/ConstrainP.h
AdjBWindow.o: /usr/include/X11/X11/ObjectP.h /usr/include/X11/X11/RectObjP.h
AdjBWindow.o: /usr/include/Xm/ColorP.h /usr/include/Xm/AccColorT.h
AdjBWindow.o: /usr/include/Xm/BaseClassP.h /usr/include/Xm/ExtObjectP.h
AdjBWindow.o: ../config.h ./../include/Account.h ./../include/Transaction.h
AdjBWindow.o: ./../include/date.h ./../include/Data.h ./../include/messages.h
AdjBWindow.o: ./../include/MainWindow.h ./../include/RegWindow.h
AdjBWindow.o: ./../include/RecnWindow.h ./../include/util.h
AdjBWindow.o: /usr/include/stdlib.h /usr/include/errno.h
AdjBWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
AdjBWindow.o: /usr/include/alloca.h ./../include/xtutil.h
BuildMenu.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
BuildMenu.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
BuildMenu.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
BuildMenu.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
BuildMenu.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
BuildMenu.o: /usr/include/X11/X11/X11/Xfuncproto.h
BuildMenu.o: /usr/include/X11/X11/X11/Xosdefs.h
BuildMenu.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
BuildMenu.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
BuildMenu.o: /usr/include/string.h /usr/include/features.h
BuildMenu.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
BuildMenu.o: /usr/include/X11/X11/Composite.h
BuildMenu.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
BuildMenu.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
BuildMenu.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
BuildMenu.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
BuildMenu.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
BuildMenu.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
BuildMenu.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
BuildMenu.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
BuildMenu.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
BuildMenu.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
BuildMenu.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
BuildMenu.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
BuildMenu.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
BuildMenu.o: /usr/include/X11/cursorfont.h /usr/include/Xm/DrawingA.h
BuildMenu.o: /usr/include/Xm/Form.h /usr/include/Xm/BulletinB.h
BuildMenu.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/MainW.h
BuildMenu.o: /usr/include/Xm/FileSB.h /usr/include/Xm/MessageB.h
BuildMenu.o: /usr/include/Xm/DialogS.h /usr/include/Xm/PanedW.h
BuildMenu.o: /usr/include/Xm/ScrolledW.h /usr/include/Xm/DrawnB.h
BuildMenu.o: /usr/include/Xm/ToggleB.h /usr/include/Xm/ToggleBG.h
BuildMenu.o: /usr/include/Xm/PushB.h /usr/include/Xm/PushBG.h
BuildMenu.o: /usr/include/Xm/CascadeB.h /usr/include/Xm/CascadeBG.h
BuildMenu.o: /usr/include/Xm/Label.h /usr/include/Xm/LabelG.h
BuildMenu.o: /usr/include/Xm/Scale.h /usr/include/Xm/Text.h
BuildMenu.o: /usr/include/stdio.h /usr/include/libio.h
BuildMenu.o: /usr/include/_G_config.h /usr/include/Xm/TextF.h
BuildMenu.o: /usr/include/Xm/SeparatoG.h ../config.h ./../include/BuildMenu.h
BuildMenu.o: /usr/include/Xm/Separator.h
Data.o: ../config.h ./../include/Account.h /usr/include/Xm/Xm.h
Data.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
Data.o: /usr/include/sys/types.h /usr/include/linux/types.h
Data.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
Data.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
Data.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/X11/Xfuncproto.h
Data.o: /usr/include/X11/X11/X11/Xosdefs.h
Data.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Data.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Data.o: /usr/include/string.h /usr/include/features.h
Data.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
Data.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
Data.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Data.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Data.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Data.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Data.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Data.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Data.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Data.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Data.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Data.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Data.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Data.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Data.o: /usr/include/Xm/XmIm.h ./../include/Transaction.h ./../include/date.h
Data.o: ./../include/Data.h ./../include/util.h /usr/include/stdlib.h
Data.o: /usr/include/errno.h /usr/include/linux/errno.h
Data.o: /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/stdio.h
Data.o: /usr/include/libio.h /usr/include/_G_config.h
Destroy.o: ../config.h ./../include/Account.h /usr/include/Xm/Xm.h
Destroy.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
Destroy.o: /usr/include/sys/types.h /usr/include/linux/types.h
Destroy.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
Destroy.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
Destroy.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/X11/Xfuncproto.h
Destroy.o: /usr/include/X11/X11/X11/Xosdefs.h
Destroy.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Destroy.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Destroy.o: /usr/include/string.h /usr/include/features.h
Destroy.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
Destroy.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
Destroy.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Destroy.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Destroy.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Destroy.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Destroy.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Destroy.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Destroy.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Destroy.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Destroy.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Destroy.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Destroy.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Destroy.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Destroy.o: /usr/include/Xm/XmIm.h ./../include/Transaction.h
Destroy.o: ./../include/date.h ./../include/AccWindow.h
Destroy.o: ./../include/AdjBWindow.h ./../include/Data.h
Destroy.o: ./../include/LedgerUtils.h ./../include/RegWindow.h
Destroy.o: ./../include/RecnWindow.h ./../include/util.h
Destroy.o: /usr/include/stdlib.h /usr/include/errno.h
Destroy.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
Destroy.o: /usr/include/alloca.h /usr/include/stdio.h /usr/include/libio.h
Destroy.o: /usr/include/_G_config.h
FileBox.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
FileBox.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
FileBox.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
FileBox.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
FileBox.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
FileBox.o: /usr/include/X11/X11/X11/Xfuncproto.h
FileBox.o: /usr/include/X11/X11/X11/Xosdefs.h
FileBox.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
FileBox.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
FileBox.o: /usr/include/string.h /usr/include/features.h
FileBox.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
FileBox.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
FileBox.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
FileBox.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
FileBox.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
FileBox.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
FileBox.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
FileBox.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
FileBox.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
FileBox.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
FileBox.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
FileBox.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
FileBox.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
FileBox.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
FileBox.o: /usr/include/Xm/XmIm.h /usr/include/Xm/FileSB.h ../config.h
FileBox.o: ./../include/FileBox.h ./../include/messages.h ./../include/util.h
FileBox.o: /usr/include/stdlib.h /usr/include/errno.h
FileBox.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileBox.o: /usr/include/alloca.h /usr/include/stdio.h /usr/include/libio.h
FileBox.o: /usr/include/_G_config.h ./../include/xtutil.h
FileIO.o: /usr/include/fcntl.h /usr/include/features.h
FileIO.o: /usr/include/sys/cdefs.h /usr/include/sys/types.h
FileIO.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
FileIO.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
FileIO.o: /usr/include/sys/bitypes.h /usr/include/gnu/types.h
FileIO.o: /usr/include/linux/fcntl.h /usr/include/asm/fcntl.h
FileIO.o: /usr/include/unistd.h /usr/include/posix_opt.h
FileIO.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
FileIO.o: /usr/include/confname.h ../config.h ./../include/Account.h
FileIO.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
FileIO.o: /usr/include/X11/X11/Xlib.h /usr/include/X11/X11/X11/X.h
FileIO.o: /usr/include/X11/X11/X11/Xfuncproto.h
FileIO.o: /usr/include/X11/X11/X11/Xosdefs.h /usr/include/X11/X11/Xutil.h
FileIO.o: /usr/include/X11/X11/Xresource.h /usr/include/string.h
FileIO.o: /usr/include/X11/X11/Core.h /usr/include/X11/X11/Composite.h
FileIO.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
FileIO.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
FileIO.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
FileIO.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
FileIO.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
FileIO.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
FileIO.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
FileIO.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
FileIO.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
FileIO.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
FileIO.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
FileIO.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
FileIO.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
FileIO.o: ./../include/Transaction.h ./../include/date.h ./../include/Data.h
FileIO.o: ./../include/FileIO.h ./../include/messages.h ./../include/util.h
FileIO.o: /usr/include/stdlib.h /usr/include/errno.h
FileIO.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileIO.o: /usr/include/alloca.h /usr/include/stdio.h /usr/include/libio.h
FileIO.o: /usr/include/_G_config.h
HelpWindow.o: /usr/include/sys/types.h /usr/include/linux/types.h
HelpWindow.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
HelpWindow.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
HelpWindow.o: /usr/include/sys/stat.h /usr/include/features.h
HelpWindow.o: /usr/include/sys/cdefs.h /usr/include/linux/stat.h
HelpWindow.o: /usr/include/fcntl.h /usr/include/gnu/types.h
HelpWindow.o: /usr/include/linux/fcntl.h /usr/include/asm/fcntl.h
HelpWindow.o: /usr/include/unistd.h /usr/include/posix_opt.h
HelpWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
HelpWindow.o: /usr/include/confname.h /usr/include/Xm/Xm.h
HelpWindow.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
HelpWindow.o: /usr/include/X11/X11/X11/X.h
HelpWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
HelpWindow.o: /usr/include/X11/X11/X11/Xosdefs.h /usr/include/X11/X11/Xutil.h
HelpWindow.o: /usr/include/X11/X11/Xresource.h /usr/include/string.h
HelpWindow.o: /usr/include/X11/X11/Core.h /usr/include/X11/X11/Composite.h
HelpWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
HelpWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
HelpWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
HelpWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
HelpWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
HelpWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
HelpWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
HelpWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
HelpWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
HelpWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
HelpWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
HelpWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
HelpWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
HelpWindow.o: /usr/include/Xm/DialogS.h /usr/include/Xm/PanedW.h
HelpWindow.o: /usr/include/Xm/Frame.h /usr/include/Xm/Form.h
HelpWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/PushB.h ../config.h
HelpWindow.o: /usr/include/X11/xpm.h ./../lib/XmHTML-1.1.0/src/XmHTML.h
HelpWindow.o: ./../lib/XmHTML-1.1.0/src/HTML.h ./../include/main.h
HelpWindow.o: ./../include/messages.h ./../include/util.h
HelpWindow.o: /usr/include/stdlib.h /usr/include/errno.h
HelpWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
HelpWindow.o: /usr/include/alloca.h /usr/include/stdio.h /usr/include/libio.h
HelpWindow.o: /usr/include/_G_config.h ./../include/xtutil.h
Ledger.o: ./../include/Ledger.h ./register/register.h ./register/basiccell.h
Ledger.o: ./register/datecell.h /usr/include/time.h /usr/include/features.h
Ledger.o: /usr/include/sys/cdefs.h /usr/include/sys/time.h
Ledger.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
Ledger.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Ledger.o: /usr/include/linux/time.h /usr/include/sys/types.h
Ledger.o: /usr/include/sys/bitypes.h ./register/quickfillcell.h
Ledger.o: ./register/QuickFill.h ../config.h ./register/pricecell.h
Ledger.o: ./register/table.h /usr/include/Xm/Xm.h
Ledger.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
Ledger.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/X11/Xfuncproto.h
Ledger.o: /usr/include/X11/X11/X11/Xosdefs.h
Ledger.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Ledger.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Ledger.o: /usr/include/string.h /usr/include/X11/X11/Core.h
Ledger.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
Ledger.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Ledger.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Ledger.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Ledger.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Ledger.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Ledger.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Ledger.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Ledger.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Ledger.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Ledger.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Ledger.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Ledger.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Ledger.o: /usr/include/Xm/XmIm.h ./register/cellblock.h ./register/recncell.h
Ledger.o: ./register/textcell.h ./../include/Transaction.h
Ledger.o: ./../include/date.h
LedgerUtils.o: ../config.h ./../include/Account.h /usr/include/Xm/Xm.h
LedgerUtils.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
LedgerUtils.o: /usr/include/sys/types.h /usr/include/linux/types.h
LedgerUtils.o: /usr/include/linux/posix_types.h
LedgerUtils.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
LedgerUtils.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
LedgerUtils.o: /usr/include/X11/X11/X11/Xfuncproto.h
LedgerUtils.o: /usr/include/X11/X11/X11/Xosdefs.h
LedgerUtils.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
LedgerUtils.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
LedgerUtils.o: /usr/include/string.h /usr/include/features.h
LedgerUtils.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
LedgerUtils.o: /usr/include/X11/X11/Composite.h
LedgerUtils.o: /usr/include/X11/X11/Constraint.h
LedgerUtils.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
LedgerUtils.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
LedgerUtils.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
LedgerUtils.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
LedgerUtils.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
LedgerUtils.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
LedgerUtils.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
LedgerUtils.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
LedgerUtils.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
LedgerUtils.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
LedgerUtils.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
LedgerUtils.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
LedgerUtils.o: /usr/include/Xm/XmIm.h ./../include/Transaction.h
LedgerUtils.o: ./../include/date.h ./../include/Data.h ./../include/util.h
LedgerUtils.o: /usr/include/stdlib.h /usr/include/errno.h
LedgerUtils.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
LedgerUtils.o: /usr/include/alloca.h /usr/include/stdio.h
LedgerUtils.o: /usr/include/libio.h /usr/include/_G_config.h
MainWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
MainWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
MainWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
MainWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
MainWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
MainWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
MainWindow.o: /usr/include/X11/X11/X11/Xosdefs.h
MainWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
MainWindow.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
MainWindow.o: /usr/include/string.h /usr/include/features.h
MainWindow.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
MainWindow.o: /usr/include/X11/X11/Composite.h
MainWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
MainWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
MainWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
MainWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
MainWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
MainWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
MainWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
MainWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
MainWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
MainWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
MainWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
MainWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
MainWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
MainWindow.o: /usr/include/Xm/ArrowB.h /usr/include/Xm/Form.h
MainWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/Label.h
MainWindow.o: /usr/include/Xm/LabelGP.h /usr/include/Xm/LabelG.h
MainWindow.o: /usr/include/Xm/GadgetP.h /usr/include/Xm/XmP.h
MainWindow.o: /usr/include/X11/IntrinsicP.h /usr/include/X11/X11/CoreP.h
MainWindow.o: /usr/include/X11/X11/CompositeP.h
MainWindow.o: /usr/include/X11/X11/ConstrainP.h
MainWindow.o: /usr/include/X11/X11/ObjectP.h /usr/include/X11/X11/RectObjP.h
MainWindow.o: /usr/include/Xm/ColorP.h /usr/include/Xm/AccColorT.h
MainWindow.o: /usr/include/Xm/BaseClassP.h /usr/include/Xm/ExtObjectP.h
MainWindow.o: /usr/include/Xm/List.h /usr/include/Xm/MainW.h
MainWindow.o: /usr/include/Xm/PanedW.h /usr/include/Xm/RowColumn.h
MainWindow.o: /usr/include/Xm/Text.h /usr/include/stdio.h
MainWindow.o: /usr/include/libio.h /usr/include/_G_config.h
MainWindow.o: /usr/X11R6/include/./Xbae/Matrix.h
MainWindow.o: /usr/X11R6/include/./Xbae/patchlevel.h ../config.h
MainWindow.o: ./../include/AdjBWindow.h ./../include/Account.h
MainWindow.o: ./../include/Transaction.h ./../include/date.h
MainWindow.o: ./../include/AccWindow.h ./../include/BuildMenu.h
MainWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/Separator.h
MainWindow.o: ./../include/Data.h ./../include/Destroy.h
MainWindow.o: ./../include/FileBox.h ./../include/FileIO.h
MainWindow.o: ./../include/HelpWindow.h ./../include/LedgerUtils.h
MainWindow.o: ./../include/main.h ./../include/messages.h
MainWindow.o: ./../include/MainWindow.h ./../include/RecnWindow.h
MainWindow.o: ./../include/RegWindow.h ./../include/Reports.h
MainWindow.o: ./../include/util.h /usr/include/stdlib.h /usr/include/errno.h
MainWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
MainWindow.o: /usr/include/alloca.h ./../include/XferWindow.h
QIFIO.o: /usr/include/fcntl.h /usr/include/features.h
QIFIO.o: /usr/include/sys/cdefs.h /usr/include/sys/types.h
QIFIO.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
QIFIO.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
QIFIO.o: /usr/include/sys/bitypes.h /usr/include/gnu/types.h
QIFIO.o: /usr/include/linux/fcntl.h /usr/include/asm/fcntl.h
QIFIO.o: /usr/include/string.h
QIFIO.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
QIFIO.o: /usr/include/unistd.h /usr/include/posix_opt.h
QIFIO.o: /usr/include/confname.h /usr/include/Xm/Xm.h
QIFIO.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
QIFIO.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/X11/Xfuncproto.h
QIFIO.o: /usr/include/X11/X11/X11/Xosdefs.h /usr/include/X11/X11/Xutil.h
QIFIO.o: /usr/include/X11/X11/Xresource.h /usr/include/X11/X11/Core.h
QIFIO.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
QIFIO.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
QIFIO.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
QIFIO.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
QIFIO.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
QIFIO.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
QIFIO.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
QIFIO.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
QIFIO.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
QIFIO.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
QIFIO.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
QIFIO.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
QIFIO.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
QIFIO.o: /usr/include/Xm/XmIm.h ../config.h ./../include/Account.h
QIFIO.o: ./../include/Transaction.h ./../include/date.h ./../include/Data.h
QIFIO.o: ./../include/messages.h ./../include/util.h /usr/include/stdlib.h
QIFIO.o: /usr/include/errno.h /usr/include/linux/errno.h
QIFIO.o: /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/stdio.h
QIFIO.o: /usr/include/libio.h /usr/include/_G_config.h
RecnWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
RecnWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
RecnWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
RecnWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
RecnWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
RecnWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
RecnWindow.o: /usr/include/X11/X11/X11/Xosdefs.h
RecnWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
RecnWindow.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
RecnWindow.o: /usr/include/string.h /usr/include/features.h
RecnWindow.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
RecnWindow.o: /usr/include/X11/X11/Composite.h
RecnWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
RecnWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
RecnWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
RecnWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
RecnWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
RecnWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
RecnWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
RecnWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
RecnWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
RecnWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
RecnWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
RecnWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
RecnWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
RecnWindow.o: /usr/include/Xm/DialogS.h /usr/include/Xm/Form.h
RecnWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/Frame.h
RecnWindow.o: /usr/include/Xm/LabelGP.h /usr/include/Xm/LabelG.h
RecnWindow.o: /usr/include/Xm/GadgetP.h /usr/include/Xm/XmP.h
RecnWindow.o: /usr/include/X11/IntrinsicP.h /usr/include/X11/X11/CoreP.h
RecnWindow.o: /usr/include/X11/X11/CompositeP.h
RecnWindow.o: /usr/include/X11/X11/ConstrainP.h
RecnWindow.o: /usr/include/X11/X11/ObjectP.h /usr/include/X11/X11/RectObjP.h
RecnWindow.o: /usr/include/Xm/ColorP.h /usr/include/Xm/AccColorT.h
RecnWindow.o: /usr/include/Xm/BaseClassP.h /usr/include/Xm/ExtObjectP.h
RecnWindow.o: /usr/include/Xm/PanedW.h /usr/include/Xm/PushB.h
RecnWindow.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/Text.h
RecnWindow.o: /usr/include/stdio.h /usr/include/libio.h
RecnWindow.o: /usr/include/_G_config.h /usr/X11R6/include/./Xbae/Matrix.h
RecnWindow.o: /usr/X11R6/include/./Xbae/patchlevel.h ../config.h
RecnWindow.o: ./../include/Account.h ./../include/Transaction.h
RecnWindow.o: ./../include/date.h ./../include/Data.h
RecnWindow.o: ./../include/RegWindow.h ./../include/MainWindow.h
RecnWindow.o: ./../include/messages.h ./../include/util.h
RecnWindow.o: /usr/include/stdlib.h /usr/include/errno.h
RecnWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
RecnWindow.o: /usr/include/alloca.h ./../include/xtutil.h
RegWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
RegWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
RegWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
RegWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
RegWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
RegWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
RegWindow.o: /usr/include/X11/X11/X11/Xosdefs.h
RegWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
RegWindow.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
RegWindow.o: /usr/include/string.h /usr/include/features.h
RegWindow.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
RegWindow.o: /usr/include/X11/X11/Composite.h
RegWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
RegWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
RegWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
RegWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
RegWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
RegWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
RegWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
RegWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
RegWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
RegWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
RegWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
RegWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
RegWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
RegWindow.o: /usr/include/Xm/DialogS.h /usr/include/Xm/Form.h
RegWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/Frame.h
RegWindow.o: /usr/include/Xm/LabelGP.h /usr/include/Xm/LabelG.h
RegWindow.o: /usr/include/Xm/GadgetP.h /usr/include/Xm/XmP.h
RegWindow.o: /usr/include/X11/IntrinsicP.h /usr/include/X11/X11/CoreP.h
RegWindow.o: /usr/include/X11/X11/CompositeP.h
RegWindow.o: /usr/include/X11/X11/ConstrainP.h /usr/include/X11/X11/ObjectP.h
RegWindow.o: /usr/include/X11/X11/RectObjP.h /usr/include/Xm/ColorP.h
RegWindow.o: /usr/include/Xm/AccColorT.h /usr/include/Xm/BaseClassP.h
RegWindow.o: /usr/include/Xm/ExtObjectP.h /usr/include/Xm/PanedW.h
RegWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/RowColumn.h
RegWindow.o: /usr/include/Xm/Text.h /usr/include/stdio.h /usr/include/libio.h
RegWindow.o: /usr/include/_G_config.h ../config.h ./../include/Account.h
RegWindow.o: ./../include/Transaction.h ./../include/date.h
RegWindow.o: ./../include/AdjBWindow.h ./../include/BuildMenu.h
RegWindow.o: /usr/include/Xm/Separator.h ./../include/Data.h
RegWindow.o: ./../include/Ledger.h ./register/register.h
RegWindow.o: ./register/basiccell.h ./register/datecell.h /usr/include/time.h
RegWindow.o: /usr/include/sys/time.h /usr/include/linux/time.h
RegWindow.o: ./register/quickfillcell.h ./register/QuickFill.h
RegWindow.o: ./register/pricecell.h ./register/table.h ./register/cellblock.h
RegWindow.o: ./register/recncell.h ./register/textcell.h
RegWindow.o: ./../include/LedgerUtils.h ./../include/MainWindow.h
RegWindow.o: ./../include/main.h ./../include/messages.h
RegWindow.o: ./../include/RecnWindow.h ./../include/util.h
RegWindow.o: /usr/include/stdlib.h /usr/include/errno.h
RegWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
RegWindow.o: /usr/include/alloca.h ./../include/xtutil.h
Reports.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
Reports.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
Reports.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
Reports.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Reports.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
Reports.o: /usr/include/X11/X11/X11/Xfuncproto.h
Reports.o: /usr/include/X11/X11/X11/Xosdefs.h
Reports.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Reports.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Reports.o: /usr/include/string.h /usr/include/features.h
Reports.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
Reports.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
Reports.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Reports.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Reports.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Reports.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Reports.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Reports.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Reports.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Reports.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Reports.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Reports.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Reports.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Reports.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Reports.o: /usr/include/Xm/XmIm.h /usr/include/Xm/Form.h
Reports.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/Text.h
Reports.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
Reports.o: /usr/include/Xm/DialogS.h /usr/include/Xm/PanedW.h
Reports.o: /usr/include/Xm/Frame.h /usr/include/Xm/PushB.h
Reports.o: /usr/include/Xm/LabelGP.h /usr/include/Xm/LabelG.h
Reports.o: /usr/include/Xm/GadgetP.h /usr/include/Xm/XmP.h
Reports.o: /usr/include/X11/IntrinsicP.h /usr/include/X11/X11/CoreP.h
Reports.o: /usr/include/X11/X11/CompositeP.h
Reports.o: /usr/include/X11/X11/ConstrainP.h /usr/include/X11/X11/ObjectP.h
Reports.o: /usr/include/X11/X11/RectObjP.h /usr/include/Xm/ColorP.h
Reports.o: /usr/include/Xm/AccColorT.h /usr/include/Xm/BaseClassP.h
Reports.o: /usr/include/Xm/ExtObjectP.h ../config.h ./../include/messages.h
Reports.o: ./../include/Reports.h ./../include/util.h /usr/include/stdlib.h
Reports.o: /usr/include/errno.h /usr/include/linux/errno.h
Reports.o: /usr/include/asm/errno.h /usr/include/alloca.h
Reports.o: ./../include/xtutil.h
TextBox.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/Xlib.h
TextBox.o: /usr/include/sys/types.h /usr/include/linux/types.h
TextBox.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
TextBox.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
TextBox.o: /usr/include/X11/X11/X11/Xfuncproto.h
TextBox.o: /usr/include/X11/X11/X11/Xosdefs.h
TextBox.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
TextBox.o: /usr/include/X11/cursorfont.h /usr/include/Xm/Xm.h
TextBox.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xutil.h
TextBox.o: /usr/include/X11/X11/Xresource.h /usr/include/string.h
TextBox.o: /usr/include/features.h /usr/include/sys/cdefs.h
TextBox.o: /usr/include/X11/X11/Core.h /usr/include/X11/X11/Composite.h
TextBox.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
TextBox.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
TextBox.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
TextBox.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
TextBox.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
TextBox.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
TextBox.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
TextBox.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
TextBox.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
TextBox.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
TextBox.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
TextBox.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
TextBox.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
TextBox.o: /usr/include/Xm/Text.h /usr/include/stdio.h /usr/include/libio.h
TextBox.o: /usr/include/_G_config.h /usr/include/Xm/PanedW.h
TextBox.o: /usr/include/Xm/Form.h /usr/include/Xm/BulletinB.h
TextBox.o: /usr/include/Xm/PushB.h /usr/include/Xm/DialogS.h
TextBox.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/MessageB.h
TextBox.o: /usr/X11R6/include/./Xbae/Matrix.h
TextBox.o: /usr/X11R6/include/./Xbae/patchlevel.h ../config.h
TextBox.o: ./../include/messages.h ./../include/util.h /usr/include/stdlib.h
TextBox.o: /usr/include/errno.h /usr/include/linux/errno.h
TextBox.o: /usr/include/asm/errno.h /usr/include/alloca.h
TextBox.o: ./../include/xtutil.h
Transaction.o: /usr/include/string.h /usr/include/features.h
Transaction.o: /usr/include/sys/cdefs.h
Transaction.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h ../config.h
Transaction.o: ./../include/date.h ./../include/Transaction.h
Transaction.o: ./../include/util.h /usr/include/stdlib.h /usr/include/errno.h
Transaction.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
Transaction.o: /usr/include/alloca.h /usr/include/stdio.h
Transaction.o: /usr/include/libio.h /usr/include/_G_config.h
XferWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
XferWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
XferWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
XferWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
XferWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
XferWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
XferWindow.o: /usr/include/X11/X11/X11/Xosdefs.h
XferWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
XferWindow.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
XferWindow.o: /usr/include/string.h /usr/include/features.h
XferWindow.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
XferWindow.o: /usr/include/X11/X11/Composite.h
XferWindow.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
XferWindow.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
XferWindow.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
XferWindow.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
XferWindow.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
XferWindow.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
XferWindow.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
XferWindow.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
XferWindow.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
XferWindow.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
XferWindow.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
XferWindow.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
XferWindow.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
XferWindow.o: /usr/include/Xm/DialogS.h /usr/include/Xm/Form.h
XferWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/LabelG.h
XferWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/Text.h
XferWindow.o: /usr/include/stdio.h /usr/include/libio.h
XferWindow.o: /usr/include/_G_config.h ../config.h ./../include/Account.h
XferWindow.o: ./../include/Transaction.h ./../include/date.h
XferWindow.o: ./../include/BuildMenu.h /usr/include/Xm/Separator.h
XferWindow.o: ./../include/Data.h ./../include/messages.h
XferWindow.o: ./../include/MainWindow.h ./../include/RecnWindow.h
XferWindow.o: ./../include/RegWindow.h ./../include/util.h
XferWindow.o: /usr/include/stdlib.h /usr/include/errno.h
XferWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
XferWindow.o: /usr/include/alloca.h ./../include/xtutil.h
date.o: /usr/include/time.h /usr/include/features.h /usr/include/sys/cdefs.h
date.o: /usr/include/sys/time.h /usr/include/linux/types.h
date.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
date.o: /usr/include/asm/types.h /usr/include/linux/time.h
date.o: /usr/include/sys/types.h /usr/include/sys/bitypes.h ../config.h
date.o: ./../include/date.h
main.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
main.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
main.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
main.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
main.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
main.o: /usr/include/X11/X11/X11/Xfuncproto.h
main.o: /usr/include/X11/X11/X11/Xosdefs.h
main.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
main.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
main.o: /usr/include/string.h /usr/include/features.h
main.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
main.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
main.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
main.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
main.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
main.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
main.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
main.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
main.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
main.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
main.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
main.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
main.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
main.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
main.o: /usr/include/Xm/XmIm.h /usr/include/stdlib.h /usr/include/errno.h
main.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
main.o: /usr/include/alloca.h ../config.h ./../include/Data.h
main.o: ./../include/Account.h ./../include/Transaction.h ./../include/date.h
main.o: ./../include/FileBox.h ./../include/FileIO.h ./../include/main.h
main.o: ./../include/MainWindow.h ./../include/util.h /usr/include/stdio.h
main.o: /usr/include/libio.h /usr/include/_G_config.h
util.o: ../config.h ./../include/messages.h ./../include/util.h
util.o: /usr/include/stdlib.h /usr/include/features.h
util.o: /usr/include/sys/cdefs.h
util.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
util.o: /usr/include/errno.h /usr/include/linux/errno.h
util.o: /usr/include/asm/errno.h /usr/include/alloca.h /usr/include/stdio.h
util.o: /usr/include/libio.h /usr/include/_G_config.h
xtutil.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/Xlib.h
xtutil.o: /usr/include/sys/types.h /usr/include/linux/types.h
xtutil.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
xtutil.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
xtutil.o: /usr/include/X11/X11/X11/Xfuncproto.h
xtutil.o: /usr/include/X11/X11/X11/Xosdefs.h
xtutil.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
xtutil.o: /usr/include/X11/cursorfont.h /usr/include/Xm/Xm.h
xtutil.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xutil.h
xtutil.o: /usr/include/X11/X11/Xresource.h /usr/include/string.h
xtutil.o: /usr/include/features.h /usr/include/sys/cdefs.h
xtutil.o: /usr/include/X11/X11/Core.h /usr/include/X11/X11/Composite.h
xtutil.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
xtutil.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
xtutil.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
xtutil.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
xtutil.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
xtutil.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
xtutil.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
xtutil.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
xtutil.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
xtutil.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
xtutil.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
xtutil.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
xtutil.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
xtutil.o: /usr/include/Xm/Text.h /usr/include/stdio.h /usr/include/libio.h
xtutil.o: /usr/include/_G_config.h /usr/include/Xm/PanedW.h
xtutil.o: /usr/include/Xm/Form.h /usr/include/Xm/BulletinB.h
xtutil.o: /usr/include/Xm/PushB.h /usr/include/Xm/DialogS.h
xtutil.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/MessageB.h
xtutil.o: /usr/X11R6/include/./Xbae/Matrix.h
xtutil.o: /usr/X11R6/include/./Xbae/patchlevel.h ../config.h
xtutil.o: ./../include/messages.h ./../include/util.h /usr/include/stdlib.h
xtutil.o: /usr/include/errno.h /usr/include/linux/errno.h
xtutil.o: /usr/include/asm/errno.h /usr/include/alloca.h
xtutil.o: ./../include/xtutil.h
