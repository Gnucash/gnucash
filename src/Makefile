######################################################################
#********************************************************************
#* Makefile -- makefile for xacc (X-Accountant)                     *
#* Copyright (C) 1997 Robin D. Clark                                *
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
#*   Author: Rob Clark                                              *
#* Internet: rclark@cs.hmc.edu                                      *
#*  Address: 609 8th Street                                         *
#*           Huntington Beach, CA 92648-4632                        *
#********************************************************************

######################################################################
# DO NOT EDIT THE STUFF BELOW THIS LINE!                             #

OBJS = Account.o AccWindow.o AdjBWindow.o BuildMenu.o Data.o date.o \
       FileBox.o FileIO.o HelpWindow.o main.o MainWindow.o \
       QIFIO.o QuickFill.o RecnWindow.o RegWindow.o Reports.o \
       Transaction.o util.o XferWindow.o

SRCS = ${OBJS:.o=.c}

TARGET = ../xacc

default: $(TARGET)

$(TARGET): $(OBJS) ../libhtmlw/libhtmlw.a
	@echo "++++++"
	$(CC) $(OBJS) $(LFLAGS) $(LIBS) ../libhtmlw/libhtmlw.a -o $@

.c.o: Makefile
	@echo "+++"
	$(CC) $(CFLAGS) -c $<

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	rm -f *.o *~ *.bak

really_clean:
	makedepend -- --

# DO NOT DELETE THIS LINE -- make  depend  depends  on it.

main.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
main.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
main.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
main.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
main.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
main.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
main.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
main.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
main.o: /usr/X11/include/X11/X11/Xutil.h /usr/X11/include/X11/X11/Xresource.h
main.o: /usr/include/string.h /usr/include/features.h
main.o: /usr/include/sys/cdefs.h /usr/X11/include/X11/X11/Core.h
main.o: /usr/X11/include/X11/X11/Composite.h
main.o: /usr/X11/include/X11/X11/Constraint.h
main.o: /usr/X11/include/X11/X11/Object.h /usr/X11/include/X11/X11/RectObj.h
main.o: /usr/X11/include/X11/Shell.h /usr/X11/include/X11/X11/SM/SMlib.h
main.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
main.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
main.o: /usr/X11/include/Xm/XmStrDefs.h /usr/X11/include/X11/StringDefs.h
main.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
main.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
main.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
main.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
main.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
main.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
main.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
main.o: /usr/X11/include/Xm/XmIm.h ../include/BuildMenu.h
main.o: /usr/X11/include/Xm/PushB.h /usr/X11/include/Xm/Separator.h
main.o: ../include/RegWindow.h ../include/Account.h ../include/QuickFill.h
main.o: ../include/Transaction.h ../include/date.h ../include/FileIO.h
main.o: ../include/Data.h ../include/FileBox.h ../include/util.h
main.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
main.o: /usr/include/errno.h /usr/include/linux/errno.h
main.o: /usr/include/asm/errno.h ../include/main.h
util.o: /usr/X11/include/X11/X11/X11/X.h /usr/X11/include/X11/X11/Xlib.h
util.o: /usr/include/sys/types.h /usr/include/linux/types.h
util.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
util.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
util.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
util.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
util.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
util.o: /usr/X11/include/X11/cursorfont.h /usr/X11/include/Xm/Xm.h
util.o: /usr/X11/include/X11/Intrinsic.h /usr/X11/include/X11/X11/Xutil.h
util.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
util.o: /usr/include/features.h /usr/include/sys/cdefs.h
util.o: /usr/X11/include/X11/X11/Core.h /usr/X11/include/X11/X11/Composite.h
util.o: /usr/X11/include/X11/X11/Constraint.h
util.o: /usr/X11/include/X11/X11/Object.h /usr/X11/include/X11/X11/RectObj.h
util.o: /usr/X11/include/X11/Shell.h /usr/X11/include/X11/X11/SM/SMlib.h
util.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
util.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
util.o: /usr/X11/include/Xm/XmStrDefs.h /usr/X11/include/X11/StringDefs.h
util.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
util.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
util.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
util.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
util.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
util.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
util.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
util.o: /usr/X11/include/Xm/XmIm.h /usr/X11/include/Xm/Text.h
util.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
util.o: /usr/X11/include/Xm/PanedW.h /usr/X11/include/Xm/Form.h
util.o: /usr/X11/include/Xm/BulletinB.h /usr/X11/include/Xm/PushB.h
util.o: /usr/X11/include/Xm/DialogS.h /usr/X11/include/Xm/RowColumn.h
util.o: /usr/X11/include/Xm/MessageB.h /usr/X11/include/Xbae/Matrix.h
util.o: /usr/X11/include/Xbae/patchlevel.h ../include/util.h
util.o: /usr/include/errno.h /usr/include/linux/errno.h
util.o: /usr/include/asm/errno.h
date.o: /usr/include/time.h /usr/include/features.h /usr/include/sys/cdefs.h
date.o: /usr/include/sys/time.h /usr/include/linux/types.h
date.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
date.o: /usr/include/asm/types.h /usr/include/linux/time.h
date.o: /usr/include/sys/types.h /usr/include/sys/bitypes.h ../include/date.h
date.o: ../include/util.h /usr/X11/include/Xm/Xm.h
date.o: /usr/X11/include/X11/Intrinsic.h /usr/X11/include/X11/X11/Xlib.h
date.o: /usr/X11/include/X11/X11/X11/X.h
date.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
date.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
date.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
date.o: /usr/X11/include/X11/X11/Xutil.h /usr/X11/include/X11/X11/Xresource.h
date.o: /usr/include/string.h /usr/X11/include/X11/X11/Core.h
date.o: /usr/X11/include/X11/X11/Composite.h
date.o: /usr/X11/include/X11/X11/Constraint.h
date.o: /usr/X11/include/X11/X11/Object.h /usr/X11/include/X11/X11/RectObj.h
date.o: /usr/X11/include/X11/Shell.h /usr/X11/include/X11/X11/SM/SMlib.h
date.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
date.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
date.o: /usr/X11/include/Xm/XmStrDefs.h /usr/X11/include/X11/StringDefs.h
date.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
date.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
date.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
date.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
date.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
date.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
date.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
date.o: /usr/X11/include/Xm/XmIm.h /usr/include/stdio.h /usr/include/libio.h
date.o: /usr/include/_G_config.h /usr/include/errno.h
date.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
MainWindow.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
MainWindow.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
MainWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
MainWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
MainWindow.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
MainWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
MainWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
MainWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
MainWindow.o: /usr/X11/include/X11/X11/Xutil.h
MainWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
MainWindow.o: /usr/include/features.h /usr/include/sys/cdefs.h
MainWindow.o: /usr/X11/include/X11/X11/Core.h
MainWindow.o: /usr/X11/include/X11/X11/Composite.h
MainWindow.o: /usr/X11/include/X11/X11/Constraint.h
MainWindow.o: /usr/X11/include/X11/X11/Object.h
MainWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
MainWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h
MainWindow.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
MainWindow.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
MainWindow.o: /usr/X11/include/Xm/XmStrDefs.h
MainWindow.o: /usr/X11/include/X11/StringDefs.h
MainWindow.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
MainWindow.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
MainWindow.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
MainWindow.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
MainWindow.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
MainWindow.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
MainWindow.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
MainWindow.o: /usr/X11/include/Xm/XmIm.h /usr/X11/include/Xm/PanedW.h
MainWindow.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
MainWindow.o: /usr/X11/include/Xm/MainW.h /usr/X11/include/Xm/Label.h
MainWindow.o: /usr/X11/include/Xm/LabelGP.h /usr/X11/include/Xm/LabelG.h
MainWindow.o: /usr/X11/include/Xm/GadgetP.h /usr/X11/include/Xm/XmP.h
MainWindow.o: /usr/X11/include/X11/IntrinsicP.h
MainWindow.o: /usr/X11/include/X11/X11/CoreP.h
MainWindow.o: /usr/X11/include/X11/X11/CompositeP.h
MainWindow.o: /usr/X11/include/X11/X11/ConstrainP.h
MainWindow.o: /usr/X11/include/X11/X11/ObjectP.h
MainWindow.o: /usr/X11/include/X11/X11/RectObjP.h
MainWindow.o: /usr/X11/include/Xm/ColorP.h /usr/X11/include/Xm/AccColorT.h
MainWindow.o: /usr/X11/include/Xm/BaseClassP.h
MainWindow.o: /usr/X11/include/Xm/ExtObjectP.h /usr/X11/include/Xm/List.h
MainWindow.o: /usr/X11/include/Xm/RowColumn.h /usr/X11/include/Xbae/Matrix.h
MainWindow.o: /usr/X11/include/Xbae/patchlevel.h ../include/main.h
MainWindow.o: ../include/date.h ../include/Transaction.h ../include/util.h
MainWindow.o: /usr/include/stdio.h /usr/include/libio.h
MainWindow.o: /usr/include/_G_config.h /usr/include/errno.h
MainWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
MainWindow.o: ../include/Data.h ../include/Account.h ../include/QuickFill.h
MainWindow.o: ../include/FileIO.h ../include/FileBox.h ../include/BuildMenu.h
MainWindow.o: /usr/X11/include/Xm/PushB.h /usr/X11/include/Xm/Separator.h
MainWindow.o: ../include/MainWindow.h ../include/RegWindow.h
MainWindow.o: ../include/XferWindow.h ../include/HelpWindow.h
RegWindow.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
RegWindow.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
RegWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
RegWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
RegWindow.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
RegWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
RegWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
RegWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
RegWindow.o: /usr/X11/include/X11/X11/Xutil.h
RegWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
RegWindow.o: /usr/include/features.h /usr/include/sys/cdefs.h
RegWindow.o: /usr/X11/include/X11/X11/Core.h
RegWindow.o: /usr/X11/include/X11/X11/Composite.h
RegWindow.o: /usr/X11/include/X11/X11/Constraint.h
RegWindow.o: /usr/X11/include/X11/X11/Object.h
RegWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
RegWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
RegWindow.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
RegWindow.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
RegWindow.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
RegWindow.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
RegWindow.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
RegWindow.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
RegWindow.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
RegWindow.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
RegWindow.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
RegWindow.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
RegWindow.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
RegWindow.o: /usr/X11/include/Xm/Text.h /usr/include/stdio.h
RegWindow.o: /usr/include/libio.h /usr/include/_G_config.h
RegWindow.o: /usr/X11/include/Xm/DialogS.h /usr/X11/include/Xm/PanedW.h
RegWindow.o: /usr/X11/include/Xm/Frame.h /usr/X11/include/Xm/RowColumn.h
RegWindow.o: /usr/X11/include/Xm/PushB.h /usr/X11/include/Xm/LabelGP.h
RegWindow.o: /usr/X11/include/Xm/LabelG.h /usr/X11/include/Xm/GadgetP.h
RegWindow.o: /usr/X11/include/Xm/XmP.h /usr/X11/include/X11/IntrinsicP.h
RegWindow.o: /usr/X11/include/X11/X11/CoreP.h
RegWindow.o: /usr/X11/include/X11/X11/CompositeP.h
RegWindow.o: /usr/X11/include/X11/X11/ConstrainP.h
RegWindow.o: /usr/X11/include/X11/X11/ObjectP.h
RegWindow.o: /usr/X11/include/X11/X11/RectObjP.h /usr/X11/include/Xm/ColorP.h
RegWindow.o: /usr/X11/include/Xm/AccColorT.h /usr/X11/include/Xm/BaseClassP.h
RegWindow.o: /usr/X11/include/Xm/ExtObjectP.h /usr/X11/include/Xbae/Matrix.h
RegWindow.o: /usr/X11/include/Xbae/patchlevel.h ../include/main.h
RegWindow.o: ../include/date.h ../include/Transaction.h ../include/util.h
RegWindow.o: /usr/include/errno.h /usr/include/linux/errno.h
RegWindow.o: /usr/include/asm/errno.h ../include/Data.h ../include/Account.h
RegWindow.o: ../include/QuickFill.h ../include/MainWindow.h
RegWindow.o: ../include/BuildMenu.h /usr/X11/include/Xm/Separator.h
RegWindow.o: ../include/RecnWindow.h ../include/AdjBWindow.h
BuildMenu.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
BuildMenu.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
BuildMenu.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
BuildMenu.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
BuildMenu.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
BuildMenu.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
BuildMenu.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
BuildMenu.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
BuildMenu.o: /usr/X11/include/X11/X11/Xutil.h
BuildMenu.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
BuildMenu.o: /usr/include/features.h /usr/include/sys/cdefs.h
BuildMenu.o: /usr/X11/include/X11/X11/Core.h
BuildMenu.o: /usr/X11/include/X11/X11/Composite.h
BuildMenu.o: /usr/X11/include/X11/X11/Constraint.h
BuildMenu.o: /usr/X11/include/X11/X11/Object.h
BuildMenu.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
BuildMenu.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
BuildMenu.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
BuildMenu.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
BuildMenu.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
BuildMenu.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
BuildMenu.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
BuildMenu.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
BuildMenu.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
BuildMenu.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
BuildMenu.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
BuildMenu.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
BuildMenu.o: /usr/X11/include/X11/cursorfont.h /usr/X11/include/Xm/DrawingA.h
BuildMenu.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
BuildMenu.o: /usr/X11/include/Xm/RowColumn.h /usr/X11/include/Xm/MainW.h
BuildMenu.o: /usr/X11/include/Xm/FileSB.h /usr/X11/include/Xm/MessageB.h
BuildMenu.o: /usr/X11/include/Xm/DialogS.h /usr/X11/include/Xm/PanedW.h
BuildMenu.o: /usr/X11/include/Xm/ScrolledW.h /usr/X11/include/Xm/DrawnB.h
BuildMenu.o: /usr/X11/include/Xm/ToggleB.h /usr/X11/include/Xm/ToggleBG.h
BuildMenu.o: /usr/X11/include/Xm/PushB.h /usr/X11/include/Xm/PushBG.h
BuildMenu.o: /usr/X11/include/Xm/CascadeBG.h /usr/X11/include/Xm/Label.h
BuildMenu.o: /usr/X11/include/Xm/LabelG.h /usr/X11/include/Xm/Scale.h
BuildMenu.o: /usr/X11/include/Xm/Text.h /usr/include/stdio.h
BuildMenu.o: /usr/include/libio.h /usr/include/_G_config.h
BuildMenu.o: /usr/X11/include/Xm/TextF.h /usr/X11/include/Xm/SeparatoG.h
BuildMenu.o: ../include/BuildMenu.h /usr/X11/include/Xm/Separator.h
AccWindow.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
AccWindow.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
AccWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
AccWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
AccWindow.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
AccWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
AccWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
AccWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
AccWindow.o: /usr/X11/include/X11/X11/Xutil.h
AccWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
AccWindow.o: /usr/include/features.h /usr/include/sys/cdefs.h
AccWindow.o: /usr/X11/include/X11/X11/Core.h
AccWindow.o: /usr/X11/include/X11/X11/Composite.h
AccWindow.o: /usr/X11/include/X11/X11/Constraint.h
AccWindow.o: /usr/X11/include/X11/X11/Object.h
AccWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
AccWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
AccWindow.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
AccWindow.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
AccWindow.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
AccWindow.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
AccWindow.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
AccWindow.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
AccWindow.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
AccWindow.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
AccWindow.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
AccWindow.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
AccWindow.o: /usr/X11/include/Xm/DialogS.h /usr/X11/include/Xm/Form.h
AccWindow.o: /usr/X11/include/Xm/BulletinB.h /usr/X11/include/Xm/Frame.h
AccWindow.o: /usr/X11/include/Xm/RowColumn.h /usr/X11/include/Xm/LabelGP.h
AccWindow.o: /usr/X11/include/Xm/LabelG.h /usr/X11/include/Xm/GadgetP.h
AccWindow.o: /usr/X11/include/Xm/XmP.h /usr/X11/include/X11/IntrinsicP.h
AccWindow.o: /usr/X11/include/X11/X11/CoreP.h
AccWindow.o: /usr/X11/include/X11/X11/CompositeP.h
AccWindow.o: /usr/X11/include/X11/X11/ConstrainP.h
AccWindow.o: /usr/X11/include/X11/X11/ObjectP.h
AccWindow.o: /usr/X11/include/X11/X11/RectObjP.h /usr/X11/include/Xm/ColorP.h
AccWindow.o: /usr/X11/include/Xm/AccColorT.h /usr/X11/include/Xm/BaseClassP.h
AccWindow.o: /usr/X11/include/Xm/ExtObjectP.h /usr/X11/include/Xm/ToggleB.h
AccWindow.o: /usr/X11/include/Xm/PushB.h /usr/X11/include/Xm/Text.h
AccWindow.o: /usr/include/stdio.h /usr/include/libio.h
AccWindow.o: /usr/include/_G_config.h ../include/Account.h
AccWindow.o: ../include/QuickFill.h ../include/Transaction.h
AccWindow.o: ../include/date.h ../include/Data.h ../include/main.h
AccWindow.o: ../include/util.h /usr/include/errno.h
AccWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileIO.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
FileIO.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
FileIO.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
FileIO.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
FileIO.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
FileIO.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
FileIO.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
FileIO.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
FileIO.o: /usr/X11/include/X11/X11/Xutil.h
FileIO.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
FileIO.o: /usr/include/features.h /usr/include/sys/cdefs.h
FileIO.o: /usr/X11/include/X11/X11/Core.h
FileIO.o: /usr/X11/include/X11/X11/Composite.h
FileIO.o: /usr/X11/include/X11/X11/Constraint.h
FileIO.o: /usr/X11/include/X11/X11/Object.h
FileIO.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
FileIO.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
FileIO.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
FileIO.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
FileIO.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
FileIO.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
FileIO.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
FileIO.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
FileIO.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
FileIO.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
FileIO.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
FileIO.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
FileIO.o: /usr/include/fcntl.h /usr/include/gnu/types.h
FileIO.o: /usr/include/linux/fcntl.h /usr/include/asm/fcntl.h
FileIO.o: ../include/main.h ../include/date.h ../include/Transaction.h
FileIO.o: ../include/util.h /usr/include/stdio.h /usr/include/libio.h
FileIO.o: /usr/include/_G_config.h /usr/include/errno.h
FileIO.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileIO.o: ../include/Account.h ../include/QuickFill.h ../include/Data.h
Account.o: ../include/util.h /usr/X11/include/Xm/Xm.h
Account.o: /usr/X11/include/X11/Intrinsic.h /usr/X11/include/X11/X11/Xlib.h
Account.o: /usr/include/sys/types.h /usr/include/linux/types.h
Account.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
Account.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
Account.o: /usr/X11/include/X11/X11/X11/X.h
Account.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
Account.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
Account.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Account.o: /usr/X11/include/X11/X11/Xutil.h
Account.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
Account.o: /usr/include/features.h /usr/include/sys/cdefs.h
Account.o: /usr/X11/include/X11/X11/Core.h
Account.o: /usr/X11/include/X11/X11/Composite.h
Account.o: /usr/X11/include/X11/X11/Constraint.h
Account.o: /usr/X11/include/X11/X11/Object.h
Account.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
Account.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
Account.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
Account.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
Account.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
Account.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
Account.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
Account.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
Account.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
Account.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
Account.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
Account.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
Account.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
Account.o: /usr/include/errno.h /usr/include/linux/errno.h
Account.o: /usr/include/asm/errno.h ../include/main.h ../include/date.h
Account.o: ../include/Transaction.h ../include/Data.h ../include/Account.h
Account.o: ../include/QuickFill.h
Data.o: ../include/Data.h ../include/Account.h ../include/QuickFill.h
Data.o: ../include/Transaction.h /usr/X11/include/Xm/Xm.h
Data.o: /usr/X11/include/X11/Intrinsic.h /usr/X11/include/X11/X11/Xlib.h
Data.o: /usr/include/sys/types.h /usr/include/linux/types.h
Data.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
Data.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
Data.o: /usr/X11/include/X11/X11/X11/X.h
Data.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
Data.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
Data.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Data.o: /usr/X11/include/X11/X11/Xutil.h /usr/X11/include/X11/X11/Xresource.h
Data.o: /usr/include/string.h /usr/include/features.h
Data.o: /usr/include/sys/cdefs.h /usr/X11/include/X11/X11/Core.h
Data.o: /usr/X11/include/X11/X11/Composite.h
Data.o: /usr/X11/include/X11/X11/Constraint.h
Data.o: /usr/X11/include/X11/X11/Object.h /usr/X11/include/X11/X11/RectObj.h
Data.o: /usr/X11/include/X11/Shell.h /usr/X11/include/X11/X11/SM/SMlib.h
Data.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
Data.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
Data.o: /usr/X11/include/Xm/XmStrDefs.h /usr/X11/include/X11/StringDefs.h
Data.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
Data.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
Data.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
Data.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
Data.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
Data.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
Data.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
Data.o: /usr/X11/include/Xm/XmIm.h ../include/date.h ../include/util.h
Data.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
Data.o: /usr/include/errno.h /usr/include/linux/errno.h
Data.o: /usr/include/asm/errno.h
XferWindow.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
XferWindow.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
XferWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
XferWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
XferWindow.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
XferWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
XferWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
XferWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
XferWindow.o: /usr/X11/include/X11/X11/Xutil.h
XferWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
XferWindow.o: /usr/include/features.h /usr/include/sys/cdefs.h
XferWindow.o: /usr/X11/include/X11/X11/Core.h
XferWindow.o: /usr/X11/include/X11/X11/Composite.h
XferWindow.o: /usr/X11/include/X11/X11/Constraint.h
XferWindow.o: /usr/X11/include/X11/X11/Object.h
XferWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
XferWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h
XferWindow.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
XferWindow.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
XferWindow.o: /usr/X11/include/Xm/XmStrDefs.h
XferWindow.o: /usr/X11/include/X11/StringDefs.h
XferWindow.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
XferWindow.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
XferWindow.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
XferWindow.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
XferWindow.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
XferWindow.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
XferWindow.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
XferWindow.o: /usr/X11/include/Xm/XmIm.h /usr/X11/include/Xm/DialogS.h
XferWindow.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
XferWindow.o: /usr/X11/include/Xm/LabelGP.h /usr/X11/include/Xm/LabelG.h
XferWindow.o: /usr/X11/include/Xm/GadgetP.h /usr/X11/include/Xm/XmP.h
XferWindow.o: /usr/X11/include/X11/IntrinsicP.h
XferWindow.o: /usr/X11/include/X11/X11/CoreP.h
XferWindow.o: /usr/X11/include/X11/X11/CompositeP.h
XferWindow.o: /usr/X11/include/X11/X11/ConstrainP.h
XferWindow.o: /usr/X11/include/X11/X11/ObjectP.h
XferWindow.o: /usr/X11/include/X11/X11/RectObjP.h
XferWindow.o: /usr/X11/include/Xm/ColorP.h /usr/X11/include/Xm/AccColorT.h
XferWindow.o: /usr/X11/include/Xm/BaseClassP.h
XferWindow.o: /usr/X11/include/Xm/ExtObjectP.h /usr/X11/include/Xm/PushB.h
XferWindow.o: /usr/X11/include/Xm/Text.h /usr/include/stdio.h
XferWindow.o: /usr/include/libio.h /usr/include/_G_config.h
XferWindow.o: ../include/BuildMenu.h /usr/X11/include/Xm/Separator.h
XferWindow.o: ../include/Account.h ../include/QuickFill.h
XferWindow.o: ../include/Transaction.h ../include/date.h ../include/Data.h
XferWindow.o: ../include/main.h ../include/util.h /usr/include/errno.h
XferWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileBox.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
FileBox.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
FileBox.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
FileBox.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
FileBox.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
FileBox.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
FileBox.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
FileBox.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
FileBox.o: /usr/X11/include/X11/X11/Xutil.h
FileBox.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
FileBox.o: /usr/include/features.h /usr/include/sys/cdefs.h
FileBox.o: /usr/X11/include/X11/X11/Core.h
FileBox.o: /usr/X11/include/X11/X11/Composite.h
FileBox.o: /usr/X11/include/X11/X11/Constraint.h
FileBox.o: /usr/X11/include/X11/X11/Object.h
FileBox.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
FileBox.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
FileBox.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
FileBox.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
FileBox.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
FileBox.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
FileBox.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
FileBox.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
FileBox.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
FileBox.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
FileBox.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
FileBox.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
FileBox.o: /usr/X11/include/Xm/FileSB.h ../include/FileBox.h
FileBox.o: ../include/util.h /usr/include/stdio.h /usr/include/libio.h
FileBox.o: /usr/include/_G_config.h /usr/include/errno.h
FileBox.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
QuickFill.o: /usr/include/string.h /usr/include/features.h
QuickFill.o: /usr/include/sys/cdefs.h
QuickFill.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
QuickFill.o: ../include/util.h /usr/X11/include/Xm/Xm.h
QuickFill.o: /usr/X11/include/X11/Intrinsic.h /usr/X11/include/X11/X11/Xlib.h
QuickFill.o: /usr/include/sys/types.h /usr/include/linux/types.h
QuickFill.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
QuickFill.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
QuickFill.o: /usr/X11/include/X11/X11/X11/X.h
QuickFill.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
QuickFill.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
QuickFill.o: /usr/X11/include/X11/X11/Xutil.h
QuickFill.o: /usr/X11/include/X11/X11/Xresource.h
QuickFill.o: /usr/X11/include/X11/X11/Core.h
QuickFill.o: /usr/X11/include/X11/X11/Composite.h
QuickFill.o: /usr/X11/include/X11/X11/Constraint.h
QuickFill.o: /usr/X11/include/X11/X11/Object.h
QuickFill.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
QuickFill.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
QuickFill.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
QuickFill.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
QuickFill.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
QuickFill.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
QuickFill.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
QuickFill.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
QuickFill.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
QuickFill.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
QuickFill.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
QuickFill.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
QuickFill.o: /usr/include/stdio.h /usr/include/libio.h
QuickFill.o: /usr/include/_G_config.h /usr/include/errno.h
QuickFill.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
QuickFill.o: ../include/main.h ../include/date.h ../include/Transaction.h
QuickFill.o: ../include/Account.h ../include/QuickFill.h
Reports.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
Reports.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
Reports.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
Reports.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Reports.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
Reports.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
Reports.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
Reports.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Reports.o: /usr/X11/include/X11/X11/Xutil.h
Reports.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
Reports.o: /usr/include/features.h /usr/include/sys/cdefs.h
Reports.o: /usr/X11/include/X11/X11/Core.h
Reports.o: /usr/X11/include/X11/X11/Composite.h
Reports.o: /usr/X11/include/X11/X11/Constraint.h
Reports.o: /usr/X11/include/X11/X11/Object.h
Reports.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
Reports.o: /usr/X11/include/X11/X11/SM/SMlib.h /usr/X11/include/X11/SM/SM.h
Reports.o: /usr/X11/include/X11/ICE/ICElib.h /usr/X11/include/X11/ICE/ICE.h
Reports.o: /usr/X11/include/X11/Xatom.h /usr/X11/include/Xm/XmStrDefs.h
Reports.o: /usr/X11/include/X11/StringDefs.h /usr/X11/include/Xm/VirtKeys.h
Reports.o: /usr/X11/include/Xm/Transfer.h /usr/X11/include/Xm/DragDrop.h
Reports.o: /usr/X11/include/Xm/DragC.h /usr/X11/include/Xm/Display.h
Reports.o: /usr/X11/include/Xm/DropSMgr.h /usr/X11/include/Xm/DragIcon.h
Reports.o: /usr/X11/include/Xm/DropTrans.h /usr/X11/include/Xm/DragOverS.h
Reports.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
Reports.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
Reports.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
Reports.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
Reports.o: /usr/X11/include/Xm/Text.h /usr/include/stdio.h
Reports.o: /usr/include/libio.h /usr/include/_G_config.h
Reports.o: /usr/X11/include/Xm/DialogS.h /usr/X11/include/Xm/PanedW.h
Reports.o: /usr/X11/include/Xm/Frame.h /usr/X11/include/Xm/PushB.h
Reports.o: /usr/X11/include/Xm/LabelGP.h /usr/X11/include/Xm/LabelG.h
Reports.o: /usr/X11/include/Xm/GadgetP.h /usr/X11/include/Xm/XmP.h
Reports.o: /usr/X11/include/X11/IntrinsicP.h /usr/X11/include/X11/X11/CoreP.h
Reports.o: /usr/X11/include/X11/X11/CompositeP.h
Reports.o: /usr/X11/include/X11/X11/ConstrainP.h
Reports.o: /usr/X11/include/X11/X11/ObjectP.h
Reports.o: /usr/X11/include/X11/X11/RectObjP.h /usr/X11/include/Xm/ColorP.h
Reports.o: /usr/X11/include/Xm/AccColorT.h /usr/X11/include/Xm/BaseClassP.h
Reports.o: /usr/X11/include/Xm/ExtObjectP.h ../include/Reports.h
Reports.o: ../include/util.h /usr/include/errno.h /usr/include/linux/errno.h
Reports.o: /usr/include/asm/errno.h
RecnWindow.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
RecnWindow.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
RecnWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
RecnWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
RecnWindow.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
RecnWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
RecnWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
RecnWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
RecnWindow.o: /usr/X11/include/X11/X11/Xutil.h
RecnWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
RecnWindow.o: /usr/include/features.h /usr/include/sys/cdefs.h
RecnWindow.o: /usr/X11/include/X11/X11/Core.h
RecnWindow.o: /usr/X11/include/X11/X11/Composite.h
RecnWindow.o: /usr/X11/include/X11/X11/Constraint.h
RecnWindow.o: /usr/X11/include/X11/X11/Object.h
RecnWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
RecnWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h
RecnWindow.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
RecnWindow.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
RecnWindow.o: /usr/X11/include/Xm/XmStrDefs.h
RecnWindow.o: /usr/X11/include/X11/StringDefs.h
RecnWindow.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
RecnWindow.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
RecnWindow.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
RecnWindow.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
RecnWindow.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
RecnWindow.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
RecnWindow.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
RecnWindow.o: /usr/X11/include/Xm/XmIm.h /usr/X11/include/Xm/RowColumn.h
RecnWindow.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
RecnWindow.o: /usr/X11/include/Xm/Text.h /usr/include/stdio.h
RecnWindow.o: /usr/include/libio.h /usr/include/_G_config.h
RecnWindow.o: /usr/X11/include/Xm/DialogS.h /usr/X11/include/Xm/PanedW.h
RecnWindow.o: /usr/X11/include/Xm/Frame.h /usr/X11/include/Xm/PushB.h
RecnWindow.o: /usr/X11/include/Xm/LabelGP.h /usr/X11/include/Xm/LabelG.h
RecnWindow.o: /usr/X11/include/Xm/GadgetP.h /usr/X11/include/Xm/XmP.h
RecnWindow.o: /usr/X11/include/X11/IntrinsicP.h
RecnWindow.o: /usr/X11/include/X11/X11/CoreP.h
RecnWindow.o: /usr/X11/include/X11/X11/CompositeP.h
RecnWindow.o: /usr/X11/include/X11/X11/ConstrainP.h
RecnWindow.o: /usr/X11/include/X11/X11/ObjectP.h
RecnWindow.o: /usr/X11/include/X11/X11/RectObjP.h
RecnWindow.o: /usr/X11/include/Xm/ColorP.h /usr/X11/include/Xm/AccColorT.h
RecnWindow.o: /usr/X11/include/Xm/BaseClassP.h
RecnWindow.o: /usr/X11/include/Xm/ExtObjectP.h /usr/X11/include/Xbae/Matrix.h
RecnWindow.o: /usr/X11/include/Xbae/patchlevel.h ../include/Data.h
RecnWindow.o: ../include/Account.h ../include/QuickFill.h
RecnWindow.o: ../include/Transaction.h ../include/date.h
RecnWindow.o: ../include/RegWindow.h ../include/MainWindow.h
RecnWindow.o: ../include/main.h ../include/util.h /usr/include/errno.h
RecnWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
HelpWindow.o: /usr/include/sys/types.h /usr/include/linux/types.h
HelpWindow.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
HelpWindow.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
HelpWindow.o: /usr/include/sys/stat.h /usr/include/features.h
HelpWindow.o: /usr/include/sys/cdefs.h /usr/include/linux/stat.h
HelpWindow.o: /usr/include/fcntl.h /usr/include/gnu/types.h
HelpWindow.o: /usr/include/linux/fcntl.h /usr/include/asm/fcntl.h
HelpWindow.o: /usr/include/unistd.h /usr/include/posix_opt.h
HelpWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
HelpWindow.o: /usr/include/confname.h /usr/X11/include/Xm/Xm.h
HelpWindow.o: /usr/X11/include/X11/Intrinsic.h
HelpWindow.o: /usr/X11/include/X11/X11/Xlib.h
HelpWindow.o: /usr/X11/include/X11/X11/X11/X.h
HelpWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
HelpWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
HelpWindow.o: /usr/X11/include/X11/X11/Xutil.h
HelpWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
HelpWindow.o: /usr/X11/include/X11/X11/Core.h
HelpWindow.o: /usr/X11/include/X11/X11/Composite.h
HelpWindow.o: /usr/X11/include/X11/X11/Constraint.h
HelpWindow.o: /usr/X11/include/X11/X11/Object.h
HelpWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
HelpWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h
HelpWindow.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
HelpWindow.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
HelpWindow.o: /usr/X11/include/Xm/XmStrDefs.h
HelpWindow.o: /usr/X11/include/X11/StringDefs.h
HelpWindow.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
HelpWindow.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
HelpWindow.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
HelpWindow.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
HelpWindow.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
HelpWindow.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
HelpWindow.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
HelpWindow.o: /usr/X11/include/Xm/XmIm.h /usr/X11/include/Xm/DialogS.h
HelpWindow.o: /usr/X11/include/Xm/PanedW.h /usr/X11/include/Xm/Frame.h
HelpWindow.o: /usr/X11/include/Xm/Form.h /usr/X11/include/Xm/BulletinB.h
HelpWindow.o: /usr/X11/include/Xm/PushB.h /usr/X11/include/X11/xpm.h
HelpWindow.o: ../include/main.h ../include/date.h ../include/Transaction.h
HelpWindow.o: ../include/util.h /usr/include/stdio.h /usr/include/libio.h
HelpWindow.o: /usr/include/_G_config.h /usr/include/errno.h
HelpWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
HelpWindow.o: ../libhtmlw/HTML.h
AdjBWindow.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
AdjBWindow.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
AdjBWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
AdjBWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
AdjBWindow.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
AdjBWindow.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
AdjBWindow.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
AdjBWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
AdjBWindow.o: /usr/X11/include/X11/X11/Xutil.h
AdjBWindow.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
AdjBWindow.o: /usr/include/features.h /usr/include/sys/cdefs.h
AdjBWindow.o: /usr/X11/include/X11/X11/Core.h
AdjBWindow.o: /usr/X11/include/X11/X11/Composite.h
AdjBWindow.o: /usr/X11/include/X11/X11/Constraint.h
AdjBWindow.o: /usr/X11/include/X11/X11/Object.h
AdjBWindow.o: /usr/X11/include/X11/X11/RectObj.h /usr/X11/include/X11/Shell.h
AdjBWindow.o: /usr/X11/include/X11/X11/SM/SMlib.h
AdjBWindow.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
AdjBWindow.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
AdjBWindow.o: /usr/X11/include/Xm/XmStrDefs.h
AdjBWindow.o: /usr/X11/include/X11/StringDefs.h
AdjBWindow.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
AdjBWindow.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
AdjBWindow.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
AdjBWindow.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
AdjBWindow.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
AdjBWindow.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
AdjBWindow.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
AdjBWindow.o: /usr/X11/include/Xm/XmIm.h /usr/X11/include/Xm/Form.h
AdjBWindow.o: /usr/X11/include/Xm/BulletinB.h /usr/X11/include/Xm/Text.h
AdjBWindow.o: /usr/include/stdio.h /usr/include/libio.h
AdjBWindow.o: /usr/include/_G_config.h /usr/X11/include/Xm/DialogS.h
AdjBWindow.o: /usr/X11/include/Xm/PanedW.h /usr/X11/include/Xm/PushB.h
AdjBWindow.o: /usr/X11/include/Xm/LabelGP.h /usr/X11/include/Xm/LabelG.h
AdjBWindow.o: /usr/X11/include/Xm/GadgetP.h /usr/X11/include/Xm/XmP.h
AdjBWindow.o: /usr/X11/include/X11/IntrinsicP.h
AdjBWindow.o: /usr/X11/include/X11/X11/CoreP.h
AdjBWindow.o: /usr/X11/include/X11/X11/CompositeP.h
AdjBWindow.o: /usr/X11/include/X11/X11/ConstrainP.h
AdjBWindow.o: /usr/X11/include/X11/X11/ObjectP.h
AdjBWindow.o: /usr/X11/include/X11/X11/RectObjP.h
AdjBWindow.o: /usr/X11/include/Xm/ColorP.h /usr/X11/include/Xm/AccColorT.h
AdjBWindow.o: /usr/X11/include/Xm/BaseClassP.h
AdjBWindow.o: /usr/X11/include/Xm/ExtObjectP.h ../include/main.h
AdjBWindow.o: ../include/date.h ../include/Transaction.h ../include/util.h
AdjBWindow.o: /usr/include/errno.h /usr/include/linux/errno.h
AdjBWindow.o: /usr/include/asm/errno.h ../include/Data.h ../include/Account.h
AdjBWindow.o: ../include/QuickFill.h ../include/MainWindow.h
QIFIO.o: /usr/X11/include/Xm/Xm.h /usr/X11/include/X11/Intrinsic.h
QIFIO.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
QIFIO.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
QIFIO.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
QIFIO.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
QIFIO.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
QIFIO.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
QIFIO.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
QIFIO.o: /usr/X11/include/X11/X11/Xutil.h
QIFIO.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
QIFIO.o: /usr/include/features.h /usr/include/sys/cdefs.h
QIFIO.o: /usr/X11/include/X11/X11/Core.h /usr/X11/include/X11/X11/Composite.h
QIFIO.o: /usr/X11/include/X11/X11/Constraint.h
QIFIO.o: /usr/X11/include/X11/X11/Object.h /usr/X11/include/X11/X11/RectObj.h
QIFIO.o: /usr/X11/include/X11/Shell.h /usr/X11/include/X11/X11/SM/SMlib.h
QIFIO.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
QIFIO.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
QIFIO.o: /usr/X11/include/Xm/XmStrDefs.h /usr/X11/include/X11/StringDefs.h
QIFIO.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
QIFIO.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
QIFIO.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
QIFIO.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
QIFIO.o: /usr/X11/include/Xm/DragOverS.h /usr/X11/include/Xm/Primitive.h
QIFIO.o: /usr/X11/include/Xm/Manager.h /usr/X11/include/Xm/Gadget.h
QIFIO.o: /usr/X11/include/Xm/TxtPropCv.h /usr/X11/include/Xm/VendorS.h
QIFIO.o: /usr/X11/include/Xm/XmIm.h /usr/include/fcntl.h
QIFIO.o: /usr/include/gnu/types.h /usr/include/linux/fcntl.h
QIFIO.o: /usr/include/asm/fcntl.h ../include/main.h ../include/date.h
QIFIO.o: ../include/Transaction.h ../include/util.h /usr/include/stdio.h
QIFIO.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/errno.h
QIFIO.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
QIFIO.o: ../include/Account.h ../include/QuickFill.h ../include/Data.h
Transaction.o: ../include/Transaction.h /usr/X11/include/Xm/Xm.h
Transaction.o: /usr/X11/include/X11/Intrinsic.h
Transaction.o: /usr/X11/include/X11/X11/Xlib.h /usr/include/sys/types.h
Transaction.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
Transaction.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Transaction.o: /usr/include/sys/bitypes.h /usr/X11/include/X11/X11/X11/X.h
Transaction.o: /usr/X11/include/X11/X11/X11/Xfuncproto.h
Transaction.o: /usr/X11/include/X11/X11/X11/Xosdefs.h
Transaction.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Transaction.o: /usr/X11/include/X11/X11/Xutil.h
Transaction.o: /usr/X11/include/X11/X11/Xresource.h /usr/include/string.h
Transaction.o: /usr/include/features.h /usr/include/sys/cdefs.h
Transaction.o: /usr/X11/include/X11/X11/Core.h
Transaction.o: /usr/X11/include/X11/X11/Composite.h
Transaction.o: /usr/X11/include/X11/X11/Constraint.h
Transaction.o: /usr/X11/include/X11/X11/Object.h
Transaction.o: /usr/X11/include/X11/X11/RectObj.h
Transaction.o: /usr/X11/include/X11/Shell.h
Transaction.o: /usr/X11/include/X11/X11/SM/SMlib.h
Transaction.o: /usr/X11/include/X11/SM/SM.h /usr/X11/include/X11/ICE/ICElib.h
Transaction.o: /usr/X11/include/X11/ICE/ICE.h /usr/X11/include/X11/Xatom.h
Transaction.o: /usr/X11/include/Xm/XmStrDefs.h
Transaction.o: /usr/X11/include/X11/StringDefs.h
Transaction.o: /usr/X11/include/Xm/VirtKeys.h /usr/X11/include/Xm/Transfer.h
Transaction.o: /usr/X11/include/Xm/DragDrop.h /usr/X11/include/Xm/DragC.h
Transaction.o: /usr/X11/include/Xm/Display.h /usr/X11/include/Xm/DropSMgr.h
Transaction.o: /usr/X11/include/Xm/DragIcon.h /usr/X11/include/Xm/DropTrans.h
Transaction.o: /usr/X11/include/Xm/DragOverS.h
Transaction.o: /usr/X11/include/Xm/Primitive.h /usr/X11/include/Xm/Manager.h
Transaction.o: /usr/X11/include/Xm/Gadget.h /usr/X11/include/Xm/TxtPropCv.h
Transaction.o: /usr/X11/include/Xm/VendorS.h /usr/X11/include/Xm/XmIm.h
Transaction.o: ../include/date.h ../include/util.h /usr/include/stdio.h
Transaction.o: /usr/include/libio.h /usr/include/_G_config.h
Transaction.o: /usr/include/errno.h /usr/include/linux/errno.h
Transaction.o: /usr/include/asm/errno.h
