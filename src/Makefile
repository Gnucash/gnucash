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

OBJS = Account.o AccountMenu.o AccWindow.o Action.o AdjBWindow.o \
       BuildMenu.o Data.o date.o \
       FileBox.o FileIO.o HelpWindow.o main.o MainWindow.o PopBox.o \
       QIFIO.o QuickFill.o RecnWindow.o RegWindow.o Reports.o \
       Transaction.o util.o XferBox.o XferWindow.o

SRCS = ${OBJS:.o=.c}

TARGET = ../xacc

default: $(TARGET)

$(TARGET): $(OBJS) ../lib/libhtmlw.a ../lib/libComboBox.a ../Xbae-4.6.2-linas/src/libXbae.a
	@echo "++++++"
	$(CC) $(OBJS) $(LFLAGS) -lhtmlw -lComboBox \
        ../Xbae-4.6.2-linas/src/libXbae.a $(LIBS) -o $@

.c.o: Makefile
	@echo "+++"
	$(CC) $(CFLAGS) -c $<

depend:
	makedepend -- $(CFLAGS) -- $(SRCS)

clean:
	rm -f tmp junk glop core *.o *~ *.bak

really_clean:
	makedepend -- --

# DO NOT DELETE THIS LINE -- make  depend  depends  on it.

Account.o: ../include/Account.h ../include/QuickFill.h
Account.o: ../include/Transaction.h /usr/include/Xm/Xm.h
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
Account.o: /usr/include/Xm/XmIm.h ../include/date.h ../include/Data.h
Account.o: ../include/main.h ../include/util.h /usr/include/stdio.h
Account.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/errno.h
Account.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
AccWindow.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
AccWindow.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
AccWindow.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
AccWindow.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
AccWindow.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
AccWindow.o: /usr/include/X11/X11/X11/Xfuncproto.h
AccWindow.o: /usr/include/X11/X11/X11/Xosdefs.h
AccWindow.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
AccWindow.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
AccWindow.o: /usr/include/string.h /usr/include/features.h
AccWindow.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
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
AccWindow.o: /usr/include/_G_config.h ../include/Account.h
AccWindow.o: ../include/QuickFill.h ../include/Transaction.h
AccWindow.o: ../include/date.h ../include/Data.h ../include/main.h
AccWindow.o: ../include/util.h /usr/include/errno.h
AccWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
Action.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
Action.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
Action.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
Action.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Action.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
Action.o: /usr/include/X11/X11/X11/Xfuncproto.h
Action.o: /usr/include/X11/X11/X11/Xosdefs.h
Action.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Action.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Action.o: /usr/include/string.h /usr/include/features.h
Action.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
Action.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
Action.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Action.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Action.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Action.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Action.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Action.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Action.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Action.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Action.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Action.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Action.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Action.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Action.o: /usr/include/Xm/XmIm.h ../include/Action.h ../include/PopBox.h
Action.o: ../include/Data.h ../include/Account.h ../include/QuickFill.h
Action.o: ../include/Transaction.h ../include/date.h ../include/util.h
Action.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
Action.o: /usr/include/errno.h /usr/include/linux/errno.h
Action.o: /usr/include/asm/errno.h
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
AdjBWindow.o: ../include/main.h ../include/date.h ../include/Transaction.h
AdjBWindow.o: ../include/util.h /usr/include/errno.h
AdjBWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
AdjBWindow.o: ../include/Data.h ../include/Account.h ../include/QuickFill.h
AdjBWindow.o: ../include/MainWindow.h
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
BuildMenu.o: /usr/include/Xm/CascadeBG.h /usr/include/Xm/Label.h
BuildMenu.o: /usr/include/Xm/LabelG.h /usr/include/Xm/Scale.h
BuildMenu.o: /usr/include/Xm/Text.h /usr/include/stdio.h /usr/include/libio.h
BuildMenu.o: /usr/include/_G_config.h /usr/include/Xm/TextF.h
BuildMenu.o: /usr/include/Xm/SeparatoG.h ../include/BuildMenu.h
BuildMenu.o: /usr/include/Xm/Separator.h
Data.o: ../include/Account.h ../include/QuickFill.h ../include/Transaction.h
Data.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
Data.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
Data.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
Data.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Data.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
Data.o: /usr/include/X11/X11/X11/Xfuncproto.h
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
Data.o: /usr/include/Xm/XmIm.h ../include/date.h ../include/Data.h
Data.o: ../include/util.h /usr/include/stdio.h /usr/include/libio.h
Data.o: /usr/include/_G_config.h /usr/include/errno.h
Data.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
date.o: /usr/include/time.h /usr/include/features.h /usr/include/sys/cdefs.h
date.o: /usr/include/sys/time.h /usr/include/linux/types.h
date.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
date.o: /usr/include/asm/types.h /usr/include/linux/time.h
date.o: /usr/include/sys/types.h /usr/include/sys/bitypes.h ../include/date.h
date.o: ../include/util.h /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
date.o: /usr/include/X11/X11/Xlib.h /usr/include/X11/X11/X11/X.h
date.o: /usr/include/X11/X11/X11/Xfuncproto.h
date.o: /usr/include/X11/X11/X11/Xosdefs.h
date.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
date.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
date.o: /usr/include/string.h /usr/include/X11/X11/Core.h
date.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
date.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
date.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
date.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
date.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
date.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
date.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
date.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
date.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
date.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
date.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
date.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
date.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
date.o: /usr/include/Xm/XmIm.h /usr/include/stdio.h /usr/include/libio.h
date.o: /usr/include/_G_config.h /usr/include/errno.h
date.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
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
FileBox.o: /usr/include/Xm/XmIm.h /usr/include/Xm/FileSB.h
FileBox.o: ../include/FileBox.h ../include/util.h /usr/include/stdio.h
FileBox.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/errno.h
FileBox.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileIO.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
FileIO.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
FileIO.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
FileIO.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
FileIO.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
FileIO.o: /usr/include/X11/X11/X11/Xfuncproto.h
FileIO.o: /usr/include/X11/X11/X11/Xosdefs.h
FileIO.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
FileIO.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
FileIO.o: /usr/include/string.h /usr/include/features.h
FileIO.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
FileIO.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
FileIO.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
FileIO.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
FileIO.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
FileIO.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
FileIO.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
FileIO.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
FileIO.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
FileIO.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
FileIO.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
FileIO.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
FileIO.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
FileIO.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
FileIO.o: /usr/include/Xm/XmIm.h /usr/include/fcntl.h
FileIO.o: /usr/include/gnu/types.h /usr/include/linux/fcntl.h
FileIO.o: /usr/include/asm/fcntl.h ../include/main.h ../include/date.h
FileIO.o: ../include/Transaction.h ../include/util.h /usr/include/stdio.h
FileIO.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/errno.h
FileIO.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
FileIO.o: ../include/Account.h ../include/QuickFill.h ../include/Data.h
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
HelpWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/PushB.h
HelpWindow.o: /usr/include/X11/xpm.h ../include/main.h ../include/date.h
HelpWindow.o: ../include/Transaction.h ../include/util.h /usr/include/stdio.h
HelpWindow.o: /usr/include/libio.h /usr/include/_G_config.h
HelpWindow.o: /usr/include/errno.h /usr/include/linux/errno.h
HelpWindow.o: /usr/include/asm/errno.h ../libhtmlw/HTML.h
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
main.o: /usr/include/Xm/XmIm.h ../include/BuildMenu.h /usr/include/Xm/PushB.h
main.o: /usr/include/Xm/Separator.h ../include/RegWindow.h
main.o: ../include/Account.h ../include/QuickFill.h ../include/Transaction.h
main.o: ../include/date.h ../include/FileIO.h ../include/Data.h
main.o: ../include/FileBox.h ../include/util.h /usr/include/stdio.h
main.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/errno.h
main.o: /usr/include/linux/errno.h /usr/include/asm/errno.h ../include/main.h
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
MainWindow.o: /usr/include/Xm/PanedW.h /usr/include/Xm/Form.h
MainWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/MainW.h
MainWindow.o: /usr/include/Xm/Label.h /usr/include/Xm/LabelGP.h
MainWindow.o: /usr/include/Xm/LabelG.h /usr/include/Xm/GadgetP.h
MainWindow.o: /usr/include/Xm/XmP.h /usr/include/X11/IntrinsicP.h
MainWindow.o: /usr/include/X11/X11/CoreP.h /usr/include/X11/X11/CompositeP.h
MainWindow.o: /usr/include/X11/X11/ConstrainP.h
MainWindow.o: /usr/include/X11/X11/ObjectP.h /usr/include/X11/X11/RectObjP.h
MainWindow.o: /usr/include/Xm/ColorP.h /usr/include/Xm/AccColorT.h
MainWindow.o: /usr/include/Xm/BaseClassP.h /usr/include/Xm/ExtObjectP.h
MainWindow.o: /usr/include/Xm/List.h /usr/include/Xm/RowColumn.h
MainWindow.o: ../Xbae-4.6.2-linas/Xbae/Matrix.h
MainWindow.o: ../Xbae-4.6.2-linas/Xbae/Xbae/patchlevel.h ../include/main.h
MainWindow.o: ../include/date.h ../include/Transaction.h ../include/util.h
MainWindow.o: /usr/include/stdio.h /usr/include/libio.h
MainWindow.o: /usr/include/_G_config.h /usr/include/errno.h
MainWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
MainWindow.o: ../include/Data.h ../include/Account.h ../include/QuickFill.h
MainWindow.o: ../include/FileIO.h ../include/FileBox.h ../include/BuildMenu.h
MainWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/Separator.h
MainWindow.o: ../include/MainWindow.h ../include/RegWindow.h
MainWindow.o: ../include/XferWindow.h ../include/HelpWindow.h
PopBox.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
PopBox.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
PopBox.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
PopBox.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
PopBox.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
PopBox.o: /usr/include/X11/X11/X11/Xfuncproto.h
PopBox.o: /usr/include/X11/X11/X11/Xosdefs.h
PopBox.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
PopBox.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
PopBox.o: /usr/include/string.h /usr/include/features.h
PopBox.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
PopBox.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
PopBox.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
PopBox.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
PopBox.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
PopBox.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
PopBox.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
PopBox.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
PopBox.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
PopBox.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
PopBox.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
PopBox.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
PopBox.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
PopBox.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
PopBox.o: /usr/include/Xm/XmIm.h ../ComboBox-1.33/ComboBox.h
PopBox.o: ../Xbae-4.6.2-linas/Xbae/Matrix.h
PopBox.o: ../Xbae-4.6.2-linas/Xbae/Xbae/patchlevel.h ../include/util.h
PopBox.o: /usr/include/stdio.h /usr/include/libio.h /usr/include/_G_config.h
PopBox.o: /usr/include/errno.h /usr/include/linux/errno.h
PopBox.o: /usr/include/asm/errno.h
QIFIO.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
QIFIO.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
QIFIO.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
QIFIO.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
QIFIO.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
QIFIO.o: /usr/include/X11/X11/X11/Xfuncproto.h
QIFIO.o: /usr/include/X11/X11/X11/Xosdefs.h
QIFIO.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
QIFIO.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
QIFIO.o: /usr/include/string.h /usr/include/features.h
QIFIO.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
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
QIFIO.o: /usr/include/Xm/XmIm.h /usr/include/fcntl.h /usr/include/gnu/types.h
QIFIO.o: /usr/include/linux/fcntl.h /usr/include/asm/fcntl.h
QIFIO.o: ../include/main.h ../include/date.h ../include/Transaction.h
QIFIO.o: ../include/util.h /usr/include/stdio.h /usr/include/libio.h
QIFIO.o: /usr/include/_G_config.h /usr/include/errno.h
QIFIO.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
QIFIO.o: ../include/Account.h ../include/QuickFill.h ../include/Data.h
QuickFill.o: /usr/include/string.h /usr/include/features.h
QuickFill.o: /usr/include/sys/cdefs.h
QuickFill.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
QuickFill.o: ../include/util.h /usr/include/Xm/Xm.h
QuickFill.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
QuickFill.o: /usr/include/sys/types.h /usr/include/linux/types.h
QuickFill.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
QuickFill.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
QuickFill.o: /usr/include/X11/X11/X11/X.h
QuickFill.o: /usr/include/X11/X11/X11/Xfuncproto.h
QuickFill.o: /usr/include/X11/X11/X11/Xosdefs.h /usr/include/X11/X11/Xutil.h
QuickFill.o: /usr/include/X11/X11/Xresource.h /usr/include/X11/X11/Core.h
QuickFill.o: /usr/include/X11/X11/Composite.h
QuickFill.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
QuickFill.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
QuickFill.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
QuickFill.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
QuickFill.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
QuickFill.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
QuickFill.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
QuickFill.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
QuickFill.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
QuickFill.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
QuickFill.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
QuickFill.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
QuickFill.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
QuickFill.o: /usr/include/stdio.h /usr/include/libio.h
QuickFill.o: /usr/include/_G_config.h /usr/include/errno.h
QuickFill.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
QuickFill.o: ../include/main.h ../include/date.h ../include/Transaction.h
QuickFill.o: ../include/Account.h ../include/QuickFill.h
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
RecnWindow.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/Form.h
RecnWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/Text.h
RecnWindow.o: /usr/include/stdio.h /usr/include/libio.h
RecnWindow.o: /usr/include/_G_config.h /usr/include/Xm/DialogS.h
RecnWindow.o: /usr/include/Xm/PanedW.h /usr/include/Xm/Frame.h
RecnWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/LabelGP.h
RecnWindow.o: /usr/include/Xm/LabelG.h /usr/include/Xm/GadgetP.h
RecnWindow.o: /usr/include/Xm/XmP.h /usr/include/X11/IntrinsicP.h
RecnWindow.o: /usr/include/X11/X11/CoreP.h /usr/include/X11/X11/CompositeP.h
RecnWindow.o: /usr/include/X11/X11/ConstrainP.h
RecnWindow.o: /usr/include/X11/X11/ObjectP.h /usr/include/X11/X11/RectObjP.h
RecnWindow.o: /usr/include/Xm/ColorP.h /usr/include/Xm/AccColorT.h
RecnWindow.o: /usr/include/Xm/BaseClassP.h /usr/include/Xm/ExtObjectP.h
RecnWindow.o: ../Xbae-4.6.2-linas/Xbae/Matrix.h
RecnWindow.o: ../Xbae-4.6.2-linas/Xbae/Xbae/patchlevel.h ../include/Data.h
RecnWindow.o: ../include/Account.h ../include/QuickFill.h
RecnWindow.o: ../include/Transaction.h ../include/date.h
RecnWindow.o: ../include/RegWindow.h ../include/MainWindow.h
RecnWindow.o: ../include/main.h ../include/util.h /usr/include/errno.h
RecnWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
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
RegWindow.o: /usr/include/_G_config.h ../Xbae-4.6.2-linas/Xbae/Matrix.h
RegWindow.o: ../Xbae-4.6.2-linas/Xbae/Xbae/patchlevel.h ../include/Account.h
RegWindow.o: ../include/QuickFill.h ../include/Transaction.h
RegWindow.o: ../include/date.h ../include/Action.h ../include/AdjBWindow.h
RegWindow.o: ../include/BuildMenu.h /usr/include/Xm/Separator.h
RegWindow.o: ../include/Data.h ../include/main.h ../include/MainWindow.h
RegWindow.o: ../include/PopBox.h ../include/RecnWindow.h ../include/util.h
RegWindow.o: /usr/include/errno.h /usr/include/linux/errno.h
RegWindow.o: /usr/include/asm/errno.h
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
Reports.o: /usr/include/Xm/ExtObjectP.h ../include/Reports.h
Reports.o: ../include/util.h /usr/include/errno.h /usr/include/linux/errno.h
Reports.o: /usr/include/asm/errno.h
Transaction.o: ../include/Transaction.h /usr/include/Xm/Xm.h
Transaction.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xlib.h
Transaction.o: /usr/include/sys/types.h /usr/include/linux/types.h
Transaction.o: /usr/include/linux/posix_types.h
Transaction.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
Transaction.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
Transaction.o: /usr/include/X11/X11/X11/Xfuncproto.h
Transaction.o: /usr/include/X11/X11/X11/Xosdefs.h
Transaction.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
Transaction.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
Transaction.o: /usr/include/string.h /usr/include/features.h
Transaction.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
Transaction.o: /usr/include/X11/X11/Composite.h
Transaction.o: /usr/include/X11/X11/Constraint.h
Transaction.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
Transaction.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
Transaction.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
Transaction.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
Transaction.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
Transaction.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
Transaction.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
Transaction.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
Transaction.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
Transaction.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
Transaction.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
Transaction.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
Transaction.o: /usr/include/Xm/XmIm.h ../include/date.h ../include/util.h
Transaction.o: /usr/include/stdio.h /usr/include/libio.h
Transaction.o: /usr/include/_G_config.h /usr/include/errno.h
Transaction.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
util.o: /usr/include/X11/X11/X11/X.h /usr/include/X11/X11/Xlib.h
util.o: /usr/include/sys/types.h /usr/include/linux/types.h
util.o: /usr/include/linux/posix_types.h /usr/include/asm/posix_types.h
util.o: /usr/include/asm/types.h /usr/include/sys/bitypes.h
util.o: /usr/include/X11/X11/X11/Xfuncproto.h
util.o: /usr/include/X11/X11/X11/Xosdefs.h
util.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
util.o: /usr/include/X11/cursorfont.h /usr/include/Xm/Xm.h
util.o: /usr/include/X11/Intrinsic.h /usr/include/X11/X11/Xutil.h
util.o: /usr/include/X11/X11/Xresource.h /usr/include/string.h
util.o: /usr/include/features.h /usr/include/sys/cdefs.h
util.o: /usr/include/X11/X11/Core.h /usr/include/X11/X11/Composite.h
util.o: /usr/include/X11/X11/Constraint.h /usr/include/X11/X11/Object.h
util.o: /usr/include/X11/X11/RectObj.h /usr/include/X11/Shell.h
util.o: /usr/include/X11/X11/SM/SMlib.h /usr/include/X11/SM/SM.h
util.o: /usr/include/X11/ICE/ICElib.h /usr/include/X11/ICE/ICE.h
util.o: /usr/include/X11/Xatom.h /usr/include/Xm/XmStrDefs.h
util.o: /usr/include/X11/StringDefs.h /usr/include/Xm/VirtKeys.h
util.o: /usr/include/Xm/Transfer.h /usr/include/Xm/DragDrop.h
util.o: /usr/include/Xm/DragC.h /usr/include/Xm/Display.h
util.o: /usr/include/Xm/DropSMgr.h /usr/include/Xm/DragIcon.h
util.o: /usr/include/Xm/DropTrans.h /usr/include/Xm/DragOverS.h
util.o: /usr/include/Xm/Primitive.h /usr/include/Xm/Manager.h
util.o: /usr/include/Xm/Gadget.h /usr/include/Xm/TxtPropCv.h
util.o: /usr/include/Xm/VendorS.h /usr/include/Xm/XmIm.h
util.o: /usr/include/Xm/Text.h /usr/include/stdio.h /usr/include/libio.h
util.o: /usr/include/_G_config.h /usr/include/Xm/PanedW.h
util.o: /usr/include/Xm/Form.h /usr/include/Xm/BulletinB.h
util.o: /usr/include/Xm/PushB.h /usr/include/Xm/DialogS.h
util.o: /usr/include/Xm/RowColumn.h /usr/include/Xm/MessageB.h
util.o: ../Xbae-4.6.2-linas/Xbae/Matrix.h
util.o: ../Xbae-4.6.2-linas/Xbae/Xbae/patchlevel.h ../include/util.h
util.o: /usr/include/errno.h /usr/include/linux/errno.h
util.o: /usr/include/asm/errno.h
XferBox.o: /usr/include/Xm/Xm.h /usr/include/X11/Intrinsic.h
XferBox.o: /usr/include/X11/X11/Xlib.h /usr/include/sys/types.h
XferBox.o: /usr/include/linux/types.h /usr/include/linux/posix_types.h
XferBox.o: /usr/include/asm/posix_types.h /usr/include/asm/types.h
XferBox.o: /usr/include/sys/bitypes.h /usr/include/X11/X11/X11/X.h
XferBox.o: /usr/include/X11/X11/X11/Xfuncproto.h
XferBox.o: /usr/include/X11/X11/X11/Xosdefs.h
XferBox.o: /usr/lib/gcc-lib/i486-linux/2.7.2/include/stddef.h
XferBox.o: /usr/include/X11/X11/Xutil.h /usr/include/X11/X11/Xresource.h
XferBox.o: /usr/include/string.h /usr/include/features.h
XferBox.o: /usr/include/sys/cdefs.h /usr/include/X11/X11/Core.h
XferBox.o: /usr/include/X11/X11/Composite.h /usr/include/X11/X11/Constraint.h
XferBox.o: /usr/include/X11/X11/Object.h /usr/include/X11/X11/RectObj.h
XferBox.o: /usr/include/X11/Shell.h /usr/include/X11/X11/SM/SMlib.h
XferBox.o: /usr/include/X11/SM/SM.h /usr/include/X11/ICE/ICElib.h
XferBox.o: /usr/include/X11/ICE/ICE.h /usr/include/X11/Xatom.h
XferBox.o: /usr/include/Xm/XmStrDefs.h /usr/include/X11/StringDefs.h
XferBox.o: /usr/include/Xm/VirtKeys.h /usr/include/Xm/Transfer.h
XferBox.o: /usr/include/Xm/DragDrop.h /usr/include/Xm/DragC.h
XferBox.o: /usr/include/Xm/Display.h /usr/include/Xm/DropSMgr.h
XferBox.o: /usr/include/Xm/DragIcon.h /usr/include/Xm/DropTrans.h
XferBox.o: /usr/include/Xm/DragOverS.h /usr/include/Xm/Primitive.h
XferBox.o: /usr/include/Xm/Manager.h /usr/include/Xm/Gadget.h
XferBox.o: /usr/include/Xm/TxtPropCv.h /usr/include/Xm/VendorS.h
XferBox.o: /usr/include/Xm/XmIm.h ../include/Data.h ../include/Account.h
XferBox.o: ../include/QuickFill.h ../include/Transaction.h ../include/date.h
XferBox.o: ../include/PopBox.h ../include/util.h /usr/include/stdio.h
XferBox.o: /usr/include/libio.h /usr/include/_G_config.h /usr/include/errno.h
XferBox.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
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
XferWindow.o: /usr/include/Xm/BulletinB.h /usr/include/Xm/LabelGP.h
XferWindow.o: /usr/include/Xm/LabelG.h /usr/include/Xm/GadgetP.h
XferWindow.o: /usr/include/Xm/XmP.h /usr/include/X11/IntrinsicP.h
XferWindow.o: /usr/include/X11/X11/CoreP.h /usr/include/X11/X11/CompositeP.h
XferWindow.o: /usr/include/X11/X11/ConstrainP.h
XferWindow.o: /usr/include/X11/X11/ObjectP.h /usr/include/X11/X11/RectObjP.h
XferWindow.o: /usr/include/Xm/ColorP.h /usr/include/Xm/AccColorT.h
XferWindow.o: /usr/include/Xm/BaseClassP.h /usr/include/Xm/ExtObjectP.h
XferWindow.o: /usr/include/Xm/PushB.h /usr/include/Xm/Text.h
XferWindow.o: /usr/include/stdio.h /usr/include/libio.h
XferWindow.o: /usr/include/_G_config.h ../include/Account.h
XferWindow.o: ../include/QuickFill.h ../include/Transaction.h
XferWindow.o: ../include/date.h ../include/BuildMenu.h
XferWindow.o: /usr/include/Xm/Separator.h ../include/Data.h ../include/main.h
XferWindow.o: ../include/util.h /usr/include/errno.h
XferWindow.o: /usr/include/linux/errno.h /usr/include/asm/errno.h
