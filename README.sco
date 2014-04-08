
I have built and tested Xacc on SCO OpenServer 5.0.4 and UnixWare 7.
To build on SCO OpenServer, run the following commands:

    # ./Configure-osr5
    # make depend
    # make
    # make install
    # cp Xacc /usr/lib/X11/app-defaults/Xacc

Ignore the make depend warnings for now.

The build environment for OpenServer included the GNU C Compiler 2.7.2.1
from SCO Skunkware 97 and the Glib graphics libraries package from
SCO Skunkware 97. The runtime environment for the OpenServer Xacc includes
the Glib graphics libraries package from SCO Skunkware 97.

You can retrieve binary distributions of the GNU C Compiler, Glib, and
Xacc from http://www.sco.com/skunkware/

To build Xacc on UnixWare 7, run the following commands:

    # cp configure configure-orig
    # cp configure-uw7 configure
    # ./Configure-uw7
    # make depend
    # make
    # make install
    # cp Xacc /usr/X/lib/X11/app-defaults/Xacc

The build environment for UnixWare 7 included the GNU C Compiler 2.8.0
from SCO Skunkware 7 and the glib graphics libraries package from
SCO Skunkware 7. The runtime environment for the UnixWare 7 Xacc includes
the glib graphics libraries package from SCO Skunkware 7.

You can retrieve binary distributions of the GNU C Compiler, glib, and
Xacc from http://www.sco.com/skunkware/uw7/

Ron Record
rr@sco.com
12-Feb-98


