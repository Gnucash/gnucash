Using CMake to build GnuCash
============================

== Intro

As part of his CuteCash work, Christian Stimming added support for
using http://www.cmake.org[CMake] to build the part of GnuCash that
went into CuteCash. This work extends the use of CMake to cover all of
GnuCash proper such that GnuCash itself can be fully compiled with
CMake.

Some advantages of using CMake:

 * The build time on Windows drops from around an hour to just a few
   minutes.
   
 * CMake supports the generation of build files for several IDEs
   such as Xcode, Eclipse, KDevelop and others. The open source Qt
   Creator and the commercial CLion C/C++ IDE from JetBrains can use
   CMake files natively.

 * The CMake language, though with significant issues, is still
   easier to maintain than autotools.

This setup also uses the http://www.ninja-build.org[Ninja] build
system to enable fast and parallel builds on Windows. (On POSIX
systems [OS X, Linux, FreeBSD, etc.] Ninja does not seem significantly
faster that using the default Makefile generator to me.)

== Scope

The scope of the current work is to duplicate the `make`,
`make check` and `make install` actions as the Autotools system would.
Currently, there is no support for `make dist`, `make distcheck` or
similar targets. Other limitations include:

  * Not all options available in `./configure` have been ported to
    this CMake system.

  * Password management is included for OS X, but not
    tested. Gnome-keyring and libsecret support has not
    been ported over.

  * The Xcode build only supports the Debug configuration. Others such
    as Release are not supported yet.

  * Python support has not been ported over.

  * Visual Studio support is out of scope. While CMake supports
    generating build files for Visual Studio on Windows, it is not
    likely at this point that either GnuCash or all of its
    dependencies can be built using the Microsoft compiler tool chain.

== Known Issues

* Sometimes the Tip of the Day is empty. I've got a stray
  carriage return somewhere.

* Clicking on 'Start AqBanking Wizard' in the AqBanking setup will
  cause a crash. I have not yet investigated this.

== Using CMake on Linux, OS X,  etc.

=== Prerequisites

The CMake setup does not support building and installing dependencies
(although it probably could some day). So you need to have the
dependencies available, most likely by having run the existing
Autotools build at least once. Various resources on the GnuCash wiki
will have advice on how to do this.

You will need to have CMake and optionally Ninja installed, either
from distro package repositories or by hand. You need at least version
3.1 of CMake.

=== Running CMake

The next step is to invoke CMake to generate the build system. Before
running CMake, you need to create a build directory:

  $ cd ..   # back to workdir
  $ mkdir gnucash-build
  $ cd gnucash-build

Then decide what cmake command line options you will need:

 * If you want to install after building, add
   `-D CMAKE_INSTALL_PREFIX=/path/to/install`

 * If your dependencies are installed in a non-standard place as is
   typical for building on OS X, put
   `-D CMAKE_PREFIX_PATH=/path/to/installed/depends`
   on the command line.

 * If you want to use the Ninja generator, put `-G Ninja` on the
   command line.

 * If you want to use the Xcode generator on OS X, put `-G Xcode` on
   the command line.

 * If you don't specify a generator, Makefiles will be generated.

 * Finally, put the path to your source directory last.
   Here, that is ../gnucash

 * There are other options available; look in the `OPTIONS` section of
   the top-level `CMakeLists.txt` file.  For example, you can disable
   SQL using these options.

 * Google Test will work without setting options in most Linux distros, but if
   it doesn't you may need to set GMOCK_ROOT or GTEST_ROOT to the root of the
   respective sources. Set GTEST_DISABLE to prevent Google Test based tests from
   running.

Some examples:

 * Build on Linux, don't want to install, use the Makefile generator:

   $ cmake ../gnucash

 * Build on Linux, install to /tmp/gnucash, use Ninja generator:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -G Ninja ../gnucash

 * Build on OS X, install to /tmp/gnucash, use Ninja generator:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -D CMAKE_PREFIX_PATH=$HOME/gnucash-unstable -G Ninja ../gnucash

 * The same, but use the Xcode generator:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -D CMAKE_PREFIX_PATH=$HOME/gnucash-unstable -G Xcode ../gnucash

  * Again, this time pointing to a gmock-1.7.0 source directory:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -D
   CMAKE_PREFIX_PATH=$HOME/gnucash-unstable -D GMOCK_ROOT=$HOME/gmock-1.7.0 -D GTEST_ROOT=$HOME/gmock-1.7.0/gtest -G Xcode ../gnucash

=== Building

The Xcode, Ninja and Makefile generators all support parallel builds,
so decide how many cores you want to use.  Ninja will pick a sensible
default.

If you chose to configure for installation, you can use the `install`
target for each generator. The Makefile and Ninja generators also
support a verbose option if you want to see all of the command lines
scroll by. Xcodebuild seems to show all the gory details whether you
want them or not.

For Ninja, use the line below. Note that the executable is called
`ninja-build` on Fedora. Also, Ninja supports the `NINJA_STATUS`
environment variable to give status on the build. I like to use
`NINJA_STATUS="%es [%p/%s/%t] "`.

   $ ninja [-v] [install]

For Makefiles:

   $ make [VERBOSE=1] -j N [install]

For Xcode via the command line (see below to build from within Xcode):

   $ xcodebuild -jobs N [-target=install]

=== Running tests

To run the Gnucash tests (also called checks), use the `check` target.
For ninja, use:

   $ ninja check

For Makefiles:

   $ make check

For Xcode via the command line:

   $ xcodebuild -jobs N -target=check

A test summary will appear in the terminal. Full logs are available
in at Testing/Temporary/LastTest.log in the build directory.

=== Launching GnuCash

Assuming the build completes successfully, in all cases you can run
directly from the build directory:

   $ bin/gnucash

In you chose to install, you can switch to the install directory and
do the same.


== Using CMake and Ninja on Windows


For Windows, follow the instructions at
https://github.com/Gnucash/gnucash-on-windows to the point where you
are ready to run install.sh.

Edit custom.sh to add these lines at the bottom:

  WITH_CMAKE=yes
  WITH_NINJA=yes

Ensure that your custom.sh file contains this line:

  MSYS_DIR=c:\\gcdev\\mingw\\msys\\1.0

Remove or move any existing install at /c/gcdev/gnucash/inst.

Then continue to follow the existing build instructions.

At this writing, generating a distribution with the CMake build
via dist.sh has not been tested.

== Using Xcode on OS X

CMake can generate build files for Xcode such that GnuCash can be
built, run and debugged from within Xcode.  Follow the instructions
above to the point where you would use `xcodebuild` to launch the
build. Instead, launch Xcode by doing:

  $ open Gnucash.xcodeproj

Xcode 7 will pop up a window about Autocreate Schemes. I usually
choose "Manually Manage Schemes". On the next window, use the "\+"
symbol to use the "ALL_BUILD" scheme. Then click on OK. If you want to
be able to run the equivalent of "make install" from within Xcode, use
the "+" symbol again and choose the "install" scheme.

Back in the Xcode main window, make sure the "ALL_BUILD" scheme is
selected (next to the stop symbol).  Click on "ALL_BUILD" and
then "Edit Scheme". On the "Info" tab, choose the executable to be
"gnucash".

Now use Command-B to build (or Product -> Build) to start the
build. When it finishes, click on the play symbol (or Product
-> Run) to verify that you can launch GnuCash from within Xcode. If
that works, you can now set breakpoints with Xcode and debug away.

To run the install script, click on the "ALL_BUILD" scheme and change
it to "install". Then press the play button to run the script.

That's it.
