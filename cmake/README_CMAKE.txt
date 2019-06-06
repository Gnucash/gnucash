Using CMake to build GnuCash
============================

== Intro

As part of his CuteCash experiment (now removed from the repository),
Christian Stimming added support for using
https://www.cmake.org[CMake] to build the part of GnuCash that
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

This setup also uses the https://www.ninja-build.org[Ninja] build
system to enable fast and parallel builds on Windows. (On POSIX
systems [OS X, Linux, FreeBSD, etc.] Ninja does not seem significantly
faster than using the default Makefile generator to me.)

== Scope

The scope of the current work is to duplicate these actions as
the Autotools system would:

   * make
   * make check
   * make install
   * make uninstall
   * make dist
   * make distcheck

Limitations include:

  * Not all options available in `./configure` have been ported to
    this CMake system.

  * Password management is included for OS X and libsecret, but not
    tested. Gnome-keyring support has not been ported over.

  * The Xcode build only supports the Debug configuration. Others such
    as Release are not supported yet.

  * Visual Studio support is out of scope. While CMake supports
    generating build files for Visual Studio on Windows, it is not
    likely at this point that either GnuCash or all of its
    dependencies can be built using the Microsoft compiler tool chain.

== Using CMake on Linux, OS X,  etc.

=== Prerequisites

The CMake setup does not support building and installing dependencies
(although it probably could some day). So you need to have the
dependencies available. Various resources on the GnuCash wiki
will have advice on how to do this.

You will need to have CMake and optionally Ninja installed, either
from distro package repositories or by hand. You need at least version
3.0 of CMake.

=== Running CMake

CMake works a lot better if you use a separate build directory, so the first
step is to make one. We recommend that you set the build directory outside of
the source tree, but it will work in the build tree if you want:

  $ cd # Change to your home directory
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

 * Finally, put the path to your source directory last. To avoid confusing
   yourself later with weird cmake or build errors, we suggest that you use the
   absolute path, for example if you cloned gnucash into your home directory,
   $HOME/gnucash. If you've cloned or untarred gnucash somewhere else you can
     $ export SRCROOT=/absolute/path/to/source/parent
     $ cmake <parameters> $SRCROOT/gnucash
   Getting into this habit will make sure that you always get the source
   directory right no matter where you put the build dir. (It's a good habit for
   `rm -rf`, too: `rm -rf *` can have very frustrating consequences if done from
   the wrong place!) Be especially careful if you decide to use an in-tree build
   directory and a relative path: It's easy to
     $ mkdir build && cd build
     $ cmake ../gnucash
   from the source directory. That will fail, because there's a "gnucash"
   subdirectory with its own CMakeLists.txt, one that doesn't know how to
   configure the build and can't see the the cmake modules that it needs to run.
   If you must do this, you want to invoke CMake like
     $ cmake ..
   to get the right CMakeLists.txt.
   
 * There are other options available; look in the `OPTIONS` section of
   the top-level `CMakeLists.txt` file.  For example, you can disable
   SQL using these options.

 * Google Test will work without setting options in most Linux distros, but if
   it doesn't you may need to set GMOCK_ROOT or GTEST_ROOT to the root of the
   respective sources.

Some examples:

 * Build on Linux, don't want to install, use the Makefile generator:

   $ cmake $SRCROOT/gnucash

 * Build on Linux, install to /tmp/gnucash, use Ninja generator:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -G Ninja $SRCROOT/gnucash

 * Build on OS X, install to /tmp/gnucash, use Ninja generator:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -D CMAKE_PREFIX_PATH=$HOME/gnucash-unstable -G Ninja $SRCROOT/gnucash

 * The same, but use the Xcode generator:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -D CMAKE_PREFIX_PATH=$HOME/gnucash-unstable -G Xcode $SRCROOT/gnucash

  * Again, this time pointing to a gmock-1.7.0 source directory:

   $ cmake -D CMAKE_INSTALL_PREFIX=/tmp/gnucash -D
   CMAKE_PREFIX_PATH=$HOME/gnucash-unstable -D GMOCK_ROOT=$SRCROOT/gmock-1.7.0 -D GTEST_ROOT=$SRCROOT/gmock-1.7.0/gtest -G Xcode $SRCROOT/gnucash

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

=== Building a Distribution Tarball

To create a distribution, use the 'dist' and 'distcheck' targets.
For ninja, use:

   $ ninja dist
   $ ninja distcheck

For Makefiles:

   $ make dist
   $ make distcheck

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
