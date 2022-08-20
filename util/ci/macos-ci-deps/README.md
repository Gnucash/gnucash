For creating the macOS dependency tarball required by the Github CI tests.

Requirements:
* Administrator privs on your Mac.
* Project admin privs for https://sourceforge.net/projects/gnucash.
* You must be running the same or newer version of macOS as the Github runner.
* Xcode or command-line tools (install the latter with `sudo xcode-select -install`).
To build this tarball you must be running the same or newer macOS version as the Github VM.

Procedure:
* Set up a Gtk-OSX build environment following the [GnuCash Quartz Build instructions](https://wiki.gnucash.org/wiki/MacOS/Quartz).
* Retrieve [jhbuildrc-custom](https://github.com/gnucash/gnucash-on-osx/jhbuildrc-custom) and put it in ~/.config.
* Set `modules = _modules_deps` on the appropriate line of `jhbuildrc-custom`.
* If you are not running the same version of macOS as the Github actions virtual environment change the `setup_SDK()` line at the bottom of `jhbuildrc-custom` to `setup_SDK('10.15')`, substituting the VM's macOS version for '10.15'.
* Create the directory `/Users/runner/` and make yourself the owner:
  ```
  sudo mkdir /Users/runner
  sudo chown me /Users/runner
  mkdir /Users/runner/gnucash
  ```
  Substituting your own userid for 'me'.
* Run `util/ci/macos-ci-deps/make-macos-deps-tarball.sh gnucash-4.2-mac-dependencies.tar.xz` from the root of the source directory, changing the tarball's name if appropriate.
* Upload the resulting tarball to the Dependencies folder in the Gnucash files section on SourceForge.
* If necessary modify `.github/workflows/mac-tests.yaml to reflect the tarball's URI.