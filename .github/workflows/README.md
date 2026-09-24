GitHub Actions Continuous Integration Testing Workflows

`ci-tests.yml` runs Ubuntu 26.04 build, distribution, and AddressSanitizer
tests. `coverage.yml` collects C++ coverage on the same platform. These jobs
exercise the Guile 2.2 configuration without optional AqBanking support.
The Ubuntu job also saves an import matcher preview from the GTK regression
test using synthetic transactions and the application stylesheets. This
artifact supports visual review without opening a user's financial data.

`linux-aqbanking-gtk4-preflight.yml` builds the pinned Gwenhywfar GTK4,
libchipcard, and AqBanking sources before running the GnuCash distribution
tests with AqBanking and Guile 3.0 enabled. This separate configuration is
needed until distribution packages provide the GTK4 Gwenhywfar backend.
It also runs the Gwenhywfar tests under a virtual display.

`ci-docker.yml` runs tests in an Arch Linux container.

`windows-tests.yml` uses the upstream UCRT64 package repository and runs the
full test suite with AqBanking. A validation fork can set the repository
variables `WINDOWS_DEPENDENCY_OVERLAY_NAME` and
`WINDOWS_DEPENDENCY_OVERLAY_URL` together to place a staging package
repository before the upstream repository. The URL must point to a release
whose package database matches the configured repository name. Use an
immutable release containing packages built from the reviewed recipes.

`mac-tests.yaml` uses the macOS 26 runner and a prebuilt dependency archive.
The archive is assembled and tested using the JHBuild procedure in
`gnucash-on-osx`; see `util/ci/macos-ci-deps/README.md` for the upstream
publication procedure. Validation forks can set `MACOS_DEPENDENCIES_URL`
and `MACOS_DEPENDENCIES_SHA256` together to test an immutable staging
archive with the same workflow. The downloaded archive must provide GTK4,
gtk4-macos, gwengui-gtk4, and AqBanking. These overrides do not replace the
upstream requirement to publish the new dependency packages/archive before
merging the migration and updating the default URL and checksum.
