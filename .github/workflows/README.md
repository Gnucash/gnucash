Github Actions Continuous Integration Testing Workflows.

ci-tests.yml runs Ubuntu 18.04 and Ubuntu 20.04 using Github-provided virtual environments. The virtual environments are about twice as fast as Docker ones and getting Docker to run Ubuntu images was problematic. Note that updating Ubuntu 18.04 updates ICU4C from v60 to v65 and that causes conflicts with Boost, so don't.

ci-docker.yml runs tests in a Docker container running Arch Linux.

mac-tests.yml runs tests on macOS using a Github-provided virtual machine. Note that this test relies on a prebuilt tarball containing all of the dependencies. Instructions for building that tarball along with some support scripts may be found in utils/ci/macos-ci-deps.
