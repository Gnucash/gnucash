#!/usr/bin/env python3
"""Exercise GnuCash's forwarded command line in an isolated Xvfb session."""

# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.

import argparse
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time


def run(command, *, env, cwd, timeout=90):
    return subprocess.run(command, env=env, cwd=cwd, text=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          timeout=timeout, check=False)


def wait_until(predicate, primary, log_path, seconds):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if primary.poll() is not None:
            raise AssertionError(
                f"GnuCash exited with {primary.returncode}:\n"
                f"{log_path.read_text(errors='replace')}")
        if predicate():
            return
        time.sleep(0.25)
    raise AssertionError(f"GnuCash did not reach the expected state:\n"
                         f"{log_path.read_text(errors='replace')}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--gnucash", type=Path, required=True)
    parser.add_argument("--build-root", type=Path, required=True)
    parser.add_argument("--sample-book", type=Path, required=True)
    parser.add_argument("--schema-dir", type=Path, required=True)
    parser.add_argument("--guile-lib-dir", type=Path, required=True)
    parser.add_argument("--guile-compiled-lib-dir", type=Path, required=True)
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix="gnucash-gapplication-") as temp:
        root = Path(temp)
        home = root / "home"
        invoker = root / "invoker"
        for directory in (home, invoker, root / "config", root / "data",
                          root / "cache", root / "gnc-config", root / "gnc-data"):
            directory.mkdir()
        book = invoker / "relative-book.gnucash"
        shutil.copyfile(args.sample_book, book)
        startup_book = invoker / "nofile-startup.gnucash"
        shutil.copyfile(args.sample_book, startup_book)
        concurrent_books = [invoker / f"concurrent-{index}.gnucash"
                            for index in (1, 2)]
        for concurrent_book in concurrent_books:
            shutil.copyfile(args.sample_book, concurrent_book)
        desktop_book = invoker / "desktop-open.gnucash"
        shutil.copyfile(args.sample_book, desktop_book)

        env = os.environ.copy()
        for name in ("GUILE_LOAD_PATH", "GUILE_LOAD_COMPILED_PATH",
                     "GUILE_AUTO_COMPILE", "GNC_MODULE_PATH"):
            env.pop(name, None)
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(root / "config"),
                   XDG_DATA_HOME=str(root / "data"),
                   XDG_CACHE_HOME=str(root / "cache"),
                   GNC_CONFIG_HOME=str(root / "gnc-config"),
                   GNC_DATA_HOME=str(root / "gnc-data"),
                   GNC_UNINSTALLED="YES", GNC_BUILDDIR=str(args.build_root),
                   GSETTINGS_BACKEND="keyfile",
                   GSETTINGS_SCHEMA_DIR=str(args.schema_dir),
                   GUILE_LIBS=str(args.guile_lib_dir),
                   GUILE_COMPILED_LIBS=str(args.guile_compiled_lib_dir),
                   LC_ALL="C")

        schema = "org.gnucash.GnuCash.dialogs.new-user"
        setting = run(["gsettings", "set", schema, "first-startup", "false"],
                      env=env, cwd=root)
        if setting.returncode:
            raise AssertionError(f"Could not disable the first-run dialog: "
                                 f"{setting.stderr}")

        def check_local_option(option, expected_output, *, option_env=env):
            result = run([str(args.gnucash), option], env=option_env,
                         cwd=invoker)
            if (result.returncode != 0 or
                    expected_output not in result.stdout or result.stderr):
                raise AssertionError(
                    f"{option} must complete in its own process: "
                    f"status={result.returncode}, stdout={result.stdout!r}, "
                    f"stderr={result.stderr!r}")

        no_display_env = env.copy()
        no_display_env.pop("DISPLAY", None)
        no_display_env.pop("WAYLAND_DISPLAY", None)
        for option, output in (("--version", "GnuCash"),
                               ("--help", "Application Options"),
                               ("--paths", "GnuCash Paths")):
            check_local_option(option, output, option_env=no_display_env)

        # --nofile suppresses only the history fallback, not an explicit book.
        startup_log_path = root / "startup.log"
        with startup_log_path.open("w", encoding="utf-8") as startup_log:
            startup = subprocess.Popen(
                [str(args.gnucash), "--nofile", startup_book.name],
                env=env, cwd=invoker, stdin=subprocess.DEVNULL,
                stdout=startup_log, stderr=subprocess.STDOUT)
        try:
            def startup_book_in_history():
                result = run(["gsettings", "get",
                              "org.gnucash.GnuCash.history", "file0"],
                             env=env, cwd=root, timeout=10)
                return (result.returncode == 0 and
                        str(startup_book) in result.stdout)

            wait_until(startup_book_in_history, startup, startup_log_path, 120)
        finally:
            startup.terminate()
            try:
                startup.wait(timeout=10)
            except subprocess.TimeoutExpired:
                startup.kill()
                startup.wait(timeout=10)

        log_path = root / "primary.log"
        with log_path.open("w", encoding="utf-8") as primary_log:
            primary = subprocess.Popen([str(args.gnucash), "--nofile"],
                                       env=env, cwd=root, stdin=subprocess.DEVNULL,
                                       stdout=primary_log,
                                       stderr=subprocess.STDOUT)
        try:
            def owns_name():
                result = run(["gdbus", "call", "--session", "--dest",
                              "org.freedesktop.DBus", "--object-path",
                              "/org/freedesktop/DBus", "--method",
                              "org.freedesktop.DBus.NameHasOwner",
                              "org.gnucash.GnuCash"], env=env, cwd=root,
                             timeout=10)
                return result.returncode == 0 and "true" in result.stdout

            wait_until(owns_name, primary, log_path, 120)

            for option, output in (("--version", "GnuCash"),
                                   ("--help", "Application Options"),
                                   ("--paths", "GnuCash Paths")):
                check_local_option(option, output)

            bad = run([str(args.gnucash), "--unsupported-gnucash-test-option"],
                      env=env, cwd=invoker)
            if (bad.returncode != 1 or
                    "--unsupported-gnucash-test-option" not in bad.stderr or
                    "A separate GnuCash instance" in bad.stderr):
                raise AssertionError(
                    f"Parser error must reach caller with status 1: "
                    f"status={bad.returncode}, stdout={bad.stdout!r}, "
                    f"stderr={bad.stderr!r}")

            opened = run([str(args.gnucash), book.name], env=env, cwd=invoker)
            if opened.returncode != 0:
                raise AssertionError(
                    f"Relative book invocation failed: status={opened.returncode}, "
                    f"stdout={opened.stdout!r}, stderr={opened.stderr!r}")

            def book_in_history():
                result = run(["gsettings", "get",
                              "org.gnucash.GnuCash.history", "file0"],
                             env=env, cwd=root, timeout=10)
                return result.returncode == 0 and str(book) in result.stdout

            wait_until(book_in_history, primary, log_path, 90)

            # The command-line handler acknowledges each request before the
            # asynchronous file open completes. Both requests must therefore
            # survive the session-transition queue, regardless of arrival order.
            secondaries = []
            try:
                for concurrent_book in concurrent_books:
                    secondaries.append(subprocess.Popen(
                        [str(args.gnucash), concurrent_book.name],
                        env=env, cwd=invoker, stdin=subprocess.DEVNULL,
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        text=True))
                for secondary in secondaries:
                    stdout, stderr = secondary.communicate(timeout=90)
                    if secondary.returncode != 0:
                        raise AssertionError(
                            f"Concurrent book invocation failed: "
                            f"status={secondary.returncode}, stdout={stdout!r}, "
                            f"stderr={stderr!r}")
            finally:
                for secondary in secondaries:
                    if secondary.poll() is None:
                        secondary.kill()
                        secondary.communicate()

            def both_books_in_history():
                entries = []
                for key in ("file0", "file1"):
                    result = run(["gsettings", "get",
                                  "org.gnucash.GnuCash.history", key],
                                 env=env, cwd=root, timeout=10)
                    if result.returncode != 0:
                        return False
                    entries.append(result.stdout)
                return all(any(str(book) in entry for entry in entries)
                           for book in concurrent_books)

            wait_until(both_books_in_history, primary, log_path, 90)

            # A desktop file-open request uses GApplication.Open rather than
            # the command-line signal used by a second executable invocation.
            desktop_open = run(
                ["gdbus", "call", "--session", "--dest", "org.gnucash.GnuCash",
                 "--object-path", "/org/gnucash/GnuCash", "--method",
                 "org.freedesktop.Application.Open", f"['{desktop_book.as_uri()}']",
                 "{}"], env=env, cwd=root)
            if desktop_open.returncode != 0:
                raise AssertionError(
                    f"Desktop open request failed: {desktop_open.stderr}")

            def desktop_book_in_history():
                result = run(["gsettings", "get",
                              "org.gnucash.GnuCash.history", "file0"],
                             env=env, cwd=root, timeout=10)
                return (result.returncode == 0 and
                        str(desktop_book) in result.stdout)

            wait_until(desktop_book_in_history, primary, log_path, 90)
        finally:
            primary.terminate()
            try:
                primary.wait(timeout=10)
            except subprocess.TimeoutExpired:
                primary.kill()
                primary.wait(timeout=10)


if __name__ == "__main__":
    main()
