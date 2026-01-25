# Developer Notes: Using GnuCash Python bindings from a build tree

This is intended for developers (not end users). If you installed GnuCash via apt/snap/flatpak,
prefer exporting data (CSV/QIF/etc.) for analytics workflows.

## Quick start

From the repo root:

```bash
contrib/dev/gnc-py-env.sh ~/projects/gnucash/build -- \
  python3 -c 'import gnucash; from gnucash import Session; print("OK: bindings load")'
