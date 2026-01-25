#!/usr/bin/env bash
# Usage:
#   contrib/dev/gnc-py-env.sh /path/to/gnucash/build -- python3 -c 'import gnucash; print("OK")'
#   source contrib/dev/gnc-py-env.sh /path/to/gnucash/build

set -u

_build_dir="${1:-}"
if [[ -z "${_build_dir}" ]]; then
  echo "ERROR: missing build dir."
  echo "Example: $0 ~/projects/gnucash/build -- python3 -c 'import gnucash'"
  return 2 2>/dev/null || exit 2
fi
shift || true
_build_dir="$(cd "${_build_dir}" && pwd)"

# Prepend DIR to VAR if not already present; never create empty entries or trailing ':'
path_prepend_unique() {
  local var="$1"
  local dir="$2"
  local cur="${!var-}"
  [[ -z "$dir" || ! -d "$dir" ]] && return 0

  # Already present?
  case ":$cur:" in
    *":$dir:"*) return 0 ;;
  esac

  if [[ -n "$cur" ]]; then
    export "$var"="$dir:$cur"
  else
    export "$var"="$dir"
  fi
}

# 1) Python bindings (build tree site-packages)
_py_site=""
for d in "${_build_dir}"/lib/python*/site-packages; do
  if [[ -d "$d" ]]; then _py_site="$d"; break; fi
done
if [[ -z "${_py_site}" ]]; then
  echo "ERROR: could not find site-packages under: ${_build_dir}/lib/python*/site-packages"
  return 2 2>/dev/null || exit 2
fi
path_prepend_unique PYTHONPATH "${_py_site}"

# 2) GnuCash modules + shared libs
path_prepend_unique GNC_MODULE_PATH "${_build_dir}/lib/gnucash"
path_prepend_unique LD_LIBRARY_PATH "${_build_dir}/lib/gnucash"
path_prepend_unique LD_LIBRARY_PATH "${_build_dir}/lib"

# 3) Shared data
path_prepend_unique XDG_DATA_DIRS "${_build_dir}/share"

# Hints for "uninstalled" builds
export GNC_UNINSTALLED=1
export GNC_BUILDDIR="${_build_dir}"

# Execute a command if requested
if [[ "${1:-}" == "--" ]]; then
  shift
  exec "$@"
fi

echo "Environment set for build dir: ${_build_dir}"
echo "PYTHONPATH=${PYTHONPATH-}"
echo "GNC_MODULE_PATH=${GNC_MODULE_PATH-}"
echo "LD_LIBRARY_PATH=${LD_LIBRARY_PATH-}"
echo "XDG_DATA_DIRS=${XDG_DATA_DIRS-}"

