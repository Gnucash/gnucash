#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 RESOURCE_DIRECTORY" >&2
    exit 2
fi

resource_directory=$1
resource_list=$(mktemp)
trap 'rm -f "$resource_list"' EXIT

if ! find "$resource_directory" -type f \( -name '*.ui' -o -name '*.glade' \) \
        -print0 > "$resource_list"; then
    echo "Failed to inventory GTK4 builder resources in $resource_directory" >&2
    exit 1
fi

if [[ ! -s "$resource_list" ]]; then
    echo "No GTK4 builder resources found in $resource_directory" >&2
    exit 1
fi

while IFS= read -r -d '' resource; do
    gtk4-builder-tool validate "$resource"
done < "$resource_list"
