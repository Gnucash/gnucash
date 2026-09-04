#!/usr/bin/env python3

import argparse
from pathlib import Path
import polib
import subprocess
import logging

logger = logging.getLogger(__name__)


def parse_field(field_str: str) -> str | None:
    """Parse a single field from the glossary txt file.

    - "" -> None
    - '""' -> empty string
    - '"..."' -> unquoted string (unescape \")
    """
    field_str = field_str.strip()
    if field_str == "":
        return None
    if len(field_str) >= 2 and field_str[0] == '"' and field_str[-1] == '"':
        inner = field_str[1:-1]
        return inner.replace('\\"', '"')
    raise ValueError("The string should be quoted if not empty")


def format_field(value: str | None) -> str:
    """Format a single field for the glossary txt file.

    - None -> ""
    - "" -> '""'
    - "..." -> '"..."' with '"' escaped as '\\"'
    """
    if value is None:
        return ""
    escaped = value.replace('"', '\\"')
    return f'"{escaped}"'


def parse_txt_line(line: str) -> tuple[str | None, str | None, str | None] | None:
    """Parse a line into (ctxt, msgid, comment). Returns None for empty lines."""
    line = line.rstrip("\n\r")
    if not line:
        return None

    parts = line.split("\t")
    if len(parts) != 3:
        raise ValueError(f"Expected 3 tab-separated fields, got {len(parts)}")

    return (parse_field(parts[0]), parse_field(parts[1]), parse_field(parts[2]))


def format_txt_line(ctxt: str | None, msgid: str | None, comment: str | None):
    """Format a line from components."""
    return "\t".join([format_field(ctxt), format_field(msgid), format_field(comment)])


def process_txt_file(
    filepath: Path, old_ctxt: str | None, old_id: str, new_ctxt: str | None, new_id: str
):
    with open(filepath, "r", encoding="utf-8") as f:
        lines = f.readlines()

    new_lines: list[str] = []
    modified = 0;

    for line in lines:
        stripped = line.rstrip("\n\r")
        if not stripped:
            new_lines.append(line)
            continue

        parsed = parse_txt_line(stripped)

        if parsed is None:
            new_lines.append(line)
            continue

        ctxt, msgid, comment = parsed

        if ctxt == old_ctxt and msgid == old_id:
            new_line = format_txt_line(new_ctxt, new_id, comment) + "\n"
            new_lines.append(new_line)
            modified += 1
        else:
            new_lines.append(line)

    with open(filepath, "w", encoding="utf-8") as f:
        f.writelines(new_lines)

    logger.info(f"Changed {modified} messages in {filepath}")


def process_po_file(
    filepath: Path, old_ctxt: str | None, old_id: str, new_ctxt: str | None, new_id: str
):
    po = polib.pofile(filepath, wrapwidth=0)
    modified = 0

    for entry in po:
        entry_ctxt = entry.msgctxt if entry.msgctxt == "" else None
        if entry_ctxt == old_ctxt and entry.msgid == old_id:
            entry.msgctxt = new_ctxt if new_ctxt is not None else ""
            entry.msgid = new_id
            modified += 1

    po.save()
    logger.info(f"Changed {modified} messages in {filepath}")


def normalize_po_file(filepath: Path):
    """Run `msgcat -o <file> <file>` to normalize/format the PO file."""
    subprocess.run(["msgcat", "-o", str(filepath), str(filepath)], check=True)


def main():
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(
        description="Replace msgctxt/msgid in PO and TXT files."
    )
    parser.add_argument(
        "--old-ctxt", "-oc", default=None, help="Old msgctxt (default: None)"
    )
    parser.add_argument(
        "--new-ctxt", "-nc", default=None, help="New msgctxt (default: None)"
    )
    parser.add_argument("--old-id", "-o", required=True, help="Old msgid")
    parser.add_argument("--new-id", "-n", required=True, help="New msgid")
    parser.add_argument("--txt-file", "-t", type=Path, help="Path to glossary txt file")
    parser.add_argument("po_files", nargs="*", type=Path, help="PO file path(s)")

    args = parser.parse_args()

    for po_file in args.po_files:
        process_po_file(po_file, args.old_ctxt, args.old_id, args.new_ctxt, args.new_id)
        normalize_po_file(po_file)

    if args.txt_file:
        process_txt_file(
            args.txt_file, args.old_ctxt, args.old_id, args.new_ctxt, args.new_id
        )


if __name__ == "__main__":
    main()
