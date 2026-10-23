#!/usr/bin/env python3
"""
Transfer DX values from a CSV file into a DSM2 INP channel table.

Historically, DSM2 channel grid configurations stored channel attributes
(CHAN_NO, LENGTH, MANNING, DISPERSION, UPNODE, DOWNNODE) in the CHANNEL block
of an INP file, while spatial discretization values (DX) were kept in a
separate CSV file.

This script parses the CHANNEL block within a DSM2 INP file, merges the DX
values from the provided CSV file on 'CHAN_NO', and produces a new INP file
containing the updated CHANNEL table (including the 'DX' column) while
preserving all other sections, comments (both standalone comment lines and
inline comments), blank lines, and structural formatting of the original
INP file.

Example usage
-------------
    python scripts/transfer_dx_channel_table.py \\
        --csv-path channel_dx.csv \\
        --inp-path hydro.inp \\
        --output-path hydro_with_dx.inp
"""

from __future__ import annotations

from pathlib import Path
import re

import click
import pandas as pd


def load_dx_data(csv_path: str | Path) -> pd.DataFrame:
    """
    Load channel DX values from a CSV or whitespace-delimited file.

    The file may either contain a header with column names 'CHAN_NO' and 'DX'
    (case-insensitive), or two headerless columns representing channel number
    and DX value respectively.

    Parameters
    ----------
    csv_path : str | Path
        Path to the CSV file containing DX values.

    Returns
    -------
    pd.DataFrame
        DataFrame with columns ['CHAN_NO', 'DX'], where CHAN_NO is integer.

    Raises
    ------
    ValueError
        If the file does not have at least two columns or CHAN_NO cannot
        be parsed.
    """
    csv_path = Path(csv_path)

    # First attempt reading with first line to inspect header
    first_line = ""
    with open(csv_path, "r", encoding="utf-8") as f:
        for line in f:
            stripped = line.strip()
            if stripped and not stripped.startswith("#"):
                first_line = stripped
                break

    # Determine delimiter: comma or whitespace
    sep = "," if "," in first_line else r"\s+"

    # Check if first non-empty line looks like a header with CHAN_NO / DX
    header_tokens = [
        token.upper() for token in first_line.replace(",", " ").split()
    ]
    has_header = "CHAN_NO" in header_tokens or "CHANNEL" in header_tokens

    if has_header:
        df_dx = pd.read_csv(csv_path, sep=sep, comment="#", engine="python")
        # Standardize column names
        rename_map = {}
        for col in df_dx.columns:
            col_upper = str(col).strip().upper()
            if col_upper in ("CHAN_NO", "CHANNEL", "CHANNEL_NO", "CHAN"):
                rename_map[col] = "CHAN_NO"
            elif col_upper in ("DX", "DELTAX", "CHAN_DX"):
                rename_map[col] = "DX"
        df_dx = df_dx.rename(columns=rename_map)
        if "CHAN_NO" not in df_dx.columns or "DX" not in df_dx.columns:
            raise ValueError(
                f"CSV header in '{csv_path}' must contain 'CHAN_NO' and 'DX'. "
                f"Found: {list(df_dx.columns)}"
            )
        df_dx = df_dx[["CHAN_NO", "DX"]]
    else:
        df_dx = pd.read_csv(
            csv_path,
            sep=sep,
            comment="#",
            header=None,
            usecols=[0, 1],
            names=["CHAN_NO", "DX"],
            engine="python",
        )

    df_dx["CHAN_NO"] = (
        pd.to_numeric(df_dx["CHAN_NO"], errors="raise").astype(int)
    )
    df_dx["DX"] = pd.to_numeric(df_dx["DX"], errors="raise")
    return df_dx


def transfer_dx_from_csv(
    csv_path: str | Path,
    inp_path: str | Path,
    output_path: str | Path,
    default_dx: float | None = None,
    float_format: str = "%.6f",
    verbose: bool = True,
) -> None:
    """
    Transfer DX values from a CSV file into the CHANNEL table of an INP file.

    Locates the CHANNEL block in the input INP file, merges the DX values
    corresponding to each channel number (CHAN_NO), writes out the updated
    CHANNEL table with the DX column, and retains all comments (including
    comment lines and inline comments within the CHANNEL block), blank lines,
    and sections outside the CHANNEL block unchanged.

    Parameters
    ----------
    csv_path : str | Path
        Path to the CSV file containing DX values.
    inp_path : str | Path
        Path to the input DSM2 INP file.
    output_path : str | Path
        Path where the modified INP file will be saved.
    default_dx : float, optional
        Default DX value to fill if a channel in the INP is not found in the
        CSV. If None, an error is raised upon missing matches.
    float_format : str, default '%.6f'
        Format specifier string used to format DX float values.
    verbose : bool, default True
        Whether to print status messages during processing.

    Raises
    ------
    ValueError
        If no CHANNEL block is found in the INP file, or if channels are
        missing DX values and no default_dx is specified.
    """
    csv_path = Path(csv_path)
    inp_path = Path(inp_path)
    output_path = Path(output_path)

    # 1. Load DX values
    df_dx = load_dx_data(csv_path)
    dx_map = dict(zip(df_dx["CHAN_NO"], df_dx["DX"]))
    if verbose:
        print(f"Loaded {len(dx_map)} DX entries from '{csv_path}'.")

    # 2. Read INP file and identify sections
    with open(inp_path, "r", encoding="utf-8") as f:
        all_lines = f.readlines()

    channel_start_idx = None
    channel_end_idx = None

    for idx, line in enumerate(all_lines):
        code_part = line.split("#")[0].strip()
        # Find start of CHANNEL table (avoid matching CHANNEL_IC, etc.)
        if channel_start_idx is None:
            if code_part.upper() == "CHANNEL":
                channel_start_idx = idx
        else:
            # Once CHANNEL table has started, find the corresponding END
            if code_part.upper() == "END":
                channel_end_idx = idx
                break

    if channel_start_idx is None:
        raise ValueError(f"Could not find 'CHANNEL' block in '{inp_path}'.")
    if channel_end_idx is None:
        raise ValueError(
            f"Found 'CHANNEL' block starting at line {channel_start_idx + 1}, "
            "but no matching 'END'."
        )

    # Lines prior to CHANNEL block
    prefix_lines = all_lines[:channel_start_idx]
    channel_line = all_lines[channel_start_idx]
    channel_block_lines = all_lines[channel_start_idx + 1:channel_end_idx]
    end_line = all_lines[channel_end_idx]
    suffix_lines = all_lines[channel_end_idx + 1:]

    # 3. Process CHANNEL table lines, preserving all comments and formatting
    new_channel_block_lines = []
    header_found = False
    chan_no_col_idx = None
    dx_col_idx = None
    channels_updated = 0
    missing_channels = []
    inp_channels = []

    for line in channel_block_lines:
        stripped = line.strip()

        # Preserve empty lines and comment lines as-is
        if not stripped or stripped.startswith("#"):
            new_channel_block_lines.append(line)
            continue

        # Separate data part from trailing inline comment
        comment_idx = line.find("#")
        if comment_idx != -1:
            data_part = line[:comment_idx]
            comment_part = line[comment_idx:]
        else:
            line_no_nl = line.rstrip("\r\n")
            comment_part = line[len(line_no_nl):]
            data_part = line_no_nl

        matches = list(re.finditer(r"\S+", data_part))
        if not matches:
            new_channel_block_lines.append(line)
            continue

        # Header line (first non-blank, non-comment line)
        if not header_found:
            header_tokens = [m.group(0).upper() for m in matches]
            if "CHAN_NO" not in header_tokens:
                raise ValueError(
                    "CHANNEL block does not contain 'CHAN_NO' header. "
                    f"Found: {header_tokens}"
                )
            chan_no_col_idx = header_tokens.index("CHAN_NO")
            if "DX" in header_tokens:
                dx_col_idx = header_tokens.index("DX")
                if verbose:
                    print(
                        "Existing 'DX' column detected in CHANNEL header; "
                        "updating values in-place."
                    )
                new_channel_block_lines.append(line)
            else:
                dx_col_idx = None
                last_end = matches[-1].end()
                new_data_part = (
                    data_part[:last_end] + "    DX" + data_part[last_end:]
                )
                new_channel_block_lines.append(new_data_part + comment_part)
            header_found = True
            continue

        # Data line
        if chan_no_col_idx >= len(matches):
            raise ValueError(
                f"Row has fewer columns than header at: '{stripped}'"
            )

        chan_str = matches[chan_no_col_idx].group(0).lstrip("^")
        try:
            chan_no = int(chan_str)
        except ValueError:
            raise ValueError(
                f"Invalid CHAN_NO '{chan_str}' in row: '{stripped}'"
            )

        inp_channels.append(chan_no)

        if chan_no in dx_map:
            dx_val = dx_map[chan_no]
        elif default_dx is not None:
            dx_val = default_dx
            missing_channels.append(chan_no)
        else:
            missing_channels.append(chan_no)
            dx_val = None

        if dx_val is not None:
            formatted_dx = float_format % dx_val
        else:
            formatted_dx = "NaN"

        if dx_col_idx is not None and dx_col_idx < len(matches):
            # Replace existing DX token in-place
            start = matches[dx_col_idx].start()
            end = matches[dx_col_idx].end()
            new_data_part = (
                data_part[:start] + formatted_dx + data_part[end:]
            )
        else:
            # Append DX token before any inline comment
            last_end = matches[-1].end()
            new_data_part = (
                data_part[:last_end]
                + f"    {formatted_dx}"
                + data_part[last_end:]
            )

        new_channel_block_lines.append(new_data_part + comment_part)
        channels_updated += 1

    if not header_found:
        raise ValueError(
            f"'CHANNEL' block in '{inp_path}' contains no header or data rows."
        )

    # Validate missing DX values
    if missing_channels:
        if default_dx is not None:
            if verbose:
                print(
                    f"Warning: {len(missing_channels)} channel(s) missing DX "
                    f"values in CSV. Used default DX = {default_dx}."
                )
        else:
            raise ValueError(
                f"{len(missing_channels)} channel(s) in INP have no DX values "
                f"in CSV (e.g. {missing_channels[:5]}). "
                "Specify a default DX value or update the CSV."
            )

    if verbose:
        print(f"Successfully processed {channels_updated} channels.")

    # 4. Write modified INP file
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as fout:
        fout.writelines(prefix_lines)
        fout.write(channel_line)
        fout.writelines(new_channel_block_lines)
        fout.write(end_line)
        fout.writelines(suffix_lines)

    if verbose:
        print(f"Modified INP file written to '{output_path}'.")


@click.command(
    help="Transfer channel DX values from CSV into a DSM2 INP CHANNEL table."
)
@click.option(
    "-c",
    "--csv-path",
    "--csv_path",
    "csv_path",
    required=True,
    help="Path to the CSV file containing DX values (CHAN_NO, DX).",
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
)
@click.option(
    "-i",
    "--inp-path",
    "--inp_path",
    "inp_path",
    required=True,
    help="Path to the source DSM2 INP file containing a CHANNEL table.",
    type=click.Path(exists=True, dir_okay=False, path_type=Path),
)
@click.option(
    "-o",
    "--output-path",
    "--output_path",
    "output_path",
    required=True,
    help="Path where the modified INP file will be saved.",
    type=click.Path(writable=True, dir_okay=False, path_type=Path),
)
@click.option(
    "-d",
    "--default-dx",
    "--default_dx",
    "default_dx",
    type=float,
    default=None,
    help="Optional default DX value for any channels not found in the CSV.",
)
@click.option(
    "-f",
    "--float-format",
    "--float_format",
    "float_format",
    type=str,
    default="%.6f",
    help="Format specifier for DX floating-point values (default: '%.6f').",
)
@click.option(
    "-q",
    "--quiet",
    "quiet",
    is_flag=True,
    default=False,
    help="Suppress informative output messages.",
)
def main(
    csv_path: Path,
    inp_path: Path,
    output_path: Path,
    default_dx: float | None,
    float_format: str,
    quiet: bool,
) -> None:
    """Entry point for CLI execution."""
    transfer_dx_from_csv(
        csv_path=csv_path,
        inp_path=inp_path,
        output_path=output_path,
        default_dx=default_dx,
        float_format=float_format,
        verbose=not quiet,
    )


if __name__ == "__main__":
    main()
