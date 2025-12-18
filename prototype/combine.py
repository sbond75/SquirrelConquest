#!/usr/bin/env python3

import sys
import os
import glob


def main():
    if len(sys.argv) != 2:
        print("Usage: python combine_gml.py <directory>")
        sys.exit(1)

    directory = sys.argv[1]

    if not os.path.isdir(directory):
        print("Error: '{}' is not a directory".format(directory))
        sys.exit(1)

    pattern = os.path.join(directory, "*.gml")
    gml_files = glob.glob(pattern)

    if not gml_files:
        print("No .gml files found in '{}'".format(directory))
        sys.exit(1)

    # Sort for deterministic order
    gml_files.sort()

    out_path = os.path.join(directory, "out.gml.txt")

    try:
        with open(out_path, "w", encoding="utf-8") as out_f:
            for gml_path in gml_files:
                filename = os.path.basename(gml_path)

                # Header using GML-style comments
                out_f.write("// ===== {} =====\n".format(filename))

                with open(gml_path, "r", encoding="utf-8") as in_f:
                    out_f.write(in_f.read())

                out_f.write("\n\n")

        print("Combined {} .gml file(s) into '{}'".format(len(gml_files), out_path))

    except OSError as e:
        print("I/O error: {}".format(e))
        sys.exit(1)


if __name__ == "__main__":
    main()
