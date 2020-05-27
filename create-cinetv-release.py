#! /usr/bin/env python
"""CineTV SQLite release creation.

Usage:
  cinetv-release [--reload] -d <cinetv_xlsx_dir> [-e <cinetv_ext_dir>] -o <sqlite_output_dir>
  cinetv-release (-h | --help)
  cinetv-release --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  -d <cinetv_xlsx_dir> Directory containing XLSX files of CineTV export.
  -e <cinetv_ext_dir> Directory containing CSV files of CineTV extensions.
  -o <sqlite_output_dir> Output directory to write SQLite databases.
  --reload      Reload all XLSX files to convert to CSV

"""
from docopt import docopt
from xlsx2csv import Xlsx2csv
import csvs_to_sqlite
import os
from os import path
from glob import glob
import re
import sys
from pathlib import Path
import subprocess

def main(args):
    dateMatch = re.search(r"""[0-9]+-[0-9]+-[0-9]+""", args['-d'])

    if dateMatch is None:
        print("[ERROR] The input CineTV export folder must contain a date in the format: YYYY-MM-DD")
        sys.exit(1)

    date = dateMatch.group(0)

    outputdir = f'{path.join(args["-o"], date)}'

    for f in glob(f'{outputdir}/*'):
        os.remove(f)
    os.rmdir(outputdir)

    for f in glob(path.join(args['-d'], '*.xlsx')):
        csvFile = path.join(args['-d'], f.replace('.xlsx', '') + '.csv')
        if args['--reload'] or not os.path.exists(csvFile):
            print(f'Converting file {f} to {csvFile}')
            Xlsx2csv(f, outputencoding='utf-8').convert(csvFile)

    csvFilesPattern = path.join(args['-d'], '*.csv')

    Path(outputdir).mkdir(parents=True, exist_ok=True)
    cinetvDbPath = path.join(outputdir, f'cinetv-{date}.db')
    print(f'Converting generated CSV files from {csvFilesPattern} to SQLite database at {cinetvDbPath}')
    if os.system(f'csvs-to-sqlite {csvFilesPattern} {cinetvDbPath}') != 0:
        return

    if args["-e"] is None:
        print(f'Migrate CineTV database {cinetvDbPath} to a public subset at {outputdir}')
        os.system(f'cinetv-migration-exe -s {cinetvDbPath} -d {outputdir}')
    else:
        print(f'Updating CineTV extension data...')
        # Regénérer les données extension de CineTV.
        os.system(f'cinetvlinking-exe filmo -d {cinetvDbPath} -o {args["-e"]}')
        os.system(f'cinetvlinking-exe nom apply -d {cinetvDbPath} -o {args["-e"]}')

        csvFilesExtPattern = path.join(args['-e'], '*.csv')
        cinetvExtDbPath = path.join(outputdir, f'cinetv-{date}-publique.db')
        print(f'Converting generated CSV files from {csvFilesPattern} and {csvFilesExtPattern} to SQLite database at {cinetvExtDbPath}')
        os.system(f'csvs-to-sqlite {csvFilesPattern} {csvFilesExtPattern} {cinetvExtDbPath}')
        print(f'Migrate CineTV database {cinetvExtDbPath} to a public subset at {outputdir}')
        os.system(f'cinetv-migration-exe -s {cinetvExtDbPath} -d {outputdir}')

if __name__ == '__main__':
    arguments = docopt(__doc__, version='cmtq-release 2.0')
    main(arguments)
