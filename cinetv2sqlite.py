#! /usr/bin/env python
"""CineTV SQLite release creation.

Usage:
  cinetv-release [--reload] -d <cinetv_xlsx_dir> [-e <cinetv_ext_dir>] [-a <cinetv_ext_auto_dir>] -o <sqlite_output_dir>
  cinetv-release (-h | --help)
  cinetv-release --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  -d <cinetv_xlsx_dir> Directory containing XLSX files of CineTV export.
  -e <cinetv_ext_dir> Directory containing CSV files of CineTV extensions.
  -a <cinetv_ext_auto_dir> Directory containing automatically generated CSV files of CineTV extensions.
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
import shutil
import tarfile

def main(args):
    dateMatch = re.search(r"""[0-9]+-[0-9]+-[0-9]+""", args['-d'])

    if dateMatch is None:
        print("[ERROR] The input CineTV export folder must contain a date in the format: YYYY-MM-DD")
        sys.exit(1)

    date = dateMatch.group(0)

    outputdir = f'{path.join(args["-o"], "cinetv-" + date)}'

    shutil.rmtree(outputdir)
    # if os.path.exists(outputdir):
    #     for f in glob(f'{outputdir}/*'):
    #         os.remove(f)
    #     os.rmdir(outputdir)

    for f in glob(path.join(args['-d'], '*.xlsx')):
        csvFile = path.join(args['-d'], f.replace('.xlsx', '') + '.csv')
        if args['--reload'] or not os.path.exists(csvFile):
            print(f'[INFO] Converting file {f} to {csvFile}')
            Xlsx2csv(f, skip_empty_lines=True, outputencoding='utf-8').convert(csvFile)

    csvFilesPattern = path.join(args['-d'], '*.csv')

    Path(outputdir).mkdir(parents=True, exist_ok=True)
    cinetvDbPath = path.join(outputdir, f'cinetv-{date}.db')
    print(f'[INFO] Converting generated CSV files from {csvFilesPattern} to SQLite database at {cinetvDbPath}')
    # if os.system(f'csvs-to-sqlite {csvFilesPattern} {cinetvDbPath}') != 0:
    #     return
    os.system(f'csvs-to-sqlite {csvFilesPattern} {cinetvDbPath}')

    cinetvExtDbPath = path.join(outputdir, f'cinetv-{date}-ext.db')
    cinetvExtAutoDbPath = path.join(outputdir, f'cinetv-{date}-ext-auto.db')
    if args["-e"] and args["-a"]:
        csvFilesExtPattern = path.join(args['-e'], '*.csv')
        print(f'[INFO] Converting CineTV CSV files ({csvFilesPattern}) + CineTV extension CSV files ({csvFilesExtPattern}) to SQLite database at {cinetvExtDbPath}')
        os.system(f'csvs-to-sqlite {csvFilesPattern} {csvFilesExtPattern} {cinetvExtDbPath}')

        print(f'[INFO] Updating CineTV automatically generated extension data...')
        os.system(f'cinetvlinking-exe filmo -d {cinetvExtDbPath} -o {args["-a"]}')
        os.system(f'cinetvlinking-exe nom apply -d {cinetvExtDbPath} -o {args["-a"]}')

        csvFilesExtAutoPattern = path.join(args['-a'], '*.csv')
        print(f'[INFO] Converting CineTV CSV files ({csvFilesPattern}) + CineTV extension CSV files ({csvFilesExtPattern}) + CineTV automatically generated CSV files {csvFilesExtAutoPattern} to SQLite database at {cinetvExtAutoDbPath}')
        # os.system(f'csvs-to-sqlite --replace-tables {csvFilesPattern} {csvFilesExtPattern} {csvFilesExtAutoPattern} {cinetvExtAutoDbPath}')
        os.system(f'csvs-to-sqlite {csvFilesPattern} {csvFilesExtPattern} {cinetvExtAutoDbPath}')
        for f in glob(path.join(args['-a'], '*.csv')):
            tablename = path.splitext(path.basename(f))[0]
            os.system(f'csvs-to-sqlite -pk NomID -t {tablename} {f} {cinetvExtAutoDbPath}')

        print(f'[INFO] Creating a public subset of CineTV database {cinetvExtAutoDbPath} at {outputdir}')
        os.system(f'cinetv2public-exe -s {cinetvExtAutoDbPath} -d {outputdir}')

    elif args["-e"] and not args["-a"]:
        csvFilesExtPattern = path.join(args['-e'], '*.csv')
        print(f'[INFO] Converting CineTV CSV files ({csvFilesPattern}) + CineTV extension CSV files ({csvFilesExtPattern}) to SQLite database at {cinetvExtDbPath}')
        os.system(f'csvs-to-sqlite {csvFilesPattern} {csvFilesExtPattern} {cinetvExtDbPath}')

        print(f'[INFO] Creating a public subset of CineTV database {cinetvExtDbPath} at {outputdir}')
        os.system(f'cinetv2public-exe -s {cinetvExtDbPath} -d {outputdir}')

    elif not args["-e"] and args["-a"]:
        print(f'[INFO] Updating CineTV automatically generated extension data...')
        os.system(f'cinetvlinking-exe filmo -d {cinetvDbPath} -o {args["-a"]}')
        os.system(f'cinetvlinking-exe nom apply -d {cinetvDbPath} -o {args["-a"]}')

        csvFilesExtAutoPattern = path.join(args['-a'], '*.csv')
        print(f'[INFO] Converting CineTV CSV files ({csvFilesPattern}) + CineTV automatically generated CSV files {csvFilesExtAutoPattern} to SQLite database at {cinetvExtAutoDbPath}')
        os.system(f'csvs-to-sqlite {csvFilesPattern} {csvFilesExtAutoPattern} {cinetvExtAutoDbPath}')

        print(f'[INFO] Creating a public subset of CineTV database {cinetvExtAutoDbPath} at {outputdir}')
        os.system(f'cinetv2public-exe -s {cinetvExtAutoDbPath} -d {outputdir}')

    else:
        print(f'[INFO] Creating a public subset of CineTV database {cinetvDbPath} at {outputdir}')
        os.system(f'cinetv2public-exe -s {cinetvDbPath} -d {outputdir}')

    cinetvPublicDbPath = path.join(outputdir, f'cinetv-{date}-publique.db')

    print("[INFO] Exporting SQLite database to CSV files...")
    cinetvCsvPath = path.join(outputdir, "csv")
    Path(cinetvCsvPath).mkdir(parents=True, exist_ok=True)
    os.system(f'sqlite-dump-to-csv --db {cinetvPublicDbPath} --output {cinetvCsvPath}')

    print("[INFO] Generating a tarball from CSV files...")
    cinetvCsvTarPath = path.join(outputdir, f'cinetv-{date}-csv.tar.gz')
    with tarfile.open(cinetvCsvTarPath, "w:gz") as tar:
        for f in glob(f'{cinetvCsvPath}/*'):
            tar.add(f, arcname=f"cinetv-{date}/{path.basename(f)}")

    print("[INFO] Generating a tarball from SQLite DB file...")
    cinetvPublicDbTarPath = path.join(outputdir, f'cinetv-{date}-sqlite.tar.gz')
    with tarfile.open(cinetvPublicDbTarPath, "w:gz") as tar:
        tar.add(cinetvPublicDbPath, arcname=f"cinetv-{date}/cinetv-{date}.db")

if __name__ == '__main__':
    arguments = docopt(__doc__, version='cmtq-release 2.0')
    main(arguments)
