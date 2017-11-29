
# Simple script to test easily multiple bond files and compare the
# output of two versions of gbc. Output includes all codegen output,
# stdout and stderr. Also compares quantity of codegen files generated
# and exit code. A diff will be output per file.

import argparse
import difflib
import glob
import os
import subprocess
import shutil

def get_bond_files(base_dir):
    bond_files = []

    os.chdir(base_dir)
    for file in glob.glob("**/*.bond", recursive=True):
        bond_files.append(os.path.join(base_dir, file))

    print("INFO Found {} bond files.".format(len(bond_files)))
    return bond_files

def generate_bond(gbc, gbc_flags, output_dir, bond_file):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    with open(os.path.join(output_dir,"stdout.txt"), "w+") as stdout_file:
        with open(os.path.join(output_dir,"stderr.txt"), "w+") as stderr_file:
            return_code = subprocess.call([gbc, gbc_flags, "-o", output_dir, bond_file], shell=False, stdout=stdout_file, stderr=stderr_file)

    return return_code

def compare_gbcs(stable_gbc, updated_gbc, gbc_flags, output_dir, bond_files):
    for bond_file in bond_files:
        first_dir = os.path.join(output_dir, "1")
        first_return_code = generate_bond(stable_gbc, gbc_flags, first_dir, bond_file)

        second_dir = os.path.join(output_dir, "2")
        second_return_code = generate_bond(updated_gbc, gbc_flags, second_dir, bond_file)

        if (first_return_code != second_return_code):
            print("ERROR File {}: on base gbc has {} return code and on updated gbc has {} return code".format(bond_file, first_return_code, second_return_code))
            return

        list_files_1 = sorted(os.listdir(first_dir))
        list_files_2 = sorted(os.listdir(second_dir))

        file_count_1 = len(list_files_1)
        file_count_2 = len(list_files_2)

        if (file_count_1 != file_count_2):
            print("ERROR File {}: on base gbc has generated {} files and updated gbc has generated {} files".format(bond_file, file_count_1, file_count_2))
            return

        for (a, b) in zip(list_files_1, list_files_2):
            with open(os.path.join(first_dir, a), 'r') as file_1:
                with open(os.path.join(second_dir, b), 'r') as file_2:
                    diff = difflib.unified_diff(
                        file_1.readlines(),
                        file_2.readlines(),
                        fromfile='stable_gbc',
                        tofile='updated_gbc',
                    )
                    anyDiffs = False
                    for line in diff:
                        anyDiffs = True
                        print(line)
                    if anyDiffs:
                        print("ERROR File {}: has different output on generated file/output {}".format(bond_file, a))
                        print("#########")

        shutil.rmtree(first_dir)
        shutil.rmtree(second_dir)

def main():
    parser = argparse.ArgumentParser()

    parser.add_argument('--stable_gbc', required=True, help="Path to stable base gbc")
    parser.add_argument('--updated_gbc', required=True, help="Path to updated gbc")
    parser.add_argument('--output_dir', required=True, help="Directory where script will write to")
    parser.add_argument('--input_dir', required=True, help="Directory of bond files to be tested, can have nested dirs with bond files")
    parser.add_argument('--gbc_args', required=True, help="Args to gbc e.g. 'cpp' or 'cs' to be appended on every gbc command")

    args = parser.parse_args()

    bond_files = get_bond_files(args.input_dir)
    compare_gbcs(args.stable_gbc, args.updated_gbc, args.gbc_args, args.output_dir, bond_files)

if __name__ == "__main__":
    main()

