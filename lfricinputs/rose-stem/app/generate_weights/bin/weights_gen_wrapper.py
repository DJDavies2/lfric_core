#!/usr/bin/env python
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Wrapper script for a set of commands/script used to create weights files
"""
import subprocess
import os


def run_command(command):
    cmd = subprocess.Popen(command, stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    stdout, stderr = cmd.communicate()
    return_code = cmd.returncode
    print(' ')
    print('Output from running: ', ' '.join(command), ':')
    print(stdout.decode())
    print(stderr.decode())
    if return_code != 0:
        raise Exception(' '.join(command) + ' failed.')


def weight_gen(um_ptype, int_method):
    os.environ["GRID"] = um_ptype
    os.environ["INT_METHOD"] = int_method
    os.environ["GRID_PATH_UM"] = "UM_grid_" + um_ptype + ".nc"
    direct = os.environ.get('INT_DIRECT')
    if direct == 'um2lfric':
        outfilename = um_ptype + "_to_FACE_CENTRE_" + int_method + ".nc"
    elif direct == 'lfric2um':
        outfilename = "FACE_CENTRE_to_" + um_ptype + "_" + int_method + ".nc"
    else:
        print('Interpolation direction not supported')

    run_command(["create_um_grid.py"])
    run_command(["generate_weights.py"])
    run_command(["mv", "regrid_weights.nc", outfilename])


if __name__ == "__main__":
    option = os.environ.get('UM_GRID_INPUT')
    os.environ["NLIST_FILE"] = "um_grid.nml"
    if option == 'dump-file':
        argument = os.environ.get('DUMPPATH')
        run_command(["create_um_grid_nml.py", "-d", argument])
    elif option == 'specified-resolution':
        argument = os.environ.get('RESOLUTION')
        run_command(["create_um_grid_nml.py", "-r", argument])
    else:
        raise Exception('UM grid generation input option invalid')
    for um_ptype in ["P", "U", "V"]:
        for int_method in ["bilinear", "neareststod"]:
            weight_gen(um_ptype, int_method)
