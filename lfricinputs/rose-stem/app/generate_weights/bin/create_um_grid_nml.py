#!/usr/bin/env python
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Script to generate um_grid namelist file
"""
import sys
import os


def write_nml_file(igrid_targ, rotated, lambda_pole, phi_pole, lambda_points,
                   phi_points, delta_lambda, delta_phi, lambda_origin,
                   phi_origin):
    fname = os.environ.get('NLIST_FILE')
    f = open(fname, "w+")
    f.write('&GRID\n')
    f.write('  POINTS_LAMBDA_TARG = {0}\n'.format(lambda_points))
    f.write('  POINTS_PHI_TARG = {0}\n'.format(phi_points))
    f.write('  LAMBDA_ORIGIN_TARG = {0}\n'.format(lambda_origin))
    f.write('  PHI_ORIGIN_TARG = {0}\n'.format(phi_origin))
    f.write('  PHI_POLE = {0}\n'.format(phi_pole))
    f.write('  LAMBDA_POLE = {0}\n'.format(lambda_pole))
    f.write('  ROTATED = {0}\n'.format(rotated))
    f.write('  DELTA_LAMBDA_TARG = {0}\n'.format(delta_lambda))
    f.write('  DELTA_PHI_TARG = {0}\n'.format(delta_phi))
    f.write('  IGRID_TARG = {0}\n'.format(igrid_targ))
    f.write('/\n')
    f.close

if __name__ == "__main__":

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-r", "--resolution", help="resolution of UM grid",
                        type=int)
    parser.add_argument("-d", "--dump", help="path to UM dump file to use")
    args = parser.parse_args()
    igrid_targ = 6

    if args.resolution:

        res = args.resolution
        print('Creating GLOBAL UM NON-ROTATED grid namelist file from given resolution: ', res)

        lambda_pole = 0.0
        phi_pole = 90.0
        rotated = 'F'

        start_lon = 0.0
        start_lat = -90.0

        lambda_points = int(2 * res)
        phi_points = int(3 * res / 2)
        delta_lambda = 360.0 / lambda_points
        delta_phi = 180.0 / phi_points
        lambda_origin = start_lon + (delta_lambda / 2)
        phi_origin = start_lat + (delta_phi / 2)

        write_nml_file(igrid_targ, rotated, lambda_pole, phi_pole,
                       lambda_points, phi_points, delta_lambda, delta_phi,
                       lambda_origin, phi_origin)

    elif args.dump:

        import mule
        df_path = args.dump
        print('Creating UM grid namelist file from dump: ', df_path)
        df = mule.DumpFile.from_file(df_path)
 
        if df.fixed_length_header.grid_staggering != 6:
            print('ERROR: Only ENDGAME grids are currently valid input')
            sys.exit(1)

        lambda_pole = df.real_constants.north_pole_lon
        phi_pole = df.real_constants.north_pole_lat
        if lambda_pole != 0.0 or phi_pole != 90.0:
            rotated = 'T'
        else:
            rotated = 'F'

        start_lon = df.real_constants.start_lon
        start_lat = df.real_constants.start_lat

        lambda_points = df.integer_constants.num_cols
        phi_points = df.integer_constants.num_rows
        delta_lambda = df.real_constants.col_spacing
        delta_phi = df.real_constants.row_spacing
        lambda_origin = start_lon + (delta_lambda / 2)
        phi_origin = start_lat + (delta_phi / 2)

        write_nml_file(igrid_targ, rotated, lambda_pole, phi_pole,
                       lambda_points, phi_points, delta_lambda, delta_phi,
                       lambda_origin, phi_origin)

    else:

        print('')
        print('Use the --help or -h flags to see commandline options')
        print('')
