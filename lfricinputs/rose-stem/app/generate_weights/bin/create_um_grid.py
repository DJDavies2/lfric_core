#!/usr/bin/env python
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""Generate a UM grid file in SCRIP format from a namelist file."""
import os
from netCDF4 import Dataset
import numpy
import iris
import iris.fileformats
import iris.analysis
import iris.coord_systems
import f90nml


class GRID:
    """Holds information from suite"""
    def __init__(self):
        self.vname = None   # Title to use in netCDF file
        self.model = None   # Model the grid refers to. Only option is 'UM'
        self.grid = None    # UM grid type: P/U/V points at grid cell centres
        self.l_area = None  # If not 'no', output grid cell surface areas
        self.dlon = None    # Longitude of grid cell centres
        self.ulon = None    # Longitude units of grid cell centres
        self.dlat = None    # Lattitude of grid cell centres
        self.ulat = None    # Lattitude units of grid cell centres
        self.dclo = None    # Longitude of grid cell corners
        self.uclo = None    # Longitude units of grid cell corners
        self.dcla = None    # Lattitude of cell corners
        self.ucla = None    # Lattitude units of grid cell corners
        self.darea = None   # Area of each grid cell
        self.uarea = None   # Area units of each grid cell
        self.shape = None


def UM_guess_bounds(cube):
    """
    Estimate area of grid cells by guessing bounds of 'latitude'
    and 'longitude' coordinates and return areas as an array.

    Args:

    * cube:
        iris.cube.Cube

    Returns:
        iris.cube.Cube with bounds on coordinates

    """

    cube.coord('latitude').guess_bounds()
    cube.coord('longitude').guess_bounds()

    return cube


def UM_sort_cube_bounds(cube):
    '''
    Sort out the coordinate bounds on a cube, add bounds for each coordinate
    and make sure that latitude bounds to not go outside [-90,90]

    Arguments:

      * cube:
         iris cube to deal with
    '''

    for coord in cube.dim_coords:
        if not coord.has_bounds():
            coord.guess_bounds()

    if len(cube.coord_dims('latitude')) > 0:
        latbounds = cube.coord('latitude').bounds.copy()
        latbounds = numpy.clip(latbounds, -90., 90.)
        cube.coord('latitude').bounds = latbounds

    if len(cube.coord_dims('longitude')) > 0:
        if cube.coord('longitude').bounds.max() == \
           cube.coord('longitude').bounds.min()+360:
            cube.coord('longitude').circular = True


def UM_gen_grids(cube):
    """Define UM grid"""
    lon = numpy.resize(cube.coord('longitude').points, (cube.shape))
    lat = numpy.zeros((cube.shape))
    for i in range(cube.shape[1]):
        lat[:, i] = cube.coord('latitude').points
    return lon, lat


def UM_gen_area(cube):
    """Calculate area for each grid"""
    return iris.analysis.cartography.area_weights(cube)


def UM_create_cube(def_name, grid):
    """Create UM cube with the grid definitions"""
    tiny = 1.0e-10

    grid_def = f90nml.read(def_name)
    delx = grid_def['grid']['delta_lambda_targ']
    dely = grid_def['grid']['delta_phi_targ']
    npx = grid_def['grid']['points_lambda_targ']
    npy = grid_def['grid']['points_phi_targ']

    xorigin = grid_def['grid']['lambda_origin_targ']
    yorigin = grid_def['grid']['phi_origin_targ']

    xcoord = numpy.arange(xorigin, xorigin + (npx * delx - tiny), delx)
    ycoord = numpy.arange(yorigin, yorigin + (npy * dely - tiny), dely)

    if grid == 'U':
        xcoord -= .5*delx
        print(npx, xcoord.shape, 'U-grid dims')

    if grid == 'V':
        ycoord = numpy.arange(yorigin, yorigin + ((npy + 1) * dely - tiny),
                              dely)
        print(npy, ycoord.shape, 'V-grid dims')
        ycoord -= .5*dely
        ycoord[0] = numpy.float32(ycoord[0])
        ycoord[-1] = numpy.float32(ycoord[-1])

    if min(xcoord) < 0.:
        raise Exception('lowest longitude less than zero')

    if min(ycoord) < -90. or min(ycoord) > 90.:
        print(min(ycoord), min(ycoord))
        print('lowest latitude less than -90 or greater than 90')

    mlong = iris.coords.DimCoord(xcoord, standard_name='longitude',
                                 circular=False, units='degrees',
                                 coord_system=iris.coord_systems.GeogCS(
                                     iris.fileformats.pp.EARTH_RADIUS))

    mlat = iris.coords.DimCoord(ycoord, standard_name='latitude',
                                units='degrees',
                                coord_system=iris.coord_systems.GeogCS(
                                    iris.fileformats.pp.EARTH_RADIUS))

    data = numpy.zeros((len(ycoord), len(xcoord)))
    cube = iris.cube.Cube(data, dim_coords_and_dims=[(mlat, 0), (mlong, 1)],
                          var_name='cube')
    UM_guess_bounds(cube)

    UM_sort_cube_bounds(cube)

    return cube


def UM_lat_corners(cube):
    """Define UM latitude corners"""
    lat_bounds = numpy.zeros((cube.shape))
    corners = numpy.zeros((cube.shape[0], cube.shape[1], 4))
    for i in range(cube.shape[1]):
        lat_bounds[:, i] = cube.coord('latitude').bounds[:, 0]
    corners[:, :, 0] = lat_bounds
    corners[:, :, 1] = lat_bounds
    for i in range(cube.shape[1]):
        lat_bounds[:, i] = cube.coord('latitude').bounds[:, 1]
    corners[:, :, 2] = lat_bounds
    corners[:, :, 3] = lat_bounds
    return corners


def UM_lon_corners(cube):
    """Define UM longitude corners"""
    lon_bounds = numpy.zeros((cube.shape))
    corners = numpy.zeros((cube.shape[0], cube.shape[1], 4))
    for i in range(cube.shape[0]):
        lon_bounds[i, :] = cube.coord('longitude').bounds[:, 0]    
    corners[:, :, 0] = lon_bounds
    corners[:, :, 3] = lon_bounds
    for i in range(cube.shape[0]):
        lon_bounds[i, :] = cube.coord('longitude').bounds[:, 1]
    corners[:, :, 1] = lon_bounds
    corners[:, :, 2] = lon_bounds
    return corners


def corners_transformation(field):
    """Transform corners to correct order"""
    return field.transpose((1, 2, 0))


def transform(fin, name):
    """Remove one dimension from the array"""
    shape = fin.shape
    if len(shape) == 2:
        if(shape[0] == 4):
            fout = numpy.transpose(fin)
        elif(shape[1] == 4):
            fout = fin
        else:
            fout = numpy.zeros((shape[0]*shape[1]), dtype=numpy.float64)
            fout = fin.flatten()
    elif len(shape) == 3:
        if shape[0] == 4:
            fin = corners_transformation(fin)
            shape_o = shape
            shape = fin.shape
            print('transforming corner data for ', name, ' from ', shape_o, \
                  ' to', shape)
        fout = numpy.zeros((shape[0]*shape[1], 4), dtype=numpy.float64)
        fout = numpy.reshape(fin, (shape[0]*shape[1], 4))
    elif len(shape) == 1:
        fout = fin
    else:
        raise Exception('problem in transform')
    return fout


def transform_and_write(filename, my_grid):
    """Transform arrays and write netcdf output"""
    rank = len(my_grid.shape)
    mrank = my_grid.shape
    print(rank, ' RANK')
    if rank == 2:
        grid_size = my_grid.shape[0]*my_grid.shape[1]
    else:
        raise Exception('transform_and_write: ' +
                        'wrong number of dimensions in input data')

    corners = 4
    fileout = filename
    outfile = Dataset(fileout, 'w')
    outfile.createDimension('grid_rank', numpy.int32(rank))
    outfile.createDimension('grid_size', numpy.int32(grid_size))
    outfile.createDimension('grid_corners', numpy.int32(corners))
    rankout = outfile.createVariable('grid_dims',
                                     numpy.dtype('int32').char,
                                     ('grid_rank', ))
    rankout.long_name = "grid_dims"
    rankout[:] = numpy.asarray(mrank).astype(numpy.int32)
    latout = outfile.createVariable('grid_center_lat',
                                    numpy.dtype('float64').char,
                                    ('grid_size', ))
    latout.long_name = "grid_center_lat"
    latout.units = str(my_grid.ulat)
    latout[:] = transform(my_grid.dlat, 'lat')
    lonout = outfile.createVariable('grid_center_lon',
                                    numpy.dtype('float64').char,
                                    ('grid_size', ))
    lonout.long_name = "grid_center_lon"
    lonout.units = str(my_grid.ulon)
    lonout[:] = transform(my_grid.dlon, 'lon')
    maskout = outfile.createVariable('grid_imask',
                                     numpy.dtype('int32').char,
                                     ('grid_size', ))
    maskout.long_name = "grid_imask"
    maskout.units = "unitless"
    print("Set mask to 1 everywhere.")
    maskout[:] = numpy.int32(1)
    if my_grid.l_area != 'no':
        srfout = outfile.createVariable('grid_area',
                                        numpy.dtype('float64').char,
                                        ('grid_size', ))
        srfout.long_name = "grid surface m^2"
        srfout.units = str(my_grid.uarea)
        srfout[:] = transform(my_grid.darea, 'area')
    claout = outfile.createVariable('grid_corner_lat',
                                    numpy.dtype('float64').char,
                                    ('grid_size', 'grid_corners'))
    claout.long_name = "grid_corner_lat"
    claout.units = str(my_grid.ucla)
    claout[:, :] = transform(my_grid.dcla, 'cla')
    cloout = outfile.createVariable('grid_corner_lon',
                                    numpy.dtype('float64').char,
                                    ('grid_size', 'grid_corners'))
    cloout.long_name = "grid_corner_lon"
    cloout.units = str(my_grid.uclo)
    cloout[:, :] = transform(my_grid.dclo, 'clo')
    outfile.title = my_grid.vname
    outfile.close()
    return


def get_env_info(my_grid, src):
    """Get information set in suite"""
    my_grid.vname = src + '_grid'
    my_grid.model = src
    my_grid.grid = os.environ.get('GRID')
    my_grid.l_area = os.environ.get('LAREA')
    my_grid.nlist = os.environ.get('NLIST_FILE')

    return my_grid


def validate_input(my_grid):
    """check sizes of arrays !!!!!!
       check units"""
    print('--------------validate---------------------')
    if my_grid.ulon is None:
        print(' Lon/Lat units not found')
        if max(numpy.abs(my_grid.dlon).max(),
               numpy.abs(my_grid.dlon).min()) > 6.5:
            my_grid.ulon = 'degrees'
            my_grid.ulat = 'degrees'
            print('  Based on values setting to degrees')
        else:
            my_grid.ulon = 'radians'
            my_grid.ulat = 'radians'
            print('  Based on values setting to radians')
    else:
        if "deg" in (my_grid.ulon).lower():
            my_grid.ulon = 'degrees'
            my_grid.ulat = 'degrees'
        elif "rad" in (my_grid.ulon).lower():
            my_grid.ulon = 'radians'
            my_grid.ulat = 'radians'
        else:
            raise Exception('Unrecognised lon/lat units: ', my_grid.ulon)
    if my_grid.uclo is None:
        print(' Lon/Lat corners units not found')
        if max(numpy.abs(my_grid.dclo).max(),
               numpy.abs(my_grid.dclo).min()) > 6.5:
            my_grid.uclo = 'degrees'
            my_grid.ucla = 'degrees'
            print('  Based on values setting corners units to degrees')
        else:
            my_grid.uclo = 'radians'
            my_grid.ucla = 'radians'
            print('  Based on values setting corners units to radians')
    else:
        if "deg" in (my_grid.uclo).lower():
            my_grid.uclo = 'degrees'
            my_grid.ucla = 'degrees'
        elif "rad" in (my_grid.uclo).lower():
            my_grid.uclo = 'radians'
            my_grid.ucla = 'radians'
        else:
            raise Exception('Unrecognised lon/lat corners unit: ',
                            my_grid.ulon)
    if my_grid.uclo != my_grid.ulon:
        raise Exception('Problem with input data: longitude (',
                        my_grid.ulon, ') and longitude corners (',
                        my_grid.uclo, ') units differ')
    print('--------------validate info---------------------')
    print('vname', my_grid.vname)
    print('model', my_grid.model)
    print('grid', my_grid.grid)
    print('l_area', my_grid.l_area)
    print('dlon', (my_grid.dlon).shape, my_grid.ulon)
    print('dlat', (my_grid.dlat).shape, my_grid.ulat)
    print('dclo', (my_grid.dclo).shape, my_grid.uclo)
    print('dcla', (my_grid.dcla).shape, my_grid.ucla)
    print('namelist ', my_grid.nlist)
    return


def get_data(my_grid):
    """Get data as requested in suite"""
    cube = None
    # longitude and latitude
    if my_grid.model == 'UM':
        cube = UM_create_cube(my_grid.nlist, my_grid.grid)
        # longitude and latitude
        my_grid.dlon, my_grid.dlat = UM_gen_grids(cube)
        # corners
        my_grid.dclo = UM_lon_corners(cube)
        my_grid.dcla = UM_lat_corners(cube)
        # area
        my_grid.darea = UM_gen_area(cube)
        my_grid.uarea = "m2"
    else:
        raise Exception('Can not generate grid information \
                         for models other than UM')
    my_grid.shape = numpy.asarray(my_grid.dlat.shape[::-1])
    return my_grid


if __name__ == "__main__":
    UM_GRID = GRID()
    UM_GRID = get_env_info(UM_GRID, 'UM')
    UM_GRID = get_data(UM_GRID)
    # end processing UM grid
    print('UM grid ')
    validate_input(UM_GRID)
    transform_and_write(os.environ["GRID_PATH_UM"], UM_GRID)
