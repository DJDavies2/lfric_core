#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
'''
Basic python script to plot the x-z profile minus a constant state of 300
from a Dynamo output file.

This version extracts data from UGRID output files using Iris and interpolates
onto a regulat grid for plotting

Levels are determined from the data.


'''

import numpy as np
# Need to set a non-interactive backend for suites
import matplotlib
matplotlib.use('Agg')

import iris
from iris.cube import Cube

import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
import matplotlib.cm as cm

from scipy.interpolate import griddata

import math

import sys

iris.FUTURE.netcdf_promote = True


def process_ugrid(filestem, field, levels_name):

  global cube, n_levs

  # Read the file and extract field of interest
  cube = iris.load_cube(filestem, field)

  # determine the number of levels
  n_levs = cube.data[0,:].shape[0]


       
def make_figure(plotpath, field, timestep):

  # get coordinates

  x = np.around(cube.coord('longitude').points)
  y = np.around(cube.coord('latitude').points)

  slice_fig = plt.figure(figsize=(15,10))
  # get min and max of x,y data for plot axes
  xmin =  min(x)
  xmax = max(x)
  ymin =  min(y)
  ymax = max(y)
  zmin = 0.0
  zmax = 6400.0

  r2d = 1.0/1000.0;
  ny = 300
  nx = 2
  nz = n_levs

  #create 2D plot
  x2d = 0.0
  z2d = np.linspace(zmin, zmax, nz)
  y2d = np.linspace(ymin, ymax, ny)
  xi, yi = np.meshgrid(x2d, y2d)    
  zi = np.zeros([ny,1,n_levs])


  for p in xrange(n_levs):

    # get the data for this level
    data = cube.data[-1,p]


    zi[:,:,p] = griddata((x,y), data, (xi, yi), method='linear') 

  yi, xi = np.meshgrid(z2d, y2d) 
  dz = np.zeros([ny,nz])
  for i in range(ny):
    dz[i,:] = zi[i,0,:] - 300.0


  matplotlib.rcParams['contour.negative_linestyle'] = 'solid'
  cc = np.linspace(-16,-1,16)
  cf = plt.contourf(xi *r2d, yi * r2d, np.round(dz,10), cc)
  cl = plt.contour(xi * r2d, yi*r2d, np.round(dz,10), cc, linewidths=1.0,colors='k', linestyle="", extend='min')
  plt.axis([0, 16, 0, 5])
  plt.xlabel("y (km)")
  plt.ylabel("z (km)")
  plt.title('max: %2.4e, min: %2.4e'%(np.max(dz),np.min(dz)))
  plt.colorbar(cf,  cmap=cm.spectral)

  out_file_name = plotpath + "/" + 'straka_y_ugrid' + "_" + timestep +  ".png"
  slice_fig.savefig(out_file_name , bbox_inches='tight')

if __name__ == "__main__":

  
  try:
    datapath, fields, levels_name, timestep, plotpath = sys.argv[1:6]
  except ValueError:
    print("Usage: {0} <datapath> <field_names> <levels_name> <timestep> <plotpath>".format(sys.argv[0]))
    exit(1)

  # Split out the list of fields
  field_list = fields.split(':')

  for field in field_list:

      process_ugrid(datapath, field, levels_name)

      # Only try to plot if we found some data for this field
      if n_levs > 0:
        make_figure(plotpath,field, timestep)

