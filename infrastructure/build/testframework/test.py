#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################

from __future__ import print_function

from abc import ABCMeta, abstractmethod
import collections
import math
import os
import subprocess
import sys
import tempfile

##############################################################################
class AbstractTest(object):
  '''
  Base functionality of a test. This class is responsible for actually
  running a test and handing the result on to the handler method.
  '''
  __metaclass__ = ABCMeta

  def __init__( self, executable ):
    '''
    Constructor.

    parameter executable - Executable command in list form.
    '''
    self._executable = executable

  @abstractmethod
  def test( self, returncode, out, err ):
    '''
    Examines the result of running the test for correctness.

    parameter returncode - OS level result of running the executable.
    parameter out        - String holding standard out from executable.
    parameter err        - String holding standard error from executable.

    Beware, liable to memory exhaustion in the face of large amounts of
    output.

    Throw TestFailed object on finding a mistake.
    '''
    pass

  def performTest( self ):
    '''
    Runs the executable and passes results to handler method.
    '''
    process = subprocess.Popen( self._executable,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE )
    out, err = process.communicate()
    return self.test( process.returncode,
                      self.filterOut( out ),
                      self.filterErr( err ) )

  def filterOut( self, out ):
    '''
    Processes the standard output string.

    The default implementation simply returns the string unprocessed.
    Only override if you need to filter.
    '''
    return out

  def filterErr( self, err ):
    '''
    Processes the standard error string.

    The default implementation simply returns the string unprocessed.
    Only override if you need to filter.
    '''
    return err

##############################################################################
class Test(AbstractTest):
  '''
  Base for serial tests.
  '''
  __metaclass__ = ABCMeta

  def __init__( self, command=sys.argv[1] ):
    if type(command) is not list:
      command = [command]
    super(Test, self).__init__( command )

##############################################################################
class MpiTest(AbstractTest):
  '''
  Base for parallel tests.
  '''
  __metaclass__ = ABCMeta

  def __init__( self, command=sys.argv[1], cores=4 ):
    self._cores = cores

    if type(command) is not list:
      command = [command]

    commandString = ' '.join( command )
    commandName   = os.path.basename( command[0] )
    self._startTag = 'Start {name}'.format( name=commandName )
    self._doneTag  = 'Done {name}'.format( name=commandName )

    filedescriptor, self._scriptname = tempfile.mkstemp( prefix='run-',
                                                         suffix='.sh',
                                                         text=True )
    handle = os.fdopen( filedescriptor, 'w' )
    print( '#!/bin/sh', file=handle )
    print( 'echo {tag}'.format( tag=self._startTag ),
           file=handle )
    print( commandString, file=handle )
    print( 'result=$?', file=handle )
    print( 'echo {tag}'.format( tag=self._doneTag ),
           file=handle )
    print( 'sync', file=handle )
    print( 'exit $result', file=handle )
    handle.close()
    os.chmod( self._scriptname, 0700 )

    mpiCommand = ['mpiexec', '-n', str(self._cores), self._scriptname]
    super(MpiTest, self).__init__( mpiCommand )

  def __del__( self ):
    os.remove( self._scriptname )

  def filterOut( self, out ):
    '''
    Strips MPI cruft from standard out.
    '''
    newOut = []
    state = 'spinup'
    processesRunning = 0
    for line in out.splitlines():
      if state == 'spinup':
        if line == self._startTag:
          processesRunning += 1
          if processesRunning == self._cores:
            state = 'spindown'
      elif state == 'spindown':
        if line == self._doneTag:
          processesRunning -= 1
          if processesRunning == 0:
            state = 'done'
        else: # line does not start with 'Done '
          newOut.append( line )

    return '\n'.join( newOut )

  def filterErr( self, err ):
    '''
    Strip MPI cruft from standard out.
    '''
    newErr = err.splitlines()

    return '\n'.join( newErr[:-self._cores] )

##############################################################################
class EsmfTest(MpiTest):
  '''
  Base for ESMF parallel tests.
  '''
  __metaclass__ = ABCMeta

  def __init__( self, command=sys.argv[1], cores=4 ):
    super(EsmfTest, self).__init__( command, cores )
    self._esmfLog = collections.defaultdict( None )

  def getEsmfLog( self, process=0 ):
    '''
    Returns the log file generated by ESMF on a particular MPI process.

    Arguments:

      process - Integer - The process for which a log file is desired.
                          Defaults to zero.
    '''
    return self._esmfLog[process]

  def performTest( self ):
    '''
    Runs the executable and handles log files.
    '''
    # Remove any existing log files
    for filename in os.listdir( '.' ):
      if filename.startswith( 'PET' ):
        os.remove( filename )

    process = subprocess.Popen( self._executable,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE )
    out, err = process.communicate()

    for number in range(0, self._cores):
      width = int( math.floor( math.log10( self._cores ) ) ) + 1
      filenameFormat = 'PET{{number:0{width}d}}.ESMF_LogFile'
      filename = filenameFormat.format( width=width ).format( number=number )
      with open( filename, 'r' ) as handle:
        self._esmfLog[number] = handle.read()

    return self.test( process.returncode,
                      self.filterOut( out ),
                      self.filterErr( err ) )
