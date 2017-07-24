#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# (c) Crown copyright 2017 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################

from __future__ import print_function

import re
import sys

from testframework import Test, TestEngine, TestFailed

##############################################################################
class one_of_each_test(Test):
  def __init__( self ):
    self._INJECT = 'one_of_each.nml'
    super(one_of_each_test, self).__init__( [sys.argv[1], self._INJECT] )

    self._precision = 0.0005

  def test( self, process ):
    out, err = process.communicate( )
    if process.returncode != 0:
      raise TestFailed( 'Unexpected failure of test executable: {code}' \
                        .format( code=process.returncode ) )

    expected = { 'angle_deg'   : 7.998,
                 'angle_rad'   : 0.1396,
                 'an_enum'     : 'second',
                 'closed_array': [0.2, 0.3, 0.4],
                 'open_array'  : [1, 2, 3, 4, 5],
                 'some_string' : 'chocolate teapot',
                 'whole_number': 13}
    seen = []
    variablePattern = re.compile( r'(.+?)\s*:\s*(.+)\s*' )
    listPattern = re.compile( r'(\'.*?\'|".*?"|[^ ]+)' )
    for line in out.splitlines():
      match = variablePattern.match( line )
      if match:
        variable = match.group(1).strip()
        seen.append( variable )
        value    = match.group(2).strip()

        listMatch = listPattern.findall( value )
        if isinstance(listMatch, list):
          value = []
          for item in listMatch:
            if item.startswith("'") or item.startswith('"'):
              value.append( item[1:-1] )
            else:
              value.append( item )

        if variable not in expected:
          message = 'Found unexpected variable "{variable}" in output'
          raise TestFailed( message.format( variable=variable ) )

        self._check( variable, expected[variable], value )

    additions = set(seen) - set(expected.keys())
    if len(additions) > 0:
      message = 'Addition variables found: {adds}'
      raise TestFailed( message.format( adds=', '.join( additions ) ) )

    return 'One of each configuration type loaded'

  def _check( self, name, expected, found ):
    if not isinstance( expected, list ):
      expected = [expected]

    if not isinstance( found, list ):
      found = [found]

    if len(found) != len(expected):
      message = 'Expected variable "{variable}" to hold "{expect}" but found "{found}", different number of values.'
      raise TestFailed( message.format( variable=name,
                                        expect=expected,
                                        found=found ) )

    for index in range( len(expected) ):
      if isinstance( expected[index], float):
        if abs(float(found[index]) - expected[index]) > self._precision:
          message = 'Expected index {index} of variable "{variable}" to be within {precision} of {expected} but found {found}'
          raise TestFailed( message.format( variable=name,
                                            index=index,
                                            precision=self._precision,
                                            expected=expected[index],
                                            found=found[index] ) )
      else:
        if found[index] != str(expected[index]):
          message = 'Expected index {index} of variable "{variable}" to hold "{expect}" but found "{found}"'
          raise TestFailed( message.format( variable=name,
                                            index=index,
                                            expect=expected[index],
                                            found=found[index] ) )

##############################################################################
if __name__ == '__main__':
  TestEngine.run( one_of_each_test() )
