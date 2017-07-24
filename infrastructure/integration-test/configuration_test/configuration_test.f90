!-----------------------------------------------------------------------------
! (C) Crown copyright 2017 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------

program configuration_test

  use ESMF,                        only : ESMF_Finalize,   &
                                          ESMF_Initialize, &
                                          ESMF_VM,         &
                                          ESMF_VMGet
  use iso_fortran_env,             only : error_unit, output_unit
  use one_of_each_test_config_mod, only : key_from_an_enum,                  &
                                      postprocess_one_of_each_test_namelist, &
                                          read_one_of_each_test_namelist,    &
                                          angle_deg,                         &
                                          angle_rad,                         &
                                          an_enum,                           &
                                          closed_array,                      &
                                          max_array_size,                    &
                                          open_array,                        &
                                          some_string,                       &
                                          whole_number

  implicit none

  integer,      parameter :: file_unit = 13
  character(*), parameter :: filename = 'one_of_each.nml'

  type(ESMF_VM) :: vm
  integer       :: rank
  integer       :: condition
  character(30) :: format_string

  call ESMF_Initialize( vm=vm, rc=condition )
  if (condition /= 0) then
    write( error_unit, '("Failed to initialise ESMF")' )
    stop 1
  end if

  call ESMF_VMGet( vm, localPET=rank, rc=condition )
  if (condition /= 0) then
    write( error_unit, '("Failed to get rank from ESMF")' )
    stop 2
  end if

  open( file_unit, file=filename, iostat=condition )

  if (condition /= 0) then
    write( error_unit, '("Failed to open file: ",A)' ) filename
    stop 3
  end if

  call read_one_of_each_test_namelist( file_unit, vm ,rank )

  close( file_unit, iostat=condition )
  if (condition /= 0) then
    write( error_unit, '("Failed to close file: ", A)' ) filename
    stop 4
  end if

  call postprocess_one_of_each_test_namelist

  call ESMF_Finalize( rc=condition )
  if (condition /= 0) then
    write( error_unit, '("Failed to finalise ESMF")' )
    stop 5
  end if

  write( output_unit, '("angle_deg: ", E14.7)' ) angle_deg
  write( output_unit, '("angle_rad: ", E14.7)' ) angle_rad
  write( output_unit, '("an_enum: ", A)' ) key_from_an_enum( an_enum )
  write( format_string, &
         '("(""closed_array: "", ", I0, "E14.7)")' ) max_array_size
  write( output_unit, format_string ) closed_array
  write( format_string, '("(""open_array: "", ", I0, "I3)")' ) max_array_size
  write( output_unit, format_string ) open_array
  write( output_unit, '("some_string: ''", A, "''")' ) trim(some_string)
  write( output_unit, '("whole_number: ", I0)' ) whole_number

end program configuration_test
