!-----------------------------------------------------------------------------
! (c) Crown copyright 2018 Met Office. All rights reserved.
! The file LICENCE, distributed with this code, contains details of the terms
! under which the code may be used.
!-----------------------------------------------------------------------------
! @brief Interface to Socrates for illumination of the atmosphere

module illuminate_kernel_mod

use argument_mod,  only : arg_type, func_type, &
                          GH_FIELD, GH_INTEGER, GH_READ, GH_WRITE, GH_INC, &
                          CELLS, ANY_SPACE_1, ANY_SPACE_2, &
                          GH_BASIS, GH_QUADRATURE_XYoZ
use constants_mod, only : r_def, i_def
use kernel_mod,    only : kernel_type

implicit none
private
public :: illuminate_kernel_type
public :: illuminate_code

!------------------------------------------------------------------------------
! Public types
!------------------------------------------------------------------------------
! The type declaration for the kernel.
! Contains the metadata needed by the Psy layer.
type, extends(kernel_type) :: illuminate_kernel_type
  private
  type(arg_type) :: meta_args(9) = (/ &
    arg_type(GH_FIELD,   GH_WRITE, ANY_SPACE_1), & ! cos_zenith_angle      
    arg_type(GH_FIELD,   GH_WRITE, ANY_SPACE_1), & ! lit_fraction          
    arg_type(GH_FIELD,   GH_INC,   ANY_SPACE_1), & ! cos_zenith_angle_rts  
    arg_type(GH_FIELD,   GH_INC,   ANY_SPACE_1), & ! lit_fraction_rts      
    arg_type(GH_FIELD,   GH_INC,   ANY_SPACE_1), & ! stellar_irradiance_rts
    arg_type(GH_FIELD,   GH_INC,   ANY_SPACE_1), & ! sin_stellar_declination_rts
    arg_type(GH_FIELD,   GH_INC,   ANY_SPACE_1), & ! stellar_eqn_of_time_rts
    arg_type(GH_FIELD*3, GH_READ,  ANY_SPACE_2), & ! chi
    arg_type(GH_INTEGER, GH_READ              )  & ! timestep
    /)
  type(func_type) :: meta_funcs(1) = (/ &
    func_type(ANY_SPACE_2, GH_BASIS) &
    /)
  integer :: iterates_over = CELLS
  integer :: gh_shape = GH_QUADRATURE_XYoZ
! Having to use quadrature rather than evaluators due to restriction in
! PSyclone. This will be addressed in PSyclone issue #196  
contains
  procedure, nopass :: illuminate_code
end type

!------------------------------------------------------------------------------
! Constructors
!------------------------------------------------------------------------------

! Overload the default structure constructor for function space
interface illuminate_kernel_type
  module procedure illuminate_kernel_constructor
end interface

!------------------------------------------------------------------------------
! Contained functions/subroutines
!------------------------------------------------------------------------------
contains

function illuminate_kernel_constructor() result(self)
  implicit none
  type(illuminate_kernel_type) :: self
  return
end function illuminate_kernel_constructor

! @param[in]     nlayers                     Number of layers
! @param[out]    cos_zenith_angle            Cosine of the stellar zenith angle
! @param[out]    lit_fraction                Lit fraction of the timestep
! @param[in,out] cos_zenith_angle_rts        Cosine of the stellar zenith angle
! @param[in,out] lit_fraction_rts            Lit fraction of the timestep
! @param[in,out] stellar_irradiance_rts      Stellar irradiance at the planet
! @param[in,out] sin_stellar_declination_rts Stellar declination
! @param[in,out] stellar_eqn_of_time_rts     Stellar equation of time
! @param[in]     chi_1                       Coordinates
! @param[in]     chi_2                       Coordinates
! @param[in]     chi_3                       Coordinates
! @param[in]     timestep                    Timestep number
! @param[in]     ndf_2d     No. of degrees of freedom per cell for 2d space
! @param[in]     undf_2d    No. unique of degrees of freedom for 2d space
! @param[in]     map_2d     Dofmap for cell at base of column for 2d space
! @param[in]     ndf_chi    No. degrees of freedom per cell for chi space
! @param[in]     undf_chi   No. unique of degrees of freedom for chi space
! @param[in]     map_chi    Dofmap for cell at base of column for chi space
! @param[in]     chi_basis  Weights
! @param[in]     nqp_h
! @param[in]     nqp_v
! @param[in]     wqp_h
! @param[in]     wqp_v
subroutine illuminate_code(nlayers,                     &
                           cos_zenith_angle,            &
                           lit_fraction,                &
                           cos_zenith_angle_rts,        &
                           lit_fraction_rts,            &
                           stellar_irradiance_rts,      &
                           sin_stellar_declination_rts, &
                           stellar_eqn_of_time_rts,     &
                           chi_1, chi_2, chi_3,         &
                           timestep,                    &
                           ndf_2d, undf_2d, map_2d,     &
                           ndf_chi, undf_chi, map_chi,  &
                           chi_basis,                   &
                           nqp_h, nqp_v, wqp_h, wqp_v)

  use xios, only: xios_date, xios_get_current_date, &
    xios_date_get_day_of_year, xios_date_get_second_of_day
  use coord_transform_mod, only: xyz2llr
  use timestepping_config_mod, only: dt
  use radiation_config_mod, only: n_radstep
  use star_config_mod, only: stellar_constant
  use orbit_config_mod, only:                                                &
    elements, elements_user, elements_earth_fixed,                           &
    elements_earth_secular_variation,                                        &
    spin, spin_user, spin_earth_day, spin_fixed_sun,                         &
    epoch, eccentricity, eccentricity_inc, arg_periapsis, arg_periapsis_inc, &
    obliquity, obliquity_inc, semimajor_axis, semimajor_axis_inc,            &
    mean_anomaly, mean_anomaly_inc, hour_angle, hour_angle_inc,              &
    fixed_zenith_angle, fixed_azimuth_angle, observer_lon, observer_lat
  use socrates_illuminate, only: illuminate,   &
    ip_elements_user, ip_elements_earth_fixed, &
    ip_elements_earth_secular_variation,       &
    ip_spin_user, ip_spin_earth_day, ip_spin_fixed_sun

  implicit none

  ! Arguments
  integer(i_def), intent(in) :: nlayers, timestep, nqp_h, nqp_v
  integer(i_def), intent(in) :: ndf_2d, ndf_chi
  integer(i_def), intent(in) :: undf_2d, undf_chi
  integer(i_def), intent(in) :: map_2d(ndf_2d)
  integer(i_def), intent(in) :: map_chi(ndf_chi)
  real(r_def),    intent(in) :: chi_basis(1,ndf_chi,nqp_h,nqp_v)
  real(r_def),    intent(in) :: chi_1(undf_chi)
  real(r_def),    intent(in) :: chi_2(undf_chi)
  real(r_def),    intent(in) :: chi_3(undf_chi)
  real(r_def),    intent(in) :: wqp_h(nqp_h)
  real(r_def),    intent(in) :: wqp_v(nqp_v)

  real(r_def), dimension(undf_2d), intent(out):: &
    cos_zenith_angle, lit_fraction
  real(r_def), dimension(undf_2d), intent(inout):: &
    cos_zenith_angle_rts, lit_fraction_rts, stellar_irradiance_rts, &
    sin_stellar_declination_rts, stellar_eqn_of_time_rts

  ! Local variables for the kernel
  integer(i_def), parameter :: n_profile = 1
  integer(i_def) :: i_elements, i_spin
  integer(i_def) :: df
  real(r_def) :: x, y, z, r

  type(xios_date) :: datetime
  integer(i_def) :: current_year, day_of_year
  real(r_def) :: second_of_day
  real(r_def) :: lat(n_profile), lon(n_profile)


  ! Get latitude and longitude
  x = 0.0_r_def
  y = 0.0_r_def
  z = 0.0_r_def
  do df = 1, ndf_chi
      x = x + chi_1(map_chi(df))*chi_basis(1,df,1,1)
      y = y + chi_2(map_chi(df))*chi_basis(1,df,1,1)
      z = z + chi_3(map_chi(df))*chi_basis(1,df,1,1)
  end do
  call xyz2llr(x,y,z,lon(1),lat(1),r)

  ! Get date and time
  call xios_get_current_date(datetime)
  current_year  = int(datetime%year, i_def)
  day_of_year   = int(xios_date_get_day_of_year(datetime), i_def) + 1_i_def
  second_of_day = real(xios_date_get_second_of_day(datetime), r_def)

  ! Set orbital elements
  select case (elements)
  case (elements_user)
    i_elements = ip_elements_user
  case (elements_earth_fixed)
    i_elements = ip_elements_earth_fixed
  case (elements_earth_secular_variation)
    i_elements = ip_elements_earth_secular_variation
  case default
    i_elements = ip_elements_earth_fixed
  end select

  ! Set motion of sun across the sky
  select case (spin)
  case (spin_user)
    i_spin = ip_spin_user
  case (spin_earth_day)
    i_spin = ip_spin_earth_day
  case (spin_fixed_sun)
    i_spin = ip_spin_fixed_sun
  case default
    i_spin = ip_spin_earth_day
  end select

  if (mod(timestep-1_i_def, n_radstep) == 0) then
    ! Calculate parameters for external illumination of the atmosphere
    ! over the radiation timestep
    call illuminate(                                                        &
      l_stellar_position       = .true.,                                    &
      l_stellar_angle          = .true.,                                    &
      n_profile                = n_profile,                                 &
      i_elements               = i_elements,                                &
      i_spin                   = i_spin,                                    &
      year                     = current_year,                              &
      day_of_year              = day_of_year,                               &
      second_of_day            = second_of_day,                             &
      length_of_timestep       = dt*real(n_radstep, r_def),                 &
      epoch                    = epoch,                                     &
      eccentricity             = eccentricity,                              &
      eccentricity_inc         = eccentricity_inc,                          &
      arg_periapsis            = arg_periapsis,                             &
      arg_periapsis_inc        = arg_periapsis_inc,                         &
      obliquity                = obliquity,                                 &
      obliquity_inc            = obliquity_inc,                             &
      semimajor_axis           = semimajor_axis,                            &
      semimajor_axis_inc       = semimajor_axis_inc,                        &
      mean_anomaly             = mean_anomaly,                              &
      mean_anomaly_inc         = mean_anomaly_inc,                          &
      hour_angle               = hour_angle,                                &
      hour_angle_inc           = hour_angle_inc,                            &
      fixed_zenith_angle       = fixed_zenith_angle,                        &
      fixed_azimuth_angle      = fixed_azimuth_angle,                       &
      observer_lat             = observer_lat,                              &
      observer_lon             = observer_lon,                              &
      latitude                 = lat,                                       &
      longitude                = lon,                                       &
      stellar_constant         = stellar_constant,                          &
      sin_stellar_declination  = sin_stellar_declination_rts(map_2d(1)),    &
      stellar_eqn_of_time      = stellar_eqn_of_time_rts(map_2d(1)),        &
      cos_zenith_angle         = cos_zenith_angle_rts(map_2d(1):map_2d(1)), &
      lit_fraction             = lit_fraction_rts(map_2d(1):map_2d(1)),     &
      stellar_irradiance       = stellar_irradiance_rts(map_2d(1):map_2d(1)) )
  end if

  if (n_radstep == 1) then
    cos_zenith_angle(map_2d(1):map_2d(1)) &
      = cos_zenith_angle_rts(map_2d(1):map_2d(1))
    lit_fraction(map_2d(1):map_2d(1)) &
      = lit_fraction_rts(map_2d(1):map_2d(1))
  else
    ! Calculate parameters for external illumination of the atmosphere
    ! over the model timestep
    call illuminate(                                                     &
      l_stellar_angle          = .true.,                                 &
      n_profile                = n_profile,                              &
      i_spin                   = i_spin,                                 &
      second_of_day            = second_of_day,                          &
      length_of_timestep       = dt,                                     &
      hour_angle_inc           = hour_angle_inc,                         &
      fixed_zenith_angle       = fixed_zenith_angle,                     &
      fixed_azimuth_angle      = fixed_azimuth_angle,                    &
      latitude                 = lat,                                    &
      longitude                = lon,                                    &
      sin_stellar_declination  = sin_stellar_declination_rts(map_2d(1)), &
      stellar_eqn_of_time      = stellar_eqn_of_time_rts(map_2d(1)),     &
      cos_zenith_angle         = cos_zenith_angle(map_2d(1):map_2d(1)),  &
      lit_fraction             = lit_fraction(map_2d(1):map_2d(1)) )
  end if

end subroutine illuminate_code

end module illuminate_kernel_mod
