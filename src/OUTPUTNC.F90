!-----------------------------------------------------------------------
! Write output
!-----------------------------------------------------------------------

subroutine OUTPUT

use DRIVING, only: &
  year,              &! Year
  month,             &! Month of year
  day,               &! Day of month
  hour                ! Hour of day

use DIAGNOSTICS, only: &
  diags,             &! Cumulated diagnostics
  Nave,              &! Number of timesteps in average outputs
  Ndiags,            &! Number of averaged diagnostics
  SWin,              &! Cumulated incoming solar radiation (J/m^2)
  SWout               ! Cumulated reflected solar radiation (J/m^2)

use GRID, only: &
  Nx,Ny               ! Grid dimensions

use STATE_VARIABLES, only: &
  Ds,                &! Snow layer thicknesses (m)
  Sice,              &! Ice content of snow layers (kg/m^2)
  Sliq,              &! Liquid content of snow layers (kg/m^2)
  Sveg                ! Snow mass on vegetation (kg/m^2)

use AVERAGED_OUTPUTS, only: &
  SWE_mean,          &
  HS_mean,           &
  alb_mean,          &
  Gsurf_mean,        &     
  Hatmo_mean,        &
  Latmo_mean,        &
  Melt_mean,         &
  Rnet_mean,         &
  Rof_mean,          &
  Tsurf_mean,        &
  Tsoil_mean

use LANDUSE_PARAMETERS, only: &
  farea

use DATAIO

implicit none

integer :: &
  i,j,               &! Point counters
  ntime               ! Time dimension size

real :: &
  alb(Nx,Ny),        &! Effective albedo
  snowdepth(Nx,Ny),  &! Snow depth (m)
  SWE(Nx,Ny),        &! Snow water equivalent (kg/m^2) 
  time(4,1)           ! Temporary variable for time

! Handle time and dimensions

call check( nf90_inquire_dimension(SWE_file%ncid, SWE_file%dim_timeid, len = ntime) )

ntime = ntime + 1

time(1,1) = real(year)
time(2,1) = real(month)
time(3,1) = real(day)
time(4,1) = hour

! Compute output variables

do j = 1, Ny
   do i = 1, Nx
      snowdepth(i,j) = sum(Ds(:,i,j))
      SWE(i,j) = sum(Sice(:,i,j)) + sum(Sliq(:,i,j))
   end do
end do

! Compute output averages

do j = 1, Ny
   do i = 1, Nx
      if (SWin(i,j) > 0) then
         alb(i,j) = SWout(i,j) / SWin(i,j)
      else
         alb(i,j) = -9
      end if
   end do
end do
diags(:,:,:) = diags(:,:,:) / Nave

! Compute averages for landuse classes

SWE_mean = 0
HS_mean = 0
alb_mean = 0
Gsurf_mean = 0   
Hatmo_mean = 0
Latmo_mean = 0
Melt_mean = 0
Rnet_mean = 0
Rof_mean = 0
Tsurf_mean = 0
Tsoil_mean = 0

do i = 1, Nx
  do j = 1, Ny
    SWE_mean(i) = SWE_mean(i) + SWE(i,j) * farea(i,j)
    HS_mean(i) = HS_mean(i) + snowdepth(i,j) * farea(i,j)
    alb_mean(i) = alb_mean(i) + alb(i,j) * farea(i,j)
    Gsurf_mean(i) = Gsurf_mean(i) + diags(i,j,1) * farea(i,j)
    Hatmo_mean(i) = Hatmo_mean(i) + diags(i,j,2) * farea(i,j)
    Latmo_mean(i) = Latmo_mean(i) + diags(i,j,4) * farea(i,j)
    Melt_mean(i) = Melt_mean(i) + diags(i,j,6) * farea(i,j)
    Rnet_mean(i) = Rnet_mean(i) + diags(i,j,7) * farea(i,j)
    Rof_mean(i) = Rof_mean(i) + diags(i,j,8) * farea(i,j)
    Tsurf_mean(i) = Tsurf_mean(i) + diags(i,j,10) * farea(i,j)
    Tsoil_mean(i) = Tsoil_mean(i) + diags(i,j,11) * farea(i,j)
  end do
end do

! Write variables to netcdfs

call write_var(SWE_file, SWE_mean, time, ntime)
call write_var(HS_file, HS_mean, time, ntime)
call write_var(alb_file, alb_mean, time, ntime)
call write_var(Gsurf_file, Gsurf_mean, time, ntime)
call write_var(Hatmo_file, Hatmo_mean, time, ntime)
call write_var(Latmo_file, Latmo_mean, time, ntime)
call write_var(Melt_file, Melt_mean, time, ntime)
call write_var(Rnet_file, Rnet_mean, time, ntime)
call write_var(Rof_file, Rof_mean, time, ntime)
call write_var(Tsurf_file, Tsurf_mean, time, ntime)
call write_var(Tsoil_file, Tsoil_mean, time, ntime)

! Reset output averages

diags(:,:,:) = 0
SWin(:,:) = 0
SWout(:,:) = 0

end subroutine OUTPUT
