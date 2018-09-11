!-----------------------------------------------------------------------
! Flexible Snow Model (FSM version 2.0)
!
! Richard Essery
! School of GeoSciences
! University of Edinburgh
!-----------------------------------------------------------------------
program FSM2

use DIAGNOSTICS, only: &
  Nave              ! Number of timesteps in average outputs

use DRIVING, only: &
  year,              &! Year
  month,             &! Month of year
  day,               &! Day of month
  hour                ! Hour of day

use DATAIO

implicit none

integer :: itime    ! Timestep counter

! Setup model

call SETUP

! Loop over timesteps

do itime = istart, istop

   call DRIVE(itime)

   call PHYSICS

   if (hour .eq. 0.0) then

      call OUTPUT
      
      !print *, year, month, day

   end if

end do

! Close netcdfs

call check( nf90_close(SWE_file%ncid) )
call check( nf90_close(HS_file%ncid) )
call check( nf90_close(alb_file%ncid) )
call check( nf90_close(Gsurf_file%ncid) )
call check( nf90_close(Hatmo_file%ncid) )
call check( nf90_close(Latmo_file%ncid) )
call check( nf90_close(Melt_file%ncid) )
call check( nf90_close(Rnet_file%ncid) )
call check( nf90_close(Rof_file%ncid) )
call check( nf90_close(Tsurf_file%ncid) )
call check( nf90_close(Tsoil_file%ncid) )


end program
