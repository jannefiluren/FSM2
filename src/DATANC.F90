
module DATAIO

  use netcdf

  type metfile
     character (len = 150) :: file_name
     character (len = 150) :: var_name
     character (len = 20) :: spatial_id_att
     integer :: ncid
     integer :: dim_timeid, dim_spaceid
     integer :: ntime, nspace
     integer :: varid
     integer :: timeid
     integer, allocatable :: spatial_id(:)
  end type metfile

  
  type resfile
     character (len = 150) :: file_name
     character (len = 150) :: var_name
     integer :: ncid
     integer :: dim_timeid, dim_spaceid, dim_ymdh
     integer :: nspace
     integer :: varid
     integer :: timeid
  end type resfile


  type paramfile
    character (len = 150) :: file_name
    character (len = 150) :: var_name
    integer :: ncid
    integer :: dim_space, dim_classes
    integer :: nspace, nclasses
    integer :: frac_landuse_id
    integer :: lai_landuse_id
    integer :: hcan_landuse_id
  end type


  type(metfile) SW_file
  type(metfile) LW_file
  type(metfile) Sf_file
  type(metfile) Rf_file
  type(metfile) Ta_file
  type(metfile) RH_file
  type(metfile) Ua_file
  type(metfile) Ps_file

  
  type(resfile) SWE_file
  type(resfile) HS_file
  type(resfile) alb_file
  type(resfile) Gsurf_file
  type(resfile) Hatmo_file
  type(resfile) Latmo_file
  type(resfile) Melt_file
  type(resfile) Rnet_file
  type(resfile) Rof_file
  type(resfile) Tsurf_file
  type(resfile) Tsoil_file

  type(paramfile) Param_file

  integer istart, istop


contains


  subroutine check(status)

    integer, intent(in) :: status

    if (status /= nf90noerr) then
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if

  end subroutine check

  
  subroutine create_resfile(fres, fmet)

    implicit none

    type(resfile), intent(inout) :: fres

    type(metfile), intent(in) :: fmet

    integer :: tmpid

    ! Create netcdf

    call check( nf90_create(fres%file_name, nf90_clobber, fres%ncid) )

    ! Create dimensions

    call check( nf90_def_dim(fres%ncid, "dim_space", fmet%nspace, fres%dim_spaceid) )

    call check( nf90_def_dim(fres%ncid, "dim_time", nf90_unlimited, fres%dim_timeid) )

    call check( nf90_def_dim(fres%ncid, "ymdh", 4, fres%dim_ymdh) )
    
    ! Define variables

    call check( nf90_def_var(fres%ncid, fres%var_name, NF90_REAL, (/fres%dim_spaceid, fres%dim_timeid/), fres%varid) )

    call check( nf90_def_var(fres%ncid, "time_array", NF90_REAL, (/fres%dim_ymdh, fres%dim_timeid/), fres%timeid) )

    call check( nf90_def_var(fres%ncid, "id", NF90_REAL, fres%dim_spaceid, tmpid) )
        
    ! Write attribute of spatial identifier
    
    call check( nf90_put_att(fres%ncid, tmpid, "id", fmet%spatial_id_att) )

    ! End definitions

    call check( nf90_enddef(fres%ncid) )

    ! Write spatial identifier

    call check( nf90_put_var(fres%ncid, tmpid, fmet%spatial_id) )

    ! Store space dimension

    fres%nspace = fmet%nspace

  end subroutine create_resfile


  subroutine get_paraminfo(file)

    implicit none

    type(paramfile), intent(inout) :: file

    ! Open netcdf

    call check(nf90_open(file%file_name, nf90_nowrite, file%ncid))

    ! Read dimensions

    call check(nf90_inq_dimid(file%ncid, "dim_space", file%dim_space))

    call check(nf90_inq_dimid(file%ncid, "dim_classes", file%dim_classes))

    call check(nf90_inquire_dimension(file%ncid, file%dim_space, len = file%nspace))

    call check(nf90_inquire_dimension(file%ncid, file%dim_classes, len = file%nclasses))

    ! Read variable id

    call check(nf90_inq_varid(file%ncid, "frac_landuse", file%frac_landuse_id))

    call check(nf90_inq_varid(file%ncid, "lai_landuse", file%lai_landuse_id))

    call check(nf90_inq_varid(file%ncid, "hcan_landuse", file%hcan_landuse_id))
  
  end subroutine


  subroutine get_metinfo(file)

    implicit none

    type(metfile), intent(inout) :: file

    integer :: tmpid

    ! Open netcdf

    call check(nf90_open(file%file_name, nf90_nowrite, file%ncid))

    ! Read size of time and space dimensions

    call check(nf90_inq_dimid(file%ncid, "dim_time", file%dim_timeid))

    call check(nf90_inq_dimid(file%ncid, "dim_space", file%dim_spaceid))

    call check(nf90_inquire_dimension(file%ncid, file%dim_timeid, len = file%ntime))

    call check(nf90_inquire_dimension(file%ncid, file%dim_spaceid, len = file%nspace))

    ! Read variable id

    call check(nf90_inq_varid(file%ncid, file%var_name, file%varid))

    call check(nf90_inq_varid(file%ncid, "time_array", file%timeid))

    ! Read spatial id (special reference to different grid resolutions)

    allocate(file%spatial_id(file%nspace))

    call check(nf90_inq_varid(file%ncid, "id", tmpid))

    call check(nf90_get_var(file%ncid, tmpid, file%spatial_id))

    ! Read attribute of spatial id
    
    call check(nf90_inq_varid(file%ncid, "dim_space", tmpid))

    call check(nf90_get_att(file%ncid, tmpid, "id", file%spatial_id_att))
    
  end subroutine get_metinfo

  
  subroutine write_var(fres, data, time, ntime)
    
    implicit none

    type(resfile), intent(in) :: fres

    real, intent(in) :: data(:)
    
    real, intent(in) :: time(:,:)

    integer, intent(in) :: ntime

    ! Write time array

    call check( nf90_put_var(fres%ncid, fres%timeid, time, start = (/1,ntime/), count = (/4,1/)) )

    ! Write variables

    call check( nf90_put_var(fres%ncid, fres%varid, data, start = (/1,ntime/), count = (/fres%nspace,1/)) )

  end subroutine write_var

end module DATAIO

