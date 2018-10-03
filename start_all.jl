#!/bin/bash
# -*- mode: julia -*-
#=
export JULIA_LOAD_PATH=@:@v#.#:@stdlib
exec julia --startup-file=no
=#




# How to start this script:

# nice nohup ~/julia-0.7.0/bin/julia -p 12 "start_all.jl"


using NetCDF
using Dates
using Distributed


@everywhere function write_nlst(filename, mod_nlst)

    str = """
    &drive
     pathmet = \"$(mod_nlst["pathmet"])\"
     pathres = \"$(mod_nlst["pathres"])\"
     spatial_res = \"$(mod_nlst["spatial_res"])\"
     pathparam = \"$(mod_nlst["pathparam"])\"
    /
    &simperiod
     istart = $(mod_nlst["istart"])
     istop = $(mod_nlst["istop"])
    /
    """

    # Write file

    fid = open(filename, "w")

    write(fid, str)

    close(fid)

    return nothing

end


function get_simperiod(pathmet, stime, etime)

    # Find start and stop time
    
    time = ncread(joinpath(pathmet, "tair_1km.nc"), "time_str")
    
    time = DateTime.(time, "yyyy-mm-dd HH:MM:SS")
    
    istart = findall(time .== stime)[1]
    
    istop = findall(time .== etime)[1]

    return istart, istop

end


function run_fsm(pathmod, pathmet, pathparam, pathres, stime, etime)

    # # Delete results folder if existing

    # isdir(pathres) && rm(pathres, recursive=true)

    # Run model using parallel loop
    
    @sync @distributed for iexp in 1:32
       
        for spatial_res in reverse(["1km", "5km", "10km", "25km", "50km"])
            
            # Options for namelist file
    
            nlst = Dict("pathmet" => pathmet,
                        "pathres" => joinpath(pathres, "results_$(iexp)"),
                        "pathparam" => joinpath(pathparam, "params_$(spatial_res).nc"),
                        "spatial_res" => spatial_res,
                        "istart" => istart,
                        "istop" => istop)
            
            # Create results folder
    
            mkpath(nlst["pathres"])
    
            # Create temporary namelist file
    
            nlst_file = tempname()
    
            write_nlst(nlst_file, nlst)
    
            # Run model for selected configuration
    
            fsmbin = joinpath(pathmod, "bin", "FSM_$(iexp)")
    
            run(pipeline(`$fsmbin`, stdin = nlst_file))
    
            # Delete temporary namelist file
    
            isfile(nlst_file) && rm(nlst_file)
    
            # Print progress
    
            @info "Finished experiment $(iexp) and resolution $(spatial_res)"
    
        end
        
    end

end


# Simulation period

pathmod = "/home/jmg/projects/FSM2"
pathmet = "/data04/jmg/fsm_simulations/netcdf/forcings_st/"

stime = DateTime(2008, 9, 1, 3, 0, 0)
etime = DateTime(2011, 9, 1, 0, 0, 0)

istart, istop = get_simperiod(pathmet, stime, etime)

@show istart
@show istop


# Run the model using forest

if true

    pathparam = "/data04/jmg/fsm_simulations/netcdf/params_forest/"
    pathres = "/data04/jmg/fsm_simulations/netcdf/fsmres_forest/"

    run_fsm(pathmod, pathmet, pathparam, pathres, stime, etime)

end


# Run the model omitting forest

if false

    pathparam = "/data04/jmg/fsm_simulations/netcdf/params_open/"
    pathres = "/data04/jmg/fsm_simulations/netcdf/fsmres_open/"

    run_fsm(pathmod, pathmet, pathparam, pathres, stime, etime)

end

