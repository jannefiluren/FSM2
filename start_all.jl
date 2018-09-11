
# How to start this script:

# nohup ~/julia-0.7.0/bin/julia -p 4 "start_all.jl"

using NetCDF
using Dates
using Distributed

@everywhere using DataFrames

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


# Settings

pathmod = "/home/jmg/projects/FSM2"
pathmet = "/data02/Ican/vic_sim/fsm_simulations/netcdf/forcings_st/"
pathparam = "/data02/Ican/vic_sim/fsm_simulations/netcdf/params/"
pathres = "/data02/Ican/vic_sim/fsm_simulations/netcdf/fsmres/"


# Find start and stop time

time = ncread(joinpath(pathmet, "tair_1km.nc"), "time_str")

time = DateTime.(time, "yyyy-mm-dd HH:MM:SS")

istart = findall(time .== DateTime(2008, 9, 1, 3, 0, 0))[1]

istop = findall(time .== DateTime(2011, 9, 1, 0, 0, 0))[1]

@show istart
@show istop


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






#=
@sync @parallel for iexp in 1:size(df_simulation,1)
    
    info("Running model for experiment $(iexp)")

    # Options from data frame

    mod_cfg = df_simulation[:mod_cfg][iexp]
    
    spatial_res = df_simulation[:spatial_res][iexp]
    
    # Options for namelist file

    nlst = Dict("pathmet" => pathmet,
                "pathres" => joinpath(pathres, "results_$(iexp)"),
                "spatial_res" => spatial_res)
    
    # Create results folder

    mkpath(nlst["pathres"])

    # Create temporary namelist file

    nlst_file = tempname()

    write_nlst(nlst_file, nlst)

    # Run model for selected configuration

    bin = joinpath(pathmod, "bin", "FSM_$(mod_cfg)")

    run(pipeline(`$bin`, stdin = nlst_file))

    # Delete temporary namelist file

    isfile(nlst_file) && rm(nlst_file)
   
end
=#



