# DEPRECATED

using NetCDF

filename = "data/params/param.nc"

frac_landuse = rand(221, 3)
canopy_height = 0 * ones(221, 3)

frac_landuse = frac_landuse ./ sum(frac_landuse, dims=2)

dim_space = collect(1:size(frac_landuse, 1))
dim_classes = collect(1:size(frac_landuse, 2))

nccreate(filename, "frac_landuse", "dim_space", dim_space, "dim_classes", dim_classes)
nccreate(filename, "canopy_height", "dim_space", dim_space, "dim_classes", dim_classes)

ncwrite(frac_landuse, filename, "frac_landuse")
ncwrite(canopy_height, filename, "canopy_height")

ncclose(filename)
