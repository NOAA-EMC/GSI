## GSI/feature/jedi_gfsv16: a version of GSI to generate GeoVaLs for JEDI UFO development

This is a modified version of the Gridpoint Statistical Interpolation (GSI) software that includes the writing out of interpolated model background fields and other variables needed to run the JEDI Unified Forward Operator (UFO) without needing the full 3D model state.

### To build
Just like standard GSI, on supported platforms:
- git clone --recursive -b feature/jedi_gfsv16 https://github.com/CoryMartin-NOAA/GSI.git GSI_forJEDI_GFSv16
- cd GSI_forJEDI_GFSv16/ush
- ./build_all_cmake.sh

Verify you have global_gsi.x in the GSI_forJEDI_GFSv16/exec directory.

### To generate GSI netCDF diag files with extra model fields included

### To convert these GSI netCDF diag files to JEDI UFO GeoVaLs files and IODA observation files

### Summary of changes made between NOAA-EMC/GSI/release/gfsda.v16.0.0 and this branch feature/jedi_gfsv16

### Example of where/how to add additional fields to GSI/ioda-converters
