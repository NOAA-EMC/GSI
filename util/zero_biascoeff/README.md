# Zero-out Bias Correction Coefficients

This utility reads in (operational) radiance and aircraft bias correction coefficient files and writes out zeroed values for the predictors.  This is desired when cold-starting an experiment at a resolution other than the operational resolution or for the use with FV3GFS.

### Compilation:
```sh
$> cd zero_biascoeff
$> mkdir -p build && cd build
$> cmake ..
$> make
```
will produce an executable ```zero_biascoeff.x```

### Inputs:
Link the (operational) non-zero bias correction coefficient files as inputs; e.g.
```sh
$> ln -s gdas.t00z.abias abias
$> ln -s gdas.t00z.abias_air abias_air
```

### Execution:
```sh
$> ./zero_biascoeff.x
```

### Outputs:
Zeroed out bias correction coefficient files will be produced:
```sh
$> ls -1 abias*.zeroed
abias.zeroed
abias_pc.zeroed
abias_air.zeroed
```

### Limitations:
The utility will only work correctly for current (circa FY16+) bias correction files, since the formatting of the files has changed.  Also, aircraft bias correction was first introduced in the FY16 implemtation.  To be able to use for files before this period, the ```format``` statement in the utility will need to be edited as appropriate.

### Last Updated:
Nov 15, 2017
Rahul.Mahajan
