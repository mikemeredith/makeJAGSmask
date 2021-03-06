Construction of a habitat availability matrix for Spatially Explicit Capture-Recapture (SECR) models in JAGS or BUGS.

See [this blog post](http://mikemeredith.net/blog/1309_SECR_in_JAGS_patchy_habitat.htm) for the background to SECR with patchy habitat.

The trick is to look up the proposed AC location in a matrix (named `habMat` in the code) which indicates whether it is in good or bad habitat, and ensure that locations in bad habitat are always rejected. This look-up procedure is repeated for every animal in the augmented data set and for every iteration of the MCMC chain; so if you have 200 animals in the augmented data set and run 100,000 interations, that means 20 million look-ups. It makes sense to minimize the code needed for the look-up and do the complicated stuff before or after the MCMC run.

The suggested look-up procedure is to truncate the `x` and `y` coordinates of the proposed AC location (say `ACx` and `ACy`) and use these as indices into `habMat`: `habMat[trunc(ACx), trunc(ACy)]`.

For this to work, `ACx` and `ACy` need to be in units equal to the pixel width of habMat, with the south-west corner at (1, 1). The `habMat` matrix needs to be appropriately scaled, and aligned so that row number corresponds to Easting and column number to Northing. This is weird, as we think of rows as horizontal and columns as vertical; we number rows from top to bottom, while Northing works from S to N. If you use `image` to plot `habMat`, it will appear transposed and vertically flipped (east at the top, north on the right)!

The package has functions to prepare `habMat` and to convert trap coordinates to the proper scale, and to convert MCMC output back to the original coordinate system.
