With statsyn_TRACK_iso.f90

You need to make sure that there is a layer at the base of the scattering layer, you do not need to change the velocity or anything, but a line in the velocity model with a depth that is the same as the base of the scattering layer must be in there.

==> This is not true for statsyn_global.f90 anymore as the VEL_MODEL_CHECK subroutine add a velocity at the base of the scattering layer if one is not present.