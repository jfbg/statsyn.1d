 -N TestJob
#PBS -l nodes=1:ppn=8
#PBS -q default
#PBS -V
#PBS -m e
#PBS -W x="PARTITION:sw121"
#PBS -M <YOUR SUNETID>@stanford.edu
#PBS -e /data/lawrence2/jguertin/PHONON1D/TESTING/test.err
#PBS -o /data/lawrence2/jguertin/PHONON1D/OUTPUT/test.out
#
#
#
cd $PBS_O_WORKDIR
#
mpirun hellompi >> OUT
