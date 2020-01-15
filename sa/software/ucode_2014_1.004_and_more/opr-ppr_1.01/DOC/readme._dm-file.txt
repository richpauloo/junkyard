The _dm file from ucode has been modified for
all of the OPR-PPR runs, as follows.

In the regression run with UCODE_2005, prior information
was specified on K_RB and VK_CB to allow the
regression run to converge. Thus, in the 
_dm file, "NUMBER PRIOR" equals 2.

In the OPR-PPR run, we want to test the worth
of data collection in the absence of this prior
information, so we want to calculate the prediction
standard deviations without the prior information
on K_RB and VK_CB. Thus, the _supri and _wtpri files
from the UCODE run are not specified as input files
for OPR-PPR, and "NUMBER PRIOR" in the _dm file
has been changed to 0.
