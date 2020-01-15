THE FIRST TIME YOU RUN THE MMA TEST:

The download of MMA comes with the results of regressions of the test data set
in the data directory. After experimenting with the MMA examples, the size of 
this directory can be reduced by about 5MBs by deleting the results of those 
regressions. A quick procedure for accomplishing this is explained below.

Normally you would have accomplished this for your own models before using 
MMA. This work may have occurred over a long period of time, perhaps days, 
months or years; and the individual regressions might have required substantial
processing time ranging from minutes to many days. It is important that those
regressions were generated with an application that wrote underscore files 
compatible with this version of MMA. If they were not, the options are to rerun
the regressions with a version of the code that creates compatible files (this
is preferable if the regressions did not require a lot of time), or to alter
the underscore files (this could be challenging if there are many changes). 
If the regressions are to be rerun, it is effective to start with the optimal 
values so the regression will converge on the first iteration.

RUNNING MMA EXAMPLES:

In the test directory

test-win\01-Run_MMA

Execute “mma_minimal.bat” or “mma_extensive.bat” to obtain a minimal (default) 
or extensive analysis of the 15 models without prior information in the data 
directory as specified in the MMA input files “testmma_min” and “testmma”. 
The root name of the output files will be testmma_min and testmma, 
respectively.

Execute “mma_modprob.bat” to obtain an analysis of the 15 models without prior 
information in the data based solely on initial model probabilities as 
specified in the MMA input file “mma_modprob”. The root name of the output files 
will be mma_modprob. For this example the posterior probabilities resulting 
from the AICc analysis of “mma_extensive.bat” were used as prior model 
probabilities so the results of the two analyses are nearly identical.

Execute “mma_extensive_prior.bat” to obtain an analysis of the 5 models with 
prior in the data directory as specified in the MMA input file “testmma_pri”. 
The root name of the output files will be testmma_pri.

Execute "clean_mma.bat" to delete the results of the example problems.

DELETING RESULTS OF THE REGRESSIONS:

After experimenting with the MMA examples, the size of the data directory can 
be reduced by about 5MBs by deleting the results of those regressions. To 
accomplish this, execute:
 
test-win\02-Clean_Regressions\CleanRegression.bat

REGENERATING THE REGRESSION RESULTS

Regenerating the regression results takes a few minutes. To accomplish this, 
open the test-win\03-Run_Regressions\ directory and Run each batch file.
 
On some computers all (or any number of these) can be executed at the same time
by highlighting the batch files using your mouse, RIGHT clicking, selecting 
open, and choosing OK to the warning about opening a number of models at once 
if it pops up. If multiple regressions are executed simulataneously, many 
command windows will open, one for each model. Each will run a regression, 
simulate predictions for a predictive scenario, simulate predictions for a 
base case scenario, and run the code linear_uncertainty. The command windows 
will close when the executions are complete. If the computer cannot 
handle this many executions at once, it may shut down, so be sure to save all 
other work before attempting to run many regressions at once. There is also 
the possibility that when running all of the rgeressions at once on a computer 
that can not handle so many concurrent computationally intensive processes that 
the regression runs will not complete properly. If that should happen the 
MMA runs will fail and will show a message that the files being sought in the 
regression folders were not found.  The solution would be to run one regression 
batch file at a time in the folder test-win\03-Run_Regressions\.

Once the regressions have been executed, MMA can be used to evaluate the 
results.

Later, to delete the regression files and recover the space on your hard disk, 
execute: test-win\02-Clean_Regressions\CleanRegression.bat







