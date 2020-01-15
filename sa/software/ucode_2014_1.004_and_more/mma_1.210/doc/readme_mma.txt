README_MMA.TXT


                 MMA - Version: Version 1.210 9/1/2011
                    Multi-Model Analysis Code


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of MMA is packaged for personal computers using one of the 
Microsoft Windows operating systems.  An executable file for personal 
computers is provided as well as the source code.  The executable
file was created using the MicroSoft Visual Studio 2005, Intel Fortran 
Compiler 11.1.065. The source code can be compiled to run on other computers. 

See the file doc\mma.txt for a description of this software. Instructions 
for installation, execution, and testing are provided below.


                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

                        mma_1.210.exe
                  
For UNIX the mma_1.210.tgz file is provided     

Numbers at the end of mma_#.### are changed to identify different 
modifications. (e.g. mma_1.000 became mma_1.100 after the 
first modification)

The distribution file contains:

          Compiled runfile and source code for MMA.
          Supplementary MMA documentation in PDF and text files.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows the user to specify the directory in which the files should
be restored (the default is C:\WRDAPP.  These installation instructions assume 
that the files are restored into directory C:\WRDAPP.  The following directory 
structure will be created in C:\WRDAPP:

   |
   |--mma_#.###
   |    |--bin            ; Compiled executables for personal computers
   |    |--doc            ; MMA Documentation files
   |    |--src            ; MMA source code for use on any computer
   |    |--data-win       ; numerous directories containing input and output 
   |                        files for regression, prediction, and confidence 
   |                        intervals for model analyzed by the example test
   |                        files for MMA
   |    |--test-win       ; Input files and batch files for MMA as well as batch
   |                        files for running the models in data-win and for 
                            removing the output associated with those models
                            
When the batch files in the test-win\01-Run_MMA subdirectory are 
executed, models in the data-win directories are evaluated and output and 
data-exchange files are created. The model files can be deleted by executing
CleanRegressions.bat in the test-win\02-Clean_Regressions directory. The files
can be recreated by running the batch files in test-win\03-Run_Regressions. 
Further information about the files in the directories listed above is provided
in Appendix 3 of the MMA documentation included as a PDF file in the doc 
directory. 
                     

Numbers at the end of mmm_#.### are changed to identify different 
modifications. (e.g. mma_1.100 became mma_1.200 after the 
first modification)


It is recommended that no user files are kept in the mma_#.### directory
structure.  If you choose to put your own files in the mma_#.###
directory structure, do so only by creating additional subdirectories.

Included in directory mma_#.###\doc are documentation files.  One
of them is a Portable Document Format (PDF) file. The PDF files are readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/

Included in the directory mma_#.###\bin are executables for MMA, as well as
executables for MODFLOW-2000, UCODE and Linear_Uncertainty which are used to 
create the files that are evaluated by the MMA test cases.


B. INSTALLING

To make the executables in the mma_#.###\bin directory accessible from 
any directory, include the mma_#.###\bin directory in the PATH 
environment variable.  Also, if a prior release of mma is installed on 
your system, the directory containing the executables for the prior release 
should be removed from the PATH environment variable.

As an alternative, the executable files in the mma_#.###\bin directory can 
be copied into a directory already included in the PATH environment variable.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
          WINDOWS9X AND WINDOWS ME SYSTEMS
          
Add the following line to the AUTOEXEC.BAT file:

  PATH=%PATH%;C:\WRDAPP\mma_#.###\bin

Reboot your system after modifying AUTOEXEC.BAT.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
               WINDOWS NT SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Environment tab. To add a new user variable, enter
"PATH" in the Variable field and enter

   %PATH%;C:\WRDAPP\mma_#.###\bin

in the Value field.  Click Set and then click OK.  If a PATH user variable
already is defined, click on it in the User Variables pane, add
";C:\WRDAPP\mma_#.###\bin" to its definition in the Value field, and 
click OK. Initiate and use a new Windows Command Prompt window after making this
change.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS
             
From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\mma_#.###\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click New.  In the New User Variable
window, define a new variable PATH as shown above.  Click OK.  Click OK
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.


C. EXECUTING THE SOFTWARE

After the executable file in the mma_#.###\bin directory is installed in
a directory that is included in your PATH, MMA is initiated in
a Windows Command-Prompt window using the command:

          MMA input-file fn

The variable "input file' is the name of the main MMA input file.
The "fn" variable is used as a filename prefix for output files.
For example, if the main MMA input file is named abc.in and the 
filename prefix is chosen to be ex1, then MMA can be run by entering:

          MMA abc.in  ex1

The data arrays in MMA are dynamically allocated, so models are not 
limited by hard-coded array limits. However, it is best to have enough 
random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, which slows computations significantly.

D. TESTING

Test data sets are provided to demonstrate that MMA is correctly
installed and running on the system.  The tests may also be looked
at as examples of how to use the program.  


E. COMPILING

The executable file provided in mma_#.###\bin was created using the MicroSoft 
Visual Studio 2005, Intel Fortran Compiler 11.0. Although an executable 
version of the program is provided, the source code is provided in the 
mma_#.###\src directory so that MMA and the other programs can be recompiled 
if necessary.  However, the USGS cannot provide assistance to those compiling 
MMA. In general, the requirements are a Fortran compiler and knowing how to 
use the compiler.

