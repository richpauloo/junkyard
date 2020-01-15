README_UCODE_2014.TXT

                       UCODE_2014 - Version: Version 1.004 
                         Universal inverse modeling code

NOTE: Reference to commercial products does not constitute endorsement 
            by the authors or IGWMC.

This version of UCODE_2014 is packaged for personal computers using
one of the Microsoft Windows operating systems.  An executable file for
personal computers is provided as well as the source code.  The executable
file was created using the MicroSoft Visual Studio 2008, Intel Fortran
Compiler 11.1. The source code can be compiled to run on other computers.

This version of UCODE is referred to as UCODE_2014 to distinguish it from
older versions.  See the file doc\UCODE_2014.txt for a description of this
software.  Instructions for installation, execution, and testing are provided
below.

                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         ucode_2014_1.004.exe

For UNIX the ucode_2014_1.004.tgz file is provided

Numbers at the end of ucode_2014_#.### are changed to identify different
modifications. (e.g. ucode_2014_1.004 will become ucode_2014_1.005 after 
the first modification)

The distribution file contains:

          Compiled runfile and source code for UCODE_2014 and the ten
              associated programs.
          Supplementary UCODE_2014 documentation in PDF and text files.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows the user to specify the directory in which the files should
be restored (the default is C:\WRDAPP).  These installation instructions assume
that the files are restored into directory C:\WRDAPP.  The following directory
structure will be created in C:\WRDAPP:

   |
   |--ucode_2014_#.###
   |    |--bin  ; Compiled executables for personal computers
   |    |--doc  ; UCODE Documentation files
   |    |--src  ; UCODE_2014 and associated source codes for 
                                use on any computer
   |    |--test-win; Input files and batch files for UCODE_2014 and
                               associated codes
                               
Starting with the ucode_2014_1.004 release, the following will also be extracted
   |
   |--mma_#.###
   |    |--bin      ; Compiled executables for personal computers (including old
   |                  versions of modflow, ucode, and linear_uncertainty from 
   |                  the time when mma was last released
   |    |--data     ; Data for test runs
   |    |--doc      ; MMA Documentation files
   |    |--src      ; MMA source codes for use on any computer
   |    |--test-win ; Input and batch files for MMA test runs
   |
   |--OPR-PPR
   |    |--bin      ; Compiled executables for personal computers (including old
   |                  a version of modflow from the time when opr-ppr was last
   |                  released
   |    |--data     ; Data and batch files for test runs including executables 
   |                  of related codes needed for the tests from the time when 
   |                  opr-ppr was last released
   |    |--doc      ; OPR-PPR Documentation files
   |    |--src      ; OPR-PPR source codes for use on any computer
   |
   |--sim_adjust_#.###
   |    |--bin      ; Compiled executables for personal computers (including old
   |                  versions of modflow. ucode and linear_uncertainty from 
   |                  the time when mma was last released
   |    |--doc      ; MMA Documentation files
   |    |--src      ; MMA source codes for use on any computer
   |    |--test-win ; Input and batch files for MMA test runs



When the batch files in the test-win subdirectories are executed, output files 
from the process model and output and data-exchange files created and used by 
UCODE_2014 and the auxiliary codes are listed in the test-win subdirectories. 
Additional information is provided in the UCODE_2014 manual pdf file
in the doc directory.

It is recommended that no user files are kept in the ucode_2014_#.### directory
structure.  If you choose to put your own files in the ucode_2014_#.###
directory structure, do so only by creating additional subdirectories.

Included in directory ucode_2014_#.###\doc are documentation files. The PDF 
files are readable and printable on various computer platforms using Acrobat 
Reader from Adobe. The Acrobat Reader is freely available from the following 
World Wide Web site:
      http://www.adobe.com/

Included in the directory ucode_2014_#.###\bin are executables for UCODE_2014,
the auxiliary programs, a program named runner needed for parallel execution,
an executable for opr-ppr, an executable for MODFLOW-2000, which is used as 
the process model in some test cases, an executable called robocopy.exe which 
is in a sub-directory called robocopy that is used for the MCMC test case, and 
the files needed to run GW_Chart (in subdirectory bin-plotting\GWchart.


B. INSTALLING

To make the executables in the ucode_2014_#.###\bin directory accessible from
any directory, include the ucode_2014_#.###\bin directory in the PATH
environment variable.  Also, if a prior release of UCODE_2014 is installed on
your system, the directory containing the executables for the prior release
should be removed from the PATH environment variable.

As an alternative, the executable files in the UCODE_2014\bin directory can
be copied into a directory already included in the PATH environment variable.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
          WINDOWS9X AND WINDOWS ME SYSTEMS

Add the following line to the AUTOEXEC.BAT file:

  PATH=%PATH%;C:\WRDAPP\ucode_2014_#.###\bin

Reboot your system after modifying AUTOEXEC.BAT.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
               WINDOWS NT SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Environment tab. To add a new user variable, enter
"PATH" in the Variable field and enter

   %PATH%;C:\WRDAPP\ucode_2014_#.###\bin

in the Value field.  Click Set and then click OK.  If a PATH user variable
already is defined, click on it in the User Variables pane, add
";C:\WRDAPP\ucode_2014_#.###\bin" to its definition in the Value field, 
And click OK. Initiate and use a new Windows Command Prompt window 
after making this change.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\ucode_2014_#.###\bin" to the end of the Variable Value 
(ensure that the current contents of the User Value are not deleted) and 
click OK.  If a PATH user variable is not already defined, in the User 
variables pane of the Environment Variables window, click New.  In the 
New User Variable window, define a new variable PATH as shown above.  
Click OK.  Click OK in the Environment Variables window and again in the 
System Properties window.  Initiate and use a new Windows Command 
Prompt window.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 7

From the Desktop, right-click the Computer icon and select Properties. 
If you don't have a Computer icon on your desktop, click the Start 
button, right-click the Computer option in the Start menu, and 
select Properties. Click the Advanced System Settings link in the left 
column. In the System Properties window, click on the Advanced tab, 
then click the Environment Variables button near the bottom of that 
tab. In the Environment Variables window, highlight the Path variable 
in the "System variables" section and click the Edit button. Add the 
path ";C:\WRDAPP\ucode_2014_#.###\bin". Each different directory 
is separated with a semicolon.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 8 and 10

From the Desktop, right-click the the very bottom left corner of the 
screen to get the Power User Task Menu. From the Power User Task 
Menu, click System. Click the Advanced System Settings link in the 
left column. In the System Properties window, click on the Advanced 
tab, then click the Environment Variables button near the bottom 
of that tab. In the Environment Variables window, highlight the 
Path variable in the "System variables" section and click the Edit 
button. Add the path ";C:\WRDAPP\ucode_2014_#.###\bin". 
Each different directory is separated with a semicolon.


C. EXECUTING THE SOFTWARE

After the executable file in the ucode_2014_#.###\bin directory is 
installed in a directory that is included in your PATH, UCODE_2014 
is initiated in a Windows Command-Prompt window using the 
command:

          UCODE_2014 input-file fn

The variable "input file' is the name of the main UCODE_2014 input 
file. The "fn" variable is used as a filename prefix for output files.
For example, if the main UCODE_2014 input file is named abc.in 
and the filename prefix is chosen to be ex1, then UCODE_2014 can 
be run by entering:

          UCODE_2014 abc.in  ex1

The data arrays in UCODE-2014 are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, which slows computations significantly.

D. TESTING

Test data sets are provided to demonstrate that UCODE-2014 is correctly
installed and running on the system.  The test files may also be looked
at as examples of how to use the program.


E. COMPILING

The executable file provided in ucode_2014_1.004\bin was created using 
The MicroSoft Visual Studio 2008, Intel Fortran Compiler 11.1. Although an 
executable version of the program is provided, the source code is provided 
in the ucode_2014_1.004\src directory so that UCODE_2014 and the 
other programs can be recompiled if necessary.  However, the USGS 
cannot provide assistance to those compiling UCODE. In general, the 
requirements are a Fortran compiler and knowing how to use the 
compiler.

For compiling on a unix/linux platform, a makefile is provided with 
defaults for the gfortran compiler.  Compiler specific sleep functions are 
used in utl.f90. If your compiler does not support a sleep function you 
should comment out this function and uncomment the generic function 
provided.
