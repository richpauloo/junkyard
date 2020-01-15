README_SIM_ADJUST.TXT


            SIM_ADJUST - Version: Version 1.000 3/27/2008
   A Computer Code that Adjusts Simulated Equivalents for Observations


NOTE: Any use of trade, product or firm names is for descriptive purposes
      only and does not imply endorsement by the U.S. Government.

This version of SIM_ADJUST is packaged for personal computers using
one of the Microsoft Windows operating systems.  An executable file for
personal computers is provided as well as the source code.  The executable
file was created using the MicroSoft Visual Studio 2005, Intel Fortran
Compiler 9.1. The source code can be compiled to run on other computers.

See the file doc\SIM_ADJUST.txt for a description of this software.  
Instructions for installation, execution, and testing are provided
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

         sim_adjust_1.000.exe

For UNIX the sim_adjust_1.000.tgz file is provided

Numbers at the end of sim_adjust_#.### are changed to identify different
modifications. (e.g. sim_adjust_1.000 will become sim_adjust_1.001 after the
first modification)

The distribution file contains:

          Compiled runfile and source code for SIM_ADJUST.
          Supplementary SIM_ADJUST documentation in PDF and text files.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows the user to specify the directory in which the files should
be restored (the default is C:\WRDAPP.  These installation instructions assume
that the files are restored into directory C:\WRDAPP.  The following directory
structure will be created in C:\WRDAPP:

   |
   |--sim_adjust_#.###
   |    |--bin            ; Compiled executables for personal computers
   |    |--doc            ; SIM_ADJUST Documentation files
   |    |--src            ; SIM_ADJUST source code for use on any computer
   |    |--test           ; Input files and batch files for SIM_ADJUST

Numbers at the end of sim_adjust_#.### are changed to identify different
modifications. (e.g. sim_adjust_1.000 will become sim_adjust_1.001 after the
first modification)

When the batch files in the test subdirectories are executed, process model
output files are created and used by SIM_ADJUST. Additional information is 
provided in Appendix A of the pdf file in the doc directory (see below).

It is recommended that no user files are kept in the sim_adjust_#.### directory
structure.  If you choose to put your own files in the sim_adjust_#.###
directory structure, do so only by creating additional subdirectories.

Included in directory sim_adjust_#.###\doc are documentation files.  One
of them is a Portable Document Format (PDF) file. The PDF files are readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/

Included in the directory sim_adjust_#.###\bin is the executable for SIM_ADJUST,
and executables for MODFLOW-2000 and MODFLOW-2005, which are used as the process 
models in the test cases.


B. INSTALLING

To make the executables in the sim_adjust_#.###\bin directory accessible from
any directory, include the sim_adjust_#.###\bin directory in the PATH
environment variable.  Also, if a prior release of SIM_ADJUST is installed on
your system, the directory containing the executables for the prior release
should be removed from the PATH environment variable.

As an alternative, the executable files in the SIM_ADJUST\bin directory can
be copied into a directory already included in the PATH environment variable.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
          WINDOWS9X AND WINDOWS ME SYSTEMS

Add the following line to the AUTOEXEC.BAT file:

  PATH=%PATH%;C:\WRDAPP\sim_adjust_#.###\bin

Reboot your system after modifying AUTOEXEC.BAT.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
               WINDOWS NT SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Environment tab. To add a new user variable, enter
"PATH" in the Variable field and enter

   %PATH%;C:\WRDAPP\sim_adjust_#.###\bin

in the Value field.  Click Set and then click OK.  If a PATH user variable
already is defined, click on it in the User Variables pane, add
";C:\WRDAPP\sim_adjust_#.###\bin" to its definition in the Value field, and
click OK. Initiate and use a new Windows Command Prompt window after making this
change.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\sim_adjust_#.###\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click New.  In the New User Variable
window, define a new variable PATH as shown above.  Click OK.  Click OK
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.


C. EXECUTING THE SOFTWARE

After the executable file in the sim_adjust_#.###\bin directory is installed in
a directory that is included in your PATH, SIM_ADJUST is initiated in
a Windows Command-Prompt window using the command:

          SIM_ADJUST input-sim_adjust fn

The "fn" variable is used as a filename prefix for Sim_Adjust output files. 
For example, if the main SIM_ADJUST input file is named 
Mysim_adjust.input and fn is Mymodel, then SIM_ADJUST can be run by entering:

          SIM_ADJUST Mysim_adjust.input Mymodel
          
The output file will be named Mymodel._output-sim_adjust.       

The data arrays in SIM_ADJUST are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, which slows computations significantly.

D. TESTING

Test data sets are provided to demonstrate that SIM_ADJUST is correctly
installed and running on the system.  The tests may also be looked
at as examples of how to use the program.


E. COMPILING

The executable file provided in sim_adjust_1.000\bin was created using the
MicroSoft Visual Studio 2005, Intel Fortran Compiler 9.1. Although an executable
version of the program is provided, the source code is provided in the
sim_adjust_1.000\src directory so that SIM_ADJUST can be recompiled if 
necessary.  However, the IGWMC cannot provide assistance to those
compiling SIM_ADJUST. In general, the requirements are a Fortran compiler and 
knowing how to use the compiler.

