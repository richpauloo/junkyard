README.TXT


                      OPR-PPR - Version: 1.01
                      
A Computer Program for Assessing Data Importance to Model Predictions 
                       Using Linear Statistics



NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of OPR-PPR is packaged for personal computers using
one of the Microsoft Windows operating systems.  An executable file for
personal computers is provided as well as the source code.  The executable
file was created using the *** compiler.  
The source code can be compiled to run on other computers.

Instructions for installation, execution, and testing are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         opr-ppr_1.01.exe

The distribution file contains:

          Compiled runfile and source code for OPR-PPR.
          Documentation.
          Test data sets.

The distribution file is a self-extracting program.  Execution of the
distribution file creates numerous individual files.  The extraction
program allows you to specify the directory in which the files should
be restored.  The installation instructions assume that the files are
restored into directory C:\WRDAPP.  The following directory structure
will be created in C:\WRDAPP:


   |
   |--opr-ppr
   |    |--bin     ; Compiled OPR-PPR executable for personal computers
   |    |--doc     ; Documentation files
   |    |--data    ; Input data to run verification tests, and associated output files
   |    |--source  ; OPR-PPR source code for use on any computer


It is recommended that no user files be kept in the opr-ppr directory
structure.  If you do plan to put your own files in the opr-ppr
directory structure, do so only by creating additional subdirectories.

Included in directory opr-ppr\doc are Portable Document Format (PDF) files. 
The PDF files are readable and printable on various computer platforms using
Acrobat Reader from Adobe. The Acrobat Reader is freely available from the
following url:
      http://www.adobe.com/


B. INSTALLING

To make the executable version of OPR-PPR accessible from any
directory, the directory containing the executable (opr-ppr\bin)
can be included in the PATH environment variable. 

As an alternative, the executable file, opr-ppr.exe, in the
opr-ppr\bin directory can be copied into a directory already
included in the PATH environment variable.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
               WINDOWS NT SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Environment tab. To add a new user variable, enter
"PATH" in the Variable field and enter

   %PATH%;C:\WRDAPP\opr-ppr\bin

in the Value field.  Click Set and then click OK.  If a PATH user variable
already is defined, click on it in the User Variables pane, add
";C:\WRDAPP\opr-ppr\bin" to its definition in the Value field, and click
OK.  Initiate and use a new Windows Command Prompt window after making this
change.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS
             
From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\opr-ppr\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click New.  In the New User Variable
window, define a new variable PATH as shown above.  Click OK.  Click OK
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.


C. EXECUTING THE SOFTWARE

After the executable file in the opr-ppr\bin directory is installed in
a directory that is included in your PATH, OPR-PPR is initiated in
a Windows Command-Prompt window using the command:

          opr-ppr [basename]

The optional basename argument is the basename of the opr-ppr input
and output files. If no argument is used, the user is prompted to enter
the basename. The basename should be entered without a file extension.


The data arrays in OPR-PPR are dynamically allocated, so opr-ppr 
calculations are not limited by hard-coded array limits. However, it
is best to have enough random-access memory (RAM) available to hold all
of the required data.  If there is less available RAM than this, the
program will use virtual memory, but this slows computations significantly.

Some of the files written by OPR-PPR are unformatted files.  The structure
of these files depends on the compiler and options in the Fortran write
statement.  Any program that reads the unformatted files produced by
OPR-PPR must be compiled with a compiler that produces programs that
use the same structure for unformatted files. 


D. TESTING

Test data sets are provided to verify that OPR-PPR is correctly
installed and running on the system.  The tests also can be used
as examples of how to set up OPR-PPR input files and run the code.
The directory opr-ppr\data contains the input data for running the
examples and the output files produced by each example. The input
files for each example are located in separate directories under
opr-ppr\data (e.g., opr-ppr\data\opr\Ex1-OPROMIT), and the output
files for each example are located in a separate subdirectory 
(e.g., opr-ppr\data\opr\Ex1-OPROMIT\outputs). The batch file 
01-opr-ppr.bat is located in the main directory for each example and can
be used to run OPR-PPR. With this setup, the user can run OPR-PPR in
the main directory for each example, and not overwrite the original
output files provided with the distribution. 

E. COMPILING

The executable file provided in opr-ppr\bin was created using the ***
compiler.  Although an executable version of the program is provided,
the source code is provided in the opr-ppr\source directory so that OPR-PPR
can be recompiled if necessary.  However, the USGS cannot provide assistance
to those compiling OPR-PPR. In general, the requirements are a Fortran compiler
and knowledge of using the compiler.  