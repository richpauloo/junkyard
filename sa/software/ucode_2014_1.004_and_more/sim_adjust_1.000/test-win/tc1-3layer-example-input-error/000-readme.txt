These files are distributed with the results of steps 1 and 2 below
If item 3 is executed, those results will be erased from the disk
They can be regrenerated in a matter of seconds by executing batch files
1_m.bat followed by 2_sim_adjust.bat

execute batch files in the following order:
1_m.bat to run modflow and create the process._ext file 
        named tc1._os in this example
2_sim_adjust.bat to run sim-adjust which evaluates process._ext and 
        replaces it with tc1._output-sim_adjust for the universal 
        code to read
3_clean.bat to delete all output from the above process
