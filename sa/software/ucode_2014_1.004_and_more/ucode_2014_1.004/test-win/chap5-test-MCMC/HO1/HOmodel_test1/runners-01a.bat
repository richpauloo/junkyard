echo off
rem directories
mkdir %1\bin
mkdir %1\HO1\HO1\HO1\HOmodel_test1\HOmodel_forward
mkdir %1\HO1\HO1\HO1\HOmodel_test1\HOmodel_prediction
rem files
..\..\..\..\bin\robocopy\robocopy HOmodel_forward      %1\HO1\HO1\HO1\HOmodel_test1\HOmodel_forward
..\..\..\..\bin\robocopy\robocopy HOmodel_prediction   %1\HO1\HO1\HO1\HOmodel_test1\HOmodel_prediction
..\..\..\..\bin\robocopy\robocopy ..\..\..\..\bin      %1\bin                       mf2005.exe
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 00-a-clean.bat
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO.inst
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO.obs
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO.pred
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO.tpl
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO_pred.inst
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO_pred.tpl
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO1._mv
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 HO1._paopt
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 obsmatrix_flow.dat
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 obsmatrix_head.dat
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 runMF.bat
..\..\..\..\bin\robocopy\robocopy .                    %1\HO1\HO1\HO1\HOmodel_test1 runMF_pred.bat
..\..\..\..\bin\robocopy\robocopy ..\..\..\..\bin      %1\HO1\HO1\HO1\HOmodel_test1 runner.exe
