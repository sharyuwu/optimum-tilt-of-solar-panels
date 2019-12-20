# Project Name Source Code

The folders and files for this project are as follows:

tiltAngPro: folder that content the result and the scource code of this project.

\# Under this file\
analemma.txt : txt file. This is the file content the data for the analemma.
    This is produce by Analemma.hs. This file is pre-ganarate for users. It is not
    required to generate when running the Control Module.
MainTable.txt : txt file. This is the file content the result of how many optimum 
    angles the code has produce and the average energy absorption for the start day 
    to end day.
AngleTable.txt : txt file. This is the file content the detail information from                     MainTainable.txt. It content the information of when to adjust the angle and 
    the degree of the optimum angles.

Test::
For now it have to test manually. Later, it will update its automatic test case. 
Sorry for the inconvenience.

For testing this software please open this software with the environment where
has implement GHC compiler.
Then input the following command in the terminal.
stack build
stack ghci
Control.main
\# input the value\
30 --Input the latitude
2018 --Input the Start Year
10 --Input the Start Month
05 --Input the Start Day
2019 --Input the End Year
03 --Input the End Month
05 --Input the End Day
1455 -- Input the weight of your solar panel
665 -- Input the height of your solar panel
\# View the result in the file MainTable.txt and AngleTable.txt\
...
