Step 1:

Use the args argument in ADMB to enter the following command line run arguments

For plotting purposes do a short run so you have one to read in...
tem -mcmc 200000 -mcsave 40

the save is the thinning, I use multiples that end up being 5000 in the output file so on your final run use

tem -mcmc 5000000 -mcsave 1000  (use ADMB run with args then type -mcmc 5000000 -mcsave 1000 when in ADStudio)

Step 2:
tem -mceval (same as above)

Step 3:
rename evalout.prj evalout.dat 

Step 4:
open evalout.dat in text editor

Step 5:
Make a header with these 3 values (make sure text is commented out or delete):
5000 #(number of thinned chain)
62 #(number of years)
30 #(number of ages)

Step 6:
run evalout.exe -nohess

Step 7:
Now you have an evalout.sdat that should load into the plotting functions.

