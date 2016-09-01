This collection of scripts and functions comprises the source code
for a Shiny app located at https://suigeneris.shinyapps.io/app_test
that calculates the isotopic fractionation of sulfur associated with
the biological process that is the sulfate reducing metabolism. If
running the app using the source code in RStudio instead of
accessing the app online, the following files must exist in the same
directory:

1) app.R [fairly self-explanatory; this is the main app script]

2) funct_metab.R [function that calculates metabolite concentrations
	which is called upon in the server section of app.R]

3) funct_env.R [function that calculates thermodynamic constants
	dependent on external sulfate concentration]

4) funct_frac.R [function that is also called in the server section
	of app.R; uses metabolite concentrations calculated by the
	aforementioned function to calculate fractionation. The
	current output is simply net fractionation; if one wished
	to change this to the fractionation of any one particular
	constituent reaction of sulfate reduction, one would could
	easily do so within this file]

5) input_sulfparam.R [file containing input parameters which also
	saves these parameters to dataframes that are then loaded
	into app.R; if one wished to change, for example, default
	values of any of the parameters, this would be the file
	to change.

	NOTE: input_sulfparam.R MUST be run before app.R is run in
	order to produce 3 input files called by app.R, and re-run
	to update these input files if changes are made. The
	generated input file names are as follows:
	i) const_input.rds
	ii) var_input.rds
	iii) char_input.rds

Thus, the directory from which this source code is run should, in
all, contain 8 files.

Fractionation is units of permil, while metabolite concentrations
and K_m are in moles. Cell-specific reduction rate and V_max are
in femtomoles per cell per day.

For further background on the mathematics and science of the
calculations that this code executes, refer to "Intracellular 
metabolite levels shape sulfur isotope fractionation during
microbial sulfate respiration" by Wing and Halevy (2015) published
in the Proceedings of the National Academy of Science, Vol. 111,
no. 51, including the supplemental material.