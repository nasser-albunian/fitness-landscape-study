

# What this directory is about
This directory includes data and scrpits that are required to analyse the experiments results. 

 - Data directory includes data to be analysed and data resulting from the analysis. 
 - Supporting directory includes scripts used by Analysis script to calculate AC and IA measures. 



## Running an experiment:

To run an experiment in EvoSuite, the two algorithms can be run using the following parameters:

**Running the random walk:**

    -Dalgorithm=RANDOM_WALK_TC -criterion branch -Doutput_variables=TARGET_CLASS,Total_Goals,BranchCoverage -Ddynamic_seeding=false -Ddynamic_pool=0 -Dseed_probability=0 -Dseed_types=false -Dprimitive_pool=0.0 -Drandom_walk_steps=1000 -Dlimited_random_walk=true -Dcount_branch_execution=true

**Running MOSA:**

    -Dalgorithm=MOSA -criterion branch -Doutput_variables=TARGET_CLASS,Total_Goals,BranchCoverage -Dsearch_budget=60 -Dcount_branch_execution=true



## Source code:

The implementation of the fitness landscape analysis in EvoSuite can be found [here](https://github.com/nasser-albunian/EvoSuite-FLA.git).

