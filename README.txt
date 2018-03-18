How To Run:

1) Compile Noah-MP model:
	1a) Need a ifort compiler. On NCCS Discover, this can be accessed by sourcing the <environment_setup.sh> file. Note that this purges all modules and changes $PATH.
	1b) In the <model_src> sibdirectory run 'make clean' and 'make'.

2) In each of the <sensitivity_*> working directories do the following:
	2a) Run the <run_init.m> MatLab script in the <sensitivity_*/initialize> subdirectory.
	2b) Run the MatLab script <set_up_directories.m>.
	2c) There are now two ways to run everything:
		1) With PODS/SLURM [*]
			i) Change the absolute path(s) in <job.slurm>
			ii) Run the <dependent_jobs.sh> script to submit to a SLURM queue.
		2) On a single node
			i) Run the <all_run.sh> script.
	
3) To test a new model configuration, the general strategy is to:
	3a) Copy one of the <sensitivity_*> working directories
	3b) Change the init.txt file in the <setup_dir> & <initialize> subdirectories to the desired model configuration.
	3c) Re-run (2) above.

4) Modify & run analysis scripts:
	4a) Calculate sensitivity indices:
		i) Modify the <calc_tsi.m> to include your new configuration directory from (3). See line 10 and lines 24-27.
		ii) Run the <calc_tsi.m> MatLab script to produce two output files in the <analysis/data> subdirectory.
	4b) Make the figures:
		i) Modify the <variance_decomposition.m> script to include your new configuration directory (see line 61).  
		ii) Runt the <variance_decomposition.m> MatLab script to produce figures stored in the <analysis/figures> subdirectory.

[*] On computers other than NCCS Discover, the user will likely have to set up their own job scheduling routines. This one is specifically designed for SLURM with PODS, and uses modules availalbe on Discover.


