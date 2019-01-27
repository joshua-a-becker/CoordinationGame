# CoordinationGame
Supplementary code for "The Network Dynamics of Conventions"

## Core Code

The file `coordination_game.R` contains the basic code for the simulation.

## Usage

Each script runs a single iteration of the parameter space.  The scripts are designed to be called from the command line with appropriate parameters.  This speeds up run-time (because R slows down quite a bit over many iterations of a loop) and allows for the parallel distribution of runs across multiple computing nodes.

## Figures 2, 3, and 7

The files `cg_ba.R` and `cg_arbitrary_deg_seq.R` contains code for running the coordination game on BA networks and networks with arbitrary degree distributions.  This code generates data necessary to generate Figures 2, 3, and 7.

In order to run `cg_arbitrary_deg_seq.R` you must first run `prep_arbitrary.R`.


## Figures 4 and 5
The file `cg_effect_of_cent.R` contains code for running the coordiation game on a BA network and generates data necessary to produce Figures 4 and 5.

In order to prevent divide-by-zero in the analysis, this script is designed to run the simulation multiple times on each network.  In order to run `cg_effect_of_cent.R` you must first run `gen_networks.R` which will generate 100 igraph objects and save them to disk.   

This also allows the convenient post-hoc analysis of other node properties that might interest you.

## Figure 6
The file `cg_density.R` contains code for running the coordination game on a decentralized network of varying density and generates data necessary to produce Figure 6.

Files labeled with Figure numbers will produce the figures in the paper from the data generated.
