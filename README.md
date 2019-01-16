# ABM Agent-based Modelling of Sound Change

This software is an implementation of an Agent-Based Model (ABM) aimed at the modellisation of sound change in speech. This software supports the research conducted at the Institute of Phonetics and Speech Processing, LMU Munich, Germany. 
It was initially developed  by Florian Schiel and Jonathan Harrington, and further restructured, modified and documented by Johanna Cronenberg and Michele Gubian, the latter being currently the main developer and maintainer. 

## Quick start

The software is entirely written in R.
There is a number of R libraries you need, see at the top of `Rcmd/loadLibraries.R`.
The top module is `Rcmd/ABMmain.R`, where you have some comments. Please consult `MANUAL.html` for detailed instructions.

## The Antarctica case study

This version is provided with data and settings ready to replicate the case study on sound change in Antarctica, which is described in a manuscript under review. 
Input data are in `data/Antarctica.csv`, settings are as in `data/params.R`,
a bundle of the original simulation output is in `data/Antarctica_results.zip`,
the data analysis reported in the manuscript is in `data/Antarctica_data_analysis.R`.













