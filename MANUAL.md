---
title: "ABM User's Manual"
author: "Michele Gubian and Johanna Cronenberg"
date: "January 10, 2019"
output: html_document
bibliography: bibliography.bib
---

## Simulation settings

The file `data/params.R` stores the list `params`, which specifies the simulation settings and it is loaded in before simulation run in `Rcmd/ABMmain.R`. 
Here you find a short description of each option. 

```r
inputDataFile = "data/Antarctica.csv"
```
the input data file (optional, you can also specify it directly in `Rcmd/ABMmain.R`). See section on input format for details.

### Production
```r
productionStrategy = "SMOTE"
```
An agent produces an acoustic token as follows. First a target word is selected at random, then the agent will produce the acoustic parameters of the corresponding phoneme for that word. 
 The acoustic parameters are randomly generated from a Gaussian distribution that is estimated on the fly based on the acoustic tokens of the target word present in the agent's memory at that moment.
 Since in some cases the number of available tokens for a given word is too small to estimate the parameters of a multi-dimensional Gaussian distribution, several strategies are available in order to make the estimation more robust:
 
 - `"targetWordTokens"` Only the available tokens of the target word are used
 - `"extraTokens"` Tokens in the neighbourhood of the target word tokens are added to the token base used for estimation. The amount of extra tokens is expressed in terms of a ratio between the extra tokens and the available tokens and set with the parameter `productionExtraTokensRatio`.
 - `"meanWords"` Extra tokens are created by averaging by word tokens of the same target phoneme. These are added to the available tokens and used for estimation.
 - `"SMOTE"` The SMOTE algorithm [@chawla2002smote] generates extra tokens by random linear interpolation of the available tokens. The user has to specify the minimum number of tokens needed for estimation by the parameter `productionMinTokens`. If the available tokens are not enough, SMOTE will generate the remaining ones. The parameter `productionSMOTENN` specifies the number of nearest neighbours that have to be considered when performing interpolation.  Whenever the available number of tokens for a given word is less than one plus the number of nearest neighbours, the neighbourhood is completed by adding tokens from other words containing the same target phoneme.

### Perception

```r
memoryIntakeStrategy = "maxPosteriorProb"
```
An essential part of the perception process is the decision on whether the token, which was produced by the speaker, is memorised by the listener. This parameter offers you two statistical ways of taking that decision: 

- `"maxPosteriorProb"` Uses a maximum posterior probability decision, i. e.
the produced token is only memorised if its probability of belonging to the
listener’s corresponding phonological category is higher than that of belonging
to any of the other categories.
- `"mahalanobisDistance"` The distance between the token and the corresponding
distribution in the listener’s memory has to be smaller than a given threshold
in order to be incorporated. The threshold is specified by setting `"mahalanobisThreshold"`.
This approach does not take into
account any of the other phonological categories, i. e. it might not be the
appropriate strategy if two or more categories in your data overlap or are very
close to each other. 

```r
memoryRemovalStrategy = "outlierRemoval"
```
The agents’ memories are limited, that means
that tokens have to be removed if the memory has reached its capacity.
The token to be removed from the memory is determined either on the basis of its
age or its position with regard to the rest of the segment distribution:

- `"outlierRemoval"` The farthest outlier of the distribution is removed before taking
in the new token.
- `"timeDecay"` Before the ABM run, each token is assigned a random time
stamp. When an agent has to memorise a new token, the oldest token from
the corresponding category is removed and the new token receives the most
recent time stamp.

```r
maxMemoryExpansion = 1.0
```
This parameter sets the limit of the agents’ memory size expressed as ratio between the max allowed size and the initial size. If the limit is reached, the selected `memoryRemovalStrategy` takes effect.

```r
splitAndMerge = FALSE
```
Perform split and merge (see [@sRetraction] for details). If `TRUE`, further parameters have to be set.
`splitAndMergeInterval` specifies the number of interactions between each run of split and merge. `doSplitAndMergeBeforeABM` specifies whether to perform split and merge before simulation start.

```r
perceptionOOVNN = 5
```
If the received word is unknown, assign label (phoneme) by applying majority vote to `perceptionOOVNN` nearest neighbours. 

### Interactions
```r
interactionPartners = "random"
```
This parameter lets you choose from which groups
the two interacting agents shall come.

- `"random"` It does not matter from which group an agent comes
- `"withinGroups"` Speaker and listener must come from the same group
- `"betweenGroups"` Speaker and listener must be members of different groups

```r
speakerProb = NULL
```
In certain cases, you may want to introduce an imbalance
to the ABM concerning the frequency with which one or more agents are chosen to
be speakers in an interaction. This option allows you to assign a speaking probability
to each agent by specifying a vectors of numbers, which do not need to sum up to one,
as they will be normalised internally. If left `NULL`, all speakers will get equal chances to
be selected as speakers in a interaction.

```r
listenerProb = NULL
```
Same as `speakerProb` applied to the selection of the listener in an interaction.

### Runs
```r
runMode = "multiple"
```
The ABM system offers you two ways of running it: You can
either perform one ABM run (`"single"`) in order to see the immediate results or you can perform
multiple independent ABM runs (`"multiple"`) which makes it possible to check whether the ABM delivers
stable results on your data. If multiple simulations are run, specify the number of runs by setting the parameter `multipleABMRuns`.

```r
nrOfSnapshots = 5,
interactionsPerSnapshot = 1000
```
A simulation is a sequence of `nrOfSnapshots * interactionsPerSnapshot` interactions. At every `interactionsPerSnapshot` interactions a snapshot of all agents' memories is taken and saved.

## Data structures and files organisation

In general, you run a set of (related) simulations by going into a (new) folder and use it as a root log directory. This contains a simulation register and several simulation directories.
The simulation register is a list of parameter lists, one per simulation.
A typical paramater list is the one in data/params.R, and its content was explained above. 
The simulation register is created by `createSimulationRegister()` and managed by other commands, you don't touch it directly (see below).

Each simulation directory is created with a name generated by `generateSimulationName()`, it has the format `ABM<date><time>` (you can change the prefix if you want).
A simulation is inserted in the register with `registerSimulation()`. 
At sim start the field `params$completed` is set to `FALSE`. Once the sim has terminated, this is set to completed by the command `setCompleted()`. In this way if a simulation crashes you can select it by looking for not completed (see below), or just use the cmd `purgeNotCompleted(rootLogDir)` to eliminate both the sim dir and its entry in register.

After finishing one or more simulations, your `rootLogDir` looks like:
```
ABM20181017115249  ABM20181017201605 .. simulations_register.rds
```
The input to a simulation is a data.table. Its columns are:

- `P1, P2, ... ` : acoustic features
- `initial` : initial labels
- `label` : current label (usually == initial)
- `speaker` : number or name
- `group` : must be there, if only one group, set it to 'dummy' or whatever
- `word` : word labels

Each simulation directory looks like this:

```
input.rds intLog.rds params.yaml pop.0.rds  pop.1.rds pop.2.rds ....
```
if you run in `params$runMode == "single"`, while if you ran as `"multiple"` you have a dir level in between that looks like:

```
1/ 2/ 3/ 4/ ...
```
how many independent runs you set in `params$multipleABMRuns`.
Let's see each file in detail.

- `params.yaml` is the saved params list. It's in `yaml` format, so you can directly open it as text and read it conveniently. All other files are stored as `rds`, so you need `readRDS()`. 
- `input.rds` is a copy of the input file, as you can start with an existing dataframe and tweak (e.g. remove speakers etc). In this way you preserve the exact input.
- `pop.<NUMBER>.rds` are as many memory snapshots as `params$nrOfSnapshots`. These are data.tables that report the state of all agents. The columns are:
```
P1, P2, ..., word, label, initial, valid, nrOfTimesHeard, producerID, timeStamp, agentID, speaker, group, equivalence, condition
```
most of them are the same as in the input. Each speaker gets an `agentID` from the start. `producerID` records who was the agent that produced the saved token. `condition` is another name for the snapshot index, i.e. `pop.0.rds` is the memory at start, that is the same as the input, so `condition == 0`, then `pop.1.rds` is the first snapshot and `condition == 1`, etc.
`valid` is very important: if `FALSE` you have to ignore the row. It is used in agents' memories to represent free space.

- `intLog.rds` is the interaction log data.table. Its colums are:
```
word, producerID, producerLabel, producerNrOfTimesHeard,  perceiverID, perceiverLabel, perceiverNrOfTimesHeard, accepted, simulationNr, valid
```
That's a log of all interactions. Each row is an interaction happened during `simulationNr` (`== condition`), where `word` produced by `producerID` was perceived by `perceiverID` and `accepted == TRUE` or `FALSE`. Here `valid` will always be `TRUE` at simulation end, but it's used internally (preallocate memory). 


## References

