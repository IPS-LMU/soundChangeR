# Agent-based Modelling of Sound Change

This software is an implementation of an agent-based model (ABM) aimed at simulating sound changes. It supports the research conducted at the Institute of Phonetics and Speech Processing, LMU Munich, Germany. 
We are funded by European Research Council Grant No. 742289 ["Human interaction and the evolution of spoken accent"](https://www.phonetik.uni-muenchen.de/Forschung/interaccent/interAccent.html) (2017–2022) awarded to Jonathan Harrington. 
The ABM was initially developed by Florian Schiel and Jonathan Harrington, and further restructured, modified and documented by Johanna Cronenberg and Michele Gubian, the latter being currently the main developer and maintainer. 

The ABM is now available as an R package called **soundChangeR**. 

## Installation

- Make sure you are using R version 4.0 or higher
- Make sure that all of your packages are up-to-date
- Install [pandoc](https://pandoc.org/installing.html)
- Install `devtools`, `BiocManager`, and `NMF`:

```
install.packages("devtools")
install.packages("BiocManager")
BiocManager::install("NMF")
```

- Install `soundChangeR`:

```
devtools::install_github("https://github.com/IPS-LMU/soundChangeR", build_vignettes = T)
```

- If you receive error messages, you might have to install further dependencies to `soundChangeR`. All dependencies are listed in the file called DESCRIPTION in this repository.

## Updates

Updates of `soundChangeR` can be checked for and installed by repeating the `install_github()` command as shown above.

## Citation & Reading Material

When you use `soundChangeR` in published work, please cite the package:

```
citation("soundChangeR")
```

There is a vignette to this R package with detailed explanations of all parameters to this model:

```
vignette("soundChangeR")
```

In the vignette, you will find further links to reading material about the ABM.

## Contact

If all the reading material doesn't help you solve problems with `soundChangeR` or you have questions about the ABM's concepts and functionalities, please contact me:

johanna.cronenberg at campus.lmu.de





