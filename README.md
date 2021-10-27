# Agent-based Modelling of Sound Change

This software is an implementation of an Agent-Based Model (ABM) aimed at modelling sound changes. It supports the research conducted at the Institute of Phonetics and Speech Processing, LMU Munich, Germany. 
We are funded by European Research Council Grant No. 742289 ["Human interaction and the evolution of spoken accent"](https://www.phonetik.uni-muenchen.de/Forschung/interaccent/interAccent.html) (2017–2022) awarded to Jonathan Harrington. 
The ABM was initially developed by Florian Schiel and Jonathan Harrington, and further restructured, modified and documented by Johanna Cronenberg and Michele Gubian, the latter being currently the main developer and maintainer. 

The ABM is now available as an R package called **soundChangeR**. In order to install `soundChangeR`, make sure you have `devtools` installed and then use `install_github()` as shown here:

```
install.packages("devtools")
devtools::install_github("https://github.com/IPS-LMU/soundChangeR/tree/main", build_vignettes = T)
```

There is a vignette to this R package with detailed explanations of all parameters to this model:

```
vignette("soundChangeR")
```

The name of this repository was accordingly changed from ABM to soundChangeR. If you need to update the remote URL to this repository, please use:

```
git remote set-url origin git@github.com:IPS-LMU/soundChangeR.git
```


