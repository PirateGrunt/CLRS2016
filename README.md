---
title: Individual Claims Reserving with Stan
---

Code and slides for a presentation to be given at the [Casualty Loss Reserve Seminar](http://www.casact.org/clrs/) in Chicago, 2016. This presentation develops ideas first proposed by James Guszcza and Jan Lommele in their paper [Loss Reserving Using Claim-Level Data](https://www.casact.org/pubs/forum/06fforum/115.pdf).

To construct the data, exhibits and presentation, clone the repository and run the "make" program. From the command line, this would be:

```
git clone https://github.com/PirateGrunt/CLRS2016.git
cd CLRS2016
make 
```

From within RStudio, simply open the project file and press CTRL-SHIFT-B to build. The Stan routines may take several minutes to run.

The `make` program must be installed in order for this to work. This is standard on Linux or Mac OS computers, but must be installed on Windows. Installation of RTools will accomplish this (and lots of other stuff). Instructions for this may be found here: https://cran.r-project.org/bin/windows/Rtools/index.html.

To convert from markdown to HTML, you must have [pandoc](http://pandoc.org/) installed. Instructions for installation may be found here: http://pandoc.org/installing.html. Note that pandoc ordinarily installs with RStudio.

You will need to have following packages installed:

* [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
* [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
* [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
* [nlme](https://cran.r-project.org/web/packages/nlme/index.html)
* [devtools](https://cran.r-project.org/web/packages/devtools/index.html)
* [scales](https://cran.r-project.org/web/packages/scales/index.html)
* [rstan](https://cran.r-project.org/web/packages/rstan/index.html)
* [ChainLadder](https://cran.r-project.org/web/packages/ChainLadder/index.html)
* [methods](https://cran.r-project.org/web/packages/methods/index.html)
* [rmarkdown](https://cran.r-project.org/web/packages/rmarkdown/index.html)

There is no check for these packages. The `make` routine will fail if they're not present.

In addition, the code relies on the package `imagine`, which is currently under development. A check is performed in the simulation script to see if the package exists. If not, it will attempt to download the package from GitHub using `devtools`.

The `imagine` project site may be found here: https://github.com/PirateGrunt/imagine. 

A users guide is under development and will eventually be located here: http://pirategrunt.com/imagine/.