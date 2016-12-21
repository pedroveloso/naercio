Non-cognitive skills, functional illiteracy and schooling in Brazil
========================================================
author: Pedro Veloso & Joseph Neto
date: 11/29/2016
autosize: true

Introduction
========================================================

- Non-cognitive skills (NCS): latent variables, gained revelance over the last decade in the economics literature
- Schooling: OCDE & Paulo Montenegro Institute
- Empirical papers: Calero & Rozo (2016)


Data
========================================================

- Inaf indicator: survey conducted by Paulo Montenegro Institute --> 2002 participants, the sample correctly represents the brazilian population
- NCSs available: Selfmanagement, Selfconcept, Openness
- Combined score < 95 --> Functional Illiterated

Methodology
========================================================

- PSM + diff-in-diff
- PSM based on the nearest neighbor, excluding the ones outside the common support region
- Treatment: NCS(i) > median(NCS)
- Diff-in-diff time dimension: Fishing High School

Slide With Plot
========================================================


```{r, echo=FALSE}

library(MatchIt)
library(plyr)
library(dplyr)
library(ggplot2)



```


Slide With Plot
========================================================


