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

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](pptNaercio-figure/unnamed-chunk-2-1.png)
