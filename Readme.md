# Introduction

This repository contains code, additional results, and anonymized annotation data, for our recently accepted CSCW'22 paper.  If you use this, please cite our paper (currently only the Arxiv version...)

```
@article{pathakMethodAnalyzeMultiple2021,
  title = {A {{Method}} to {{Analyze Multiple Social Identities}} in {{Twitter Bios}}},
  author = {Pathak, Arjunil and Madani, Navid and Joseph, Kenneth},
  year = {2021},
  month = jul,
  language = {en}
}
```


There are three directories here, described below.

In addition, we include two other files:

- Our final document describing the annotation task in ```Annotation Task Description.docx```
- Results for our Demographic and Twitter Status analysis for more identities, in ```image_more_demographics.pdf```

## analysis

This directory contains the R code we used to perform the reliability and validity analyses, along with data on the pre-existing lists of identities, behaviors, and modifiers we used for that analysis

## annotated data

This directory contains only datasets that we do not believe can be used to easily re-identify individuals within the datasets we used, but still provide details on our qualitative analyses. The main file of interest is  ```labels_identity.xlsx```, which contains results from our 3 rounds of coding discussed in the paper. The other files are just intermediary files used for generating that annotation task.

## method

This directory contains code to run our method for extracting personal identifiers (see ```01_gen_identifiers.ipynb``` and the associated utility code in ```identifiers_from_bio.py```) and the code to run our cluster analysis ```02_cluster_analysis.ipynb```).
