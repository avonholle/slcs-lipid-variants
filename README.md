# Analytic work for project titled, "Lipid-related Genetic Variants and Lipid Outcomes in a Cohort of Chilean Children" 

## Statistical Analysis Plan

[Statistical Analysis Plan](https://avonholle.github.io/ms-201608-1/StatisticalAnalysisPlan.html)

### Steps to process analysis plan

Steps:

1. Edit [Google docs working draft]

2. Process in RStudio

3. Publish to github pages associated with this repository [here](https://avonholle.github.io/ms-201608-1/StatisticalAnalysisPlan.html).

---

## List of SNPS for analyses

[List of SNPS](https://avonholle.github.io/ms-201608-1/snp-list.html) -- [.csv file with list](https://avonholle.github.io/ms-201608-1/lipid-snps.txt) -- [code](snp-list.Rmd)

---

## Analyses on Kure

- [File making results](kure-analysis/analysis-summary.Rnw)

- Steps to process [report](http://avonholle.github.io/ms-201608-1/analysis-summary.pdf) are
    1. Run the [R file to knit a .Rnw document](kure-analysis/make-summary.R): bsub R CMD BATCH make-summary.R &
    2. Transfer the .tex file created in step 1 above, analysis-summary.tex, to a local drive at ~\Dropbox\unc.grad.school\my-papers\ms-201608-1\programs\kure-analysis\tex\ to run on TeXWorks or TeXnicCenter programs.


## Poster (and slides) for AHA EPID/Lifestyle March 2017 conference

* Run [aha-2017-slcs.Rnw](aha-poster-201703/aha-2017-slcs.Rnw) to get [poster](http://avonholle.github.io/ms-201608-1/aha-2017-slcs.pdf)

* Run [aha-2017-slcs-slides.Rnw](aha-poster-201703/aha-2017-slcs-slides.Rnw) to get [slides for poster](http://avonholle.github.io/ms-201608-1/aha-2017-slcs-slides.pdf)


