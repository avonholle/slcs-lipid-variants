# a2.R
# initial association tests

library(GenABEL)
#library(PredictABEL)
library(knitr)
#library(snpStats)
#biocLite("snpStats")


#### #--------- get data -------------

## @knitr table2


load("a1.Rda") # load a1 object from a1.R

# ^^^^^^^^^^^^--------------------------------------------
# contents of object -----------------
# ldl.position is index number of snps related to ldl risk score
# hdl.position is same as ldl.position but for hdl
# tg.position is same as above
# combo.dat is combined pheno and geno data in a data.frame for risk score analysis
#   Can use index number in vectors above to locate appropriate snps
# dat1 is the gwaa.data object for use in genABEL (lipid phenotypes and genotype info)
# ^^^^^^^^^^^^--------------------------------------------

descriptives.trait(dat1)
descriptives.marker(dat1)

# Do tests for table 1 shell
# ^^^^^^^^^^^^------------------------------------------

# get snp subset from power calcs in 
# ~/Documents/dissertation/unc-dissertation-markdown/includes/scripts/power-calcs-ind-assoc.html#371_hdl-related_snps

test.1 = qtscore(hdl.e ~ sex + pc1 + pc2 + pc3 + pc4 + pc5, data=dat1, trait.type="gaussian",
                 snpsubset = c('rs3764261'))
test.2 = qtscore(ldl.e ~ sex + pc1 + pc2 + pc3 + pc4 + pc5, data=dat1, trait.type="gaussian",
                snpsubset=c( 	'rs4420638', 'rs629301', 'rs6511720'))
test.3 = qtscore(log(tg.e) ~ sex + pc1 + pc2 + pc3 + pc4 + pc5, data=dat1, trait.type="gaussian",
                 snpsubset = c('rs1260326', 'rs964184')) # tg.e is log transformed
test.4 = qtscore(tc.e ~ sex + pc1 + pc2 + pc3 + pc4 + pc5, data=dat1, trait.type="gaussian",
                 snpsubset=c('rs6511720'))

## @knitr table2-results-1
s1 = summary(test.1); s1
## @knitr table2-results-2
s2 = summary(test.2); s2
## @knitr table2-results-3
s3 = summary(test.3); s3
## @knitr table2-results-4
s4 = summary(test.4); s4

sumdat = summary(dat1)

save(sumdat, s1, s2, s3, s4, file="a2-sum.Rda")


