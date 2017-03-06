# a3.R
# info for table 3 shell:  Proportion of variance explained by known SNPs

## @knitr table3

library(GenABEL)
#library(PredictABEL)
library(knitr)
#library(snpStats)
#biocLite("snpStats")
library(data.table)
library(xtable)


##### ^^^^^^^^^^^^^^^- get data -------------

load("a1.Rda") # load a1 object from a1.R

# contents of object -----------------
# ldl.position is index number of snps related to ldl risk score
# hdl.position is same as ldl.position but for hdl
# tg.position is same as above
# combo.dat is combined pheno and geno data in a data.frame for risk score analysis
#   Can use index number in vectors above to locate appropriate snps
# dat1 is the gwaa.data object for use in genABEL (lipid phenotypes and genotype info)

# get sign of correct direction for effects
correct.effect.vec
signs.eff = sign(correct.effect.vec)

descriptives.trait(dat1)
descriptives.marker(dat1)


# Do tests for table 2 shell

# use data.frame for these analyses, combo.dat

# first get all snp names
spn = snpnames(dat1)

#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/kure-analysis")
#load("spn.Rda")
spn # all snps in genotype data

# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^------------------------------
# Function to run linear regression model to get heritability estimate
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^------------------------------

get.h = function(df, outcome.position, outcome.left.text){
  # df: data framed
  # outcome.position: vector of positions for snps related to outcome (from a1.R)
  # outcome.left.text: left hand side of equation with outcome variable
  
  m.null = lm(as.formula(paste(outcome.left.text,
                               "scale(age, center=T, scale=F)")),
                  data=df)

  # now make a formula with all hdl-related variants
  names = spn[outcome.position]
  names

  names = names[!(names %in% 'rs13238203')] # take out this snp name. not in genotype file.}
  # to do:ask about this snp (and the other one)
  
  m.v =  lm(as.formula(paste(outcome.left.text,
                                 paste0(names, collapse= " + "), 
                                 "+ scale(age, center=T, scale=F)")),
                data=df) # note that the sign of effect doesn't matter for r.squared calcs
  summary(m.v)
  
  # difference in r.squared between null and model with all variants
  summary(m.null)$r.squared
  summary(m.v)$r.squared
  prop.var.exp = summary(m.v)$r.squared - summary(m.null)$r.squared
  return(list(var=list(prop.var.exp), names=list(names)))
  
}

# create parameters for the function above

dfs = list(combo.dat[combo.dat$sex==1,], combo.dat[combo.dat$sex==0,],
           combo.dat[combo.dat$sex==1,], combo.dat[combo.dat$sex==0,],
           combo.dat[combo.dat$sex==1,], combo.dat[combo.dat$sex==0,])

positions = list(hdl.position, hdl.position,
                 ldl.position, ldl.position,
                 tg.position, tg.position)
texts = list(rep("hdl ~",2),
             rep("ldl ~",2),
             rep("tg ~", 2))

# Run function with all parameters --------------------

list.outcomes = mapply(get.h, df=dfs,
                       outcome.position=positions,
                       outcome.left.text=texts) # apply the function over all these values to get the heritability estimates.

# put all the heritability estimates together into one data frame
vals = lapply(list.outcomes, "[[", 1) #extract out all items from list
#vals[seq(1,12,2)]
#vals

all.h = do.call(rbind.data.frame, lapply(list.outcomes, "[[", 1)[seq(1,12,2)])
head(all.h)

names(all.h) = "h2"
all.h$gender = rep(c("male", "female"), 3)
all.h$outcome = c(rep("hdl",2), rep("ldl",2), rep('tg', 2))

head(all.h)

# Convert from long to wide with outcome in rows and gender as col headers
t2 = dcast(setDT(all.h), 
           gender ~ outcome, 
           value.var=c("h2"))
t2 # values for table 2 shell


write.csv(t2, file="t2.csv") # write the h2 values to table

## @knitr table3-results

library(xtable)

xt2 <- xtable(t2, method = "compact",
              caption="Proportion of variance explained by known SNPs")

print.xtable(xt2, booktabs = TRUE, 
             caption.placement = "top",
             floating = FALSE,
             include.rownames = FALSE  # because addtorow will substitute the default row names
)



## @knitr table3-check

# check the snp names used for each outcome
# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

names.h = lapply(list.outcomes, "[[", 1)[seq(2,12,2)]
names.h[[1]] # hdl, males
names.h[[3]] # ldl, males
names.h[[5]] # tg, males

names.h[[2]] # hdl, females
names.h[[4]] # ldl, females
names.h[[6]] # tg, females
