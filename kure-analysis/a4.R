# a4.R
# info for table 4 shell: Association between the genetic risk score and serum lipid levels

## @knitr table4

library(GenABEL)
#library(PredictABEL)
library(knitr)
#library(snpStats)
#biocLite("snpStats")
library(data.table)
library(Hmisc)


#### >>>>>>>>>>----- get data -------------

load("a1.Rda") # load a1 object from a1.R

# contents of object -----------------
# ldl.position is index number of snps related to ldl risk score
# hdl.position is same as ldl.position but for hdl
# tg.position is same as above
# combo.dat is combined pheno and geno data in a data.frame for risk score analysis
#   Can use index number in vectors above to locate appropriate snps
# df.fix.e is a data frame that contains correct.effect.vec with the correct effect as matched with what is the effect allele and beta given in TEslovich along with effect allele in genotype file.
# dat1 is the gwaa.data object for use in genABEL (lipid phenotypes and genotype info)

descriptives.trait(dat1)
descriptives.marker(dat1)

# Demo area for risk score calcs -------------------------------------
# Test out how to transform the snps with negative betas to positive and switch allele count for risk score 
df2 = c(1,-2,2) # betas
df1 = data.frame(rs1=c(2,2,1),rs2=c(0,1,2), rs3=c(1,1,1)) # risk allele frequencies

vec.neg = (sign(df2)<0); vec.neg # vector indicating which columns/snps have negative betas
vec.pos = (sign(df2)>0); vec.pos

df1.neg = apply(df1[vec.neg], 2, function(x) 2-x) # reverse direction so number of effect alleles matches a positive coefficient
df1.neg

df1.rev = cbind(df1[vec.pos], df1.neg)
df1.rev = df1.rev[colnames(df1)]

rs.prep = sweep(df1.rev, MARGIN=2, abs(df2), `*`) # multiply each sum of allele frequencies by the vector of numbers
rs.prep
rs = rowSums(rs.prep)
rs # risk scores for individuals

# End of demo area -----------------------
# >>>>>>>>>>-------------------------------------

gt.df = as.numeric(gtdata(dat1)) # risk allele frequencies according to genotype file in dat1, a genabel gwaa object
# NOTE: in qc genabel omits one snp. need to follow up

# Function to make weighted risk scores for all variants
# >>>>>>>>>>------------------------------------------------------------
make.risk.scores = function(orig.dat, orig.betas) {
  
  # get vector of position for negative and positive betas
  vec.neg = (sign(orig.betas)<0); vec.neg # vector indicating which columns/snps have negative betas
  vec.pos = (sign(orig.betas)>=0); vec.pos
  
  gt.neg = apply(data.frame(orig.dat)[vec.neg], 2, function(x) 2-x) # reverse direction so number of effect alleles matches a positive coefficient
  head(gt.neg)
  
  gt.rev = cbind(data.frame(orig.dat)[vec.pos], gt.neg)
  gt.rev = gt.rev[colnames(orig.dat)] # re-order the columns back to original order
  
  rs.prep = sweep(gt.rev, MARGIN=2, abs(orig.betas), `*`) # multiply each sum of allele frequencies by the vector of numbers
  head(rs.prep)
  rs = rowSums(rs.prep)
  head(rs) # risk scores for individuals
  return(rs) # vector of risk scores for individuals
}

# Append risk scores to phenotype data
# >>>>>>>>>>--------------------------------------------------
snp.order = data.frame(snp=names(coding(dat1)))
head(snp.order)
head(df.fix.e)

df.fix.e = df.fix.e[match(snp.order$snp, df.fix.e$snp),] # need the snp order to match the genotype file
cev = df.fix.e$correct.effect.vec; cev
snp.order$snp
rs = make.risk.scores(gt.df, cev)
head(rs)


# get vector of position for negative and positive betas
vec.neg = (sign(cev)<0); vec.neg # vector indicating which columns/snps have negative betas
vec.pos = (sign(cev)>=0); vec.pos
#head(data.frame(gt.df)[vec.neg]) # gt.df is a matrix so need to convert to data frame for indexing to work
#length(colnames(gt.df)) # how many genotypes in the gt data file?

gt.neg = apply(data.frame(gt.df)[vec.neg], 2, function(x)  2-x) # reverse direction so number of effect alleles matches a positive coefficient
#head(gt.neg)

gt.rev = cbind(data.frame(gt.df)[vec.pos], gt.neg)
gt.rev = gt.rev[colnames(gt.df)] # re-order the columns back to original order

rs.prep = sweep(gt.rev, MARGIN=2, abs(cev), `*`) # multiply each sum of allele frequencies by the vector of numbers
#head(rs.prep)
rs = rowSums(rs.prep)
head(rs) # risk scores for individuals, double check with function


# >>>>>>>>>>------------------------
all.dat = cbind(dat1@phdata, make.risk.scores(gt.df, cev))
head(all.dat)

# >>>>>>>>>>---------------------------------------------
# Now run models
# >>>>>>>>>>---------------------------------------------


# get trait-specific score --------------------

# HDL ----------------------------------

hdl.position

# check
coding(dat1)[hdl.position] # coding where the second allele is the 'effect' or 'coded' one (see documentation at http://bit.ly/2dJJMsz) 
refallele(dat1)[hdl.position] # check reference allele for all snps in analysis
effallele(dat1)[hdl.position] # check effect allele for all snps in analysis

# make the risk score here
gt.df.hdl = gt.df[,hdl.position] # genotype data for hdl 
cev.hdl = cev[hdl.position]
cev.hdl

colnames(gt.df.hdl) # check snps

rs.hdl = make.risk.scores(gt.df.hdl, cev.hdl)
summary(rs.hdl)

# LDL ----------------------------------
ldl.position
gt.df.ldl = gt.df[,ldl.position]
cev.ldl = cev[ldl.position]
colnames(gt.df.ldl) #check snps

rs.ldl = make.risk.scores(gt.df.ldl, cev.ldl)
summary(rs.ldl)

# TG  -----------------------------------------------
tg.position
gt.df.tg = gt.df[,tg.position]
cev.tg = cev[tg.position]
cev.tg # check
colnames(gt.df.tg) # check

rs.tg =  make.risk.scores(gt.df.tg, cev.tg)
summary(rs.tg)

# Convert gwaa genabel object to data frame of phenotype data
# >>>>>>>>>>-----------------------------------------------------

phdat = dat1@phdata 

# Convert outcome variables to mmol/L (multiply mg/dL by 0.0555 to get mmol/L)
# >>>>>>>>>>------------------------------------------------------

phdat = within(phdat, {
  hdl.e = 0.0555*hdl
  ldl.e = 0.0555*ldl
  tg.e = 0.0555*tg
})

# make lipid specific data frames
all.dat.hdl = cbind(phdat, rs.hdl=rs.hdl)
head(all.dat.hdl)

all.dat.ldl = cbind(phdat, rs.ldl=rs.ldl)
head(all.dat.ldl)

all.dat.tg = cbind(phdat, rs.tg=rs.tg)
head(all.dat.tg)


# Run models
# >>>>>>>>>>---------------------------------------------

model.1.female = lm(hdl.e ~ scale(rs.hdl) + pc1 + pc2 + pc3 + pc4 + pc5, 
                    data=all.dat.hdl[all.dat.hdl$sex==0,])
summary(model.1.female)

model.1.male = lm(hdl.e ~ scale(rs.hdl) + pc1 + pc2 + pc3 + pc4 + pc5, 
                  data=all.dat.hdl[all.dat.hdl$sex==1,])
summary(model.1.male)

model.2.female = lm(ldl.e ~ scale(rs.ldl) + pc1 + pc2 + pc3 + pc4 + pc5, 
                    data=all.dat.ldl[all.dat.ldl$sex==0,])
summary(model.2.female)

model.2.male = lm(ldl.e ~ scale(rs.ldl) + pc1 + pc2 + pc3 + pc4 + pc5, 
                  data=all.dat.ldl[all.dat.ldl$sex==1,])
summary(model.2.male)

model.3.female = lm(tg.e ~ scale(rs.tg) + pc1 + pc2 + pc3 + pc4 + pc5, 
                    data=all.dat.tg[all.dat.tg$sex==0,])
summary(model.3.female)

model.3.male = lm(tg.e ~ scale(rs.tg) + pc1 + pc2 + pc3 + pc4 + pc5, 
                  data=all.dat.tg[all.dat.tg$sex==1,])
summary(model.3.male)

# Adjusting for BMI --------------------

model.1.female.bmi = lm(hdl.e ~ scale(rs.hdl) + bmi + pc1 + pc2 + pc3 + pc4 + pc5, 
                        data=all.dat.hdl[all.dat.hdl$sex==0,])
summary(model.1.female)

model.1.male.bmi = lm(hdl.e ~ scale(rs.hdl) + bmi + pc1 + pc2 + pc3 + pc4 + pc5, 
                      data=all.dat.hdl[all.dat.hdl$sex==1,])
summary(model.1.male)

model.2.female.bmi = lm(ldl.e ~ scale(rs.ldl) + bmi + pc1 + pc2 + pc3 + pc4 + pc5, 
                        data=all.dat.ldl[all.dat.ldl$sex==0,])
summary(model.2.female)

model.2.male.bmi = lm(ldl.e ~ scale(rs.ldl) + bmi + pc1 + pc2 + pc3 + pc4 + pc5, 
                      data=all.dat.ldl[all.dat.ldl$sex==1,])
summary(model.2.male)

model.3.female.bmi = lm(log(tg.e) ~ scale(rs.tg) + bmi + pc1 + pc2 + pc3 + pc4 + pc5, 
                        data=all.dat.tg[all.dat.tg$sex==0,])
summary(model.3.female)

model.3.male.bmi = lm(log(tg.e) ~ scale(rs.tg) + bmi + pc1 + pc2 + pc3 + pc4 + pc5, 
                      data=all.dat.tg[all.dat.tg$sex==1,])
summary(model.3.male)

# Take results from linear regressions and put in table

list.results = list(m1f = model.1.female, m1m = model.1.male,
                    m2f = model.2.female, m2m = model.2.male,
                    m3f = model.3.female, m3m = model.3.male,
                    m1f.b = model.1.female.bmi, m1m.b = model.1.male.bmi,
                    m2f.b = model.2.female.bmi, m2m.b = model.2.male.bmi,
                    m3f.b = model.3.female.bmi, m3m.b = model.3.male.bmi)

results = lapply(list.results, function(x) summary(x)$coefficients) #extract out coefficients from linear regression models

# put all the coefficient estimates together into one data frame
all.est = do.call(rbind.data.frame, results) 

all.est

# now output slope coefficients into table
# >>>>>>>>>>-------------------------------------
colnames(all.est)[2] = "std.err"
colnames(all.est)[4] = "pval"
all.est$rn = rownames(all.est)
grepl(c("Intercept|bmi|pc1|pc2|pc3|pc4|pc5") , all.est$rn) # nuisance params
  
all.est = all.est[!(grepl(c("Intercept|bmi|pc1|pc2|pc3|pc4|pc5") , all.est$rn)), ] # select out slope coefficients

all.est = within(all.est, {
  est.se = paste0(round(Estimate,2), " (", round(std.err,2), ")")
  round.p = round(pval,4)
  bmi = ifelse(grepl(".b.", rn)==T, "yes", "no")
  outcome = ifelse(grepl("m1", rn)==T, "HDL",
                   ifelse(grepl("m2", rn)==T, "LDL",
                          ifelse(grepl("m3", rn)==T, "TG", NA)))
  female = ifelse(grepl("f.", rn)==T, 1, 0)
})

all.est

# Convert from long to wide with bmi in rows and outcome/gender as col headers
t3 = dcast(setDT(all.est), 
                    outcome ~ female + bmi, 
                    value.var=c("est.se", "round.p"))
t3 # values for table 3 shell

write.csv(t3, file="t3.csv")

# prep t3 to output as table

t3.est = t3[,1:5, with=F] # estimates w/ se
t3.est
cnames.t3 =  c("Outcome", 
            "Female (no adj)", "Female (adj BMI)",
            "Male (no adj)", "Male (adj BMI)")
colnames(t3.est) = cnames.t3

t3.pval = t3[,c(1, 6:9), with=F] # p-values
t3.pval
colnames(t3.pval)=cnames.t3


## @knitr t3-demo

# demo ----------------------------------------
y = rnorm(100); y1=rnorm(100)
x = rnorm(100); x1=rnorm(100)
test.lm = lm(y~x)
test.lm2 = lm(y1~x1)
list1 = list(m1=test.lm, m2=test.lm2)
results = lapply(list1, function(x) summary(x)$coefficients) #extract out coefficients from linear regression models
results
all.est = do.call(rbind.data.frame, results) 
all.est

## @knitr table4-results1

library(xtable)

xt3 <- xtable(t3.est, method = "compact",
              caption="Association between the genetic risk score and serum lipid levels, coefficient (se)")
print.xtable(xt3, booktabs = TRUE, 
             caption.placement = "top",
             table.placement="H")

## @knitr table4-results2

library(xtable)

xt3p <- xtable(t3.pval, method = "compact", digits=4,
              caption="Association between the genetic risk score and serum lipid levels")

print.xtable(xt3p, booktabs = TRUE, 
             caption.placement = "top",
             table.placement="H",
             include.rownames = FALSE  # because addtorow will substitute the default row names
)

## @knitr table4-descriptive1

head(all.dat.hdl)
# 
# all.dat.hdl.1 = all.dat.hdl[c("rs.hdl", "sex")]; all.dat.hdl.1$lipid='hdl'
# all.dat.ldl.1 = all.dat.ldl[c("rs.ldl", "sex")]; all.dat.ldl.1$lipid='ldl'
# all.dat.tg.1 = all.dat.tg[c("rs.tg", "sex")]; all.dat.tg.1$lipid='tg'
# 
# colnames(all.dat.hdl.1)[1] = "rs"
# colnames(all.dat.ldl.1)[1] = "rs"
# colnames(all.dat.tg.1)[1] = "rs"
# 
# all.dat.lipid = rbind(all.dat.hdl.1, all.dat.ldl.1, all.dat.tg.1)

all.dat.lipid = cbind(all.dat.hdl[c("sex", "rs.hdl")], 
                      all.dat.ldl[c("rs.ldl")], 
                      all.dat.tg[c('rs.tg')])

head(all.dat.lipid)

x2<-upData(all.dat.lipid, 
          labels=c(rs.hdl="Risk score, HDL", 
                   rs.ldl="Risk score, LDL",
                   rs.tg = "Risk score, TG",
                   sex="Sex"),
          levels=list(sex=c("Female", "Male")))
contents(x2)

# make table
summary(sex ~ ., data=x2, method="reverse")

# output to file to put in poster, etc...
names(all.dat.lipid)
x2.dt = data.table(all.dat.lipid)
x2.dt
names(x2.dt)

rs.tab = x2.dt[, list(mean.hdl=mean(rs.hdl), sd.hdl=sd(rs.hdl),
                      mean.ldl=mean(rs.ldl), sd.ldl=sd(rs.ldl),
                      mean.tg=mean(rs.tg), sd.tg=sd(rs.tg)),
               by=sex]
head(rs.tab) # checking numbers below

# alternate table, for some reason this is not working
x2.dt[, sapply(.SD, function(x) list(mean=round(mean(x), 3), sd=round(sd(x), 3))),
      by=sex]

make.2 = function(x) {
  formatC(as.numeric(x), format = 'f', flag='0', digits = 2)
}

rs.tab2 <- setnames(x2.dt[, sapply(.SD, 
                                   function(x) list(median.iqr=paste0(make.2((mean(x))), 
                                                                      " (", 
                                                                      make.2(sd(x)), ")"))), 
                          by=sex],
                     c(names(all.dat.lipid))
                    )
rs.tab2

write.csv(rs.tab2, file="rs.csv")




## @knitr table4-descriptive

latex(summary(sex ~ ., data=x2, method="reverse"),
      digits=4, file="",
      prmsd=T,
      options = list( tabular="longtable"), 
      booktabs=T,
      where="H",
      caption="Descriptive statistics for the risk scores")
