# table1.R

#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/kure-analysis")

## @knitr table1

library(Hmisc)
library(tableone) 
library(data.table)
library(ztable)

# Note: the next few sections are taken from create-dat1.R

ids = read.csv("IDsToMatch.csv", header=T) # ids from Anne that have the .ped/.fam file ids
colnames(ids) = tolower(colnames(ids))
ids$id_v1 = ids$id # make an id to merge onto the phenotype file
head(ids)


# Get phenotype data file --------------------------------------------
phen = read.csv("slcs-lipid-phen-2016.csv")
colnames(phen) = tolower(colnames(phen))
colnames(phen)

# Determine how many rows with any non-missing lipid data -----------
phen$miss.lip = apply(phen[colnames(phen) %in% c("a_tg",
                                                 "a_ldlc",
                                                 "a_hdlc",
                                                 "a_total_cholesterol")],
                      1, anyNA) # go through each row and indicate if any lipid values are non-missing
summary(phen$miss.lip) # 672 rows have all non-missing lipid data


# merge phenotype to genotype to select people (before .tped generation) ---------------------------------------
phen.2a = merge(phen[phen$miss.lip==F,], ids, by="id_v1", all.x=T) # left outer join
colnames(phen.2a)
nrow(phen.2a) # now file only has the 672 individuals with all non-missing lipid values.
head(phen.2a)

phen.2b = phen.2a[!(is.na(phen.2a$fid)==T),]
nrow(phen.2b) # 546 individuals with non-missing fid and lipid values. Keep these individuals for the analyses
head(phen.2b)

# make a phenotype file for use with GenAbel -------------------------

# create .dat file to include for genabel package
phendat = phen.2b[c("iid", "a_sex", "a_tg", "a_ldlc", "a_hdlc", "a_total_cholesterol",
                    "a_age", "a_bmi_derived")]

colnames(phendat)
sapply(phendat,class)
colnames(phendat) = c("id", "sex", "tg", "ldl", "hdl", "tc", "age", "bmi") # re-order names for use in GenAbel
phendat$sex = ifelse(phendat$sex=="male", 1, 0)
table(phendat$sex)

# now create summary data table using Hmisc package
# see http://biostat.mc.vanderbilt.edu/wiki/Main/HmiscSummaryFormulaFunction
summary(phendat)

# for this table convert units from mg/dL to mmol/L (multiply by 0.00555=1/18)
phendat[c("tg", "ldl", "hdl", "tc")] = apply(phendat[c("tg", "ldl", "hdl", "tc")], 2, 
                                                     function(x) (1/18)*x)
head(phendat)

# export data to make tables of data on local drive
write.csv(phendat, file="phendat.csv") # write the phendat values to table

phendat.2 = phendat[,-1]
phendat.2$log.tg = log(phendat.2$tg)

x<-upData(phendat.2, # get rid of id for summary table
          labels=c(sex="Sex", log.tg="log(TG)", tg="TG", 
                   ldl="LDL", hdl="HDL", tc="TC",
                   age="Age", bmi="BMI"),
          levels=list(sex=c("Female", "Male")),
          units=c(age="years", bmi="kg/m2", log.tg="log(mmol/l)",
                  tg="mmol/l", ldl="mmol/l",
                  hdl="mmol/l", tc="mmol/l"))
contents(x)

#tg="mg/dL", ldl="mg/dL",hdl="mg/dL", tc="mg/dL"

# make table
summary(sex ~ ., data=x, method="reverse")

myvars = c("sex", "bmi", "log.tg", "ldl", "hdl", "tc")
catvars = c("sex")
myvars2 = myvars[!(myvars %in% catvars)]
tab1 = CreateTableOne(vars=myvars2, data=phendat, strata="sex")
x$total="total"

## @knitr table1-print

latex(summary(sex ~ ., data=x[,!(colnames(x) %in% c("total"))], 
              method="reverse"),
      digits=3, file="",
      options = list( tabular="longtable"), 
      booktabs=T,
      where="H", prmsd=TRUE)


latex(summary(total ~ ., data=x, method="reverse"),
      digits=3, file="",
      options = list( tabular="longtable"), 
      booktabs=T,
      where="H", prmsd=TRUE)


## @knitr table1extra

print(summary(tab1), test=F, nonnormal=myvars2)

