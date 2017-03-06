# create-dat1.R
# make a .tped file I can move into a .raw file for analysis in GenAbel
# code adapted from Howrigan document titled, 'Cleaning SNP data for Association analysis'
# and http://mga.bionet.nsc.ru/~yurii/courses/2012.05.09_Edinburgh/useOrgMode/wOutputs/example_analysis_complete.html
# >>>>>>>>>>>>>-----------------------------------------------------------------

#system("plink",intern=TRUE) # test to make sure plink is working

# Find ids that have at least one non-missing lipid value and keep only those individuals in the transposed plink file below
# >>>>>>>>>>>>>--------------------------------------------------------------------

#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/kure-analysis")

## @knitr createdat1

ids = read.csv("IDsToMatch.csv", header=T) # ids from Anne that have the .ped/.fam file ids
colnames(ids) = tolower(colnames(ids))
ids$id_v1 = ids$id # make an id to merge onto the phenotype file
head(ids)

# extract out digits from the tubodna id
# colnames(ids) = tolower(colnames(ids))
# head(ids)
# 
# new.ids = do.call('rbind', strsplit(as.character(ids$tubodna),'-',fixed=TRUE))
# head(new.ids)
#ids$id_v1 = as.numeric(new.ids[,2])

# Get PC for ancestry
# ----------------------------------------------------




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

# MERGE phenotype file to the PC file -----------------------------------
# Note: got this file from Yujie on 12/5/2016 --------------------
pcs = read.delim("/nas02/depts/epi/Genetic_Data_Center/chile/phenotypes/PCs_chileonly.txt",
                 header=T)

colnames(pcs)[1]="iid" #make id colname same as phenotype file id below

phen.2c = merge(phen.2b, pcs[,1:11], by="iid", all.x=T) # left outer join
nrow(phen.2c)
head(phen.2c)
summary(phen.2c)
# Note: there are 8 people with missing PCs for this file

# make a phenotype file for use with GenAbel -------------------------

# create .dat file to include for genabel package
phendat = phen.2c[c("iid", "a_sex", "a_tg", "a_ldlc", "a_hdlc", "a_total_cholesterol",
                   "a_age", "a_bmi_derived",
                   "PC1", "PC2", "PC3", "PC4", "PC5")]

colnames(phendat)
sapply(phendat,class)
colnames(phendat) = c("id", "sex", "tg", "ldl", "hdl", "tc", "age", "bmi",
                      "pc1", "pc2", "pc3", 'pc4', 'pc5') # re-order names for use in GenAbel
phendat$sex = ifelse(phendat$sex=="male", 1, 0)
table(phendat$sex)

# add lipids transformed from mg/dL to mmol/l
phendat = within(phendat, {
  hdl.e = hdl/18
  ldl.e = ldl/18
  tg.e = tg/18
  tc.e = tc/18
})

# Double check that sex is coded correctly (look at .ped file coding)
check.g = read.table("/nas02/depts/epi/CVDGeneNas/SLCS/LipidsProject/FinalPlink/lipids_variants_imputed_genotyped_merged_10042016.ped")
check.g = check.g[c("V2", "V5")]
colnames(check.g) = c("id", "sex.gdat")
head(check.g)

compare.gender = merge(check.g, phendat[c("id", "sex")], by="id")
head(compare.gender)
compare.gender$sex.gdat = 2-compare.gender$sex.gdat # convert to same format as phendat
compare.gender$diff = with(compare.gender, {
  diff.sex = ifelse(sex.gdat-sex==0, 0, 1)
})

head(compare.gender)

compare.gender[compare.gender$diff.sex==1,] # people who have sex in phenotype file diff from genotype file
# NOTE: there are no people with different reported gender from sex in genotype

#phendat$id = as.character(phendat$id) # ids are integers in the .bed file
head(phendat)

# export data to load into the load.gwaa.data function below

write.table(phendat, "phen.dat")

# write ids into file so I can exclude them in .tped file --------------------

o.ids = phen.2b[c("fid", "iid")]
o.ids = o.ids[order(c(o.ids$fid, o.ids$iid)),]
head(o.ids)

write.table(o.ids, file='keepids.txt', sep='\t', row.names=F)

# >>>>>>>>>>>>>------------------------------------------------------------

# make transposed .ped file, .tped
system("plink --file /nas02/depts/epi/CVDGeneNas/SLCS/LipidsProject/FinalPlink/lipids_variants_imputed_genotyped_merged_10042016 --keep keepids.txt  --recode transpose --out lipids_variants_imputed_genotyped_merged_10042016", 
       intern=TRUE)

library(GenABEL)
#library(snpStats) # need to see if I can get this installed?

check1 = read.table("/nas02/depts/epi/CVDGeneNas/SLCS/LipidsProject/FinalPlink/lipids_variants_imputed_genotyped_merged_10042016.ped")
sapply(check1, class) # ids are integers
head(check1)
ids.p = check1[,2]
ids.p # look at list of ids

# Convert genotypic data to internal GA format

convert.snp.tped(tped="lipids_variants_imputed_genotyped_merged_10042016.tped",
                 tfam="lipids_variants_imputed_genotyped_merged_10042016.tfam",
                 out="lipid1.raw")


# >>>>>>>>>>>>>------------ combine phenotype and genotype data

dat1 <- load.gwaa.data("phen.dat", "lipid1.raw",
                       makemap = T)

save(dat1, file="dat1.Rda") # save to permanent file
