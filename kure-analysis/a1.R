# a1.R
# initial analysis file for the SLCS lipids 2017 AHA abstract

library(GenABEL)
#library(PredictABEL)
library(knitr)
#library(snpStats)
#biocLite("snpStats")

#### ^^^^^^------ get data -------------

## @knitr datahandle

load("dat1.Rda") # load dat1 object from create-dat1.R

# quality control (doesn't need quality control because already done.)

qc = check.marker(dat1, maf=0.01, p.lev = 1e-6)
summary(qc)

# clean data

dat1.qc = dat1[qc$idok, qc$snpok]

nids(dat1.qc)
nsnps(dat1.qc)

# complete summaries for clean data

smr <- summary(gtdata(dat1.qc))

# look at data, dat1.qc is same as dat1

dat1.qc@phdata[1:10,]
dat1.qc@gtdata[1:10,1:12]
nids(dat1.qc) # how many people
nsnps(dat1.qc) # how many snps
coding(dat1.qc)[1:25] # coding where the second allele is the 'effect' or 'coded' one (see documentation at http://bit.ly/2dJJMsz) 

refallele(dat1) # check reference allele for all snps in analysis
ea = effallele(dat1) # check effect allele for all snps in analysis
length(ea) # note that if I used the qc version it omits one SNP. NEED TO FOLLOW UP!!

# check that the effect allele specified in the genotype file is the same as specified in the Tikkanen paper
# ^^^^^^---------------------------------------------

# read in effect allele from tikkanen (see snp-list.Rmd)
eff.a.tikk = read.csv("lipid-snps.csv")
head(eff.a.tikk)
gt.tikk = data.frame(snp=eff.a.tikk$snp2, 
                     eff.a.tikk = eff.a.tikk$effect.allele, 
                     beta.tikk = eff.a.tikk$beta)
head(gt.tikk)

# make a data frame with genotype data and reference allele/snp rsid
gt.df1 = data.frame(snp=names(ea), gt.eff.a = ea)
head(gt.df1)
nrow(gt.df1)
nrow(gt.tikk)

# merge the two data frames together and determine if effect allele is the same
check.1 = merge(gt.tikk, gt.df1, by='snp')
check.1
nrow(check.1)

# Change direction of beta if the effect allele is different between tikkanen and genotype file
# Note: all effects are positive in Tikkanen paper so they changed the effect allele to match the direction
check.1 = within(check.1, {
  beta.2 = ifelse((as.character(substr(gt.eff.a,1,1))==
                   as.character(substr(eff.a.tikk,1,1)))==T,
                  beta.tikk,
                  -1*beta.tikk)
})



# Now make sure that the direction of these snps now matches that of the Teslovich 2010 paper
# doi:10.1038/nature09270
# I read in table 1 from this paper into phantompdf and saved as a Word file. then cut and pasted
# the word table into excel. Then saved as a .csv file. Re-checked all effect sizes for all snps in csv file
# with the pdf to make sure I have the correct effect sizes as listed in the paper.
# ^^^^^^---------------------------------------------------------------
#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/kure-analysis/")

tes = read.csv(file="teslovich-snps.csv", header=T)
head(tes)

tes = within(tes, {
  
  major.allele.tes = substr(Alleles.MAF, 1, 1)
  minor.allele.tes = substr(Alleles.MAF, 3, 3)

  # Match the all positive values from Buscot (meant for polygenic risk score)
  # if the beta is negative then the effect allele will be the major allele after
  # inverse sign (negative to positive)
  e.size.tes = abs(Effect.size)
  e.allele.tes = ifelse(Effect.size>0, minor.allele.tes,
                             major.allele.tes) 

  e.size = as.numeric(substr(as.character(tes$Effect.size), 2,
                             nchar(as.character(Effect.size))))
})

check3 = tes[c("Lead.SNP", "e.size.tes", "e.allele.tes")] # Checking here with the table 1 in paper.
check3
colnames(check3)[1] = 'snp'
nrow(check3)

check.4 = merge(check3, check.1, by='snp', all.y=T)
check.4$same.effect.check = ifelse((round(check.4$e.size.tes,2)==round(check.4$beta.2,2))==T, 1, 0)
check.4
nrow(check.4)

# the snps that have different values in Teslovich than the Tikkanan -- due to different effect allele.
# keep the effect size as the Tikkanen effect size because the effect allele is different than what is indicated in TEslovich
# ^^^^^^--------------------------------------------------
check.4[check.4$same.effect.check==0,]

check.4 = within(check.4, {
  effect.fixed = ifelse(same.effect.check==0 & (gt.eff.a==e.allele.tes)==T, 
                        e.size.tes, 
                        beta.2)
})

check.4[check.4$same.effect.check==0,] # now recheck with Teslovich table 1 to make sure effect propertly matches the genotype 
# effect allele. it does.


df.fix.e = data.frame(snp=check.4$snp, correct.effect.vec = check.4$effect.fixed) # should have 76 values
df.fix.e

# Now coerce genetic data into data frame I can use to make risk score
# ^^^^^^-----------------------------------------
gt.df = as.numeric(gtdata(dat1.qc))
ph.df = dat1.qc@phdata

head(gt.df)
head(ph.df)

dim(gt.df)
dim(ph.df)

combo.dat = cbind(gt.df, ph.df)
str(combo.dat)


# Get a list of snpnames so I can figure out which ones to select for the GRS
# ^^^^^^--------------------------------------------------------

spn = snpnames(dat1)
save(spn, file="spn.Rda") # Save list of snp names so I can figure out which ones belong in the grs

#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/kure-analysis")
#load("spn.Rda")

head(spn)
spn # all snps in genotype data

# rs2126259 is not in the list of snps, but is a snp of interest and in Teslovich and Tikkanen. Why not here?
"rs2126259" %in% spn
"rs12678919" %in% spn
c('rs10321548', 'rs17149780', 'rs386558067', 'rs60484430') %in% spn #these are archive snps from dbsnp that are synonyms




# Explore data ------------------------------------------
# ^^^^^^----------------------------------------------------

# Note: can subset an object of the gwaa.data class by [i,j]: i=index of study subject and j is index of snp
summary(gtdata(dat1[1:3, 1:5]))

# Note: a1=allele frequency for allele 1, a2=allele frequency for allele 2, q.2=coded allele frequencies

perid.summary(dat1[1:10,])


# make a list of snp positions in the dat1 object so I can make the genetic risk score
# ^^^^^^---------------------------------------------------------------------
getwd()

# first get list of snps -----------------------
#setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs")

lipids = read.csv("lipid-snps.csv") # get file from snp-list.Rmd program
lipids

lip.ldl = lipids[lipids$trait=="ldl", c("snp2")]
lip.ldl

ldl.logical = (spn %in% lip.ldl); ldl.logical

# position number of ldl snps in the spn object, representing the GenAbel object
ldl.position = match(lip.ldl, spn); length(ldl.position)
ldl.position

ldl.single.position = match(c('rs6511720'), spn)
ldl.single.position

summary(dat1[,ldl.single.position])

hdl.position = match(lipids[lipids$trait=="hdl", c("snp2")], spn)
hdl.position
length(hdl.position)
hdl.logical = (spn %in% lipids[lipids$trait=="hdl", c("snp2")]); hdl.logical

tg.position = match(lipids[lipids$trait=="tg", c("snp2")], spn)
tg.position
length(tg.position)
tg.logical = (spn %in% lipids[lipids$trait=="tg", c("snp2")]); tg.logical


# total number of snps is 76
length(ldl.position) + length(hdl.position) + length(tg.position)


# Save data into .Rda file so I can use it in another program
# ^^^^^^--------------------------------------------------------

save(ldl.logical, ldl.position,
     hdl.logical, hdl.position,
     tg.logical, tg.position,
     combo.dat, df.fix.e, 
     dat1, file="a1.Rda")

# ldl.position is index number of snps related to ldl risk score
# hdl.position is same as ldl.position but for hdl
# tg.position is same as above
# combo.dat is combined pheno and geno data in a data.frame for risk score analysis
#   Can use index number in vectors above to locate appropriate snps
# df.fix.e is a data frame that contains correct.effect.vec with the correct effect as matched with what is the effect allele and beta given in TEslovich along with effect allele in genotype file.
# dat1 is the gwaa.data object for use in genABEL (lipid phenotypes and genotype info)

