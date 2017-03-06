# a4.R
# info for table 3 shell: Association between the genetic risk score and serum lipid levels

## @knitr checksnp

library(GenABEL)
#library(PredictABEL)
library(knitr)
#library(snpStats)
#biocLite("snpStats")

# ^^^^^^^^^^^^^-- get data -------------

load("a1.Rda") # load a1 object from a1.R

tes = read.csv(file="teslovich-snps.csv", header=T) # Teslovich effect sizes
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

tes.2 = tes[c("Lead.SNP", "e.size.tes", "e.allele.tes")] # Checking here with the table 1 in paper.
colnames(tes.2)[1] = 'snp'


# HDL ----------------------------------

hdl.position # position in snp vector in hdl-related snps

# check
check.1 = function(lp){
  code = coding(dat1)[lp] # coding where the second allele is the 'effect' or 'coded' one (see documentation at http://bit.ly/2dJJMsz) 
  ref = refallele(dat1)[lp] # check reference allele for all snps in analysis
  eff = effallele(dat1)[lp] # check effect allele for all snps in analysis
  eff.corr = correct.effect.vec[lp]

  dat = data.frame(gt.code=code, 
                       gt.ref=ref, 
                       gt.eff = eff, 
                       snp = names(code))
  dat = merge(dat, tes.2, by='snp', all.x=T)
  dat = merge(dat, df.fix.e, by='snp', all.x=T)
  return(dat)
}

df.hdl = check.1(hdl.position)
df.hdl$outcome = 'hdl'
df.hdl

# hdl.code = coding(dat1)[hdl.position] # coding where the second allele is the 'effect' or 'coded' one (see documentation at http://bit.ly/2dJJMsz) 
# hdl.ref = refallele(dat1)[hdl.position] # check reference allele for all snps in analysis
# hdl.eff = effallele(dat1)[hdl.position] # check effect allele for all snps in analysis
# hdl.eff.corr = correct.effect.vec[hdl.logical]
# df.fix.e$correct.effect.vec # check
# 
# hdl.dat = data.frame(gt.code=hdl.code, 
#                      gt.ref=hdl.ref, 
#                      gt.eff = hdl.eff, 
#                      snp = names(hdl.code))
# hdl.dat = merge(hdl.dat, tes.2, by='snp', all.x=T)
# hdl.dat = merge(hdl.dat, df.fix.e, by='snp', all.x=T)
# hdl.dat

# LDL ----------------------------------

df.ldl = check.1(ldl.position)
df.ldl$outcome='ldl'
df.ldl

# TG  -----------------------------------------------

df.tg = check.1(tg.position)
df.tg$outcome = 'tg'
df.tg

a.df = rbind(df.hdl, df.ldl, df.tg)
a.df

colnames(a.df)
a.df = a.df[c("snp", 
              "gt.code",
              "gt.ref",
              "e.size.tes",
              "e.allele.tes",
              "correct.effect.vec",
              "gt.eff",
              "outcome")] # reorder columns

colnames(a.df) = c("snp",
                   "genotype code",
                   "genotype ref",
                   "Teslo beta",
                   "Teslo effect",
                   "Analysis beta",
                   "genotype effect",
                   "Outcome")

write.csv(a.df, 
          file="check-snp-betas.csv")



## @knitr checksnp-table

library(xtable)

xt.cs <- xtable(a.df, method = "compact",
              caption="List of SNPS and how coded in analyses")
print.xtable(xt.cs, booktabs = TRUE, 
             caption.placement = "top",
             tabular.environment = "longtable", 
             floating = FALSE,
             include.rownames = FALSE  # because addtorow will substitute the default row names
)

             