# tables-slides.R
# All .csv files are summary statistics from Kure server
# after running \kure-analysis\make-summary.R

# Table 1 data -------------------------------
# -----------------------------------------------

library(ztable)
library(data.table)
library(htmlTable)
library(readxl)
library(ggplot2)
library(plyr)

## @knitr table1-1
# setwd("C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-1/programs/slides")
phendat = read.csv("phendat.csv")
head(phendat)

phendat[,4] = log(phendat[,4])
colnames(phendat)[4] = "log(tg)"
t1.dt = data.table(phendat[,-c(1,2)])
head(t1.dt)

# # get median and iqr + add names to the columns 
# means <- setnames(t1.dt[, sapply(.SD, function(x) list(median=round(median(x), 3), iqr=round(IQR(x), 3))), 
#                         by=sex],
#                   c("sex", sapply(names(t1.dt)[-1], paste0, c(".median", ".iqr"))))
# head(means)

means.cl <- setnames(t1.dt[, sapply(.SD, function(x) list(median.iqr=paste0(round(median(x), 2), " (", 
                                                                     round(IQR(x), 2), ")"))), 
                        by=sex],
                  names(t1.dt))
head(means.cl)
class(means.cl)

means.cl$country="Chile"

# read in prs from ~\Dropbox\unc.grad.school\my-papers\ms-201608-1\programs\kure-analysis\a4.R
rsdat = read.csv("rs.csv")
means.cl = cbind(means.cl, rsdat[,3:5]); means.cl

# Add info from Tikkanen 2010 paper
# DOI: 10.1161/CIRCGENETICS.111.960369

tikk.dem.dat = data.frame(
  sex=c(0,1),   
  log.tg=c("0.900 (0.37)", "0.911 (0.39)"),
  ldl = c('3.07 (0.79)', '2.91 (0.79)'),
  hdl = c('1.55 (0.29)', '1.34 (0.24)'),
  tc = c('5.02 (0.89)', '4.67 (0.84)'),
  age=c(18,18),
  country = c("Finland", "Finland"),
  bmi=c('--','--'),
  rs.hdl = c("32.46 (3.36)", "32.62 (3.41)"), #Note this are based on Buscot study because Tikkanen didn't publish their average prs
  rs.ldl = c('42.1 (6.60)', '41.9 (6.90)'),
  rs.tg = c('132.71 (16.81)', '131.91 (15.72)')
)
colnames(tikk.dem.dat)[2]="log(tg)"

means = rbind.fill(means.cl, tikk.dem.dat)
data.table(means)

# fix so table is in format for slides
means.2 = dcast.data.table(melt(setDT(means), id.vars = c("country", "sex")), 
                      variable ~ country + sex); means.2

means.2$variable =  c("log(TG (mmol/l))", "LDL-C (mmol/l)", "HDL-C (mmol/l)", "TC (mmol/l)", 
                       "Age (years)", "BMI (kg/m2)", 'HDL wGRS', 'LDL wGRS', 'TG wGRS')

# get number of people in group for Chile
tab.gender = table(phendat$sex); tab.gender
colnames(means.2) = c("Measure", paste0("n=", tab.gender[1]),
                      paste0("n=", tab.gender[2]), "n=661", "n=555")
dim(means.2)

options(ztable.colnames.bold=TRUE)
z = ztable(means.2, tabular=T)
# ,
#            zebra=2,
#            zebra.color = "platinum",
#            include.rownames=FALSE)

cgroup = c("", "Chile", "Finland")
ncgp = c(1,2,2)

z = addcgroup(z, cgroup=cgroup, n.cgroup=ncgp)
z = update_ztable(z, include.rownames=F)

## @knitr table1-2

#z = addcgroup(z, cgroup = cgroup, n.cgroup=ncgp)
#z = addrgroup(z, rgroup=rgroup, n.rgroup=nrgp)

# update_ztable(z,
#               include.rownames=F,
#               caption="Descriptive statistics, median (interquartile range)",
#               size=12)

htmlTable(means.2, rnames=F,
          cgroup=cgroup, n.cgroup=ncgp,
          align = paste(rep("l", ncol(means.2)), collapse = ""),
          caption="Descriptive statistics by gender, median (interquartile range)")


# Table 2, prop var --------------------------------
# -------------------------------------------------------

## @knitr table2-1

# t2.csv is from a3.R program in ~\Dropbox\unc.grad.school\my-papers\ms-201608-1\programs\kure-analysis\

t2 = read.csv("t2.csv")
t2

rownames(t2) = NULL
t2.1 = t2[,-1]
t2.1[,-1] = round(t2.1[,-1], 3) 
t2.1

#colnames(t2.1) = c("Gender", "HDL-C", "LDL-C", "TG")
colnames(t2.1) = NULL

# z2 = ztable(t2.1,
#            zebra=2,
#            zebra.color = "platinum")
# 
# 
# update_ztable(z2,
#               include.rownames=F)
# caption="Proportion of lipid traits variance explained by lipid-related variants, by gender"

## @knitr table2-2

htmlTable(t2.1, rnames=F,
          n.cgroup=c(1,1,1),
          cgroup = c("Gender", "HDL-C", "LDL-C", "TG"),
          caption="Proportion of lipid traits variance explained by lipid-related variants, by gender")


## @knitr fig2-1

# make bar chart

t2 = read.csv("t2.csv")
t2[,-1]
t2$gender = ifelse(t2$gender=="female", "Female", "Male")
colnames(t2)[2:5] = c("Gender", "HDL-C", "LDL-C", "TG")
t2.long = melt(t2[,-1], id.vars="Gender"); t2.long
t2.long$Country =  "Chile (n=546)"

# add Finnish estimates

# Specify sheet with a number or name
tt2 = read_excel("compare-est-rev.xlsx", sheet = "table 2, Tikkanen")
tt2

tt2.long = melt(tt2, id.vars="Gender"); 
tt2.long$Country="Finland (n=1,216)"
tt2.long

# append Chile to Finland
tboth = rbind(t2.long, tt2.long)
names(tboth)
tboth$Country = factor(tboth$Country, levels=c("Finland (n=1,216)", "Chile (n=546)"))

levels(tboth$Country) # switch levels so appears in correct order on plots

## @knitr fig2-2

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

ggplot(tboth, 
       aes(x=variable, y=value, fill=Country)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Country",
                    breaks=c("Finland (n=1,216)", "Chile (n=546)"),
                    values=cbPalette) +
  facet_grid( ~ Gender) +
  theme_bw(base_size = 18) +
  xlab("Lipid Outcome") +
  ylab("Proportion Variance") +
  theme(legend.position='bottom')

## @knitr fig2-2poster


# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

ggplot(tboth, 
       aes(x=variable, y=value, fill=Country)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(name="Country",
                    breaks=c("Finland (n=1,216)", "Chile (n=546)"),
                    values=cbPalette) +
  facet_grid( ~ Gender) +
  theme_bw(base_size = 40) +
  xlab("Lipid Outcome") +
  ylab("Proportion Variance") +
  theme(legend.position='bottom')


# Table 3, PRS -----------------------------------------------
# -------------------------------------------------------


## @knitr table3-1

# t2.csv is from a4.R program in ~\Dropbox\unc.grad.school\my-papers\ms-201608-1\programs\kure-analysis\

t3 = read.csv(file="t3.csv")
t3

# prep t3 to output as table

t3.est = t3[,2:6] # estimates w/ se
t3.est
cnames.t3 =  c("Outcome/trait", 
            rep(c("Not adjusted", "Adjusted for BMI"),2))
colnames(t3.est) = cnames.t3
t3.est

t3.pval = t3[,c(2, 7:10)] # p-values
t3.pval
colnames(t3.pval)=cnames.t3

# Make the ztable for PRS coefficients + se
z3 = ztable(t3.est,
           zebra=2,
           zebra.color = "platinum")

cgroup = c("", "Female", "Male")
ncgp = c(1,2,2)
z3 = addcgroup(z3, cgroup = cgroup, n.cgroup=ncgp)

update_ztable(z3,
              include.rownames=F,
              caption="Association between the genetic risk score and serum lipid levels, coefficient (SE)")

## @knitr table3-2

htmlTable(t3.est, rnames=F,
          n.cgroup = c(1, 2, 2),
          cgroup = c("", "Female", "Male"), # 0 is female and 1 is male
          align = paste(rep("l", ncol(t3.est)), collapse = ""),
          caption="Association between the genetic risk score and serum lipid levels, coefficient (SE)")


## @knitr fig3-1

t3f = read.csv(file="t3.csv")
t3f

# Specify sheet with a number or name
tt4 = read_excel("compare-est.xlsx", sheet = "table 4, Tikkanen")
tt4
colnames(tt4) = c("lipid", "Female", "Male")
tt4$country = "Finland (n=1,216)"

t3.sub = t3f[,c(2,3,5)] # take out unadjusted estimates
colnames(t3.sub) = c("lipid", "Female", "Male")
t3.sub$country = "Chile (n=546)"



fig3.both = rbind(tt4, t3.sub)
fig3.both

fig3.long = melt(fig3.both, id.var=c("lipid",'country'))
fig3.long

fig3.long$est = as.numeric(substr(fig3.long$value,1,4))
fig3.long$se = as.numeric(substr(fig3.long$value,7,10))
fig3.long


## @knitr fig3-2


# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Define the top and bottom of the errorbars
limits <- aes(ymax = est + 2*se, ymin = est - 2*se)

p <- ggplot(fig3.long, aes(y=est, x=country, colour=country)) +
  scale_colour_manual("Country", values=cbPalette,
                      breaks=c("Finland (n=1,216)", "Chile (n=546)")) +
  facet_grid(lipid~variable) +
  geom_point(size=4) +
  geom_errorbar(limits, width=0.25, lwd=2) +
  theme_bw(base_size = 18) +
  coord_flip() +
  #  xlab("Country") +
  ylab("Coefficient* (95% CI)") + xlab("") +
  geom_hline(yintercept = 0, colour="grey", linetype = "longdash", lwd=1) +
  theme(axis.text.y=element_blank(),
        strip.text.y = element_text(angle = 0), 
        legend.position='bottom',
        legend.text=element_text(size=18)) 


p


## @knitr fig3-2-poster

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Define the top and bottom of the errorbars
limits <- aes(ymax = est + 2*se, ymin = est - 2*se)

p <- ggplot(fig3.long, aes(y=est, x=country, colour=country)) +
  scale_colour_manual("Country", values=cbPalette,
                      breaks=c("Finland (n=1,216)", "Chile (n=546)")) +
  facet_grid(lipid~variable) +
  geom_point(size=12) +
  geom_errorbar(limits, width=0.25, lwd=4) +
  theme_bw(base_size = 40) +
  coord_flip() +
#  xlab("Country") +
  ylab("Coefficient* (95% CI)") + xlab("") +
  geom_hline(yintercept = 0, colour="grey", linetype = "longdash", lwd=1) +
  theme(axis.text.y=element_blank(),
        strip.text.y = element_text(angle = 0), 
        legend.position='bottom',
        legend.text=element_text(size=40)) 

p


# Table 4: Association tests -----------------------------------------------
# -------------------------------------------------------

## @knitr assoc-1

load("a2-sum.Rda") # see \kure-analysis\a2.R for source of data 
# (run on Kure, only using summary data on local drive.)
class(s1)
# sumdat: summary of snps, s1: hdl, s2: ldl, s3: tg, s4: tc; A2 is ref allele and A1 is predictor allele
head(sumdat) # Q.2 is allele freq for A2
sumdat$rsid = rownames(sumdat)


hdl = s1[1,c(1,4:10)]; hdl$Trait="HDL-C"; hdl$rsid = rownames(hdl); hdl
#sumdat[sumdat$rsid %in% hdl$rsid,] # check that a1 and a2 are same in each file
hdl = merge(hdl, sumdat[,colnames(sumdat) %in% c("rsid", "Q.2")], by.x="rsid")

ldl = s2[1:3,c(1,4:10)]; ldl$Trait="LDL-C"; ldl$rsid = rownames(ldl)
ldl = merge(ldl, sumdat[,colnames(sumdat) %in% c("rsid", "Q.2")], by.x="rsid"); ldl

tg = s3[1:2,c(1,4:10)]; tg$Trait="TG"; tg$rsid = rownames(tg); tg
tg = merge(tg, sumdat[,colnames(sumdat) %in% c("rsid", "Q.2")], by.x="rsid"); tg

tc = s4[1,c(1,4:10)]; tc$Trait="TC"; tc$rsid = rownames(tc); tc
tc = merge(tc, sumdat[,colnames(sumdat) %in% c("rsid", "Q.2")], by.x="rsid"); tc

assoc = rbind(hdl, ldl, tg, tc); assoc
assoc$effect = paste0(round(assoc$effB,3), " (", round(assoc$se_effB,3), ")")
assoc$P1df = round(assoc$P1df, 4); assoc
colnames(assoc)[c(3,4,6,11,12,9)] = c("Ref. Allele", "Minor allele",  "effect",  "MAF", 
                                      'Effect (se)',"p-value")
#assoc$Locus = c("CETP", "LIPC", "LDLR", 
#                "GCKR", "APOA1-C3-A4-A5", "LDLR")
assoc
assoc$ref.min = paste0(assoc[,3], "/", assoc[,4])
colnames(assoc)[13] = c("Ref/Min allele")
colnames(assoc)

assoc2 = assoc[,c(1,10,13,11,12,9)]
assoc2[,4] = round(assoc2[,4],2)
assoc2

## @knitr assoc-2

htmlTable(assoc2,
          rnames=F,
          align = paste0(rep("l", ncol(assoc2))),
          caption="Association between single variants and and serum lipid levels, additive model",
          align.header = c(rep("l",ncol(assoc2)))
)


## @knitr fig-assoc-1

# Specify sheet with a number or name
tt3 = read_excel("compare-est-rev.xlsx", sheet = "table 3, Tikkanen")
# NOTE: I revised the excel file to update included snps
tt3
colnames(tt3)[1] = "rsid"
tt3$effect = as.numeric(substr(tt3$`Effect (se)`,1,6)); tt3$effect
tt3$se = as.numeric(substr(tt3$`Effect (se)`,9,13)); tt3$se
tt3


fa = tt3[,c(1,2,6:7)]; fa
colnames(fa) = c("rsid", "Trait", "Effect", "se")
fa$country = "Finland (n=1,216)"


ca = assoc[,c(1,10,6,7)]; ca
colnames(ca) = c("rsid", "Trait", "Effect", "se"); ca
ca$country="Chile (n=546)"

both.assoc = rbind(fa,ca)
both.assoc$rsid.trait = paste0( both.assoc$rsid, ": \n",both.assoc$Trait)
both.assoc


## @knitr fig-assoc-2

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Define the top and bottom of the errorbars
limits <- aes(ymax = Effect + 2*se, ymin = Effect - 2*se)

pa <- ggplot(both.assoc, aes(y=Effect, x=country, color=country)) +
  scale_colour_manual("Country", values=cbPalette,
                      breaks=c("Finland (n=1,216)", "Chile (n=546)")) +
  facet_grid(rsid.trait~.) +
  geom_point(size=4) +
  geom_errorbar(limits, width=0.75, lwd=2) +
  theme_bw(base_size = 18) +
  coord_flip()+
  xlab("") +  ylab("Coefficient (95% CI)") +
  scale_y_continuous(breaks = c(-1, -0.5, -0.25, 0, 0.25, 0.5,1)) +
  theme(axis.text.y=element_blank(),
        strip.text.y = element_text(angle = 0), 
        legend.position='bottom',
        legend.text=element_text(size=18)) +
  geom_hline(yintercept = 0, colour="gray", linetype = "longdash", lwd=2)

pa


## @knitr fig-assoc-2-poster

# make text bigger and change colors for figure on poster


# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#999999", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# Define the top and bottom of the errorbars
limits <- aes(ymax = Effect + 2*se, ymin = Effect - 2*se)

pa <- ggplot(both.assoc, aes(y=Effect, x=country, color=country)) +
  scale_colour_manual("Country", values=cbPalette,
                      breaks=c("Finland (n=1,216)", "Chile (n=546)")) +
  facet_grid(rsid.trait~.) +
  geom_point(size=12) +
  geom_errorbar(limits, width=0.75, lwd=4) +
  theme_bw(base_size = 40) +
  coord_flip()+
  xlab("") +  ylab("Coefficient (95% CI)") +
  scale_y_continuous(breaks = c(-1, -0.5, -0.25, 0, 0.25, 0.5,1)) +
  theme(axis.text.y=element_blank(),
        strip.text.y = element_text(angle = 0), 
        legend.position='bottom',
        legend.text=element_text(size=40)) +
  geom_hline(yintercept = 0, colour="gray", linetype = "longdash", lwd=2)

pa

#axis.text.x = element_text(angle = -90, hjust = 1),


# Tikkanen tables ------------------------------------------
# ----------------------------------------------------------
# Note: copied values from paper into an excel spreadsheet following same 
# format as prior tables.


# Table 1: Descriptive ---------------------------------

## @knitr t-t1-1

# Specify sheet with a number or name
tt1 = read_excel("compare-est.xlsx", sheet = "table 1, Tikkanen")
tt1 = tt1[1:5,]
colnames(tt1)=NULL

## @knitr t-t1-2

htmlTable(tt1,
          rnames=F,
          n.cgroup = c(1, 1, 1),
          cgroup = c("Outcome/Trait", "Female", "Male"), # 0 is female and 1 is male
          align = paste(rep("l", ncol(tt1)), collapse = ""),
          align.header = c(rep("l",3)),
          caption="Descriptive statistics")


# Table 2: prop var explained tests ---------------------------------

## @knitr t-t2-1

# Specify sheet with a number or name
tt2 = read_excel("compare-est.xlsx", sheet = "table 2, Tikkanen")
tt2

## @knitr t-t2-2

htmlTable(tt2,
          rnames=F,
          align = paste(rep("l", ncol(tt2)), collapse = ""),
          align.header = c(rep("l",4)),
          caption="Proportion of lipid traits variance explained by lipid-related variants, by gender")


# Table 3: Association tests ---------------------------------

## @knitr t-t3-1

# Specify sheet with a number or name
tt3 = read_excel("compare-est.xlsx", sheet = "table 3, Tikkanen")
tt3
colnames(tt3)[1] = "rs id"

## @knitr t-t3-2

htmlTable(tt3,
          rnames=F,
          align = paste(rep("l", ncol(tt3)), collapse = ""),
          caption="Association between the genetic risk score and serum lipid levels, coefficient (SE)")



# Table 4: PRS -------------------------------------------------

## @knitr t-t4-1

# Specify sheet with a number or name
tt4 = read_excel("compare-est.xlsx", sheet = "table 4, Tikkanen")
tt4
colnames(tt4) = NULL

## @knitr t-t4-2

htmlTable(tt4,
          rnames=F,
          n.cgroup = c(1, 1, 1),
          cgroup = c("Outcome/Trait", "Female", "Male"), # 0 is female and 1 is male
          align = paste(rep("l", ncol(tt4)), collapse = ""),
          caption="Association between the genetic risk score and serum lipid levels, coefficient (SE)")


