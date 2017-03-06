# explore-pc.R

# Get PC for ancestry
# ----------------------------------------------------
# Note: got this file from Yujie on 12/5/2016 --------------------

pcs = read.delim("/nas02/depts/epi/Genetic_Data_Center/chile/phenotypes/PCs_chileonly.txt",
                 header=T)
class(pcs)
summary(pcs)
head(pcs)
sapply(pcs, class)
colnames(pcs)
colnames(pcs)[1]="iid" #make id colname same as phenotype file id below


# see if ID for PC file is same as id in my phenotype file. The NIH id?
# ----------------------------------------------------------



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

phen.2c = merge(phen.2b, pcs[,1:11], by="iid", all.x=T) # left outer join
nrow(phen.2c)
head(phen.2c)
summary(phen.2c)

# note: there are 8 people with missing PCs for this file