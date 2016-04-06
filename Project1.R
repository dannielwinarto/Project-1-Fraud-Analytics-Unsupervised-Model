setwd("C:/Users/dan_9/Desktop/DSO 599 fraud Analytics/Project 1 Fraud Analytics Unsupervised Model")
data=read.csv("property values.csv")
library(dplyr)

### Create our varaibles in the dataset and change INF to NA
data1=data%>%
  mutate(x1=AVTOT/(LTFRONT*LTDEPTH))%>%
  mutate(x2=AVLAND/(LTFRONT*LTDEPTH))%>%
  mutate(x3=AVTOT/(BLDFRONT*BLDDEPTH))%>%
  mutate(x4=AVLAND/STORIES)%>%
  mutate(x5=FULLVAL/(LTFRONT*LTDEPTH))

data1$x1[!is.finite(data1$x1)] <-NA
data1$x2[!is.finite(data1$x2)] <-NA
data1$x3[!is.finite(data1$x3)] <-NA
data1$x4[!is.finite(data1$x4)] <-NA
data1$x5[!is.finite(data1$x5)] <-NA

### Create the dataset only has the following column
data2=data1[,c("BLOCK","LOT","BLDGCL","TAXCLASS","ZIP","x1","x2","x3","x4","x5")]

### Create the dataset that has no NA value in every column and adding the rownames for joining score in later purpose
data3=na.omit(data2)
data3[,11] = rownames(data3)
colnames(data3)[11] = "rowIndex"  

dim(data3)[1]/dim(data)[1] #76.72 percent of initial dataset remaining

### TOP FIVE FREQUENCIES FOR BLOCK,LOT,BLDGCL,TAXCLASS,ZIP
head(sort(table(data3$BLOCK),decreasing = T),5)
# Block: 1047 2925 7405 7422 1484 
# Count: 835  572  539  482  460 
head(sort(table(data3$LOT),decreasing = T),5)
# LOT: 1    20    14    15    12 
# COUNT: 20776 11338 11254 11252 11233
head(sort(table(data3$BLDGCL),decreasing = T),5)
# BLDGCL: A1     A5     B1     B2     C0 
# COUNT: 119266  92759  83940  72996  72956 
head(sort(table(data3$TAXCLASS),decreasing = T),5)
# TAX:  1      4      2     2A     2B 
# COUNT: 642001  59548  40912  40433  13781 
head(sort(table(data3$ZIP),decreasing = T),5)
# ZIP: 10314 11234 10306 11385 11236 
# COUNT: 20956 19414 15685 14342 14324 
#####################################################
### Block Analysis
#####################################################
block_1047 = data3[data3[,1] == 1047,] 
block_2925 = data3[data3[,1] == 2925,] 
block_7405 = data3[data3[,1] == 7405,] 
block_7422 = data3[data3[,1] == 7422,] 
block_1484 = data3[data3[,1] == 1484,] 
block_others = data3[data3[,1] !=1047 & data3[,1] !=2925  & data3[,1] !=7405  & data3[,1] !=7422  & data3[,1] !=1484,]
nrow(block_1047) + nrow(block_2925)+ nrow(block_7405)+ nrow(block_7422)+ nrow(block_1484)+ nrow(block_others) #total row = 804471

## transforming each data by dividing with mean

block_1047_t = block_1047[6:11]
block_2925_t = block_2925[6:11]
block_7405_t = block_7405[6:11]
block_7422_t = block_7422[6:11]
block_1484_t = block_1484[6:11]
block_others_t = block_others[6:11]

for(i in 1 : 5){
  block_1047_t[,i] = block_1047[,(i+5)]/mean(block_1047[,i+5])
}
for(i in 1 : 5){
  block_2925_t[,i] = block_2925[,(i+5)]/mean(block_2925[,i+5])
}
for(i in 1 : 5){
  block_7405_t[,i] = block_7405[,(i+5)]/mean(block_7405[,i+5])
}
for(i in 1 : 5){
  block_7422_t[,i] = block_7422[,(i+5)]/mean(block_7422[,i+5])
}
for(i in 1 : 5){
  block_1484_t[,i] = block_1484[,(i+5)]/mean(block_1484[,i+5])
}
for(i in 1 : 5){
  block_others_t[,i] = block_others[,(i+5)]/mean(block_others[,i+5])
}

##z-scaling and combining all the data

block_1047_tz = block_1047_t
block_2925_tz = block_2925_t
block_7405_tz = block_7405_t
block_7422_tz = block_7422_t
block_1484_tz = block_1484_t
block_others_tz = block_others_t

for(i in 1 : 5){
  block_1047_tz[,i] =  (block_1047_t[,i] - mean(block_1047_t[,i]))/sd(block_1047_t[,i])
}

for(i in 1 : 5){
  block_2925_tz[,i] =  (block_2925_t[,i] - mean(block_2925_t[,i]))/sd(block_2925_t[,i])
}

for(i in 1 : 5){
  block_7405_tz[,i] =  (block_7405_t[,i] - mean(block_7405_t[,i]))/sd(block_7405_t[,i])
}

for(i in 1 : 5){
  block_7422_tz[,i] =  (block_7422_t[,i] - mean(block_7422_t[,i]))/sd(block_7422_t[,i])
}

for(i in 1 : 5){
  block_1484_tz[,i] =  (block_1484_t[,i] - mean(block_1484_t[,i]))/sd(block_1484_t[,i])
}

for(i in 1 : 5){
  block_others_tz[,i] =  (block_others_t[,i] - mean(block_others_t[,i]))/sd(block_others_t[,i])
}

block_1047_tz[,c(1:5)] = abs(block_1047_tz[,c(1:5)])
block_2925_tz[,c(1:5)] = abs(block_2925_tz[,c(1:5)])
block_7405_tz[,c(1:5)] = abs(block_7405_tz[,c(1:5)])
block_7422_tz[,c(1:5)] = abs(block_7422_tz[,c(1:5)])
block_1484_tz[,c(1:5)] = abs(block_1484_tz[,c(1:5)])
block_others_tz[,c(1:5)] = abs(block_others_tz[,c(1:5)])


block_1047_tz[,7] = rowSums(block_1047_tz[,c(1:5)])
block_2925_tz[,7] = rowSums(block_2925_tz[,c(1:5)])
block_7405_tz[,7] = rowSums(block_7405_tz[,c(1:5)])
block_7422_tz[,7] = rowSums(block_7422_tz[,c(1:5)])
block_1484_tz[,7] = rowSums(block_1484_tz[,c(1:5)])
block_others_tz[,7] = rowSums(block_others_tz[,c(1:5)])


blockScore = rbind(block_1047_tz[,c(6,7)],
                   block_2925_tz[,c(6,7)],
                   block_7405_tz[,c(6,7)],
                   block_7422_tz[,c(6,7)],
                   block_1484_tz[,c(6,7)],
                   block_others_tz[,c(6,7)]
)
colnames(blockScore)[2] = "blockScore" 
head(blockScore,20)
#####################################################
### LOT Analysis
# LOT: 1    20    14    15    12 
# COUNT: 20776 11338 11254 11252 11233
#####################################################
lot_1 = data3[data3[,2] == 1,] 
lot_2 = data3[data3[,2] == 20,] 
lot_3 = data3[data3[,2] == 14,] 
lot_4 = data3[data3[,2] == 15,] 
lot_5 = data3[data3[,2] == 12,] 
lot_others = data3[data3[,2] !=1 & data3[,2] !=20  & data3[,2] !=14  & data3[,2] !=15  & data3[,2] !=12,]
nrow(lot_1) + nrow(lot_2)+ nrow(lot_3)+ nrow(lot_4)+ nrow(lot_5)+ nrow(lot_others) #total row = 804471

1## transforming each data by dividing with mean

lot_1_t = lot_1[6:11]
lot_2_t = lot_2[6:11]
lot_3_t = lot_3[6:11]
lot_4_t = lot_4[6:11]
lot_5_t = lot_5[6:11]
lot_others_t = lot_others[6:11]

for(i in 1 : 5){
  lot_1_t[,i] = lot_1[,(i+5)]/mean(lot_1[,i+5])
}
for(i in 1 : 5){
  lot_2_t[,i] = lot_2[,(i+5)]/mean(lot_2[,i+5])
}
for(i in 1 : 5){
  lot_3_t[,i] = lot_3[,(i+5)]/mean(lot_3[,i+5])
}
for(i in 1 : 5){
  lot_4_t[,i] = lot_4[,(i+5)]/mean(lot_4[,i+5])
}
for(i in 1 : 5){
  lot_5_t[,i] = lot_5[,(i+5)]/mean(lot_5[,i+5])
}
for(i in 1 : 5){
  lot_others_t[,i] = lot_others[,(i+5)]/mean(lot_others[,i+5])
}

##z-scaling and combining all the data

lot_1_tz = lot_1_t
lot_2_tz = lot_2_t
lot_3_tz = lot_3_t
lot_4_tz = lot_4_t
lot_5_tz = lot_5_t
lot_others_tz = lot_others_t

for(i in 1 : 5){
  lot_1_tz[,i] =  (lot_1_t[,i] - mean(lot_1_t[,i]))/sd(lot_1_t[,i])
}

for(i in 1 : 5){
  lot_2_tz[,i] =  (lot_2_t[,i] - mean(lot_2_t[,i]))/sd(lot_2_t[,i])
}

for(i in 1 : 5){
  lot_3_tz[,i] =  (lot_3_t[,i] - mean(lot_3_t[,i]))/sd(lot_3_t[,i])
}

for(i in 1 : 5){
  lot_4_tz[,i] =  (lot_4_t[,i] - mean(lot_4_t[,i]))/sd(lot_4_t[,i])
}

for(i in 1 : 5){
  lot_5_tz[,i] =  (lot_5_t[,i] - mean(lot_5_t[,i]))/sd(lot_5_t[,i])
}

for(i in 1 : 5){
  lot_others_tz[,i] =  (lot_others_t[,i] - mean(lot_others_t[,i]))/sd(lot_others_t[,i])
}

lot_1_tz[,c(1:5)] = abs(lot_1_tz[,c(1:5)])
lot_2_tz[,c(1:5)] = abs(lot_2_tz[,c(1:5)])
lot_3_tz[,c(1:5)] = abs(lot_3_tz[,c(1:5)])
lot_4_tz[,c(1:5)] = abs(lot_4_tz[,c(1:5)])
lot_5_tz[,c(1:5)] = abs(lot_5_tz[,c(1:5)])
lot_others_tz[,c(1:5)] = abs(lot_others_tz[,c(1:5)])

lot_1_tz[,7] = rowSums(lot_1_tz[,c(1:5)])
lot_2_tz[,7] = rowSums(lot_2_tz[,c(1:5)])
lot_3_tz[,7] = rowSums(lot_3_tz[,c(1:5)])
lot_4_tz[,7] = rowSums(lot_4_tz[,c(1:5)])
lot_5_tz[,7] = rowSums(lot_5_tz[,c(1:5)])
lot_others_tz[,7] = rowSums(lot_others_tz[,c(1:5)])


lotScore = rbind(lot_1_tz[,c(6,7)],
                 lot_2_tz[,c(6,7)],
                 lot_3_tz[,c(6,7)],
                 lot_4_tz[,c(6,7)],
                 lot_5_tz[,c(6,7)],
                 lot_others_tz[,c(6,7)]
)
colnames(lotScore)[2] = "lotScore" 

#####################################################
### BLDGCL Analysis
# BLDGCL: A1     A5     B1     B2     C0 
# COUNT: 119266  92759  83940  72996  72956
#####################################################

bld_1 = data3[data3[,3] == "A1",] 
bld_2 = data3[data3[,3] == "A5",] 
bld_3 = data3[data3[,3] == "B1",] 
bld_4 = data3[data3[,3] == "B2",] 
bld_5 = data3[data3[,3] == "C0",] 
bld_others = data3[data3[,3] !="A1" & data3[,3] !="A5"  & data3[,3] !="B1"  & data3[,3] !="B2"  & data3[,3] !="C0",]
nrow(bld_1) + nrow(bld_2)+ nrow(bld_3)+ nrow(bld_4)+ nrow(bld_5)+ nrow(bld_others) #total row = 804471

## transforming each data by dividing with mean

bld_1_t = bld_1[6:11]
bld_2_t = bld_2[6:11]
bld_3_t = bld_3[6:11]
bld_4_t = bld_4[6:11]
bld_5_t = bld_5[6:11]
bld_others_t = bld_others[6:11]

for(i in 1 : 5){
  bld_1_t[,i] = bld_1[,(i+5)]/mean(bld_1[,i+5])
}
for(i in 1 : 5){
  bld_2_t[,i] = bld_2[,(i+5)]/mean(bld_2[,i+5])
}
for(i in 1 : 5){
  bld_3_t[,i] = bld_3[,(i+5)]/mean(bld_3[,i+5])
}
for(i in 1 : 5){
  bld_4_t[,i] = bld_4[,(i+5)]/mean(bld_4[,i+5])
}
for(i in 1 : 5){
  bld_5_t[,i] = bld_5[,(i+5)]/mean(bld_5[,i+5])
}
for(i in 1 : 5){
  bld_others_t[,i] = bld_others[,(i+5)]/mean(bld_others[,i+5])
}

##z-scaling and combining all the data

bld_1_tz = bld_1_t
bld_2_tz = bld_2_t
bld_3_tz = bld_3_t
bld_4_tz = bld_4_t
bld_5_tz = bld_5_t
bld_others_tz = bld_others_t

for(i in 1 : 5){
  bld_1_tz[,i] =  (bld_1_t[,i] - mean(bld_1_t[,i]))/sd(bld_1_t[,i])
}

for(i in 1 : 5){
  bld_2_tz[,i] =  (bld_2_t[,i] - mean(bld_2_t[,i]))/sd(bld_2_t[,i])
}

for(i in 1 : 5){
  bld_3_tz[,i] =  (bld_3_t[,i] - mean(bld_3_t[,i]))/sd(bld_3_t[,i])
}

for(i in 1 : 5){
  bld_4_tz[,i] =  (bld_4_t[,i] - mean(bld_4_t[,i]))/sd(bld_4_t[,i])
}

for(i in 1 : 5){
  bld_5_tz[,i] =  (bld_5_t[,i] - mean(bld_5_t[,i]))/sd(bld_5_t[,i])
}

for(i in 1 : 5){
  bld_others_tz[,i] =  (bld_others_t[,i] - mean(bld_others_t[,i]))/sd(bld_others_t[,i])
}

bld_1_tz[,c(1:5)] = abs(bld_1_tz[,c(1:5)])
bld_2_tz[,c(1:5)] = abs(bld_2_tz[,c(1:5)])
bld_3_tz[,c(1:5)] = abs(bld_3_tz[,c(1:5)])
bld_4_tz[,c(1:5)] = abs(bld_4_tz[,c(1:5)])
bld_5_tz[,c(1:5)] = abs(bld_5_tz[,c(1:5)])
bld_others_tz[,c(1:5)] = abs(bld_others_tz[,c(1:5)])

bld_1_tz[,7] = rowSums(bld_1_tz[,c(1:5)])
bld_2_tz[,7] = rowSums(bld_2_tz[,c(1:5)])
bld_3_tz[,7] = rowSums(bld_3_tz[,c(1:5)])
bld_4_tz[,7] = rowSums(bld_4_tz[,c(1:5)])
bld_5_tz[,7] = rowSums(bld_5_tz[,c(1:5)])
bld_others_tz[,7] = rowSums(bld_others_tz[,c(1:5)])


bldScore = rbind(bld_1_tz[,c(6,7)],
                 bld_2_tz[,c(6,7)],
                 bld_3_tz[,c(6,7)],
                 bld_4_tz[,c(6,7)],
                 bld_5_tz[,c(6,7)],
                 bld_others_tz[,c(6,7)]
)
colnames(bldScore)[2] = "bldScore" 

#####################################################
### TAX Analysis
# TAX:  1      4      2     2A     2B 
# COUNT: 642001  59548  40912  40433  13781 
#####################################################
tax_1 = data3[data3[,4] == 1,] 
tax_2 = data3[data3[,4] == 4,] 
tax_3 = data3[data3[,4] == 2,] 
tax_4 = data3[data3[,4] == "2A",] 
tax_5 = data3[data3[,4] == "2B",] 
tax_others = data3[data3[,4] !=1 & data3[,4] !=4  & data3[,4] !=2  & data3[,4] !="2A"  & data3[,4] !="2B",]
nrow(tax_1) + nrow(tax_2)+ nrow(tax_3)+ nrow(tax_4)+ nrow(tax_5)+ nrow(tax_others) #total row = 804471

## transforming each data by dividing with mean

tax_1_t = tax_1[6:11]
tax_2_t = tax_2[6:11]
tax_3_t = tax_3[6:11]
tax_4_t = tax_4[6:11]
tax_5_t = tax_5[6:11]
tax_others_t = tax_others[6:11]

for(i in 1 : 5){
  tax_1_t[,i] = tax_1[,(i+5)]/mean(tax_1[,i+5])
}
for(i in 1 : 5){
  tax_2_t[,i] = tax_2[,(i+5)]/mean(tax_2[,i+5])
}
for(i in 1 : 5){
  tax_3_t[,i] = tax_3[,(i+5)]/mean(tax_3[,i+5])
}
for(i in 1 : 5){
  tax_4_t[,i] = tax_4[,(i+5)]/mean(tax_4[,i+5])
}
for(i in 1 : 5){
  tax_5_t[,i] = tax_5[,(i+5)]/mean(tax_5[,i+5])
}
for(i in 1 : 5){
  tax_others_t[,i] = tax_others[,(i+5)]/mean(tax_others[,i+5])
}

##z-scaling and combining all the data

tax_1_tz = tax_1_t
tax_2_tz = tax_2_t
tax_3_tz = tax_3_t
tax_4_tz = tax_4_t
tax_5_tz = tax_5_t
tax_others_tz = tax_others_t

for(i in 1 : 5){
  tax_1_tz[,i] =  (tax_1_t[,i] - mean(tax_1_t[,i]))/sd(tax_1_t[,i])
}

for(i in 1 : 5){
  tax_2_tz[,i] =  (tax_2_t[,i] - mean(tax_2_t[,i]))/sd(tax_2_t[,i])
}

for(i in 1 : 5){
  tax_3_tz[,i] =  (tax_3_t[,i] - mean(tax_3_t[,i]))/sd(tax_3_t[,i])
}

for(i in 1 : 5){
  tax_4_tz[,i] =  (tax_4_t[,i] - mean(tax_4_t[,i]))/sd(tax_4_t[,i])
}

for(i in 1 : 5){
  tax_5_tz[,i] =  (tax_5_t[,i] - mean(tax_5_t[,i]))/sd(tax_5_t[,i])
}

for(i in 1 : 5){
  tax_others_tz[,i] =  (tax_others_t[,i] - mean(tax_others_t[,i]))/sd(tax_others_t[,i])
}

tax_1_tz[,c(1:5)] = abs(tax_1_tz[,c(1:5)])
tax_2_tz[,c(1:5)] = abs(tax_2_tz[,c(1:5)])
tax_3_tz[,c(1:5)] = abs(tax_3_tz[,c(1:5)])
tax_4_tz[,c(1:5)] = abs(tax_4_tz[,c(1:5)])
tax_5_tz[,c(1:5)] = abs(tax_5_tz[,c(1:5)])
tax_others_tz[,c(1:5)] = abs(tax_others_tz[,c(1:5)])

tax_1_tz[,7] = rowSums(tax_1_tz[,c(1:5)])
tax_2_tz[,7] = rowSums(tax_2_tz[,c(1:5)])
tax_3_tz[,7] = rowSums(tax_3_tz[,c(1:5)])
tax_4_tz[,7] = rowSums(tax_4_tz[,c(1:5)])
tax_5_tz[,7] = rowSums(tax_5_tz[,c(1:5)])
tax_others_tz[,7] = rowSums(tax_others_tz[,c(1:5)])

taxScore = rbind(tax_1_tz[,c(6,7)],
                 tax_2_tz[,c(6,7)],
                 tax_3_tz[,c(6,7)],
                 tax_4_tz[,c(6,7)],
                 tax_5_tz[,c(6,7)],
                 tax_others_tz[,c(6,7)]
)
colnames(taxScore)[2] = "taxScore" 

#####################################################
### ZIP Analysis
# ZIP: 10314 11234 10306 11385 11236 
# COUNT: 20956 19414 15685 14342 14324 
#####################################################
zip_1 = data3[data3[,5] == 10314,] 
zip_2 = data3[data3[,5] == 11234,] 
zip_3 = data3[data3[,5] == 10306,] 
zip_4 = data3[data3[,5] == 11385,] 
zip_5 = data3[data3[,5] == 11236,] 
zip_others = data3[data3[,5] !=10314 & data3[,5] !=11234  & data3[,5] !=10306  & data3[,5] !=11385  & data3[,5] !=11236,]
nrow(zip_1) + nrow(zip_2)+ nrow(zip_3)+ nrow(zip_4)+ nrow(zip_5)+ nrow(zip_others) #total row = 804471

## transforming each data by dividing with mean

zip_1_t = zip_1[6:11]
zip_2_t = zip_2[6:11]
zip_3_t = zip_3[6:11]
zip_4_t = zip_4[6:11]
zip_5_t = zip_5[6:11]
zip_others_t = zip_others[6:11]

for(i in 1 : 5){
  zip_1_t[,i] = zip_1[,(i+5)]/mean(zip_1[,i+5])
}
for(i in 1 : 5){
  zip_2_t[,i] = zip_2[,(i+5)]/mean(zip_2[,i+5])
}
for(i in 1 : 5){
  zip_3_t[,i] = zip_3[,(i+5)]/mean(zip_3[,i+5])
}
for(i in 1 : 5){
  zip_4_t[,i] = zip_4[,(i+5)]/mean(zip_4[,i+5])
}
for(i in 1 : 5){
  zip_5_t[,i] = zip_5[,(i+5)]/mean(zip_5[,i+5])
}
for(i in 1 : 5){
  zip_others_t[,i] = zip_others[,(i+5)]/mean(zip_others[,i+5])
}

##z-scaling and combining all the data

zip_1_tz = zip_1_t
zip_2_tz = zip_2_t
zip_3_tz = zip_3_t
zip_4_tz = zip_4_t
zip_5_tz = zip_5_t
zip_others_tz = zip_others_t

for(i in 1 : 5){
  zip_1_tz[,i] =  (zip_1_t[,i] - mean(zip_1_t[,i]))/sd(zip_1_t[,i])
}

for(i in 1 : 5){
  zip_2_tz[,i] =  (zip_2_t[,i] - mean(zip_2_t[,i]))/sd(zip_2_t[,i])
}

for(i in 1 : 5){
  zip_3_tz[,i] =  (zip_3_t[,i] - mean(zip_3_t[,i]))/sd(zip_3_t[,i])
}

for(i in 1 : 5){
  zip_4_tz[,i] =  (zip_4_t[,i] - mean(zip_4_t[,i]))/sd(zip_4_t[,i])
}

for(i in 1 : 5){
  zip_5_tz[,i] =  (zip_5_t[,i] - mean(zip_5_t[,i]))/sd(zip_5_t[,i])
}

for(i in 1 : 5){
  zip_others_tz[,i] =  (zip_others_t[,i] - mean(zip_others_t[,i]))/sd(zip_others_t[,i])
}

zip_1_tz[,c(1:5)] = abs(zip_1_tz[,c(1:5)])
zip_2_tz[,c(1:5)] = abs(zip_2_tz[,c(1:5)])
zip_3_tz[,c(1:5)] = abs(zip_3_tz[,c(1:5)])
zip_4_tz[,c(1:5)] = abs(zip_4_tz[,c(1:5)])
zip_5_tz[,c(1:5)] = abs(zip_5_tz[,c(1:5)])
zip_others_tz[,c(1:5)] = abs(zip_others_tz[,c(1:5)])

zip_1_tz[,7] = rowSums(zip_1_tz[,c(1:5)])
zip_2_tz[,7] = rowSums(zip_2_tz[,c(1:5)])
zip_3_tz[,7] = rowSums(zip_3_tz[,c(1:5)])
zip_4_tz[,7] = rowSums(zip_4_tz[,c(1:5)])
zip_5_tz[,7] = rowSums(zip_5_tz[,c(1:5)])
zip_others_tz[,7] = rowSums(zip_others_tz[,c(1:5)])


zipScore = rbind(zip_1_tz[,c(6,7)],
                 zip_2_tz[,c(6,7)],
                 zip_3_tz[,c(6,7)],
                 zip_4_tz[,c(6,7)],
                 zip_5_tz[,c(6,7)],
                 zip_others_tz[,c(6,7)]
)
colnames(zipScore)[2] = "zipScore" 

##########################################################################################
##########################################################################################
##JOINING THE SCORE DATASET
##########################################################################################
##########################################################################################
head(lotScore)
dim(lotScore)
head(blockScore)
dim(lotScore)
dim(lotScore)
head(taxScore)
dim(taxScore)

ScoreCompiled = inner_join(inner_join(inner_join(inner_join(lotScore, blockScore),taxScore ),zipScore),bldScore)
finalScore = data.frame(ScoreCompiled[,1],rowSums(ScoreCompiled[,c(2:6)]))
colnames(finalScore) = c("rowIndex", "totalScore")

summary(finalScore$totalScore)
hist(finalScore$totalScore)
plot(finalScore$totalScore, ylab = "Total Score", xlab ="rowIndex/ID", 
     main = "Scatter plot of  Total Score" )

quantile(finalScore$totalScore, probs = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
#       0%          5%         10%         25%         50%         75%         90%         95%        100% 
# 1.157079    2.350473    2.692988    3.406586    4.531219    6.088292    8.781467   12.006262 3136.674247 

###Finding the outliers
# outliers are top 5% of the total score 
outliers = finalScore[finalScore[,2] >= 12.006262,]
dim(outliers)

mean(finalScore$totalScore)
sd(finalScore$totalScore)
library(Hmisc)
describe(finalScore$totalScore)
quantile(finalScore$totalScore, probs = c(.95))

### Outlier Analysis

library(ggplot2)

summary(finalScore$totalScore)

outliers=boxplot(finalScore$totalScore, ylab = "Total score",main = "Boxplot of Total Score")$out

nrow(Final_Score_final[Final_Score_final$Score_sum %in% outliers,])

### 50 bins quantile analysis and histogram based on the quantile distribuion

quantile(finalScore$totalScore,probs = seq(0,1,0.02))

ggplot(finalScore,aes(x=totalScore))+
  geom_histogram(bins=35)+
  xlim(0,25) + ggtitle("Histogram of the Total Score")

# Scratch:
# how this is not working??? ask abbass:
# z = inner_join(lotScore,blockScore)
# a = z %>% mutate(final = inner_join(z,taxScore))


# finalScore$quantileGroups = cut(finalScore$totalScore, 
#                         breaks = quantile(finalScore$totalScore))
# 
# finalScore$scoreGroups = cut(finalScore$totalScore, 
#                                 breaks = quantile(finalScore$totalScore, probs = seq(from = 0, to = 1, by = 0.05)))
# table(finalScore$quantileGroups,finalScore$scoreGroups)
# 

?quantile
