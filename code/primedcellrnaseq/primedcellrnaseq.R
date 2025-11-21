
library(dplyr)
library(tidyr)
library(ggplot2)
library(GenomicFeatures)
library(rtracklayer)
library(stringr)
library(ggfortify)
library(reshape2)
library(DESeq2)
library(ComplexHeatmap)


setwd("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/primedcellrnaseq")


tpmData <- read.csv("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/primedcellrnaseq/WM989_timeMachineSort_TPMdata.csv", header = TRUE, sep = ",")
metadata <- read.csv("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/primedcellrnaseq/WM989_timeMachineSort_metaData_jessAppended.csv", header = TRUE, sep = ",")
  
tpmDataAndMeta <- left_join(tpmData, metadata, by = "sampleID")
data <- tpmDataAndMeta

dataFiltered <- filter(data, gene_biotype=='protein_coding')
  
forMatrix <- dataFiltered[,c(2,4,14)]
forMatrix <- forMatrix %>% group_by(gene_name, sampleID) %>% summarise(Log2TPM = mean(log2TPM))
forMatrixSpread <- forMatrix %>% spread(sampleID, Log2TPM)
rownames <- forMatrixSpread$gene_name
  
temp <- forMatrixSpread[, 2:16]
temp <- data.matrix(temp)
row_sd <- apply(temp, 1, sd) 
row_sum <- apply(temp, 1, sum) 
temp <- cbind(temp, sd = row_sd)
temp <- cbind(temp, sum = row_sum)
rownames(temp) = rownames

tempDF <- data.frame(temp)
tempDF <- dplyr::filter(tempDF, sd > 3)
  
rownamesFiltered <- rownames(tempDF)
tempFiltered <- data.matrix(tempDF[,1:15])
  
list <- c("TM22A_CC_A647Neg3", "TM22A_CC_A647Neg2","TM22A_CC_A647Neg1","TM22_CC_Neg","TM16_CC_Neg","TM16_1uMA_CarbonCopy_barcodeNeg2","TM16_1uMA_CarbonCopy_barcodeNeg1", #non-primed
            "TM22A_CC_A647Pos","TM16_1uMA_CarbonCopy_barcodePos", #primed
            "TM22A_PLX_GFPPos","TM22A_PLX_A647Pos","TM22_PLX","TM16_PLX","TM16_1uMA_PLX_barcodePos","TM16_1uMA_PLX_barcodeNeg") #plx resistant
kept <- c("TM22_CC_Neg","TM16_1uMA_CarbonCopy_barcodeNeg1", #non-primed
            "TM22A_CC_A647Pos","TM16_1uMA_CarbonCopy_barcodePos", #primed
            "TM22_PLX","TM16_PLX","TM16_1uMA_PLX_barcodePos","TM16_1uMA_PLX_barcodeNeg") #plx resistant
  
orderedMatrix <- tempFiltered[,list]
orderedMatrixKeep <- tempFiltered[,kept]

genes_to_remove <- grep("\\.", rownames(orderedMatrixKeep))
orderedMatrixKeep <- orderedMatrixKeep[-genes_to_remove,]

pheatmap(orderedMatrix, cluster_cols=T)
pheatmap(orderedMatrix, cutree_rows = 8, cluster_cols=F, color=colorRampPalette(c("navy", "white", "red"))(50))
  
pheatmap(orderedMatrixKeep, cluster_cols=T)

# customizing pdf output
pdf("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/primedcellrnaseq/primedcellrnaseq_heatmap.pdf",         # File name
    width = 8, height = 16, # Width and height in inches
    bg = "white"
    )

# create heatmap
pheatmap(orderedMatrixKeep, cutree_rows = 5, cluster_cols=F,  color=colorRampPalette(c("grey", "#ECAB21", "#C23B1C","#C23B1C"))(100))

dev.off()




#examples of specific genes
geneNames <- c("AXL", "SERPINE1", "ITGA3")

# Loop through each gene name
for(geneName in geneNames) {
  
  test <- filter(data, gene_name == geneName)
  test$condition <- factor(test$condition, levels = c("non-primed", "primed", "PLX-resistant"))
  
  bp <- ggplot(data = test, aes(x=condition,y=tpm))+
    geom_boxplot(aes(fill=condition),alpha=0.7, outlier.shape = NA)+
    ylab('reads in tpm')+
    theme_minimal()+
    ggtitle(geneName)
  
  # Generate file path dynamically based on gene name
  filePath <- paste0("/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/primedcellrnaseq/", geneName, ".pdf")
  
  pdf(filePath,    # File name
      width = 6, height = 6, # Width and height in inches
      bg = "white"
  )
  
  print(bp + scale_fill_manual(values=c("#707071", '#ECAB21', "#C23B1C")))
  
  dev.off()
}

