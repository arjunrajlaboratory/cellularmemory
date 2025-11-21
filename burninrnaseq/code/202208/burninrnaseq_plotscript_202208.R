
library(dplyr)
library(ggplot2)
library(pheatmap)
library(tidyr)
library(GenomicFeatures)
library(rtracklayer)
library(viridis)

input_directory <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/burninrnaseq/202208"
output_directory <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/burninrnaseq/202208"
 
data <- read.table(file.path(input_directory, "log2CPM.txt"), header = TRUE, sep = "\t")

dataCPM <- read.table(file.path(input_directory, "UnLOG2CPM.txt"), header = TRUE, sep = "\t")  



targets <- read.table(file.path(input_directory,'studydesign.tsv'), header = TRUE, sep = "\t")

dataCPM <- left_join(dataCPM, targets, by = "samples")
dataCPM <- dataCPM[,c(1:3,5:12)]

data <- left_join(data, targets, by = "samples")
data <- data[,c(1:3,5:11)]


dataCPM$treatment <- factor(dataCPM$treatment,levels = c("DMSO","dexamethasone","compound C","AICAR","PMA","trametinib","dex+tram","cmpdC+tram","AICAR+tram","PMA+tram","tram+JNKIN8", "dex+tram+JNKIN8","cmpdC+tram+JNKIN8","AICAR+tram+JNKIN8","PMA+tram+JNKIN8", "tram+T5224", "dex+tram+T5224","cmpdC+tram+T5224","AICAR+tram+T5224","PMA+tram+T5224"))
data$treatment <- factor(data$treatment,levels = c("DMSO","dexamethasone","compound C","AICAR","PMA","trametinib","dex+tram","cmpdC+tram","AICAR+tram","PMA+tram","tram+JNKIN8", "dex+tram+JNKIN8","cmpdC+tram+JNKIN8","AICAR+tram+JNKIN8","PMA+tram+JNKIN8", "tram+T5224", "dex+tram+T5224","cmpdC+tram+T5224","AICAR+tram+T5224","PMA+tram+T5224"))


#### isolate dex data
dexCPMData <- dplyr::filter(dataCPM, group.x == "dexamethasone")
dexData <- dplyr::filter(data, group.x == "dexamethasone")


hg19_GTF <- file.path(input_directory, "Homo_sapiens.GRCh38.107.gtf")
GTF <- readGFF(hg19_GTF)



#### graph for one gene                                 


gene = "FKBP5"

graphData <- dplyr::filter(dexCPMData, geneID == gene & condition.y %in% c("DMSO_1","Dex",
                                                                           #"pre_Dex",
                                                                           "pre_DMSO_1","T_1","pre_T_1","pre_T_8_1"
                                                                           #,"pre_T_5224_1"
                                                                           ))
graphData$condition.y <- factor(graphData$condition.y,levels = c("DMSO_1","Dex","pre_Dex","pre_DMSO_1","T_1","pre_T_1","pre_T_8_1","pre_T_5224_1"))

log2cpmplot <- ggplot(data = graphData, aes(x=condition.y, y=expression))+
  ggtitle(gene)+
  geom_boxplot(aes(fill=treatment.x),alpha=0.2, outlier.shape = NA)+
  geom_jitter(color="black", size= 1, alpha=0.9)+
  theme_bw()+ylab('log2 CPM')

cpmplot <- ggplot(data = graphData, aes(x=condition.y, y=unLog2Expression))+
  ggtitle(gene)+
  geom_boxplot(aes(fill=treatment.x),alpha=0.2, outlier.shape = NA)+
  geom_jitter(color="black", size= 1, alpha=0.9)+
  theme_bw()+ylab('CPM')

cpmplot

# Filename for the plot
filename <- "fkbp5_cpm.pdf"
# Save the plot
ggsave(filename = file.path(output_directory, filename), plot = cpmplot, width = 8, height = 8, dpi = 300)

 




samplesAbv <- c("Sample_1", "Sample_2", "Sample_3", #dmso (1-3)
                "Sample_4", "Sample_5", "Sample_6", #dex11d (4-6),
                "Sample_16", "Sample_17", "Sample_18", #dex14d
                "Sample_13", "Sample_14", "Sample_15", #dexOff (7-9)
                "Sample_7", "Sample_8", "Sample_9", #trametinib (10-12)
                "Sample_19", "Sample_20", "Sample_21", #burnIn (13-15)
                "Sample_37", "Sample_38", "Sample_39"  #pre+JNK8+T
               ,"Sample_43", "Sample_44", "Sample_45" #pre+T5224+T
                ) 

geneVarSamples <- c("Sample_1", "Sample_2", "Sample_3", #dmso (1-3)
                    "Sample_4", "Sample_5", "Sample_6", #dmso --> dex (4-6)
                    "Sample_13", "Sample_14", "Sample_15", #dex --> dmso
                    "Sample_16","Sample_17","Sample_18") #dex




#### create list of genes whose expression is altered by treatment by dexamethasone

geneVarData <- dexCPMData[,c(1:3,10)]
geneVarData <- dplyr::filter(geneVarData, samples %in% geneVarSamples) 

gene_id_to_biotype <- GTF[-which(duplicated(GTF$gene_id)),c(9,11,12,13)]
geneVarData <- left_join(geneVarData, gene_id_to_biotype, by=c('geneID'='gene_name'))
geneVar <- dplyr::filter(geneVarData, gene_biotype=='protein_coding')

geneVar <- geneVar %>% group_by(geneID, samples) %>% summarise(cpm = mean(unLog2Expression))

geneVarMatrix <- geneVar[,c(1:3)]
geneVarMatrix <- spread(geneVarMatrix,samples,cpm)
rownamesGeneVar <- geneVarMatrix$geneID

geneVarMatrix <- dplyr::mutate(geneVarMatrix, dmso14d = (Sample_1 + Sample_2 + Sample_3)/3)
geneVarMatrix <- dplyr::mutate(geneVarMatrix, dex11d = (Sample_4 + Sample_5 + Sample_6)/3)
geneVarMatrix <- dplyr::mutate(geneVarMatrix, dexOff = (Sample_13 + Sample_14 + Sample_15)/3)
geneVarMatrix <- dplyr::mutate(geneVarMatrix, dex14d = (Sample_16 + Sample_17 + Sample_18)/3)

geneVarMatrix <- dplyr::mutate(geneVarMatrix, dexFC = log((dex14d/dmso14d),2))


write.table(geneVarMatrix, file = file.path(output_directory, "geneVarMatrix.csv"), row.names = F, col.names = T, quote = T, sep =",")

geneVarList <- geneVarMatrix %>% dplyr::filter(abs(dexFC) > 3)


geneVarLog <- geneVarData %>% group_by(geneID, samples) %>% summarise(Log2cpm = mean(expression))
geneVarLogMatrix <- spread(geneVarLog,samples,Log2cpm)
geneVarLogMatrix <- dplyr::filter(geneVarLogMatrix, geneID %in% geneVarList$geneID) 

rownamesGeneVarLog <- geneVarLogMatrix$geneID
gvlm <- geneVarLogMatrix[,c(2,9,10,6,7,8,3,4,5)]
gvlm <- data.matrix(gvlm)
rownames(gvlm) = rownamesGeneVarLog


pheatmap(gvlm, cutree_rows = 5, cluster_cols=F, color=colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(50))

## use genes identified by dex comparison to evaluate the effect of trametinib ----
                    
forMatrix <- dexCPMData[,c(1:3)]
forMatrix <- dplyr::filter(forMatrix, samples %in% samplesAbv) 

forMatrix <- dplyr::filter(forMatrix, geneID %in% geneVarList$geneID)
                          
forMatrix <- spread(forMatrix,samples,expression)
rownames <- forMatrix$geneID

forMatrix <- forMatrix[, c(2,7,10,14,18,19,3:5,20:22,6,8,9,11:13)]

forMatrix <- data.matrix(forMatrix)
rownames(forMatrix) = rownames

pheatmap(forMatrix, cutree_rows = 4, cluster_cols=F, color=viridis(250))

# Open PDF device
pdf(file.path(output_directory, "burninrnaseq_heatmap_virdis.pdf"), width = 8, height = 11)  # Width and height in inches
pheatmap(forMatrix, cutree_rows = 4, cluster_cols=F, color=viridis(250))
# Close the PDF device
dev.off()

pdf(file.path(output_directory, "burninrnaseq_heatmap_sienna.pdf"), width = 8, height = 11)  # Width and height in inches
pheatmap(forMatrix, cutree_rows = 4, cluster_cols=F, color=colorRampPalette(c("#F7F7F7", "#7c2b11"))(50))
# Close the PDF device
dev.off()

pheatmap(forMatrix, cutree_rows = 4, cluster_cols=F, color=colorRampPalette(c("#F7F7F7", "#7c2b11"))(250))


#find genes for FISH probes
#geneVarMatrix <- dplyr::mutate(geneVarMatrix, dex14d = (Sample_16 + Sample_17 + Sample_18)/3)

geneVarMatrix <- dplyr::mutate(geneVarMatrix, dexOffFC = log((dex14d/dexOff), 2))
#dex turn on > log2FC of 3
geneVarMatrix <- geneVarMatrix %>% dplyr::filter(dexFC > 3)
#dex turns off > log2FC of 2 (dex v dex off)
geneVarMatrix <- geneVarMatrix %>% dplyr::filter(dexOffFC > 2)

forprobetarget <- dexCPMData[,c(1:3,10)]
forprobetarget <- dplyr::filter(forprobetarget, samples %in% samplesAbv) 
forprobetarget <- forprobetarget %>% group_by(geneID, samples) %>% summarise(cpm = mean(unLog2Expression))

forprobetarget <- spread(forprobetarget,samples,cpm)

forprobetarget <- dplyr::filter(forprobetarget, geneID %in% geneVarList$geneID)

rownames <- forprobetarget$geneID

#tram does not turn on more log2FC of 5
forprobetarget <- dplyr::mutate(forprobetarget, tram = (Sample_7 + Sample_8 + Sample_9)/3)
forprobetarget <- dplyr::mutate(forprobetarget, dmso = (Sample_1 + Sample_2 + Sample_3)/3)
forprobetarget <- dplyr::mutate(forprobetarget, tramFC = log((tram/dmso), 2))

forprobetarget <- dplyr::mutate(forprobetarget, dex14d = (Sample_16 + Sample_17 + Sample_18)/3)
forprobetarget <- dplyr::mutate(forprobetarget, dexFC = log((dex14d/dmso), 2))
forprobetarget <- forprobetarget %>% dplyr::filter(tramFC < 5)

#dex-> tram turns on more than dmso->tram with a log2FC > 1
forprobetarget <- dplyr::mutate(forprobetarget, tramburnin = (Sample_19 + Sample_20 + Sample_21)/3)
forprobetarget <- dplyr::mutate(forprobetarget, burninFC = log((tramburnin/tram), 2))
forprobetarget <- forprobetarget %>% dplyr::filter(burninFC > 1.2)














