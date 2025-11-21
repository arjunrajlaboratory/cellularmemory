

#read in shaved starcode barcode file from step 3

input_directory <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/extractedData/barcodednaseq/20230726/exp1/output"
output_directory <- "/Users/jessi/RajLab Dropbox/Jess Li/Shared_JessL/paper/plots/barcodednaseq/20230726/exp1"

shavedStarcode <- read.table(file.path(input_directory, "stepThreeStarcodeShavedReads.txt"), header = TRUE)

#separate out each sample so that we can normalize to the number of reads total and set a cutoff that can be applied to all samples
#hi
a <- dplyr::filter(shavedStarcode, SampleNum == "JL_AT_1")
#lo
b <- dplyr::filter(shavedStarcode, SampleNum == "JL_AT_2")
#hi
c <- dplyr::filter(shavedStarcode, SampleNum == "JL_AT_3")
#lo->hi
d <- dplyr::filter(shavedStarcode, SampleNum == "JL_AT_4")

sampletables <- c(a,c,b,d)


  
  #subset 30bp barcodes   #collapse to combine UMIs 
  a <- a %>% dplyr::select(UMI, BC30StarcodeD8, SampleNum) %>% group_by(BC30StarcodeD8) %>% summarise(UMI = sum(UMI))
  b <- b %>% dplyr::select(UMI, BC30StarcodeD8, SampleNum) %>% group_by(BC30StarcodeD8) %>% summarise(UMI = sum(UMI))
  c <- c %>% dplyr::select(UMI, BC30StarcodeD8, SampleNum) %>% group_by(BC30StarcodeD8) %>% summarise(UMI = sum(UMI))
  d <- d %>% dplyr::select(UMI, BC30StarcodeD8, SampleNum) %>% group_by(BC30StarcodeD8) %>% summarise(UMI = sum(UMI))
  
    
  #normalize to total UMIs
  a$UMI = (a$UMI)/sum(a$UMI)*10000
  b$UMI = (b$UMI)/sum(b$UMI)*10000
  c$UMI = (c$UMI)/sum(c$UMI)*10000
  d$UMI = (d$UMI)/sum(d$UMI)*10000

  #subset everything above count of 2
  a <- dplyr::filter(a, UMI > 2)
  b <- dplyr::filter(b, UMI > 2)
  c <- dplyr::filter(c, UMI > 2)
  d <- dplyr::filter(d, UMI > 2)

  
  # Extracting the BC30StarcodeD8 columns
  a_bc30 <- a$BC30StarcodeD8
  b_bc30 <- b$BC30StarcodeD8
  c_bc30 <- c$BC30StarcodeD8
  d_bc30 <- d$BC30StarcodeD8
  
  # Finding the common values
  a_c_overlap <- intersect(a_bc30, c_bc30)
  a_b_overlap <- intersect(a_bc30, b_bc30)
  b_c_overlap <- intersect(b_bc30, c_bc30)
  b_d_overlap <- intersect(b_bc30, d_bc30)
  a_d_overlap <- intersect(a_bc30, d_bc30)
  d_c_overlap <- intersect(d_bc30, c_bc30)
  
  #create a list that is exportable
  overlap_results <- data.frame(Name = c("a_bc30", "b_bc30", "c_bc30", "d_bc30", 
                                        "a_c_overlap", "a_b_overlap", "b_c_overlap", "b_d_overlap", "a_d_overlap", "d_c_overlap"), 
                               Length = sapply(list(a_bc30, b_bc30, c_bc30, d_bc30,
                                                    a_c_overlap, a_b_overlap, b_c_overlap, b_d_overlap, a_d_overlap, d_c_overlap), length))

  write.table(overlap_results, file = "overlap_results.txt", row.names = FALSE, col.names = TRUE, sep='\t')
  
  
  
  


  library(VennDiagram)
  
  # After your existing code, add the following:
  
  # Create a list of sets
  sets <- list(a = a_bc30, b = b_bc30, c = c_bc30, d = d_bc30)
  
  # Function to create and save Venn diagram
  create_venn <- function(set_list, set_names, output_file) {
    venn <- venn.diagram(
      x = set_list,
      category.names = set_names,
      filename = output_file,
      output = TRUE,
      imagetype = "png",
      height = 3000,
      width = 3000,
      resolution = 300,
      compression = "lzw",
      lwd = 2,
      col = "black",
      alpha = 0.50,
      label.col = "black",
      cex = 1.5,
      fontfamily = "sans",
      fontface = "bold",
      cat.cex = 1.5,
      cat.fontfamily = "sans"
    )
  }
  
  # Create all possible combinations of 2, 3, and 4 sets
  combinations <- list(
    list(c("a", "b"), "ab_venn.png"),
    list(c("a", "c"), "ac_venn.png"),
    list(c("a", "d"), "ad_venn.png"),
    list(c("b", "c"), "bc_venn.png"),
    list(c("b", "d"), "bd_venn.png"),
    list(c("c", "d"), "cd_venn.png")
  )
  
  # Create and save Venn diagrams for all combinations
  for (combo in combinations) {
    set_names <- combo[[1]]
    output_file <- file.path(output_directory, combo[[2]])
    create_venn(sets[set_names], set_names, output_file)
  }
