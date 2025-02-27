library(readxl)
library(dplyr)
library(gplots)
library(tibble)

# HEK293T_hACE2
HEK293T_hACE2_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                                           col_types = c("text", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric"))
View(HEK293T_hACE2_sheet)

HEK293T_hACE2_filtered_with_padj <- HEK293T_hACE2_sheet %>%
                                    filter(padj < 0.05,
                                    log2FoldChange >= 2 | log2FoldChange <= -1.5) 

View(HEK293T_hACE2_filtered_with_padj)

HEK293T_hACE2_filtered <- HEK293T_hACE2_sheet %>%
                          filter(log2FoldChange >= 2 | log2FoldChange <= -1.5) 

colnames(HEK293T_hACE2_filtered)[1] <- "Gene"
View(HEK293T_hACE2_filtered)
dim(HEK293T_hACE2_filtered) # 78 rows and 6 column

# HEK293T_hACE2_log2FC
HEK293T_hACE2_log2FC_filtered <- HEK293T_hACE2_filtered[,c("Gene","log2FoldChange")]
colnames(HEK293T_hACE2_log2FC_filtered)[2] <- "HEK293T_hACE2"
View(HEK293T_hACE2_log2FC_filtered)


# filter gene
gene_names <- HEK293T_hACE2_filtered$Gene
filtered_gene_name <- unique(gene_names)
filtered_gene_name

filtered_gene <- c(
  "TNFRSF9", "PTAFR", "IL23R", "CTH", "PTGER3", "LOC100131564",
  "TBX15", "TXNIP", "MIR5087", "HSPA6", "HSPA7", "SLC9C2",
  "PIGR", "FCAMR", "G0S2", "ATF3", "MRC1", "DDIT4",
  "P4HA1", "LGI1", "CYP2C18", "CYP2C19", "SNHG1", "RAPGEF3",
  "INHBE", "DDIT3", "SLC17A8", "IGF1", "PCK2", "DICER1-AS1",
  "CHAC1", "ANPEP", "NUPR1", "MVP", "ANKRD26P1", "TVP23C",
  "RDM1", "PECAM1", "SOX9-AS1", "CATSPERD", "KIAA1683", "LINC00663",
  "FUT1", "ALLC", "ERICH2", "CPO", "TRIB3", "CEBPB-AS1",
  "PCK1", "C1QTNF6", "ADM2", "LAMB2P1", "SLC7A11-AS1", "SLC7A11",
  "NRN1", "SLC17A3", "HSPA1A", "HSPA1B", "C6orf48", "GCM1",
  "FLJ46906", "FRMD1", "INHBA", "ASNS", "TFR2", "MUC17",
  "DNAJB9", "MGAM", "MGAM2", "STC1", "SCARA5", "PPAPDC1B",
  "LINC01301", "C8orf46", "ATP6V0D2", "TG", "VLDLR-AS1", "LURAP1L",
  "IFNA22P", "TMEM215", "LINC00950", "ANXA1", "AKNA", "SAT1","NXF3"
)

A549_hACE2_log2FC_sheet <- A549_hACE2_sheet_filtered[,c("Gene","log2FoldChange")]

View(A549_hACE2_log2FC_sheet)

#  A549
A549_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                                           sheet = "A549",
                                          col_types = c("text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))
View(A549_sheet)
colnames(A549_sheet)[1] <- "Gene"


A549_sheet_filtered <- A549_sheet %>%
                       filter(Gene %in% filtered_gene)

View(A549_sheet_filtered)

#  A549_log2FC
A549_sheet_log2Fc <- A549_hACE2_sheet_filtered %>%
                      select(Gene,log2FoldChange)

colnames(A549_sheet_log2Fc)[2] <- "A549"
View(A549_sheet_log2Fc)



#A549-hACE2

A549_hACE2_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                         sheet = "A549-hACE2",
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"))

View(A549_hACE2_sheet)
colnames(A549_hACE2_sheet)[1] <- "Gene"


A549_hACE2_sheet_filtered <- A549_hACE2_sheet%>%
                            filter(Gene %in% filtered_gene)


View(A549_hACE2_sheet_filtered)

dim(A549_hACE2_sheet_filtered)

#A549-hACE2_log2Foldchange
A549_hACE2_log2FC_sheet <- A549_hACE2_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(A549_hACE2_log2FC_sheet)[2] <- "A549_A"
View(A549_hACE2_log2FC_sheet)



# Caco2


Caco2_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                               sheet = "Caco2",
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))

View(Caco2_sheet)
colnames(Caco2_sheet)[1] <- "Gene"

Caco2_sheet_filtered <- Caco2_sheet%>%
                        filter(Gene %in% filtered_gene)

View(Caco2_sheet_filtered)

dim(Caco2_sheet_filtered)

# Caco2_log2FC

Caco2_log2FC_sheet <- Caco2_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(Caco2_log2FC_sheet)[2] <- "Caco2"

View(Caco2_log2FC_sheet)


#Calu3_B


Calu3_B_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                          sheet = "Calu3_B",
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))

View(Calu3_B_sheet)
colnames(Calu3_B_sheet)[1] <- "Gene"

Calu3_B_sheet_filtered <- Calu3_B_sheet%>%
                          filter(Gene %in% filtered_gene)

View(Calu3_B_sheet_filtered)

dim(Calu3_B_sheet_filtered)

# Calu3_B_log2FC
Calu3_B_log2FC_sheet <- Calu3_B_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(Calu3_B_log2FC_sheet)[2] <- "Calu3_B"

View(Calu3_B_log2FC_sheet)


#Calu3_C


Calu3_C_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                            sheet = "Calu3_C",
                            col_types = c("text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))

View(Calu3_C_sheet)
colnames(Calu3_C_sheet)[1] <- "Gene"

Calu3_C_sheet_filtered <- Calu3_C_sheet%>%
                          filter(Gene %in% filtered_gene)

View(Calu3_C_sheet_filtered)

dim(Calu3_C_sheet_filtered)

#Calu3_C_log2FC

Calu3_C_log2FC_sheet <- Calu3_C_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(Calu3_C_log2FC_sheet)[2] <- "Calu3_C"

View(Calu3_C_log2FC_sheet)





# hBO

hBO_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                            sheet = "hBO",
                            col_types = c("text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))

View(hBO_sheet)
colnames(hBO_sheet)[1] <- "Gene"

hBO_sheet_filtered <- hBO_sheet%>%
  filter(Gene %in% filtered_gene)

View(hBO_sheet_filtered)

dim(hBO_sheet_filtered)

# hBO_log2FC 

hBO_log2FC_sheet <- hBO_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(hBO_log2FC_sheet)[2] <- "hBO"

View(hBO_log2FC_sheet)






#hAE


hAE_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                        sheet = "hAE",
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

View(hAE_sheet)
colnames(hAE_sheet)[1] <- "Gene"

hAE_sheet_filtered <- hAE_sheet%>%
  filter(Gene %in% filtered_gene)

View(hAE_sheet_filtered)

dim(hAE_sheet_filtered)

# hAE_log2fc

hAE_log2FC_sheet <- hAE_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(hAE_log2FC_sheet)[2] <- "hAE"

View(hAE_log2FC_sheet)


# hCM


hCM_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                        sheet = "hCM",
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

View(hCM_sheet)
colnames(hCM_sheet)[1] <- "Gene"

hCM_sheet_filtered <- hCM_sheet%>%
                      filter(Gene %in% filtered_gene)

View(hCM_sheet_filtered)

dim(hCM_sheet_filtered)

# hCM_log2 fc

hCM_log2FC_sheet <- hCM_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(hCM_log2FC_sheet)[2] <- "hCM"

View(hCM_log2FC_sheet)



# Merging table
# function
merge_table <- function(y){
                               merge(heatmap_log2FC_table,
                                y,
                                by.x = "Gene",
                                by.y = "Gene",
                                all = TRUE)
}
# first merge
heatmap_log2FC_table <- merge(x = HEK293T_hACE2_log2FC_filtered,
                              y = A549_sheet_log2Fc,
                              by.x = "Gene",
                              by.y = "Gene",
                              all = TRUE)
View(heatmap_log2FC_table)
#second merge
heatmap_log2FC_table <- merge_table(A549_hACE2_log2FC_sheet)
View(heatmap_log2FC_table)


# third merge
heatmap_log2FC_table <-merge_table(Caco2_log2FC_sheet)
View(heatmap_log2FC_table)

# fourth merge
heatmap_log2FC_table <-merge_table(Calu3_B_log2FC_sheet)
View(heatmap_log2FC_table)

# fifth merge

heatmap_log2FC_table <-merge_table(Calu3_C_log2FC_sheet)
View(heatmap_log2FC_table)


# sixth

heatmap_log2FC_table <-merge_table(hBO_log2FC_sheet)
View(heatmap_log2FC_table)


#seven 

heatmap_log2FC_table <-merge_table(hAE_log2FC_sheet)
View(heatmap_log2FC_table)

#eigth


heatmap_log2FC_table <-merge_table(hCM_log2FC_sheet)
View(heatmap_log2FC_table)

# heatmap


new_heatmap_log2FC_table <- column_to_rownames(heatmap_log2FC_table,
                                                      var = "Gene")
View(new_heatmap_log2FC_table)

heatmap_log2FC_table_matrix <- as.matrix(new_heatmap_log2FC_table)

View(heatmap_log2FC_table_matrix)

dim(heatmap_log2FC_table_matrix)


# heatmap

heatmap.2(heatmap_log2FC_table_matrix)
# column  to rownames



