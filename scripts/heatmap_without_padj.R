library(readxl) # excel
library(dplyr) # filter and select
library(gplots) # heatmap.2 
library(tibble) # column_to_rowname and rowname_to_columns
library(RColorBrewer) # colour heatmap 
library(BiocManager) # install.packages("BiocManger") BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap) # Draw package and Heatmap
library(circlize) # for Legends in ComplexHeatmap
# HEK293T_hACE2

HEK293T_hACE2_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                                  col_types = c("text", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric"))

HEK293T_hACE2_filtered_without_padj <- HEK293T_hACE2_sheet %>%
  filter(log2FoldChange >= 2 | log2FoldChange <= -1.5) 

colnames(HEK293T_hACE2_filtered_without_padj)[1] <- "Gene"
View(HEK293T_hACE2_filtered_with_padj)
dim(HEK293T_hACE2_filtered_without_padj) # 85 rows and 6 column


HEK293T_hACE2_log2FC_filtered_without_padj <- HEK293T_hACE2_filtered_without_padj[,c("Gene","log2FoldChange")]
colnames(HEK293T_hACE2_log2FC_filtered_without_padj)[2] <- "HEK293T_A"
View(HEK293T_hACE2_log2FC_filtered_without_padj)


# filter gene without padj
gene_names <- HEK293T_hACE2_filtered_without_padj$Gene
filtered_gene_without_padj_name <- unique(gene_names)
filtered_gene_without_padj_name


#filtered without padj
filtered_gene_without_padj <- c(
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



#  A549
A549_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                         sheet = "A549",
                         col_types = c("text", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"))
#View(A549_sheet)
colnames(A549_sheet)[1] <- "Gene"


A549_sheet_filtered <- A549_sheet %>%
  filter(Gene %in% filtered_gene_without_padj)

#View(A549_sheet_filtered)

#  A549_log2FC

A549_sheet_log2Fc_without <- A549_sheet_filtered %>%
  select(Gene,log2FoldChange)

colnames(A549_sheet_log2Fc)[2] <- "A549"
#View(A549_sheet_log2Fc)



#A549-hACE2

A549_hACE2_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                               sheet = "A549-hACE2",
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))

#View(A549_hACE2_sheet)
colnames(A549_hACE2_sheet)[1] <- "Gene"


A549_hACE2_sheet_filtered <- A549_hACE2_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)


#View(A549_hACE2_sheet_filtered)

dim(A549_hACE2_sheet_filtered)

#A549-hACE2_log2Foldchange
A549_hACE2_log2FC_sheet <- A549_hACE2_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(A549_hACE2_log2FC_sheet)[2] <- "A549_A"
#View(A549_hACE2_log2FC_sheet)



# Caco2
Caco2_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                          sheet = "Caco2",
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))

#View(Caco2_sheet)
colnames(Caco2_sheet)[1] <- "Gene"

Caco2_sheet_filtered <- Caco2_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)

#View(Caco2_sheet_filtered)

dim(Caco2_sheet_filtered)

# Caco2_log2FC

Caco2_log2FC_sheet <- Caco2_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(Caco2_log2FC_sheet)[2] <- "Caco2"

#View(Caco2_log2FC_sheet)


#Calu3_B
Calu3_B_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                            sheet = "Calu3_B",
                            col_types = c("text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))

#View(Calu3_B_sheet)
colnames(Calu3_B_sheet)[1] <- "Gene"

Calu3_B_sheet_filtered <- Calu3_B_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)

#View(Calu3_B_sheet_filtered)

dim(Calu3_B_sheet_filtered)

# Calu3_B_log2FC
Calu3_B_log2FC_sheet <- Calu3_B_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(Calu3_B_log2FC_sheet)[2] <- "Calu3_B"

#View(Calu3_B_log2FC_sheet)


#Calu3_C


Calu3_C_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                            sheet = "Calu3_C",
                            col_types = c("text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric"))

#View(Calu3_C_sheet)
colnames(Calu3_C_sheet)[1] <- "Gene"

Calu3_C_sheet_filtered <- Calu3_C_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)

#View(Calu3_C_sheet_filtered)

dim(Calu3_C_sheet_filtered)

#Calu3_C_log2FC

Calu3_C_log2FC_sheet <- Calu3_C_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(Calu3_C_log2FC_sheet)[2] <- "Calu3_C"

#View(Calu3_C_log2FC_sheet)



# hBO

hBO_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                        sheet = "hBO",
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

#View(hBO_sheet)
colnames(hBO_sheet)[1] <- "Gene"

hBO_sheet_filtered <- hBO_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)

#View(hBO_sheet_filtered)

dim(hBO_sheet_filtered)

# hBO_log2FC 

hBO_log2FC_sheet <- hBO_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(hBO_log2FC_sheet)[2] <- "hBO"

#View(hBO_log2FC_sheet)



#hAE
hAE_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                        sheet = "hAE",
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

#View(hAE_sheet)
colnames(hAE_sheet)[1] <- "Gene"

hAE_sheet_filtered <- hAE_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)

#View(hAE_sheet_filtered)

dim(hAE_sheet_filtered)


# hAE_log2fc
hAE_log2FC_sheet <- hAE_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(hAE_log2FC_sheet)[2] <- "hAE"

#View(hAE_log2FC_sheet)


# hCM
hCM_sheet <- read_excel("data/41598_2021_96462_MOESM1_ESM.xls", 
                        sheet = "hCM",
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric"))

#View(hCM_sheet)

colnames(hCM_sheet)[1] <- "Gene"

hCM_sheet_filtered <- hCM_sheet%>%
  filter(Gene %in% filtered_gene_without_padj)

#View(hCM_sheet_filtered)

dim(hCM_sheet_filtered)

# hCM_log2 fc

hCM_log2FC_sheet <- hCM_sheet_filtered[,c("Gene","log2FoldChange")]

colnames(hCM_log2FC_sheet)[2] <- "hCM"

#View(hCM_log2FC_sheet)



# Merging table

log2FC_data <- list(HEK293T_hACE2_log2FC_filtered_without_padj,A549_sheet_log2Fc,A549_hACE2_log2FC_sheet,
                    Caco2_log2FC_sheet,Calu3_B_log2FC_sheet,
                    Calu3_C_log2FC_sheet,hBO_log2FC_sheet,
                    hAE_log2FC_sheet,hCM_log2FC_sheet)
typeof(log2FC_data)
class(log2FC_data)
str(log2FC_data)

merge_heatmap_table <- Reduce(function(x,y) merge(x,y, by ="Gene", all = TRUE),
                              log2FC_data)

View(merge_heatmap_table)


# gene column to rowname
# tibble

merge_heatmap_table <- column_to_rownames(merge_heatmap_table, var ="Gene")

dim(merge_heatmap_table)

typeof(merge_heatmap_table)
class(merge_heatmap_table)

matrix_heatmap_table <- as.matrix(merge_heatmap_table)

typeof(matrix_heatmap_table)
class(matrix_heatmap_table)

View(matrix_heatmap_table)
dim(matrix_heatmap_table)


transpose_heatmap_table <- t(matrix_heatmap_table)

View(transpose_heatmap_table)
dim(transpose_heatmap_table)

##set up red blue color 

bwr <- colorRampPalette(c("blue", "white", "Salmon"))(200)

# heatmap(gplot2)

heatmap.2(matrix_heatmap_table)
heatmap.2(transpose_heatmap_table)


# transpose

heatmap.2(transpose_heatmap_table,col=bluered, scale="column", tracecol="#303030",
          lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ))

heatmap.2(transpose_heatmap_table,col=bluered,  tracecol="#303030",
          lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ))


heatmap.2(transpose_heatmap_table,col=bluered , trace = "none", offsetCol = 0,
          lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ))

heatmap.2(transpose_heatmap_table,col=bwr , trace = "none", offsetCol = 0,
          lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ))

heatmap.2(transpose_heatmap_table,col=bwr , trace = "none", offsetCol = 0,
          keysize = 2., lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ), main = "Heatmap without padj")



heatmap.2(transpose_heatmap_table,col=bwr, scale = "column", trace = "none", offsetCol=0,
          lmat=rbind( c(0, 3, 4), c(2,1,0 ) ), lwid=c(1.5, 4, 2 ))

heatmap.2(transpose_heatmap_table, col = bwr, trace = "none", offsetCol = 0, offsetRow = 0,
          keysize = 2,  # First row : dendrogram (3) + heat map (4),  # Second row: Color Key (2) + Main heat map (1)     # Minimize the size of column of  Color Key 
          lhei = c(1.5, 4),
          main = "Heatmap without padj")        # Balance the size of row 



#heatmap(Complexhetmap)

# setting row order

cell_line_order <- c("hCM","A549","Calu3_C",
                     "Calu3_B","hAE","A549_A",
                     "hBO","Caco2","HEK293T_A")

# setting heatmap legend scale
col_scale = colorRamp2(c(-4,0,4), c("blue","white","red"))
lgd = Legend(col_fun = col_scale, title = "log2FC")

complex_heatmap_table <- Heatmap(transpose_heatmap_table, name = "log2FC",
                                 cluster_rows = TRUE,
                                 show_column_names = FALSE,
                                 column_dend_side = "bottom",
                                 row_order = cell_line_order,
                                 heatmap_width = unit(10.5, "cm"), 
                                 heatmap_height = unit(8, "cm"),
                                 show_heatmap_legend = FALSE)


complex_heatmap_table
# draw the heat map 

draw(complex_heatmap_table,heatmap_legend_list = (list(lgd)),
     heatmap_legend_side ="left")


