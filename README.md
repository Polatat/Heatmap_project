# Heatmap_project


## Clone this project 
To clone this repository, use the following command:
```
 git clone https://github.com/Polatat/Heatmap_project.gi
```

## Group Members 

Polatat Suwanit 6736156`<br>`
Sorawan Tiratrakoonwichaya 6737929  `<br>`
Chakrit Jiarasatit 6737934


## Requirement 
Ensure the following R packages are installed prior to running the project:

```
install.packages(c("dplyr","tibble","RColorBrewer","BiocManager"))

# Install ComplexHeatmap via BioCManager

BiocManager::install("ComplexHeatmap")
```

## Project Description

This heatmap project is part of the team assignment for the course SIRE516: R Programming for Bioinformaticians.

Course repository: [SIRE516 Team Assignments](https://github.com/si-medbif/SIRE516/tree/main/Team_Assignments/Heatmap)

The objective of this project is to reproduce the heatmap figure (Figure 4B) from the publication by Sun et al. ([DOI: 10.1038/s41598-021-96462-w](https://doi.org/10.1038/s41598-021-96462-w)).

In our implementation, we utilized two R packages to generate the heatmap figure:

1. **gplots** package (function: `heatmap.2`)
2. **ComplexHeatmap** package from Bioconductor


## Heatmap example

### Heatmap by heatmap.2
![Heatmap_1](https://github.com/user-attachments/assets/6f11cd78-6749-43f8-ac14-86f64878dd99)

### Heatmap by ComplexHeatmap

![Heatmap_2](https://github.com/user-attachments/assets/86bb0b10-98ce-43f0-aef2-defaa3fff40a)




