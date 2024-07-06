# Survival-analysis-of-METABRIC-data

This project involves the analysis of the METABRIC (Molecular Taxonomy of Breast Cancer International Consortium) dataset. The focus is on visualizing the data, preprocessing it, and performing survival analysis using Kaplan-Meier curves and hazard ratio calculations.

## Project Overview

1. **Data Visualization**
    - Initial exploration and visualization of the dataset to understand the distribution and relationships of various features.
    
2. **Data Preprocessing**
    - Handling missing values and converting categorical variables to factors.

3. **Survival Analysis**
    - Drawing Kaplan-Meier curves stratified by the type of breast surgery performed on patients.
    - Calculating hazard ratios to assess the significance of different factors on patient survival.

## Files in the Repository

- `Breast Cancer METABRIC/`: Contains the METABRIC dataset used for analysis.
- `survival analysis/`: R script for each step of the analysis.
- `figures/`: Plots and results generated from the analysis.
- `README.md`: Project overview and instructions.

## Getting Started

### Prerequisites

- R version 4.3.3 or higher
- The following R packages:
    - `dplyr`
    - `survival`
    - `survminer`
    - `ggplot2`
    - `gridExtra`

## Conclusion

This project demonstrates the process of visualizing, preprocessing, and analyzing survival data from the METABRIC dataset. It was found that mastectomy increase risk of death by 1.38 compared to breast conserving. This finding is consistent with literature: https://jamanetwork.com/journals/jamasurgery/fullarticle/2779531 -
https://academic.oup.com/bjsopen/article/8/3/zrae040/7675985

## Acknowledgments

- METABRIC Consortium for providing the dataset: https://www.cbioportal.org/study/summary?id=brca_metabric.

