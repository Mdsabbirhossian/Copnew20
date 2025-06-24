# **Clustering Reader Profiles from MECO-L2 Eye-Tracking Data**

This project explores unsupervised clustering techniques to uncover individual reading patterns from the MECO-L2 Wave 1 eye-tracking dataset. It involves gaze feature extraction, standardization, clustering, and comparison with literature-based cognitive reader groups (Hyönä & Nurminen, 2006).


## **Table of Contents**
1. [Overview](#project-overview)
2. [Folder Structure](#how-it-Works)
3. [Feature Overview](#setup-nstructions)
4. [How to Run](#project-structure-diagram)
5. [Key Outputs](#key-outputs)
6. [Dependencies](#dependencies)

## **Project Overview**
The goal of this project is to:
1. **Extract Gaze Features** from the MECO-L2 Wave 1 dataset for all second-language (L2) participants.
2. **Cluster Readers** based on their gaze behavior using unsupervised methods (K-means, GMM).
3. **Compare Clusters** with theoretical cognitive reader groups (GG1–GG3) defined by Hyönä & Nurminen (2006).
4. **Evaluate Cluster Quality** using PCA visualizations, silhouette scores, and centroid distance comparisons.

### **Components**:
1. **Preprocessing Scripts**: Clean, filter, and prepare MECO eye-tracking data; apply z-score normalization.
2. **Clustering Scripts**: Perform clustering on selected features (3-feature, 8-feature, and 3+1 feature variants).
3. **Evaluation Scripts**: Align discovered clusters with literature-based groups using cosine/Mahalanobis distances and confusion matrices.
4. **Visualization Scripts**: Generate PCA plots, centroid feature profiles, scree/silhouette plots, and alignment heatmaps.

---

## **How It Works**

### 1. **Data Preparation**:
   - The `scripts/01a_preprocessing_and_zscoring.R` script loads and filters MECO-L2 gaze data.
   - Participants with missing values or extreme behavior are filtered out (e.g., based on accuracy and fixation thresholds).
   - Ten gaze features are extracted, including durations, skipping, regressions, and reading rate.
   - Features are z-scored across participants and saved to `data/features/by_sub_features_wave1_final_10features.csv`.

### 2. **Clustering Readers**:
   - The `scripts/03a_clustering_8features.R` script runs clustering on 8 behavioral features.
   - The `scripts/03b_K-means_clustering_on_3_z-scored_features.R` performs clustering using only fixation-based standardized features.
   - Additional experiments in `scripts/08_cluster_3plus1_*.R` explore clustering using 3 fixation features plus one behavioral feature at a time.

### 3. **G-Group Assignment & Evaluation**:
   - The `scripts/06_compare_clusters_to_ggroups.R` script aligns each cluster with G1–G3 groups (GG1: Efficient, GG2: Careful, GG3: Risky) based on centroid distances.
   - Cosine and Mahalanobis distances are computed between cluster centroids and theoretical group profiles.
   - Confusion matrices and alignment heatmaps are generated and stored in the `outputs/cluster_comparison/` folder.

### 4. **Results and Outputs**:
   - Final cluster assignments are stored in `data/features/clustered_*.csv`.
   - Visualizations (PCA, scree, silhouettes, feature bar plots, centroid comparisons) are saved under `outputs/plots/` and `outputs/cluster_comparison/`.
   - All key metrics (ARI, NMI, silhouette, distance scores) are logged in summary CSVs for reporting.



## **Setup Instructions**

### 1. **Clone the repository**
To get started with the project, open your terminal (or Git Bash) and clone the repository:

```bash
git clone https://github.com/Mdsabbirhossian/Copnew20.git
cd Copnew20

```

### 2. **Install the dependencies**:
Make sure you have **R (version ≥ 4.0)** installed.

Then open R or RStudio, and run:

```r
install.packages(c("tidyverse", "cluster", "factoextra", "mclust", "ggpubr", "psych"))
```

### 4. **Prepare Your Dataset**:
   - source("scripts/01a_preprocessing_and_zscoring.R")


### 5. **Run the Script**:
   To run the script, execute the following command:

      ```bash
      # K-means clustering with 3 fixation-based features
source("scripts/03b_K-means_clustering_on_3_z-scored_features.R")

# GMM clustering with 8 behavioral features
source("scripts/03a_clustering_8features.R")

# Compare cluster assignments to theoretical GG1–GG3 groups
source("scripts/06_compare_clusters_to_ggroups.R")
source("scripts/07_compare_cluster_to_Ggroup_summary.R")

      ```

## **Project Structure Diagram**

Here is the project structure diagram:

![Project Structure](assets/PipelineFlow.png)


## **File Structure**

Here’s the breakdown of the file structure:

```
Copnew20/
├── data/                                                # Folder for input features and processed data
│   └── features/                                        # Participant-level datasets
│       ├── by_sub_features_wave1.csv                    # Raw extracted features
│       ├── by_sub_features_wave1_final_10features.csv   # Cleaned final dataset with 10 features
│       ├── by_sub_with_ggroups.csv                      # Dataset with assigned G1–G3 groups
│       ├── clustered_3features.csv                      # Cluster assignments for 3-feature model
│       ├── clustered_8features.csv                      # Cluster assignments for 8-feature model
│       └── ...                                          
│
├── outputs/                                             # Output folder for all result files
│   ├── plots/                                           # PCA, scree, silhouette, feature profile visualizations
│   │   ├── pca_3feature.png
│   │   ├── pca_8feature.png
│   │   ├── feature_profiles.png
│   │   └── ...
│   │
│   ├── cluster_comparison/                              # Evaluation results (metrics, alignment, heatmaps)
│   │   ├── heatmap_cosine.png
│   │   ├── heatmap_mahalanobis.png
│   │   ├── confusion_matrix.png
│   │   ├── comparison_metrics.csv
│   │   └── ...
│   │
│   └── centroids/                                       # Cluster centroid values
│       ├── centroids_3features.csv
│       ├── centroids_8features.csv
│       └── ...
│
├── scripts/                                             # All R scripts for running the project pipeline
│   ├── 01a_preprocessing_and_zscoring.R
│   ├── 03a_clustering_8features.R
│   ├── 03b_K-means_clustering_on_3_z-scored_features.R
│   ├── 04_cluster_comparison_final_cleaned.R
│   ├── 06_compare_clusters_to_ggroups.R
│   ├── 07_compare_cluster_to_Ggroup_summary.R
│   ├── 08_cluster_3plus1_skip_analysis.R
│   ├── 10_cluster_3plus1_refix_analysis.R
│   └── ...
│
├── .gitignore                                            # Git ignore file to exclude non-essential files from version control
├── README.md                                             # Project documentation file
└── Copnew20.Rproj                                        # R project file for convenient opening in RStudio

```

## **Dependencies**

This project is implemented in **R** and requires the following packages:

- **tidyverse**: For data manipulation and plotting
- **cluster**: For clustering analysis
- **factoextra**: For visualizing clusters and PCA
- **mclust**: For Gaussian Mixture Model clustering
- **ggpubr**: For combining and customizing plots
- **psych**: For descriptive statistics and correlations

You can install all dependencies by running:

```r
install.packages(c("tidyverse", "cluster", "factoextra", "mclust", "ggpubr", "psych"))

## **License**

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
