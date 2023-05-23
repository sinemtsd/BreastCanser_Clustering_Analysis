# Breast Cancer Clustering Analysis
Clustering of breast cancer cells using the Wisconsin Diagnostic Breast Cancer (WDBC) dataset.
## The Aim of the Project
The purpose is to partition the tumor into clusters indicating whether it is benign or malignant based on the cell nucleus features using the K-means and K-medoids clustering algorithms.
## Dataset Information
Features are computed from a digitized image of a fine needle aspirate (FNA) of a breast mass. They describe characteristics of the cell nuclei present in the image.
The dataset consists of 569 observations and 32 variables
### Attribute Information:
- ID number
- Diagnosis (M = malignant, B = benign)
Ten real-valued features are calculated for each cell nucleus:
- radius (mean of distances from center to points on the perimeter)
- texture (standard deviation of gray-scale values)
- perimeter
- area
- smoothness (local variation in radius lengths)
- compactness (perimeter^2 / area - 1.0)
- concavity (severity of concave portions of the contour)
- concave points (number of concave portions of the contour)
- symmetry
- fractal dimension ("coastline approximation" - 1)
