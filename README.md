
<!-- README.md is generated from README.Rmd. Please edit that file -->
eda
===

EDA is critical to every Data Analytics engagement. The intent is to:

-   Establish a consistent and aspiringly exhaustive structure to the EDA process
-   Create a R based library of functions to explore data - through metadata, univariate and bivariate analysis with respect to a list of tables and charts - easily exportable to excel.

Installation
------------

##### You can install eda from github with:

``` r
install.packages("devtools")
devtools::install_github("gramener/eda")
```

##### Next, you load the package with:

``` r
library(eda)
```

##### To view all the functions that the package contains do:

``` r
help(package = eda)
```

##### To view the usage of the available functions do:

``` r
help(metadata)
help(univariate)
help(bivariate)
```

### Metadata Creation :

##### Functional capability to read any file format and provide a tabular exportable format containing :

-   Column Name
-   Column Type - Numeric(Discrete/Continuous), Date ,Categorical(Normal/Ordered)
-   Unique Key
-   Description - \[Column to be left blank - to be filled by User\]
-   Notes - \[Column to be left blank - to be filled by User on any quick observations/annotations\]
-   Missing Value %
-   No. of Unique Values
-   Min Value
-   Max Value
-   Q1 Value
-   Q3 Value
-   Average Value
-   Standard Deviation Value
-   Median Value
-   Top 5 Values

##### To compute the metadata do:

``` r
meta <- metadata$new(path = "Specify path to the file you want to conduct EDA on")
```

##### To view the metadata output in the console do:

``` r
meta$output()
```

##### You can make changes to the contents of the metadata, for example:change the type of the age column from continuous to discrete.

``` r
meta$columns$age$type <- "discrete"
```

##### To output the metadata in a structured format into excel do the following. If sheet name is not specified by default it will be named "Metadata".

``` r
meta$save(savepath = "path to the existing excel file or a new excel file to be created",sheet = "Metadata Analysis")
```

### Univariate Analysis:

##### Functional capability to read any file format and provide a tabular exportable format containing :

-   Column Name
-   % Values above/below the IQR Limit
-   % Values above/below the Mean Limit
-   Concentration - (80% of data is covered by x % of the Values) - Provide X
-   Priority Metric \[Column to be left blank - to be filled by User\]
-   Performance Metric \[Column to be left blank - to be filled by User\]
-   Notes - \[Column to be left blank - to be filled by User on any quick observations/annotations\]

##### provide a chart containing :

-   Column Name - Rank Freq Chart for Categorical Variables - (Unordered)
-   Column Name - Bar Chart for Categorical Variables - (Ordered)
-   Column Name - Histogram Chart for Numeric(Discrete & Continuous) Variables
-   Column Name - Box Plot Chart for Numeric(Discrete & Continuous) Variables

Binning: <https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule>

##### To conduct univariate analysis do the following.The "k" in the outlier detection technique(mean +/- k\* stardard deviation). By default k = 3

``` r
uni <- univariate$new(metadata = meta,k = 3)
```

##### To view the univariate analysis in the console do:

``` r
uni$output()
```

##### To output the univariate analysis in a structured format into excel do the following. If sheet name is not specified by default it will be named "Univariate".

``` r
uni$save(path = "path to the existing excel file or a new excel file to be created",sheet = "Univariate Analysis")
uni$saveplot(path = "path to the existing excel file or a new excel file to be created")
```

##### To output the univariate plots in a structured format into excel do the following. By default the Histogram uses Diaconis Rule to determine the number of breaks. If you want to set breaks manually set the breaks argument.

``` r
uni$saveplot(path = "path to the existing excel file or a new excel file to be created")
```

### Bivariate Analysis

#### Functional capability to read any file format and provide a tabular exportable format containing :

##### Bivariate Tables for

    - Categorical - Categorical Variable : Cross Tab of Count and Proportion of Records 
    - Numeric - Categorical Variable : Sum, Average, Min, Max of Records

##### Bivariate Plots

    - Numeric - Categorical Variable : Bar Plot for Sum, Average, Min, Max Records
    - Numeric - Numeric : Scatter Plot and Correlation Plot

##### To conduct univariate analysis do the following.

``` r
bi <- bivariate$new(metadata = meta)
```

##### To view the bivarate analysis in the console do:

``` r
bi$output()
```

##### To output the Bivariate tables in a structured format into excel do the following.

``` r
bi$save(path = "path to the existing excel file or a new excel file to be created")
```

##### To output the Bivariate Plots in a structured format into excel do the following.The method argument is the correlation computation method that can either be pearson or spearman

``` r
bi$saveplot(path = "path to the existing excel file or a new excel file to be created",method = "pearson")
```

Example
-------

This is a basic example which shows you how to solve a common problem:
