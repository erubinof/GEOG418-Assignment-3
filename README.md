# GEOG 418 Assignment 3
## Introduction
Describe the concept of libraries.

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
install.packages("knitr")
install.packages("sf")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("shinyjs")
install.packages("e1071")

#Load in libraries:
library("knitr")
library("sf")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library("st")

```



To start, you will want to read the shapefile for the census boundaries into ‘R’ as a st dataframe and the census data into a dataframe. Before you do this you may want to describe what a working directory is and how to set it. Also, make sure you can explain what the two files below represent.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#Create working directory
dir <- "C:/Users/Ezra Rubinoff/Desktop/UVIC/Term 12-Fall 2024/GEOG 418/Assignment 3"
setwd(dir)

#From the working dir read in the csv
csv <- read.csv("Assignment3_Data/ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("Assignment3_Data/lda_000a16a_e.shp")

```

Next, we want to clean up our data and make it easier to use. First we will create a vector of the column names so we understand what columns refer to what data. Then, we will remove any unwanted rows and merge the result with our spatial polygon data frame, and finally, we will subset to only the city of interest, for this analysis 'Kamloops'. The last step is to turn any absolute count data into a rate for mapping and analysis.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len >= 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Lethbridge, Alberta
Municp <- subset(census_DAs, census_DAs$CMANAME == "Lethbridge")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Before we can start to analyze our data, we need to be sure that the data we are looking at is relevant. Often, missing data in the form of NA or 0 values can change the results of an analysis. To make sure that the polygons we are looking at actually contain values for our variables of interest. To do this we can remove any polygon that contains an NA value for either median total income or knowledge of French.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$'Median total income')),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

Next, we will take a closer look at the two variables we are interested in: Median total income and Percentage of respondents with French language knowledge. We will look at some descriptive stats and do a final check for NA values in the data.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for Median Income and Percentage of French Speakers ", 2016, " census variables"))
```

Describe how the map is created.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Toronto census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
#tmaptools::palette_explorer() #Tool for selecting pallettes
#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "Purples", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "YlOrRd", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/aea8f45a-67c2-404e-83c4-cf9ff763fcf4" alt="Map_Income" width="500" />
  <img src="https://github.com/user-attachments/assets/dce2aee0-530d-4801-b941-630275572d3c" alt="Map_French" width="500" />
</div>
<p style="text-align: center;"><em>Figure 1: Lethbridge census dissemination areas showing median total income (left) and percentage of respondants
with knowledge of french (right).</em></p>

## Neighbourhood matrix

Describe the concept of a weighted neighbourhood matrix.

The code to create a list of neighbours in R is very simple thanks to the poly2nb() function in the ‘spdep’ package. If we want to change from default queen weighting to rook weighting in our selection, we simply change the ‘queen = TRUE’ to ‘queen = FALSE’.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(Income_noNA))


#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(Income_noNA))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(French_noNA))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(French_noNA))
crs(French.net2) <- crs(French_noNA)

```

Explain how the maps below are created and what they show.

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='green')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='green', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/8a7dbc48-e36d-47d2-9e72-84dba52c669f" alt="Map_IncomQueen" width="325" />
  <img src="https://github.com/user-attachments/assets/60e0844d-c0a6-4779-869e-b34a05c59fac" alt="Map_IncomeRook" width="325" />
  <img src="https://github.com/user-attachments/assets/98fa6b63-eaa2-4eda-87b9-9953ea97bcad" alt="Map_IncomeBoth" width="325" />  
</div>
<p style="text-align: center;"><em>Figure 2: Lethbridge census dissemination areas showing median total income neighbours queens weight (left), rooks weight (middle), and the combination of the two (right).</em></p>

Describe the code for the weighted matrix file.

Weights are defined by “style” (ie. type), and can include “B”, “W”, and “C”. The B weights matrix is the most basic of the three, as it employs a binary weighting scheme, whereby each neighbour is given a weight of 1, and all other polygons are given a weight of 0 (see figures above). A W weights matrix employs a row standardized weighting scheme, with each neighbour given equal weights that sum to 1 [11]. Comparatively, a C weights matrix is a globally standardized method of weighting, with all neighbours given equal weight across the entire study area [13].

Creating a weights matrix in R uses the “nb2listw” function from the “spdep” library. We can apply this function to the vri.nb variable created above, as it contains all of the neighbour links to which we want to assign weights. Additionally, if there are any polygons in our file with zero neighbour links, we still want the program to run. Therefore, we define “zero.policy” as equal to “TRUE”, which assigns weights vectors of zero length for regions with no neighbours [13]. Subsequently, we can print off our list of weights matrices (“print.listw”) in order to assess the distribution of weights for each observation (i) and its neighbours (j). The example of code below is using a weights matrix of type W. You can read more about the different styles of spatial weighting [here](https://r-spatial.github.io/spdep/reference/nb2listw.html).


```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```


## Global Moran’s I

Now that we have determined how to choose and weight our neighbours, we can calculate the Global Moran’s I statistic. This method of testing for spatial autocorrelation looks across the entire study area for every location simultaneously [14]. The equation for this statistic is

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

The denominator in this case is used to standardize our values, and therefore relatively high values of I correspond with positive spatial autocorrelation, and relatively low values of I correspond with negative spatial autocorrelation. Remember that the global Moran’s I statistic provides an indication of how spatially autocorrelated our data is over the entire dataset, thus representing a spatial pattern at the global scale [15].


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```


Describe the results.


```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

Describe what the results indicate.

However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is ?, and the alternate hypothesis is ?. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say ?. A value greater than +1.96 would imply ?, and a value less than -1.96 would imply ?.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that ?

## Local spatial autocorrelation

Explain local spatial autocorrelation

The calculation for Local Moran’s I has many of the same features as our global calculation, although arranged in a different way.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2} \sum_{j=1}^n W_{i,j}(x_j - \bar{x}) \quad \text{where} \quad S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1}
$$

Again, instead of typing out these calculations, we can use the localmoran() function to deal with all of the messy calculations for us, as long as we input our variable and weighting scheme.


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing.


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "PiYG", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/16f6083c-aee3-4b12-8629-7a6388301ea0" alt="map_LISA_French" width="500" />
  <img src="https://github.com/user-attachments/assets/c277e0d7-e3a6-4829-aac1-4a2e4ec3e8a4" alt="map_LISA_Income" width="500" />
</div>
<p style="text-align: center;"><em>Figure 3: Lethbridge census dissemination areas showing LISA z-scores for median total income (left) and
percentage of respondants with knowledge of french (right).</em></p>

Explain the results.


While these maps are great for visualizing where the data is and getting a rough idea of how many polygons are significantly positively or negatively spatially autocorrelated, it can be even more informative to graph these trends.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/86ca8e5d-0584-4ed7-ae1c-61585a092f68" alt="Moran_Income_Plot" width="800" />
</div>
<p style="text-align: center;"><em>Figure 4: Moran’s I scatter plot for median total income.</em></p>

<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/b8c8fc54-7846-4179-a17b-4222382cb032" alt="Moran_French_Plot" width="800" />
</div>
<p style="text-align: center;"><em>Figure 5: Moran's I scatter plot for percentage of respondants with knowledge of french.</em></p>

In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows?




## Summary

Provide a brief summary.

## References
