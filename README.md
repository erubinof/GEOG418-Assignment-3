# GEOG 418 Assignment 3: Spatial Autocorrelation Tutorial
## Introduction
For this tutorial, we will be using R to conduct our analysis. As the standard installation of R only has so many functions, when we are trying to do more specific calculations and visualizations, packages and libraries are needed. These can be installed and loaded through RStudio and allow for more complex coding within R.

In this case, we are interested in having the ability to create maps, make lists of neighbours, define weight matrices, and import shapefiles, all functions that require additional packages and libraries. To install and load these, we first use the install.packages() code to install them on our device, and then we use the library() code to import them for use in our project file. Once this is all set up, the functions seen below will work as intended.

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
install.packages("knitr")
install.packages("sf")
install.packages("st")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("shinyjs")
install.packages("e1071")

#Load in libraries:
library("knitr")
library("sf")
library("st")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")

```

Starting off, you must set your working directory to tell the code where to look for the data and where to save your figures. This working directory only needs to be set once for the project, and everything from now on will be pulled from or saved into that folder. The next step is to import the required data into R. Below, the code demostrates how to bring the census boundaries shapefile in as a st dataframe, and the census data in as a dataframe. 

The census boundaries shapefile is a spatial dataset that contains all census boundaries across Canada from 2016. This will be used to help us map out the spatial autocorrelation that we are analyzing. The census dataset contains all of the non-spatial information from the Canadian census in 2016 and will be used to collect the data we need to run our spatial autocorrelation analysis.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#Create working directory
dir <- "C:/Users/Name/Desktop/GEOG418/Assignment3"
setwd(dir)

#From the working dir read in the csv
csv <- read.csv("Assignment3_Data/ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("Assignment3_Data/lda_000a16a_e.shp")

```

Now that the data has been imported into R, we must clean it up to make it useable. This process of cleaning the data involves creating a vector of the column names, removing unwanted rows, joining the aspatial census data with the census area spatial data, and creating a subset for the city of interest. Doing these steps will make the data readable, spatial, and contain only what we will use for the analysis. For this analysis, we will use the city of Lethbridge, Alberta.

After this, we convert all the absolute count data into a rate so it is standardized and can be used for analysis. Once this is all complete, the data will be ready for our analysis.

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

Prior to conducting the data analysis, the data needs to be relevant and free of values that could introduce inaccuracies. An example of that is missing data, defined as values that are NA. These can change the results and must be removed to make sure the output is accurate. This is done below by removing the polygons that have a NA value in the median total income or knowledge of french columns. Now, we have two cleaned datasets that contain spatial and aspatial data about both income and french knowledge which do not contain any NA values.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$'Median total income')),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

To start the analysis, we can look at some descriptive statistics for both the median total income and percentage of respondents with french language knowledge in Lethbridge. These values will give us some base values to begin our analysis.

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
kable(data, caption = paste0("Descriptive statistics for Median Income and Percentage of French Speakers in Lethbridge in ", 2016))
```
![DescriptiveStatsTable](https://github.com/user-attachments/assets/eeb724e1-04b7-47f9-aa53-cd76bcf578c1)

Now it is time to make a map to show the spatial distribution of both events throughout the Lethbridge census area. To do this, we will use the tmap library, which allows you to customize the title, classification style, colour palette, and legend. You can explore the different colour palettes by uncommenting the code: tmaptools::palette_explorer(). This pop up will give you the option to see many different available palettes for different numbers of classes and the code you need to use to add them to your map. 

Once both maps have been created, we can display them side by side for analysis. As you can see below, the outputs are clean and give a good visual representation of how both datasets are distributed.

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Lethbridge census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
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

A weighted neighbourhood matrix is used to identify the neighbours of an event or area and assign weights to calculate their spatial influence or similarity. In spatial autocorrelation analysis, statistical tests to assess how an event relates to its neighbours are only meaningful if the neighbours are clearly defined and their importance is accounted for through appropriate weighting. 

The approaches we will use for defining neighbours are the queen and rook weighting methods. The queen method considers neighbouring areas that share either a border or a corner, while the rook method only considers areas that share a common border. These names evidently come from the game of Chess. Below shows a diagram of both queen and rooks weighting.

<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/da6ee0d0-e0d8-4bc6-a737-5e49139ea338" alt="Queen Weights Neighbours" width="300" />
  <img src="https://github.com/user-attachments/assets/7d34efa5-9b4a-4431-9938-88aed40ca536" alt="Rooks Weights Neighbours" width="300" />
</div>
<p style="text-align: center;"><em> Figure 2: Queen weights neighbours (Left) and rook weights neighbours (Right) .</em></p>

We will use the R 'spdep' package's poly2nb() function to create a list of neighbours. To change between queen and rook weighting, you can change the code to include queen = FALSE as a parameter. If that code is not present, it will default to queen = TRUE, meaning that it will conduct queen weighting.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(Income_noNA))
crs(Income.net) <- crs(Income_noNA)

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

To visualize the median total income neighbours queen and rooks weight, we will use the tmap plugin again to make some maps. As these maps are just showing lines and not polygons with a classification, the code to create it is much simpler than the previous map. For each map, we provide the census polygons, the border colour, the neighbour lines data, and the line colour. For the last one, we add both the the neighbour lines data and colour for both queen and rooks weight, as it will be used to compare them. Once all three maps are created, we can display them all side by side.

Using lines, these maps show all of the neighbour connections between the different census areas. As the boundaries of these areas are not uniform, these lines look very different from the diagrams above. This map allows you to see the benefits and drawbacks of both the queen and rook methods.

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Lethbridge census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

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
<p style="text-align: center;"><em>Figure 3: Lethbridge census dissemination areas showing median total income neighbours queens weight (left), rooks weight (middle), and the combination of the two (right).</em></p>

To define weights, types are used, and the 3 main common ones are called "B", "W", and "C". Starting with B, it uses a simple binary weighting system where neighbours are given a weight of 1 and all others are given a weight of 0. This is what was employed in the previous diagrams used to explain queen and rook weights. Moving onto W, it uses a row standardized weighting system where each neighbour is given equal weights that total 1 when added together. An example of this would be a polygon with 4 neighbours, where each of the neighbours are given a weight of 0.25. Lastly, C weights uses a globally standardized system where all neighbours have an equal weight across the entire study area.

Now that we understand 3 different types of weighting, we can move on to applying these to our data and continue with our analysis. To create a weight matrix, we will use the spdep library's “nb2listw” function. This function can be applied to the Income.nb, French.nb, Income.nb2, or French.nb2, depending on which variable we want to look at and if we want to use rook or queen weights. This variable contains the links between neighbours to use while creating the matrices. Also, there are some census areas that have no links to neighbours so we can set the “zero.policy” parameter to TRUE to make sure weights vectors of zero length is set for them. Once the matrices are made, we can display them with the code “print.listw” to understand the distribution of weights for all of the observations, refered to as "i", and their neighbours, refered to as "j". We will use a W type matrix below for our analysis. To see more in depth details about “nb2listw” function and some examples of its usage, use this link: [Spatial weights for neighbours lists](https://r-spatial.github.io/spdep/reference/nb2listw.html). 

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```

![image](https://github.com/user-attachments/assets/f1cb00f9-6669-43cf-9bdb-4b0266102114)

## Global Moran’s I

Now that we have determined how to choose and weight our neighbours, we can calculate the Global Moran’s I statistic. This method of testing for spatial autocorrelation looks across the entire study area for every location simultaneously [14]. The equation for this statistic is

Our neighbours have now been chosen and weighted, so we can continue on to calculating the Global Moran's I statistic. This is used to test for spatial autocorrelation for the entire study area. It is able to examine all areas at the same time to help with the analysis of this data. 

The equation is:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

***Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

***The denominator in this case is used to standardize our values, and therefore relatively high values of I correspond with positive spatial autocorrelation, and relatively low values of I correspond with negative spatial autocorrelation. Remember that the global Moran’s I statistic provides an indication of how spatially autocorrelated our data is over the entire dataset, thus representing a spatial pattern at the global scale [15].


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


***Describe the results.


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

***Describe what the results indicate.

***However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is ?, and the alternate hypothesis is ?. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say ?. A value greater than +1.96 would imply ?, and a value less than -1.96 would imply ?.

This code is used to calculate the Z-score:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

***The zscores for both variable confirm that ?

## Local spatial autocorrelation

***Explain local spatial autocorrelation

This is the equation for the Local Moran's I statistic:

$$
I_i = \frac{x_i - \bar{x}}{S_i^2} \sum_{j=1}^n W_{i,j}(x_j - \bar{x}) \quad \text{where} \quad S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1}
$$

***Again, instead of typing out these calculations, we can use the localmoran() function to deal with all of the messy calculations for us, as long as we input our variable and weighting scheme.


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

***Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing.

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Lethbridge census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
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
<p style="text-align: center;"><em>Figure 4: Lethbridge census dissemination areas showing LISA z-scores for median total income (left) and
percentage of respondants with knowledge of french (right).</em></p>

***Explain the results.

***While these maps are great for visualizing where the data is and getting a rough idea of how many polygons are significantly positively or negatively spatially autocorrelated, it can be even more informative to graph these trends.

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
<p style="text-align: center;"><em>Figure 5: Moran’s I scatter plot for median total income.</em></p>

<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/b8c8fc54-7846-4179-a17b-4222382cb032" alt="Moran_French_Plot" width="800" />
</div>
<p style="text-align: center;"><em>Figure 6: Moran's I scatter plot for percentage of respondants with knowledge of french.</em></p>

***In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows?


## Summary

***Provide a brief summary.

## References
