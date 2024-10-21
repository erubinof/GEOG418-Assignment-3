# GEOG 418 Assignment 3: Spatial Autocorrelation Tutorial
Created By: Ezra Rubinoff
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

Starting off, you must set your working directory to tell the code where to look for the data and where to save your figures. This working directory only needs to be set once for the project, and everything from now on will be pulled from or saved into that folder. The next step is to import the required data into R. Below, the code demonstrates how to bring the census boundaries shapefile in as a st dataframe, and the census data in as a dataframe. 

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
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/eeb724e1-04b7-47f9-aa53-cd76bcf578c1" alt="Descriptive Stats Table" width="500" />
</div>

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
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
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

Now that we understand 3 different types of weighting, we can move on to applying these to our data and continue with our analysis. To create a weight matrix, we will use the spdep library's “nb2listw” function. This function can be applied to the Income.nb, French.nb, Income.nb2, or French.nb2, depending on which variable we want to look at and if we want to use rook or queen weights. This variable contains the links between neighbours to use while creating the matrices. Also, there are some census areas that have no links to neighbours so we can set the “zero.policy” parameter to TRUE to make sure weights vectors of zero length is set for them. Once the matrices are made, we can display them with the code “print.listw” to understand the distribution of weights for all of the observations, referred to as "i", and their neighbours, referred to as "j". We will use a W type matrix below for our analysis. To see more in depth details about “nb2listw” function and some examples of its usage, use this link: [Spatial weights for neighbours lists](https://r-spatial.github.io/spdep/reference/nb2listw.html). 

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```

<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/f1cb00f9-6669-43cf-9bdb-4b0266102114" alt="Income Weights Matrix" width="500" />
</div>

## Global Moran’s I

Now that we have determined how to choose and weight our neighbours, we can calculate the Global Moran’s I statistic. This method of testing for spatial autocorrelation looks across the entire study area for every location simultaneously [14]. The equation for this statistic is

Our neighbours have now been chosen and weighted, so we can continue on to calculating the Global Moran's I statistic. This is used to test for spatial autocorrelation for the entire study area. It is able to examine all areas at the same time to help with the analysis of this data. 

The equation is:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

To understand this equation, it is sometimes easier to break it down to see how the variable $x$ is used. $x_i$ is the value of the variable at point i and $x_j$ is a neighbour to $x_i$. The neighbours for this analysis is determined by the queen weight system. Then, the weighting matrix is used to apply the weighting $W_{i,j}$ and is multiplied by the differences of $x_i$ and the mean $x$, and $x_j$ and the mean of $x$.

This calculated value is then standardzied by the denominator to calculate the value for I. High values of I means that there is positive spatial autocorrelation, and lower values tend to be associated with negative spatial autocorrelation. The pattern calculated is for the entire dataset, as this is a global statistic, and therefore shows how similar things are either closer or further from each other on a global scale.

We then also can calculate the expected I and the variance to give us a better understanding of how significant the Moran's I value is. The expected I tells us what the I would be if there was no spatial autocorrelation present in the data. If the I is different from the expected I, then it is either positively or negatively autocorrelated. The variance is how varied the values are in the dataset and will be used later on to calculate a Z-score. 

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

# Create a table to display Global Moran's I results for Income and French
results_table <- data.frame(
  Variable = c("Income", "French"),
  Moran_I = c(mIIncome, mIFrench),
  Expected_I = c(eIIncome, eIFrench),
  Variance = c(varIncome, varFrench)
)

# Print the table
print(results_table)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/0643efb4-5842-41cc-beec-10382d6d9043" alt="Global Moran's I Results Table" width="500" />
</div>

This table shows the results of the Global Moran's I for each variable. As the Moran's I value is bigger than the expected I value for both, they both are showing evidence of being positively spatially autocorrelated. Next, we will calculate the range of the Global Moran's I. 

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

Results:

Moran's I for Income min range: -0.7301612

Moran's I for Income max range: 1.063473

These values show that some parts of the study area are more positively spatially autocorrelated and some areas are more negatively spatially autocorrelated. 

Now, it is important to see if the results we are getting are statistically significant. For this, a Z-test is used along with a null and alternative hypothesis to make sure that we are following the scientific method to come to a conclusion. The null hypothesis is that there is no spatial autocorrelation for median income (or percentage of french speakers) in Lethbridge's census areas. The alternate hypothesis is that there is spatial autocorrelation for median income (or percentage of french speakers) in Lethbridge's census areas. We can pick a 95% confidence interval and therefore state that our $\alpha$ is 0.05. This means that if the Z-Score is higher than 1.96, we can reject the null hypothesis and say that our data is positively spatially autocorrelated and alternatively if the Z-score falls below -1.96, we can reject the null hypothesis and say that the data is negatively spatially autocorrelated. 

This code is used to calculate the Z-score:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```
Results:

Z-Score for Median Income: 8.609242

Z-Score for Percentage of French Speakers: 7.928283

These results indicate that both Income and French variables are significantly positively spatially autocorrelated in Lethbridge. This means that on a global scale, areas of higher income are clustered together and areas of higher french speakers are also clustered. 

## Local spatial autocorrelation

Local spatial autocorrelation is similar to global in the way it can quantify the similarities of polygons to their neighbours. The differences arise when you look into what values it produces. Local spatial autocorrelation produces a statistic for every observation in a dataset, unlike global which produces one for the entire dataset. This means that outliers will have less impact and the only way for observations to have very high values is if they are completely surrounded by similar values.

This is the equation for the Local Moran's I statistic:

$$
I_i = \frac{x_i - \bar{x}}{S_i^2} \sum_{j=1}^n W_{i,j}(x_j - \bar{x}) \quad \text{where} \quad S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1}
$$

This equation results in a measure of the difference between observation i and the mean multiplied by the sum of differences of its neighbours and the mean. Then, it is standardized by dividing it by the standard deviation of I, writen above as S_i^2. 

This can also be run in R using the localmoran() function, to avoid doing any of the complicated calculations by hand. 

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

Once the values have all been calculated, we can map the results to visualize our Local Moran's I results.

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

This map shows the census areas where the Z-Score of the Local Moran's I test is both above 1.96 and below -1.96, indicating the areas where there is positive spatial autocorrelation and negative spatial autocorrelation. By showing this for both variables, we can see which census tracts are more similar to their neighbours and which are more different than their neighbours, both being at a statistically significant level.

Maps are just one way of visualizing the data, and while it is easy to look at them and understand some basic outcomes, plotting this data on a graph can be more informative and provide some more depth to the conclusions.

In these plots, the x-axis represents the values at location i, while the y-axis represents values in the neighbourhood of location i. Values in the top-right of the plot represent locations where the value of the attribute at i and its neighbours are high above the mean, showing positive spatial autocorrelation. The points in the lower-corner of the plot are locations where the value of the attribute at i and its neighbours are lower than the mean which also represents locations of positive spatial autocorrelation.

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

For these graphs, the diamond shaped points are significantly significant census tracts, and the overall trend is shown in the regression line. Both plots show that there is positive spatial autocorrelation shown by the regression line going from the bottom-left to the top-right. 

## Summary

In summary, using R to conduct a neighbourhood matrix and a Global and Local Moran's I statistical test, we were able to conclude that both median income and percentage of french speakers are positively spatially autocorrelated in Lethbridge, Alberta. By creating maps and plots along the way, we were able to visualize the process and strengthen our spatial statistics skills in R. Now that you have completed this, you can go out and do spatial autocorrelation tests on any data that you find and answer your own research questions!

## References
Moraga, P. (n.d.). Chapter 7: Spatial Neighborhood Matrices. In Spatial Statistics for Data Science: Theory and Practice with R. Retrieved October 20, 2024, from https://www.paulamoraga.com/book-spatial/spatial-neighborhood-matrices.html

Sergio J., R., Dani, A.-B., & Levi J., W. (2020). Spatial Weights. In Geographic Data Science with Python. https://geographicdata.science/book/notebooks/04_spatial_weights.html

Spatial weights for neighbours lists—Nb2listw. (n.d.). Retrieved October 20, 2024, from https://r-spatial.github.io/spdep/reference/nb2listw.html
