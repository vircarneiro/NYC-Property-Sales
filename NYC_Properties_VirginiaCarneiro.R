################################################################
# Get NYC Property Data Set
################################################################
## Install and Loading libraries
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(data.table)
library(caret)
library(ggplot2)
library(dplyr)
library(corrplot)

# Add an if condition to run easily my code.
if (file.exists("NYCProperty")) {
  ## Read NYYProperty dataset
  NYCProperty <- readRDS(file="NYCProperty")
} else {
  ## Download NYC Property Data Set
  dl <- tempfile()
  download.file("https://github.com/vircarneiro/NYC-Property-Sales/blob/main/nyc-rolling-sales.zip?raw=true", dl)
  
  NYCProperty <- fread(text = gsub("::", ",", readLines(unzip(dl, "nyc-rolling-sales.csv"))),
                       col.names = c("Index","Borough","Neighborhood", "BuildingClassCategory", "TaxClassPresent", "Block", 
                                     "Lot", "EASE-MENT", "BuildingClassCategoryPresent", "Address", "Apartment", "ZipCode",
                                     "ResidentionUnit", "CommercialUnit", "TotalUnit", "LandFeet", "GrossFeet", 
                                     "YearBuilt", "TaxClassSale", "BuildingClassSale", "SalePrice", "SaleDate"))
  
  ## Delete objects from the memory
  rm(dl)
  
  ## Save NYYProperty dataset
  saveRDS(NYCProperty, file="NYCProperty")
}

################################################################
# Data Validation and Cleaning - NYCProperty dataset    
################################################################

# Data set structure
str(NYCProperty)
# There are 83981 NA
sum(is.na(NYCProperty)) 
colnames(NYCProperty)[colSums(is.na(NYCProperty)) > 0] #EASE-MENT

## EASY_MENT 
### Erase column EASY_MENT. Filled with NA
NYCProperty <- NYCProperty[,-c("EASE-MENT")]

## Index 
## Erase column Index.  Not useful for analysis.
NYCProperty <- NYCProperty[,-c("Index")]

## Verify duplicated observations 
NYCProperty <- unique(NYCProperty)

## SalePrice (target feature)
SalePrice <- NYCProperty %>% 
  select(SalePrice) %>%
  distinct()
### Delete rows with Sale Price == "-"
NYCProperty <-NYCProperty[!(NYCProperty$SalePrice == "-"),]
### Convert to numeric
NYCProperty$SalePrice <- as.numeric(NYCProperty$SalePrice)
### Properties with a sale price < USD 100.000 Im going to remove them.
NYCProperty <-NYCProperty[!(NYCProperty$SalePrice <= 100000),]

## Showing the outliers
ggplot(NYCProperty, aes(x= "", y= SalePrice)) + 
  geom_boxplot(color= 'dark grey', fill = 'light grey',)+
  coord_flip() +
  labs(title = "Properties Sale Price - Outlier",
       y = "Sale Price") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## Filtering Properties with sale price <= USD 200.000.000 before remove outliers
SalePrice <- NYCProperty %>% select(SalePrice, Borough, Block, Lot)%>%
  filter(SalePrice <= 200000000)%>%
  group_by(SalePrice) %>%
  count(SalePrice) %>%
  arrange((SalePrice))

## Delete rows with Sale Price < USD 200.000.000 to remove outliers.
NYCProperty <- NYCProperty[(NYCProperty$SalePrice < 200000000),  ]

ggplot(SalePrice, aes(x=SalePrice, y= n)) + 
  geom_point()+
  labs(title = "Properties with Sale Price Between $100K and $200MM",
       x = "Sale Price",
       y = "Quantity of Properties")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Apply logarithm to reduce skew to left.  
#https://www.analyticsvidhya.com/blog/2020/10/how-to-use-stacking-to-choose-the-best-possible-algorithm/
NYCProperty$SalePrice <- log10(NYCProperty$SalePrice)

### Plot after remove skew
SalePrice <- NYCProperty %>% select(SalePrice, Borough, Block, Lot)%>%
  group_by(SalePrice) %>%
  count(SalePrice) %>%
  arrange((SalePrice))

ggplot(SalePrice, aes(x=SalePrice, y= n)) + 
  geom_point()+
  labs(title = "Sale Price After Remove Skewer",
       x = "Sale Price",
       y = "Quantity of Properties") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## Borough
### Verify values
NYCProperty %>% select(Borough) %>%
  distinct()

# Plot by Borough
Borough <- NYCProperty %>% 
  select(Borough, Block, Lot) %>% 
  group_by(Borough) %>% 
  summarize(Qty = n()) %>%
  arrange((Borough))

Borough %>%
  arrange(Qty) %>%    # First sort by Qty
  mutate(Borough=factor(Borough, levels=Borough)) %>%
  ggplot(aes(x=Borough, y=Qty)) +
  geom_segment( aes(xend=Borough, yend=0)) +
  geom_point( size=5, color= "light blue") +
  labs(title = "NYC Properties by Borough",
       x = "Borough",
       y = "Qty of Property") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## Neighborhood
### Verify values
Neighborhood <- NYCProperty %>% 
  select(Neighborhood) %>% 
  count(Neighborhood) %>% 
  distinct()
### Label Encoding - 251 categories
NYCProperty$Neighborhood = as.numeric(as.factor(NYCProperty$Neighborhood))

## BuildingClassCategory
### Verify values
BuildingClassCategory <- NYCProperty %>% 
  select(BuildingClassCategory) %>% 
  count(BuildingClassCategory) %>% 
  distinct()
### Label Encoding - 45 Categories
NYCProperty$BuildingClassCategory = as.numeric(as.factor(NYCProperty$BuildingClassCategory))

## TaxClassPresent 
### Verify values. Im going to remove the values " ".  
TaxClassPresent  <- NYCProperty %>% 
  select(TaxClassPresent)%>% 
  #filter(TaxClassPresent == "")%>% 
  count(TaxClassPresent) %>% 
  distinct()
### Delete rows with TaxClassPresent null. These observation has null others important features(YearBuild, GrossFeet, etc) 
NYCProperty <- NYCProperty[!(NYCProperty$TaxClassPresent == ""),  ]
### Plot
TaxClassPresent <- NYCProperty %>% select(TaxClassPresent, Borough, Block, Lot)%>%
  group_by(TaxClassPresent) %>% 
  count(TaxClassPresent) %>%
  arrange(n)

TaxClassPresent %>%
  ggplot(aes(x = reorder(TaxClassPresent, -n), y = n)) +
  geom_bar(stat = "identity", color="light blue", fill= "light blue") +
  labs(title = "NYC Properties Sold by Tax Class Present",
       x = "Tax Class Present",
       y = "Quantity of Properties") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))
### Label Encoding - 9 Categories
NYCProperty$TaxClassPresent = as.numeric(as.factor(NYCProperty$TaxClassPresent))

## Block 
### Verify values
Block <- NYCProperty %>% 
  select(Block ) %>% 
  distinct()

## Lot
### Verify values
Lot <- NYCProperty %>% 
  select(Lot) %>% 
  distinct()

## BuildingClassCategoryPresent
### Verify values 
BuildingClassCategoryPresent <- NYCProperty %>% 
  select(BuildingClassCategoryPresent)  %>% 
  count(BuildingClassCategoryPresent) %>% 
  distinct()
### Label Encoding - 149 Classes
NYCProperty$BuildingClassCategoryPresent = as.numeric(as.factor(NYCProperty$BuildingClassCategoryPresent))

## Address
### Drop Address.  This features is not important for my analysis.  
NYCProperty <- NYCProperty[,-c("Address")]

## Apartment
### Verify values
Apartment  <- NYCProperty %>% 
  select(Apartment) %>% 
  count(Apartment) %>% 
  distinct()
### There are 43147 obs not valid.  It doesn't bring important information for my analysis.
sum((NYCProperty$Apartment == "")) 
sum((NYCProperty$Apartment == "`")) 
### Erase column Apartment most of the values are null.
NYCProperty <- NYCProperty[,-c("Apartment")]

## Zip Code
ZipCode  <- NYCProperty %>% 
  select(ZipCode) %>% 
  count(ZipCode) %>% 
  distinct()
ZipCode  <- NYCProperty %>% 
  filter(ZipCode == 0)
count(ZipCode) %>% 
  distinct()
### Erase the 219 zip codes with value 0. This is an important features to analize.
sum((NYCProperty$ZipCode == 0)) 
NYCProperty <-NYCProperty[!(NYCProperty$ZipCode == "0"),]

ZipCode <- NYCProperty %>% 
  select(Borough, Block, Lot, ZipCode) %>% 
  group_by(ZipCode) %>% 
  count(ZipCode) %>%
  arrange((ZipCode))

ggplot(data = ZipCode, aes(x = reorder(ZipCode, -n),y = n)) +
  geom_bar(stat = "identity", color="light blue", fill="light blue") +
  labs(title = "NYC Properties Sold by Zip Code",
       x = "Zip Code",
       y = "Quantity of Properties") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 5, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## Convert to numeric
NYCProperty$ZipCode <- as.numeric(NYCProperty$ZipCode)

## ResidentionUnit
### Verify values.  Im not going to remove the null values because it's related with TotalUnit for analysis.
ResidentionUnit  <- NYCProperty %>% 
  filter(ResidentionUnit == 0) %>%
  #select(ResidentionUnit) %>% 
  distinct()

## CommercialUnit
### Verify values. Im not going to remove the null values because it's related with TotalUnit for analysis.
CommercialUnit  <- NYCProperty %>% 
  filter(CommercialUnit == 0) %>%
  #select(CommercialUnit) %>% 
  distinct()

## TotalUnit = CommercialUnit + ResidentionUnit. Highest correlated.
### Verify values
TotalUnit  <- NYCProperty %>% 
  #select(TotalUnit)%>% 
  filter(TotalUnit == 0)%>% 
  distinct()
### Delete rows with TotalUnit null. 
NYCProperty <-NYCProperty[!(NYCProperty$TotalUnit == "0"),]

## LandFeet
LandFeet  <- NYCProperty %>% 
  select(LandFeet) %>% 
  count(LandFeet)%>%
  distinct()
### Erase the 4227 rows with 0
sum((NYCProperty$LandFeet == 0)) 
NYCProperty <-NYCProperty[!(NYCProperty$LandFeet == "0"),]
### Erase the 8773 rows with "-"
sum((NYCProperty$LandFeet == "-")) 
NYCProperty <-NYCProperty[!(NYCProperty$LandFeet == "-"),]
### Convert to numeric
NYCProperty$LandFeet <- as.numeric(NYCProperty$LandFeet)
### Plot para llevarlos a un distribucion gausiana para ayudar al modelos
LandFeet  <- NYCProperty %>% 
  select(LandFeet) %>% 
  count(LandFeet)%>%
  distinct()

ggplot(data=LandFeet, aes(x=LandFeet,y=n)) +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Apply logarithm to reduce skew to left.  
NYCProperty$LandFeet <- log10(NYCProperty$LandFeet)

LandFeet  <- NYCProperty %>% 
  select(LandFeet) %>% 
  count(LandFeet)%>%
  distinct()

ggplot(data=LandFeet, aes(x=LandFeet,y=n)) +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## GrossFeet
GrossFeet  <- NYCProperty %>% 
  select(GrossFeet) %>% 
  count(GrossFeet)%>%
  distinct()
### Erase the 50 rows with 0
sum((NYCProperty$GrossFeet == 0)) 
NYCProperty <-NYCProperty[!(NYCProperty$GrossFeet == "0"),]
### Erase the 137 rows with "-"
sum((NYCProperty$GrossFeet == "-")) 
NYCProperty <-NYCProperty[!(NYCProperty$GrossFeet == "-"),]
### Convert to numeric
NYCProperty$GrossFeet <- as.numeric(NYCProperty$GrossFeet)

### Plot para llevarlos a un distribucion gausiana para ayudar al modelos
GrossFeet  <- NYCProperty %>% 
  select(GrossFeet) %>% 
  count(GrossFeet)%>%
  distinct()
ggplot(data=GrossFeet, aes(x=GrossFeet,y=n)) +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# Apply logarithm to reduce skew to left.  
NYCProperty$GrossFeet <- log10(NYCProperty$GrossFeet)

GrossFeet  <- NYCProperty %>% 
  select(GrossFeet) %>% 
  count(GrossFeet)%>%
  distinct()
ggplot(data=GrossFeet, aes(x=GrossFeet,y=n)) +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## YearBuilt
YearBuilt  <- NYCProperty %>% 
  select(YearBuilt) %>% 
  count(YearBuilt)%>%
  distinct()
### Erase the 8 rows with 0
sum((NYCProperty$YearBuilt == 0)) 
NYCProperty <-NYCProperty[!(NYCProperty$YearBuilt == "0"),]
### Convert to numeric
NYCProperty$YearBuilt <- as.numeric(NYCProperty$YearBuilt)

YearBuilt  <- NYCProperty %>% 
  select(YearBuilt) %>% 
  count(YearBuilt)%>%
  distinct()
ggplot(data=YearBuilt, aes(x=YearBuilt,y=n)) +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## TaxClassSale
### Verify values
TaxClassSale  <- NYCProperty %>% 
  select(TaxClassSale) %>% 
  count(TaxClassSale)%>%
  distinct()
### Label Encoding
NYCProperty$TaxClassSale = as.numeric(as.factor(NYCProperty$TaxClassSale))

## BuldingClassSale
### Verify values
BuildingClassSale  <- NYCProperty %>% 
  select(BuildingClassSale) %>% 
  count(BuildingClassSale)%>%
  distinct()
### Label Encoding - 124
NYCProperty$BuildingClassSale = as.numeric(as.factor(NYCProperty$BuildingClassSale))

## SaleDate
### Format as date
NYCProperty$SaleDate <- as.Date(NYCProperty$SaleDate, "%Y-%m-%d %H:%M:%S")
### # Extract YearSale/MonthSale/DaySale from SaleDate
NYCProperty <- NYCProperty %>% mutate(YearSale = as.numeric(format(NYCProperty$SaleDate, "%Y")))
NYCProperty <- NYCProperty %>% mutate(MonthSale = as.numeric(format(NYCProperty$SaleDate, "%m")))
NYCProperty <- NYCProperty %>% mutate(DaySale = as.numeric(format(NYCProperty$SaleDate, "%d")))
### Drop SaleDate after
NYCProperty <- NYCProperty[,-c("SaleDate")]

### Plot Properties sold by Months
MonthSale <- NYCProperty %>% select(MonthSale, Borough, Block, Lot)%>%
  group_by(MonthSale) %>% 
  count(MonthSale) %>%
  arrange(n)

MonthSale %>%
  ggplot(aes(x = reorder(MonthSale, -n), y = n)) +
  geom_bar(stat = "identity", color="light blue", fill= "light blue") +
  labs(title = "NYC Properties Sold by Months",
       x = "Months",
       y = "Quantity of Properties Sold") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

## Drop duplicated observations after normalization
count(unique(NYCProperty))
NYCProperty <- unique(NYCProperty)

# This is my final dataset
str(NYCProperty)
summary(NYCProperty)

## Delete objects from the memory
rm(Apartment, Block, BuildingClassCategory, BuildingClassCategoryPresent, BuildingClassSale, CommercialUnit,
   GrossFeet, LandFeet, Lot, Neighborhood, ResidentionUnit, SalePrice, TaxClassPresent, TaxClassSale, TotalUnit,
   YearBuilt, ZipCode, Borough, MonthSale)

# Reorder Columns
NYCProperty <- NYCProperty %>% select(Borough, Block, Lot, Neighborhood:TaxClassPresent, BuildingClassCategoryPresent:BuildingClassSale, YearSale:DaySale, SalePrice)

################################################################
# Split the dataset - Create NYCProperties dataset, Validation dataset.
################################################################

# Validation set will be 10% of NYCProperty data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = NYCProperty$SalePrice, times = 1, p = 0.1, list = FALSE)
NYCProperties <- NYCProperty[-test_index,]
temp <- NYCProperty[test_index,]

## Make sure Borough, Block and Lot in validation set are also in NYC Properties data set.
validation <- temp %>% 
  semi_join(NYCProperties, by = "Borough") %>%
  semi_join(NYCProperties, by = "Block") %>%
  semi_join(NYCProperties, by = "Lot")

## Add rows removed from validation set back into NYCProperties data set
removed <- anti_join(temp, validation)
NYCProperties <- rbind(NYCProperties, removed)

## Delete objects from the memory
rm(NYCProperty, removed, temp, test_index)

################################################################
# Data Visualization - NYCProperties dataset    
################################################################

## Heatmap to Drop Correlative Features 
#https://jkzorz.github.io/2019/06/11/Correlation-heatmaps.html
Columms <- NYCProperties[,Borough:DaySale]
Data <- cor(Columms, method = "spearman")
corrplot(Data, tl.col = "black", order = "hclust", hclust.method = "average", addrect = 4, tl.cex = 0.7)

### Drop YearSale (Highly uncorrelated with MonthYear)
NYCProperties <- NYCProperties[,-c("YearSale")]
### Drop BuildingClassCategoryPresent, BuildingClassSale, BuildingClassCategory (Highly correlated with TotalUnit)
NYCProperties <- NYCProperties[,-c("BuildingClassCategoryPresent", "BuildingClassSale", 
                                   "BuildingClassCategory","ResidentionUnit")]
### Drop TaxClassPresent (Highly correlated with TaxClassSale)
NYCProperties <- NYCProperties[,-c("TaxClassPresent")]

## After remove these features.
Columms <- NYCProperties[,Borough:DaySale]
Data <- cor(Columms, method = "spearman")
corrplot(Data, tl.col = "black", order = "hclust", hclust.method = "average", addrect = 4, tl.cex = 0.7)

##Validation dataset
### Drop YearSale (Highly uncorrelated with MonthYear)
validation <- validation[,-c("YearSale")]
### Drop BuildingClassCategoryPresent, BuildingClassSale, BuildingClassCategory (Highly correlated with TotalUnit)
validation <- validation[,-c("BuildingClassCategoryPresent", "BuildingClassSale", "BuildingClassCategory","ResidentionUnit")]
### Drop TaxClassPresent (Highly correlated with TaxClassSale)
validation <- validation[,-c("TaxClassPresent")]

## Delete objects from the memory
rm(Columms,Data,)

str(NYCProperties)

# Normalization NYCProperty

# Min-Max normalization function (Same scale)
normalize <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
NYCProperties$Borough <- normalize(NYCProperties$Borough)
NYCProperties$Neighborhood <- normalize(NYCProperties$Neighborhood)
NYCProperties$Block <- normalize(NYCProperties$Block)
NYCProperties$Lot <- normalize(NYCProperties$Lot)
NYCProperties$ZipCode <- normalize(NYCProperties$ZipCode)
NYCProperties$CommercialUnit <- normalize(NYCProperties$CommercialUnit)
NYCProperties$TotalUnit <- normalize(NYCProperties$TotalUnit)
NYCProperties$LandFeet <- normalize(NYCProperties$LandFeet)
NYCProperties$GrossFeet <- normalize(NYCProperties$GrossFeet)
NYCProperties$YearBuilt <- normalize(NYCProperties$YearBuilt)
NYCProperties$TaxClassSale <- normalize(NYCProperties$TaxClassSale)
NYCProperties$MonthSale <- normalize(NYCProperties$MonthSale)
NYCProperties$DaySale <- normalize(NYCProperties$DaySale)

str(NYCProperties)

# Normalization Validation
validation$Borough <- normalize(validation$Borough)
validation$Neighborhood <- normalize(validation$Neighborhood)
validation$Block <- normalize(validation$Block)
validation$Lot <- normalize(validation$Lot)
validation$ZipCode <- normalize(validation$ZipCode)
validation$CommercialUnit <- normalize(validation$CommercialUnit)
validation$TotalUnit <- normalize(validation$TotalUnit)
validation$LandFeet <- normalize(validation$LandFeet)
validation$GrossFeet <- normalize(validation$GrossFeet)
validation$YearBuilt <- normalize(validation$YearBuilt)
validation$TaxClassSale <- normalize(validation$TaxClassSale)
validation$MonthSale <- normalize(validation$MonthSale)
validation$DaySale <- normalize(validation$DaySale)

str(validation)

################################################################
# Create training and test sets from NYCProperties dataset.
################################################################
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = NYCProperties$SalePrice, times = 1, p = 0.1, 
                                  list = FALSE)  # I'm taking the same porcentage as Validation dataset (10%)
train_NYCProperties <- NYCProperties[-test_index,]
test_NYCProperties <- NYCProperties[test_index,]

saveRDS(train_NYCProperties, file = "train_NYCProperties.rds")
saveRDS(test_NYCProperties, file = "test_NYCProperties.rds")

## Delete objects from the memory
rm(NYCProperties, test_index)

train_NYCProperties <- readRDS(file="train_NYCProperties.rds")
test_NYCProperties <- readRDS(file="test_NYCProperties.rds")

### DATA PREPARATION
y_train <- train_NYCProperties$SalePrice
x_train <- train_NYCProperties[,!("SalePrice")]

y_test <- test_NYCProperties$SalePrice
x_test <- test_NYCProperties[,!("SalePrice")]

y_val <- validation$SalePrice
x_val <- validation[,!("SalePrice")]

############################################################################
############# Model #1 -  linear regression - Baseline Approach
############################################################################
set.seed(1, sample.kind = "Rounding")

lr.fit <- lm(
  formula = SalePrice ~., 
  data = train_NYCProperties)

lr.yhat_train <- lr.fit %>% predict(as.data.frame(x_train))
lr.yhat_test <- lr.fit %>% predict(as.data.frame(x_test))

lr.train_rmse <- RMSE(lr.yhat_train, y_train)
lr.test_rmse <- RMSE(lr.yhat_test, y_test)

tmp_table <- data_frame(Method = "Linear Regression - Train", RMSE = lr.train_rmse)
tmp_table <- rbind(tmp_table, data_frame(Method = "Linear Regression - Test", RMSE = lr.test_rmse))
tmp_table %>% knitr::kable(caption = "RMSEs")

rmse_table <- data_frame(Method = "Linear Regression - Test", RMSE = lr.test_rmse)

## plot
ggplot() +
  geom_line(aes(x=1:length(y_test), y=(10^y_test) - (10^lr.yhat_test)), color="pink") +
  labs(title = "Linear Regression",
       x = "Index",
       y = "y_tes - yhat_test")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

############################################################################
############# Model #2 -  Support Vector Regression
############################################################################
library(e1071)
set.seed(1, sample.kind = "Rounding")

svr.fit <- svm(
  formula = SalePrice ~ ., 
  data=train_NYCProperties) 

svr.yhat_train <- svr.fit %>% predict(as.data.frame(x_train))
svr.yhat_test <- svr.fit %>% predict(as.data.frame(x_test))

svr.train_rmse <- RMSE(svr.yhat_train, y_train)
svr.test_rmse <- RMSE(svr.yhat_test, y_test)

tmp_table <- data_frame(Method = "Support Vector Regression - Train", RMSE = svr.train_rmse)
tmp_table <- rbind(tmp_table, data_frame(Method = "Support Vector Regression - Test", RMSE = svr.test_rmse))
tmp_table %>% knitr::kable(caption = "RMSEs")

rmse_table <- rbind(rmse_table, data_frame(Method = "Support Vector Regression - Test", RMSE = svr.test_rmse))

## plot
ggplot() +
  geom_line(aes(x=1:length(y_test), y=(10^y_test) - (10^svr.yhat_test)), color="red") +
  labs(title = "Support Vector Regression",
       x = "Index",
       y = "y_tes - yhat_test")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

############################################################################
############# Model #3 -  KNN
############################################################################
set.seed(1, sample.kind = "Rounding")
knn.fit <- train(
  form = SalePrice ~ .,
  method = "knn", 
  data = train_NYCProperties,
  tuneGrid = data.frame(#k = seq(15, 35, 2)
    k = 15)) #bestTune
knn.fit$bestTune

knn.yhat_train <- knn.fit %>% predict(as.data.frame(x_train))
knn.yhat_test <- knn.fit %>% predict(as.data.frame(x_test))

knn.train_rmse <- RMSE(knn.yhat_train, y_train)
knn.test_rmse <- RMSE(knn.yhat_test, y_test)

tmp_table <- data_frame(Method = "KNN - Train", RMSE = knn.train_rmse)
tmp_table <- rbind(tmp_table, data_frame(Method = "KNN - Test", RMSE = knn.test_rmse))
tmp_table %>% knitr::kable(caption = "RMSEs")

rmse_table <- rbind(rmse_table, data_frame(Method = "KNN - Test", RMSE = knn.test_rmse))

## plot
ggplot() +
  geom_line(aes(x=1:length(y_test), y=(10^y_test) - (10^knn.yhat_test)), color="dark green") +
  labs(title = "KNN",
       x = "Index",
       y = "y_tes - yhat_test")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

############################################################################
############# Model #4 -  Regression Trees
############################################################################
library(rpart)
set.seed(1, sample.kind = "Rounding")

rt.fit <- rpart(
  formula = SalePrice ~ ., 
  method="anova", 
  data=train_NYCProperties)

rt.yhat_train <- rt.fit %>% predict(as.data.frame(x_train))
rt.yhat_test <- rt.fit %>% predict(as.data.frame(x_test))

rt.train_rmse <- RMSE(rt.yhat_train, y_train)
rt.test_rmse <- RMSE(rt.yhat_test, y_test)

tmp_table <- data_frame(Method = "Regression Trees - Train", RMSE = rt.train_rmse)
tmp_table <- rbind(tmp_table, data_frame(Method = "Regression Trees - Test", RMSE = rt.test_rmse))
tmp_table %>% knitr::kable(caption = "RMSEs")

rmse_table <- rbind(rmse_table, data_frame(Method = "Regression Trees - Test", RMSE = rt.test_rmse))

## plot
ggplot() +
  geom_line(aes(x=1:length(y_test), y=(10^y_test) - (10^rt.yhat_test)), color="orange")+
  labs(title = "Regression Trees",
       x = "Index",
       y = "y_tes - yhat_test") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))
############################################################################
############# Model #5 - Random Forest
############################################################################
library(randomForest)
library(caret)

set.seed(1, sample.kind = "Rounding")

rf.fit <- randomForest(
  formula = SalePrice ~ ., 
  data = train_NYCProperties, 
  nodesize = 50 # min. #observations in a terminal node
  )

rf.yhat_train <- rf.fit %>% predict(as.data.frame(x_train))
rf.yhat_test <- rf.fit %>% predict(as.data.frame(x_test))

rf.train_rmse <- RMSE(rf.yhat_train, y_train)
rf.test_rmse <- RMSE(rf.yhat_test, y_test)

tmp_table <- data_frame(Method = "Random Forest - Train", RMSE = rf.train_rmse)
tmp_table <- rbind(tmp_table, data_frame(Method = "Random Forest - Test ", RMSE = rf.test_rmse))
tmp_table %>% knitr::kable(caption = "RMSEs")

rmse_table <- rbind(rmse_table, data_frame(Method = "Random Forest - Test", RMSE = rf.test_rmse))

## plot
ggplot() +
  geom_line(aes(x=1:length(y_test), y=(10^y_test) - (10^rf.yhat_test)), color="brown") +
  labs(title = "Random Forest",
       x = "Index",
       y = "y_tes - yhat_test") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

############################################################################
############# Model #6 - Gradient Boosting Machines
############################################################################
# To replace XGBoost used in the MovieLens project.
library(gbm)
set.seed(1, sample.kind = "Rounding")

gbm.grid <- expand.grid(#interaction.depth = seq(12, 30, 2), # max_depth
  interaction.depth = 30,             # bestTune
  n.trees = 1000,                     # nround / # of Iterations
  shrinkage = 0.01,                   # ETA / Learning Rate (Default Value)
  n.minobsinnode = 10)                # Default Value
gbm.ctrl <- trainControl(method="cv",                       # Cross Validation
                         number = 5,
                         verboseIter = FALSE)             

gbm.fit <- train(
  form = SalePrice ~ .,
  method = "gbm", 
  data = train_NYCProperties,
  tuneGrid = gbm.grid,
  trControl = gbm.ctrl,
  verbose = FALSE)

gbm.fit$bestTune

gbm.yhat_train <- gbm.fit %>% predict(as.data.frame(x_train))
gbm.yhat_test <- gbm.fit %>% predict(as.data.frame(x_test))

gbm.train_rmse <- RMSE(gbm.yhat_train, y_train)
gbm.test_rmse <- RMSE(gbm.yhat_test, y_test)

tmp_table <- data_frame(Method = "Gradient Boosting Machines - Train", RMSE = gbm.train_rmse)
tmp_table <- rbind(tmp_table, data_frame(Method = "Gradient Boosting Machines - Test ", RMSE = gbm.test_rmse))
tmp_table %>% knitr::kable(caption = "RMSEs")

rmse_table <- rbind(rmse_table, data_frame(Method = "Gradient Boosting Machines - Test", RMSE = gbm.test_rmse))

## plot
ggplot() +
  geom_line(aes(x=1:length(y_test), y=(10^y_test) - (10^gbm.yhat_test)), color="blue") +
  labs(title = "Gradient Boosting Machines",
       x = "Index",
       y = "y_tes - yhat_test")+
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 8, angle = 90, hjust = 0.5, 
                                   vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 8),
        strip.text = element_text(face = "italic"),
        text = element_text(size = 10))

# The Best RMSE running in Validation Dataset.
best.yhat <- gbm.fit %>% predict(validation)
best.val_rmse = RMSE(best.yhat, y_val)

rmse_table <- rbind(rmse_table, data_frame(Method = "Gradient Boosting Machines - Validation", RMSE = best.val_rmse))
rmse_table %>% knitr::kable(caption = "Final RMSE")

