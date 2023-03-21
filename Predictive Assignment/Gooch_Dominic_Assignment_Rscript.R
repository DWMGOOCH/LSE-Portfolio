## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

## Note:
## Markdowns / insights will begin with a double ## followed by markdown any following 
# sentences will continue with a single # like in this example
# code comments will just have a # hashtag

###############################################################################

# 1. Load and explore the data
install.packages("rmarkdown")

# Install and import Tidyverse.
library('tidyverse')
library(dplyr)

# Import the data set.
turtle_sales <- read.csv(file.choose(), header=T)

# File used was turtle_sales found in ....
# in /Documents/LSE Course/Course 3/Assignment/LSE_DA301_assignment_files

# Print the data frame.
view(turtle_sales)

## viewed the data frame as a table / spreadsheet format 

# Explore the data
# Convert data frame to a tibble.
# Note that as.tibble() might be outdated depending on your R version.
as_tibble(turtle_sales)

# Use the glimpse() function.
glimpse(turtle_sales)

# Use the summary() function.
summary(turtle_sales)

# Various methods of exploring and checking the data 
# All file types, columns look to be in the right format

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_subset <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

## Created a data frame with only the relevant files

# View the data frame.
view(sales_subset)
head(sales_subset)

# View the descriptive statistics.
summary(sales_subset)

## Everything with the new data frame seems to be in good working order

## Create a new columns titled ROW (Rest of the world) to show the sales outside 
# Europe and North America 
sales_subset$ROW <- sales_subset$Global_Sales - (sales_subset$EU_Sales + 
                                                   sales_subset$NA_Sales)

# Rounded column to two decimal points
sales_subset$ROW <- round(sales_subset$ROW, digits = 2)

# View data frame to ensure calculations have worked properly
view(sales_subset)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
# x = NA_Sales, y = EU_Sales, Colour = Platform
qplot(NA_Sales, EU_Sales, colour=Platform, data=sales_subset,
      main = "Product Sales in North America and Europe",
      ylab = 'Number of European sales',
      xlab = 'Number of North America sales')

qplot(NA_Sales, ROW, colour=Platform, data=sales_subset,
      main = "Product Sales in North America and Rest of the World",
      ylab = 'Number of Rest of the World sales',
      xlab = 'Number of North America sales')

qplot(EU_Sales, ROW, colour=Platform, data=sales_subset,
      main = "Product Sales in North America and Europe",
      ylab = 'Number of European sales',
      xlab = 'Number of Rest of the World sales')

# x = Global_Sales, y = EU_Sales, Colour = Platform
qplot(Global_Sales, EU_Sales, colour=Product, data=sales_subset,
      main = "Product Sales in Globally and Europe",
      ylab = 'Number of European sales',
      xlab = 'Number of Global sales')

# x = Global_Sales, y = NA_Sales, Colour = Platform
qplot(Global_Sales, NA_Sales, colour=Platform, data=sales_subset)

# x = North American Sales, y = ROW sales
qplot(NA_Sales, ROW, colour=Platform, data=sales_subset)

## 2b) Histograms
# Create a histogram of Global Sales data
qplot(Global_Sales, data=sales_subset, colour=I("white"),
      bins=20, geom='histogram',
      main = "Frequency of Product sales Globally",
      ylab = 'Frequency',
      xlab = 'Number of Global sales') +
        scale_x_continuous(breaks = round(seq(min(sales_subset$Global_Sales), 
                                            max(sales_subset$Global_Sales), by = 5),1))+
  scale_y_continuous(breaks = round(seq(min(0), max(160), by = 20),1))

# Create histogram for NA_Sales 
qplot(NA_Sales, data=sales_subset, colour=I("white"), fill=I('blue'),
      bins=20, geom='histogram', 
      main = "Frequency of Product sales in North America",
      ylab = 'Frequency',
      xlab = 'Number of North American sales') +
  scale_x_continuous(breaks = round(seq(min(sales_subset$NA_Sales), 
                                        max(sales_subset$NA_Sales), by = 2),1)) +
  scale_y_continuous(breaks = round(seq(min(0), max(200), by = 20),1)) 
                
# Create histogram for EU_Sales 
qplot(EU_Sales, data=sales_subset, colour=I("white"), fill=I('red'), fill=Platform,
      bins=20, geom='histogram',
      main = "Frequency of Product sales in Europe",
      ylab = 'Frequency',
      xlab = 'Number of European sales') +
  scale_x_continuous(breaks = round(seq(min(sales_subset$EU_Sales), 
                                        max(sales_subset$EU_Sales), by = 2),1)) +
  scale_y_continuous(breaks = round(seq(min(0), max(200), by = 20),1))

# Create histogram for ROW Sales 
qplot(ROW, data=sales_subset, colour=I("white"), fill=I("green"), fill=Platform,
      bins=20, geom='histogram',
      main = "Frequency of Product sales in the Rest of the World",
      ylab = 'Frequency',
      xlab = 'Number of ROW sales') +
  scale_x_continuous(breaks = round(seq(min(sales_subset$ROW), 
                                        max(sales_subset$ROW), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(0), max(200), by = 20),1))

## 2c) Boxplots
# Create boxplots.

# Boxplot looking at the Global Sales column
boxplot(sales_subset$Global_Sales,horizontal=TRUE, 
xlab = 'Global sales in Millions')

# Boxplot looking at North American Sales column
boxplot(sales_subset$NA_Sales,horizontal=TRUE, 
        xlab = 'North America sales in Millions')

# Boxplot looking at European Sales column
boxplot(sales_subset$EU_Sales,horizontal=TRUE, 
        xlab = 'European sales in Millions')

# Boxplot looking at the Rest of the World Sales column
boxplot(sales_subset$ROW,horizontal=TRUE, 
        xlab = 'Rest of the World sales in Millions')

# Boxplot looking at global sales filtered by each platform
qplot(Global_Sales, Platform, fill = Platform, data=sales_subset, geom='boxplot',
      main = "Sales per Platform Globally",
      xlab = 'Global Sales') + 
  scale_x_continuous(breaks = round(seq(min(sales_subset$Global_Sales), 
                                        max(sales_subset$Global_Sales), by = 10),1))

# Boxplot looking at North American sales filtered by each platform
qplot(NA_Sales, Platform, fill = Platform, data=sales_subset, geom='boxplot',
      main = "Sales per Platform in North America",
      xlab = 'North American Sales') + 
  scale_x_continuous(breaks = round(seq(min(sales_subset$NA_Sales), 
                                        max(sales_subset$NA_Sales), by = 10),1))

# Boxplot looking at EU sales filtered by each platform
qplot(EU_Sales, Platform, fill = Platform, data=sales_subset, geom='boxplot',
      main = "Sales per Platform in Europe",
      xlab = 'European Sales') + 
scale_x_continuous(breaks = round(seq(min(sales_subset$EU_Sales), 
                                      max(sales_subset$EU_Sales), by = 10),1))

# Boxplot looking at ROW sales filtered by each platform
qplot(ROW, Platform, fill = Platform, data=sales_subset, geom='boxplot',
      main = "Sales per Platform in the Rest of the World",
      xlab = 'Rest of the World Sales') + 
  scale_x_continuous(breaks = round(seq(min(sales_subset$ROW), 
                                        max(sales_subset$ROW), by = 10),1))

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......

## Markdown: Scatterplots
## Chose to filter by platform as provided a better colour scale and could identify which
# products on which platforms are performing well
## Provides a insight sales for Platform across two sales categories. Each point is a 
# product code with the colour resembling the Platform. Can see in all four scatterplots that 
# a product sold for the Wii platform is far outselling any other product on any other platform.
# This is true across Europe, North America and ROW.
# In terms of the three sales sub categories products seem to have the highest sales in
# North America, then Europe and lastly ROW.
# Difficult to make any meaningful insights from the graph due to the high numnber of data 
# points which makes reading the values and determine which Platforms difficult. 
# Early insight to gain here is the values that each sales column ranges from and identify 
# and potential outliers.
# Not great graphs.

## Markdown: Histogram
## All four histograms for each sales column display a right skew, with longer tails towards
# the higher sales relevant to each column. 
# Indicates that there are a number of values which are greater than the mean causing this skew
# Looking at each histogram we can see a product to the far right of each plot 
# which corresponds with the scatterplots where we saw a product sold on the Wii platform
# far out performing any other. 
# For the three sub sales columns (NA, EU and ROW) the majority of products sold 
# in each region sold between between 0 and 2 million for each region. 

## Markdown: Boxpots
# Allows us to get another perspective on the shape and skew of the data 
# Also provides us with an insight into the min, max values, where the median lies and 
# any outliers 
# First looked at boxplots for all sales columns ...
# As with the histograms can see from looking at each of the boxplots 
# that the majorty of the products at the lower end of each scale we can see this skewness 
# resembled by the fact that in all the boxplots Q1 is much closer to the min values 
# with longer whiskers after Q3 - followed by outliers.
# We can also see a number of outliers in each column at the end of box plot ... 
# we could omit these outliers from out analysis later, but we have chosen to keep them in 
# as they are still product sales that exist for Turtle Games and therefore relevant to the
# analysis 
# These boxplots were produced to provide us with an understanding of how the data is skewed
# and where any outliers are.

# We also created a boxplot for each sales column filtered by platform to give us a perspective 
# on how sales for each product range
# Gives us a clearer idea of the sale range for each platform in each region
# Can see that both Globally the Wii has the largest spread of sales, 
# and can now clearly visualise the outlier we identified which is significantly higher sales 
# than any other product 
# Can also see that both globally and in North America the NES has a high spread
# of data - now we could think the NES platform is worth investigating but doing some research
# outside of the analysis we can find the NES is an old platform which was released in 1983. 
# Looking at the original data set we can see that data started being recorded in 1982, with 
# the most recent data recorded in 2016 - so whilst NES might have a high number of sales it 
# is unlikely it will be relevant to our analysis now as other platforms like the wii will
# have taken over. 

# Interestingly, for ROW we can see four products all manufactured by 
# Nintendo have large spreads of data, with much higher Q3 and max values 
# compared to other platforms. Nintendo is a japanese games manufacturer - likely that many 
# of these sales come from an Asian market where there is a bias towards these products
# Turtle Games could look to increase their promotion / marketing of products and services 
# in Asia - with a heavy focus on Nintendo products to capitalise on this market. 

# In terms of answering the business question of impact each product has on sales - we would be
# it is difficult to make any clear insights at this stage - due to limitations of visualisations
# It is clear that historically, products on the NES platforms have performed well
# particularly in North America. 

# We can also see that the PS2, again a slightly older format - 
# given there is a PS3 and PS4 in the data performs well across the ROW compared to Europe and 
# North America - typically we might presume these are historic sales and it could be that is 
# the case but we would perhaps expect the PS2 which was a major gaming platform at the time
# to have performed comparatively well in NA and EU. This suggests that there is still a market 
# for the PS2 products in the ROW.  

# Of the modern gaming platforms the Wii seems to be a platform which has products that sell
# well across North America, Europe and the Rest of the World.

# The data is not recent enough to account for sales of the PS4 and XOne which at the time of 
# data recording will have only just been released but have gone on to become highly successful
# gaming platforms which dominated the market - it would be wise for Turtle Games to stock 
# products on these platforms for future sales. 

# Overall not the best insights and offer little insight to helping Turtle Games solve 
# the larger business problem of improving sales performance - much deeper analysis is required

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(sales_subset)

# Check output: Determine the min, max, and mean values.
# Determine min values
min(sales_subset$Global_Sales)
min(sales_subset$EU_Sales)
min(sales_subset$NA_Sales)

# Determine max values
max(sales_subset$Global_Sales)
max(sales_subset$EU_Sales)
max(sales_subset$NA_Sales)

# Determine mean values
mean(sales_subset$Global_Sales)
mean(sales_subset$EU_Sales)
mean(sales_subset$NA_Sales)

# View the descriptive statistics.
summary(sales_subset)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
prod_sales <- sales_subset %>% group_by(Product) %>%
  summarise(sum_global = sum(Global_Sales),
            sum_NA = sum(NA_Sales),
            sum_EU = sum(EU_Sales),
            .groups='drop')

## Markdown: new dataframe created where unique products code are grouped 
# For instance - one product code could be the same game on different platforms 
# Enables us to see which products are best performing
# file names have been changed to to represent they are now the sum total of each 
# sale type e.g. sum_Global is the sum of Global_Sales for that Product

# View the data frame.
prod_sales
nrow(prod_sales)

# Sense checked the number of rows to see if the group by function had worked
# reduced down from 352 rows to 175

# Ordered data frame by ascending order 
prod_sales[order(prod_sales$sum_global),]

# Ordered data frame by descenidng order 
prod_sales[order(-prod_sales$sum_global),]

# Ordered to gauge an idea of the range of values

# Explore the data frame.
summary(prod_sales)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

# Scatterplot looking at relationship between Product and sales globally
p1 <- ggplot(data=prod_sales,
       mapping=aes(x = Product, y = sum_global,)) +
  geom_point(colour = 'red',
             alpha = 0.5,
             size = 1,) +
  # Add the line-of-best-fit to the plot
  geom_smooth(method = 'lm',
              se = FALSE,
              linewidth = 0.5)

p1

p1 + ggtitle('Global Sales per Product') +
  ylab('Number of Global sales') +
  xlab('Product Code')

# Scatterplot looking at relationship between Product and sales in North America (NA)
p2 <- ggplot(data=prod_sales,
       mapping=aes(x = Product, y = sum_NA,)) +
  geom_point(colour = 'red',
             alpha = 0.5,
             size = 1) +
  # Add the line-of-best-fit to the plot
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 0.5)

p2 + ggtitle('North American Sales per Product ')+
  ylab('Number of North American Sales')+
  xlab('Product Code')

# Scatterplot looking at relationship between Product and sales in Europe (EU)
p3 <- ggplot(data=prod_sales,
       mapping=aes(x = Product, y = sum_EU)) +
  geom_point(colour = 'red',
             alpha = 0.5,
             size = 1) + 
  # Add the line-of-best-fit to the plot
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 0.5)

p3 + ggtitle('European Sales per Product ')+
  ylab('Number of European Sales')+
  xlab('Product Code')

# Create histograms.

# Histogram showing the sum of Global Sales and the count of products in each bin
ggplot(data=prod_sales, aes(x = sum_global, stat = 'count')) +
  geom_histogram(fill = 'green', colour = 'black', bins = 20) +
  labs( x = 'Global Sales',
        y = 'Count of Product') +
  scale_x_continuous(breaks = round(seq(min(sales_subset$Global_Sales),
                                        max(sales_subset$Global_Sales), by = 5),1))+
  scale_y_continuous(breaks = round(seq(min(0), max(160), by = 5),1))


ggplot(data=prod_sales, aes(x = sum_NA, stat = 'count')) +
  geom_histogram(fill = 'green', colour = 'black', bins = 20) +
  labs( x = 'North American Sales',
        y = 'Count of Product') +
  scale_x_continuous(breaks = round(seq(min(sales_subset$Global_Sales),
                                        max(sales_subset$Global_Sales), by = 5),1))+
  scale_y_continuous(breaks = round(seq(min(0), max(160), by = 5),1))


ggplot(data=prod_sales, aes(x = sum_EU, stat = 'count')) +
  geom_histogram(fill = 'green', colour = 'black', bins = 20) +
  labs( x = 'European Sales',
        y = 'Count of Product') +
  scale_x_continuous(breaks = round(seq(min(sales_subset$Global_Sales),
                                        max(sales_subset$Global_Sales), by = 5),1))+
  scale_y_continuous(breaks = round(seq(min(0), max(160), by = 5),1))

view(prod_sales)
view(turtle_sales)

###############################################################################
## Markdown: Observations and insights of the graphs:
# Note: Removed boxplots - too many product codes to make valuable insights on the data and 
# boxplots on each sales column as a whole does not provide any particularly interesting 
#insights
# Also, for this analysis only looked at the three main sales columns in the data being 
# global sales, EU sales and NA sales 

# Scatterplots: 
## All three scatterplots display a negative relationship between product code and the total 
# sales globally, in north america and then europe
# Looking at the meta data product code doesn't have any real value - 'Unique code allocated 
# to product based on the item description' - this relationship is probably more coincidental 
# than anything else - in fact if we removed the outliers in any of the graphs
# the line of fit would become a lot flatter. 
# 
# Looking at the NA Sales graph and then comparing with the prod_sales data set 
# we can see that many of the products with sales over 20 million in the NA 
# perform disproportionately better in NA than compared to EU - looking back to the original 
# and corresponding the product codes we can see these products are on the NES platform
# generally released in the late 1980's - perhaps not relevant to Turtle Games now. 
#
# This data frame and scatterplots do allow Turtle Games to see which products sell better 
# in each region - provide better insights. 
#
# Conducted some extra analysis below looking at more relevant time frames 
#
## Histograms:
#
# Given the look of the scatterplots we would expect to see a right skew again in any histograms 
# which is the case in all three graphs. 
#
# As before all three graphs have a left skew to the data - difficult to compared the two histograms
# as the product codes have now been grouped so the sales values are going to be much different 

# Scatterplot graphs here provide the best insight into products and sales 

###############################################################################
# Additional insights
# Want to look at how sales for each platform have changed in the last ten years
# compared to the previous ten years from the data set 
# Group data based on Product and determine the sum per Product.
view(prod_sales)

# Removed any unnecessary columns for this analysis 
plot_sales <- select(turtle_sales, -Ranking, -Genre, -Publisher)

# Main focus of this analysis will be looking at the change in sales for Platforms from ...
# the most recent ten years and the previous ten years 
sales_below <- plot_sales[plot_sales$Year >= "1995" & plot_sales$Year <= "2005", ]
sales_above <- plot_sales[plot_sales$Year >= "2006" & plot_sales$Year <= "2016", ]

# Viewed each data frame to make sure columns have been dropped and right dates have 
# been selected 
view(sales_below)
view(sales_above)

# subsetted the data frame to group each platform in this period and then summed their sales
below_subset <- sales_below %>% group_by(Platform) %>%
  summarise(sum_global = sum(Global_Sales),
            sum_NA = sum(NA_Sales),
            sum_EU = sum(EU_Sales),
            .groups='drop')

# Row 12 has NA values - dropped this from the data frame
below_subset <- below_subset[-12,]

# Viewed the data frame to check the drop has been removed 
view(below_subset)

# subsetted the data frame to group each platform in this period and then summed their sales
above_subset <- sales_above %>% group_by(Platform) %>% 
  summarise(sum_global = sum(Global_Sales),
            sum_NA = sum(NA_Sales),
            sum_EU = sum(EU_Sales),
            .groups='drop')

# Row 14 has NA values - dropped this from the data frame
above_subset <- above_subset[-14,]

# Viewed the data frame to check the drop has been removed 
view(above_subset)

# Created a point plot for below_subset and its global sales
p4 <- ggplot(data=below_subset,
       mapping=aes(x = Platform, y = sum_global))+
  geom_point(colour = 'red',
           alpha = 0.5,
           size = 2) 

p4 + ggtitle('Global Sales per Product 1995 - 2005')+
  ylab('Number of Global Sales')+
  xlab('Product Code')

# Created a point plot for above_subset and its global sales
p5 <- ggplot(data=above_subset,
             mapping=aes(x = Platform, y = sum_global))+
  geom_point(colour = 'red',
             alpha = 0.5,
             size = 2)

p5 + ggtitle('Global Sales per Product 2006 - 2016')+
  ylab('Number of Global Sales')+
  xlab('Product Code')

view(above_subset)

###############################################################################
## Analysis and insights .. 

# Created two subsets with dates looking at the most recent ten years and then the 
# ten years prior to that - wanted to gauge how trends have changed in sales 
# then grouped data frames by platform - wanted to see which platforms have seen an increase 
# in sales and which have seen a decrease - how have customer trends changed over a 20 year 
# period

# Comparing the two scatterplots can see that in the ten years between 1995 to 2005 
# PS2 accounted for the most sales globally with DS second and then the original playstation 
# third.
# Then looking at the most recent ten years we can see a huge change in the platform sales -
# for instance PS2 is now the fourth lowest selling platform (of the platforms which have 
# products sold in this period), and the original playstation doesn't even feature on the list. 
# Can now see the sharp rise in Wii sales following its release. 
# Note two of the most recent platforms, PS4 and XOne, have relatively low sales compared to 
# the other platforms - however the data ends in 2016, if we looked at data from 2016 to today 
# would expect sales in this platforms to be amongst the highest. 
# Additionally, can see in the last ten years there has been a significant increase in the 
# overall sales of products. In 1996 - 2006 the best performing platform (PS2) is the only
# platform to have sales over 100 million. In the years 2006 - 2016 five platforms have sales 
# over 100 million - shows how the market has grown.
# Lastly, there seems to be a particular preference to products on consoles 
# like Wii, X360, PS3 etc. In the ten years prior (1995-2005) handheld platforms like
# the DS, GB (GameBoy) and GBA (GameBoy Advanced) had a much larger market share.

###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plot of Global Sales
qqnorm(prod_sales$sum_global)
qqline(prod_sales$sum_global)

# Create Q-Q Plot of North American Sales 
qqnorm(prod_sales$sum_NA)
qqline(prod_sales$sum_NA)

# Create Q-Q Plot of European Sales
qqnorm(prod_sales$sum_EU)
qqline(prod_sales$sum_EU)

###############################################################################
## Markdown 
# Q-Q line: Helps us to gain an idea of the skewness of the data. Plots two sets of quantiles 
# against each other, in this case sample and theoretical. QQ norm is where we are comparing 
# against normal distribution and allows to see how the data is distributed - comparing the 
# shape of a normal distribution. We also add in a QQ line and if the data is a perfect fit it 
# will lie on this line. 

# Looking at all of the qqnorm plots we can see that many of the data points stray from the 
# qq line with many of the points above the line.
# Generally, each plot has an upward curve shape with the qqline cutting along the bottom 
# of the curve - with most points either on or above the qqline 
# If our qq norm looks like this it gives us an idea that our data is skewed and in this instance
# right skewed - as we analysed in earlier histograms. 

# All three plots display a right skew 

###############################################################################

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test for global sales columnn
shapiro.test(prod_sales$sum_global)

# Perform Shapiro-Wilk test for North American sales columnn
shapiro.test(prod_sales$sum_NA)

# Perform Shapiro-Wilk test for European sales columnn
shapiro.test(prod_sales$sum_EU)

###############################################################################
# Shapiro test

# Purpose of the Shapiro-Wilk test is to check if that data whether the data is normally
# distributed. The null hypothesis of this test is that the data is distributed
# a small p-value would lead us to reject the null hypothesis 
# All three shapiro tests produce a p-value smaller than our significance level (0.05), 
# meaning we can reject the null hypothesis that the data is normally distributed
# Check the size of the df
dim(prod_sales)
# Shapiro-test  works best on larger data sets and with smaller data sets it can
# sometimes become misleading or hard to completely reject the null hypothesis
# would need a larger data set to do this properly therefore, it would be sensible to 
# refer to our Q-Q plots - also shows the data is not normally distributed.  

###############################################################################

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# Skewness of Global Sales
skewness(prod_sales$sum_global) 
 
# Skewness of North American sales 
skewness(prod_sales$sum_NA) 

#Skewness of European Sales 
skewness(prod_sales$sum_EU) 

# Kurtosis of Global Sales
kurtosis(prod_sales$sum_global)

# Kurtosis of North American Sales
kurtosis(prod_sales$sum_NA)

# Kurtosis of European Sales 
kurtosis(prod_sales$sum_EU)

## 3d) Determine correlation
# Determine correlation.
# Specify the cor function.
# Set the first and second variables
cor(prod_sales$sum_global, prod_sales$sum_NA)
cor(prod_sales$sum_global, prod_sales$sum_EU)
cor(prod_sales$sum_NA, prod_sales$sum_EU)

# Can also see the correlation of all the columns in a table 
round (cor(prod_sales),
       digits=2)

# Can also view correlation in a plot
corPlot(prod_sales, cex=2)

###############################################################################

## Markdown:
# Skewness of 0 is perfectly symmetric distribution data set. In the case of our columns 
# we would expect positive values from the skewness test and this is the case for each column 
# with the results: global sales: 3.066769, NA: 3.048198, EU: 2.886029. Confirms this right 
# or positive skew where the larger values / outliers are further towards the right

# Kurtosis: 
# Gives us an idea of how heavy the tails of the distribution are. Kurtosis tell us ...
# if we have thin tails or fat fails from out data. The bench mark for kurtosis
# is a normal distribution which has a values of 3. If the values are below 3 then it would
# indicate light tail distribution whilst above three would signify heavy tail distribution.
# A positive kurtosis - above 3 - indicates fat tails and that extreme outcomes are more 
# common than would be predicted by a stanard normal distriubtion. 

# All three columns display a positive values, significantly higher than 3 and therefore
# indicates that these columns have heavy tails 

## Markdown: Correlation / Pearson Correlation
# The scores here range from -1 to +1. 
# -1 is a strong negative correlation and +1 is a strong positive correlation
# Now given that NA Sales and EU Sales make up part of Global Sales we would expect a strong
# correlation between all three columns - which is exactly the case
# global and NA: 0.9162292, global and EU: 0.8486148, NA & EU: 0.6209317

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Visualise the data.
# Histogram:
# Global Sales histogram
hist(prod_sales$sum_global)

# North American Sales Histogram
hist(prod_sales$sum_NA)

# European Sales Histogram
hist(prod_sales$sum_EU)

# Boxplot:
# Plot Global Sales boxplot
boxplot(prod_sales$sum_global)

# Plot North American sales boxplot
boxplot(prod_sales$sum_NA)

# Plot European Sales boxplot
boxplot(prod_sales$sum_EU)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# The histograms and boxplots confirm the analysis from the skewness and kurtosis tests 
# regarding skewness and kurtosis. 

# Important to note that skewness is normal and common in data set
# All of the above data sets experience skewness - now the severity of this skewness is caused 
# by the outliers in the data, which have been identified throughout this analysis. 
# The skewness and kurtosis from all three of our data sets are perhaps higher than we would be 
# looking for. 
# Ideally we would want a skewness of -3 to +3 and a kurtosis of -10 to +10. 
# The global and north american sales have a skewness slightly above 3 whilst the European Sales 
# has a skewness of 2.87. Euro pean sales skewness isn't too much of a problem but the other 
# columns are a concern. 
# Additionally, the kurtosis for all three columns is significantly over 10, with the lowest 
# value being 16.23 for the European Sales column.
# Having data which is too skewed can cause issues when applying statistical models 
# If we removed the outliers in out data set then the skewness and kurtosis values would be 
# much better. However, we have left these in as part of our analysis as these are still 
# products and sales which exist and so are important in our analysis. 

# In terms of how reliable our data is - could argue there is issues about the reliability due 
# to the large skewness and kurtosis values. Could have some reservations about applying 
# statistical models using this data
# As stated above if we removed the outliers this would improve the skewness and kurtosis 
# which therefore might apply increase the reliability of the data. 

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
view(prod_sales)

# Determine a summary of the data frame.
summary(prod_sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
round (cor(prod_sales),
       digits=2)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
model_global_NA <- lm(sum_global~sum_NA,
             data=prod_sales)

# lm stands for linear regression
# want to create a regression of the sales columnns together

# View the model.
model_global_NA

# View more outputs for the model - the full regression table.
summary(model_global_NA)

# View the coefficients 
coefficients(model_global_NA)

# View residuals on a plot.
plot(model_global_NA$residuals)
plot(prod_sales$sum_NA, prod_sales$sum_global)
abline(coefficients(model_global_NA))

# Complete a log transformation with dplyr's mutate() function.
global_NA <- mutate(prod_sales, 
              logIndex=log(sum_global))

# View new object with new variable
head(global_NA)

# Create a new model using logIndex.
model_global_NA_log <- lm(logIndex~sum_NA,
                          data=global_NA)

# View full regression table.
summary(model_global_NA_log)

###############################################################################

# Second linear regression

# Basic visualisation.
model_EU_global <- lm(sum_global~sum_EU,
                      data=prod_sales)

# View the model
model_EU_global

# View the coefficients 
coefficients(model_EU_global)

# View more outputs for the model - the full regression table
summary(model_EU_global)

# Plot the residuals 
plot(model_EU_global$residuals)

# Plot the two variables
plot(prod_sales$sum_EU, prod_sales$sum_global)
# View line based on the coefficients 
abline(coefficients(model_EU_global))

# Complete a log transformation with dplyr's mutate() function.
global_EU <- mutate(prod_sales, 
                    logIndex=log(sum_global))

# View new object with new variable
head(global_EU)

# Create a new model using logIndex.
model_global_EU_log <- lm(logIndex~sum_EU,
                          data=global_EU)

# View full regression table.
summary(model_global_EU_log)

###############################################################################
# Third linear regression
# Create a simple linear regression
model_NA_EU <- lm(sum_NA~sum_EU,
                  data=prod_sales)

# View the model
model_NA_EU
summary(model_NA_EU)

# View the residuals 
plot(model_NA_EU$residuals)

# Plot the two variables on a graph
plot(prod_sales$sum_EU, prod_sales$sum_NA)
# apply a line based on the coefficients 
abline(coefficients(model_NA_EU))

# Complete a log transformation with dplyr's mutate() function.
NA_EU <- mutate(prod_sales, 
                    logIndex=log(sum_NA))

# View new object with new variable
head(NA_EU)

# Create a new model using logIndex.
model_NA_EU_log <- lm(logIndex~sum_EU,
                          data=NA_EU)

# View full regression table.
summary(model_NA_EU_log)

###############################################################################
## Markdowns
# Linear regression models:
# Note whilst log regression models have been created they will be ignored for analysis 
# purposes on the basis that the R-squared is much lower than the standard linear 
# regression models. Wouldn't recommend these models over any of the other 
# models - therefore we will focus on using the standard linear regression models.
# Linear regression models allow us to identify the relationship between two variables
# in this case, the sales columns against each other. 
# When creating models of global sales against other columns important to keep global 
# sales as the dependent variable - the other way round would create a negative intercept 
# sales of NA and EU couldn't be explained by global sales - value too large 
# 
# R-squared and p-values are our central focus here - like in Python
# R-squared results: sum_global~sum_NA = 0.8395, sum_global~sum_EU = 0.7201 and 
# sum_NA~sum_EU = 0.3856. 
# p-values - the p-values in each of the regression models is especially low <2e-16
# so all the variables are significant 
# Model with the strongest R-squared is global sales and NA sales, this is perhaps expected as
# these have the highest correlation between them
# 83.9% of the variation in global sales is explained by NA sales - if we had a new product 
# this model would give us a reasonably accurate prediction of its global sales.
# The sum_global~sum_Eu model also produces a high good R-squared value, however compared to
# the previous model it is significantly lower and so wouldn't choose this model.
# The sum_NA~sum_EU model r-squared value it much too low - suggests that perhaps there isn't
# that strong relationship between NA and EU sales. 

# To potentially achieve a higher R-squared value we will run an mlr model combining 
# plotting the global sales column against the NA and EU columns from the original data set 
# can then make predictions based on this model .... 

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
library(psych)

view(turtle_sales)
as.tibble(turtle_sales)

# Multiple linear regression model.
modela = lm(Global_Sales~NA_Sales+EU_Sales,
             data=turtle_sales)


# View a summary model
summary(modela)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
new_sales <- data.frame(NA_Sales=c(34.02, 3.93, 2.73, 2.26, 22.08),
                        EU_Sales=c(23.80, 1.56, 0.65, 0.97, 0.52))

predicttest <- predict(modela, newdata = new_sales,
                       interval = 'confidence')

predicttest

view(turtle_sales)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# Mlr model produces a much exceptionally high r-squared of 0.9687
# p-values both lower than 0.05 
# 96.9% of of the variation in global sales is explained by NA and EU sales 
# Whilst these are from varying data sets, so difficult to properly compared, but given the 
# The accuracy of the model can be seen with the predict test. 
# For instance if a new product had NA sales and EU sales of 34.02 and 23.80 our model
# would predict this product has sales of 71.47. When we check our original data set we can
# see product ranked number 1 has this exact number of NA and EU sales and has global sales of 
# 67.85 - the difference between the two values is around is minimal around 3.62. Shows our 
# model is good at placing accurate predictions 

# There is a strong relationship between NA and EU sales to Global Sales - which we would expect

###############################################################################
###############################################################################




