# clear the environment
rm(list = ls())

#set working directory
setwd("C:/Users/LisaJin/Documents/University/CA/Unilever")

#load packages
library(readxl)
library(polyglotr)
library(dplyr)
library(readr)
library(tm)
library(stm)
library(stargazer)
library(wordcloud)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(cluster)
library(ggfortify)
library(gridExtra)
library(reshape2)
library(translateR)


#open xls file
amazon <- read_excel("amazon_reviews.xlsx") # all
Knorr_hellmans <- read_excel("amazon_reviews1.xlsx") # Knorr and Hellmans
knorr_hellmans_cat <- read.csv("Amazon_categories_final.csv")# Knorr and Hellmans with category
knorr_hellmans_cat_clean <- read.csv("Amazon_categories_final_clean.csv")# Knorr and Hellmans with category cleaned

#-----------------------
# Cleaning the data
#-----------------------

#check what is in GPT estimated
knorr_hellmans_cat$GPT_estimated

#count all unique categories in gpt_estimated descending
sort( table(knorr_hellmans_cat$GPT_estimated), decreasing = TRUE)
    # Fix produkte, snacks fertiggerichte, suppen most reviews

#count amount of unique categories
length(unique(knorr_hellmans_cat$GPT_estimated))
    # 26 categories 

#check for duplicate columns
are_duplicates <- identical(knorr_hellmans_cat$x, knorr_hellmans_cat$x.1)
    #TRUE
are_duplicates <- identical(knorr_hellmans_cat$Product_name_x, knorr_hellmans_cat$Product_name_y)
    #TRUE
are_duplicates <- identical(knorr_hellmans_cat$GPT_estimated_cleaned, knorr_hellmans_cat$GPT_estimated)
    #FALSE but cleaned version is different because it is reduced from 15 to 9 categories due to string incompatability

#delete columns X.1, Product_name_y, n
knorr_hellmans_cat <- knorr_hellmans_cat[, !(names(knorr_hellmans_cat) %in% c("Product_name.y", "X", "n", "GPT_estimated"))] 

#rename product_name_x to product_name
names(knorr_hellmans_cat)[names(knorr_hellmans_cat) == "Product_name.x"] <- "Product_name"

#save dataframe as csv  
write.csv(knorr_hellmans_cat, "Amazon_categories_final_clean.csv")

#---------------------------------

#creating some useful variables for in knorr_hellmans_cat_clean
knorr_hellmans_cat_clean$Fivestars <- as.numeric(knorr_hellmans_cat_clean$Rating == 5) #isolate as new variable
knorr_hellmans_cat_clean$Onestar <- as.numeric(knorr_hellmans_cat_clean$Rating == 1) #isolate as new variable
knorr_hellmans_cat_clean$Brand <- factor(knorr_hellmans_cat_clean$Brand) #translate to factor
knorr_hellmans_cat_clean$Date <- as.Date(knorr_hellmans_cat_clean$Date, format = "%Y-%m-%d") # convert to date variable
knorr_hellmans_cat_clean$Year <- as.numeric(format(knorr_hellmans_cat_clean$Date, "%Y")) #extract year

knorr_hellmans_cat_clean$Time <- (knorr_hellmans_cat_clean$Year -
                                    min(knorr_hellmans_cat_clean$Year,na.rm=T)) * 12 + knorr_hellmans_cat_clean$Month

#--------------------------------
# Explore knorr_hellmans_cat_clean
#--------------------------------

head(knorr_hellmans_cat_clean)

#see different products within brand = knorr
knorr <- subset(knorr_hellmans_cat_clean, Brand == "Knorr")

#list of unique product_names in knorr
unique(knorr$Product_name)

#count unique product_names in knorr
length(unique(knorr$Product_name))
#493 products

#create new variable product_name_c that contain no digits and "knorr"

  #remove all numbers and "knorr"from the string from product_name
  knorr$Product_name_c <- gsub("Knorr", "", knorr$Product_name)
  #remove all numbers from the string product_name_c
  knorr$Product_name_c <- gsub("[0-9]", "", knorr$Product_name_c)
  #remove ml too
  knorr$Product_name_c <- gsub("ml", "", knorr$Product_name_c)
  #check how many unique names 
  unique(knorr$Product_name_c)
  length(unique(knorr$Product_name_c))

#NEED FIX - list of all unique products in knorr product_name and its one star ratings visualized
knorr %>%
  group_by(Product_name) %>%
  summarize(
    mean_rating = mean(Rating,na.rm=T),
    median_rating = median(Rating,na.rm=T),
    share_5stars = mean(Rating == 5,na.rm=T),
    share_1star = mean(Rating == 1,na.rm=T)
  ) %>%
  arrange(desc(share_1star)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(Product_name, share_1star), y = share_1star)) +
  geom_bar(stat = "identity") +
  labs(title = "Share of 1 star ratings per product") +
  xlab("Product") +
  ylab("Share of 1 star ratings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#see different products within brand = hellmans
hellmans <- subset(knorr_hellmans_cat_clean, Brand == "Hellmann's")

# Calculate average rating for each unique product_name in knorr
avg_rating_knorr <- aggregate(knorr$Rating, by = list(knorr$Product_name), FUN = mean)
head(avg_rating_knorr)
    #Group.1 x
    #1 Knorr Fix fuer Hackfleisch 4.5
    #2 Knorr Fix fuer Spaghetti Bolognese 4.5
    #3 Knorr Fix fuer Sauerbraten 4.5
    #4 Knorr Fix fuer Chili con Carne 4.5
    #5 Knorr Fix fuer Hackbraten 4.5
    #6 Knorr Fix fuer Gulasch 4.5

#unique product names in avg_rating_knorr group.1 
unique(avg_rating_knorr$Group.1)

#count unique product names in avg_rating_knorr group.1
length(unique(avg_rating_knorr$Group.1))
    #355 products

#explore mean_rating for both brands 
rating_summary <- knorr_hellmans_cat_clean %>%
  group_by(Brand) %>%
  summarize(
    mean_rating = mean(Rating,na.rm=T),
    median_rating = median(Rating,na.rm=T),
    share_5stars = mean(Rating == 5,na.rm=T),
    share_1star = mean(Rating == 1,na.rm=T)
  )
      # Brand      mean_rating median_rating share_5stars share_1star
      # <fct>            <dbl>         <int>        <dbl>       <dbl>
      #   1 Hellmann's        4.21             5        0.633      0.0903
      # 2 Knorr             4.11             5        0.552      0.0782

#explore mean_rating for different categories across brands 
rating_summary_cat <- knorr_hellmans_cat_clean %>%
  group_by(GPT_estimated_cleaned) %>%
  summarize(
    mean_rating = mean(Rating,na.rm=T),
    median_rating = median(Rating,na.rm=T),
    share_5stars = mean(Rating == 5,na.rm=T),
    share_1star = mean(Rating == 1,na.rm=T)
  )
    # GPT_estimated_cleaned     mean_rating median_rating share_5stars share_1star
    # <chr>                           <dbl>         <dbl>        <dbl>       <dbl>
    #   1 BBQ and Grilling Sauces          4.11           5          0.544      0.0690
    # 2 Brühen, Bouillons & Würz…        4.31           5          0.659      0.0678
    # 3 Die Bio Gemüse Bouillon          4.29           5          0.647      0.0751
    # 4 Fix Produkte                     4.28           5          0.626      0.0645
    # 5 Grillsaucen, Ketchup & M…        4.26           5          0.631      0.0705
    # 6 Ketchup and Tomato Produ…        4.27           5          0.687      0.0855
    # 7 Mayonnaise, Mayo Alterna…        4.23           5          0.663      0.0976
    # 8 Miscellaneous                    3.89           4.5        0.5        0.111 
    # 9 Mustard and Condiments           4.25           5          0.613      0.0755
    # 10 Natürlich Lecker!                4.27           5          0.622      0.0801
    # 11 Salatzutaten                     4.25           5          0.618      0.0705
    # 12 Sauces                           4.25           5          0.626      0.0672
    # 13 Snacks & Fertiggerichte          3.83           4          0.487      0.122 
    # 14 Specialty Sauces and Fla…        3.87           4          0.422      0.133 
    # 15 Suppen                           4.27           5          0.621      0.0661
    # 16 NA                               4.02           5          0.560      0.121

#plot mean rating per brand
plot_mean_rating_per_brand <-
  ggplot(rating_summary, aes(x = Brand, y = mean_rating)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Rating per Brand") +
  xlab("Brand") +
  ylab("Mean Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
      #small discrepancy between brands

#compare bad reviews across brands 
plot_1star_per_brand <-
  ggplot(rating_summary, aes(x = Brand, y = share_1star)) +
  geom_bar(stat = "identity") +
  labs(title = "Share of 1 star ratings") +
  xlab("Brand") +
  ylab("Mean Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
      #knorr has slightly less 1 star ratings

#compare bad reviews across categories
plot_1star_per_category <-
  ggplot(rating_summary_cat, aes(x = GPT_estimated_cleaned , y = share_1star)) +
  geom_bar(stat = "identity") +
  labs(title = "Share of 1 star ratings") +
  xlab("Category") +
  ylab("Mean Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    #speciality sauces & flavors has the most 1 star ratings then snacks & fertiggerichte
    #suppen have ineed the least 1 star ratings

#compare good reviews across brands
plot_5stars_per_brand <-
  ggplot(rating_summary, aes(x = Brand, y = share_5stars)) +
  geom_bar(stat = "identity") +
  labs(title = "Share of 5 stars' ratings") +
  xlab("Brand") +
  ylab("Mean Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
      #hellmans has slightly more 5 star ratings

#plot histograms of ratings per brand
plot_hist_rating_per_brand <-
  ggplot(knorr_hellmans_cat_clean, aes(x = Rating)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  labs(title = "Histogram of Ratings per Brand", x = "Rating", y = "Frequency") +
  facet_wrap(~ Brand)
  #hellmans has less reviews than knorr

knorr_hellmans_cat_clean$GPT_estimated_cleaned 

#--------------------------------------------------
# most 1star review category analysis within snacks
#--------------------------------------------------

#filter on GPT_estimated_cleaned == Snacks & Fertiggerichte 
snacks_fertiggerichte <- subset(knorr_hellmans_cat_clean, GPT_estimated_cleaned == "Snacks & Fertiggerichte")

#filter out product_names with less than 30 reviews
snacks_fertiggerichte <- subset(snacks_fertiggerichte, Product_name %in% names(sort(table(snacks_fertiggerichte$Product_name), decreasing = TRUE)[1:30]))

#unique product names in snacks_fertiggerichte
unique(snacks_fertiggerichte$Product_name)

# count
length(unique(snacks_fertiggerichte$Product_name))
  #61 products

#Top 5 products with most 1 star ratings
top_5_most_1_star <- snacks_fertiggerichte %>%
  group_by(Product_name) %>%
  summarize(
    mean_rating = mean(Rating,na.rm=T),
    median_rating = median(Rating,na.rm=T),
    share_5stars = mean(Rating == 5,na.rm=T),
    share_1star = mean(Rating == 1,na.rm=T)
  ) %>%
  arrange(desc(share_1star)) %>%
  head(5)

      # # A tibble: 5 × 5
      # Product_name              mean_rating median_rating share_5stars share_1star
      # <chr>                           <dbl>         <dbl>        <dbl>       <dbl>
      #   1 Knorr Taste the World Re…        2.03           1          0.111       0.583
      # 2 Knorr  Vollkorn Pasta Sn…        1.5            1.5        0           0.5  
      # 3 Knorr Sweety Milchreis m…        3.09           3          0.279       0.279
      # 4 Knorr Taste the World Pa…        2.96           3          0.284       0.270
      # 5 Knorr Asia Noodles Insta…        3.26           3          0.429       0.262


#mean rating snacks fertiggerichte category
mean(snacks_fertiggerichte$Rating, na.rm = T)
  #3.834212

#median rating snacks fertiggerichte category
median(snacks_fertiggerichte$Rating, na.rm = T)
  #4

#distribution rating snacks fertiggerichte category
ggplot(snacks_fertiggerichte, aes(x = Rating)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  labs(title = "Histogram of Ratings for Snacks & Fertiggerichte", x = "Rating", y = "Frequency")

#word cloud of snacks fertiggerichte category
snacks_fertiggerichte$text <- as.character(snacks_fertiggerichte$Text)
wordcloud(out$text, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#remove words that are repetive in each row from product_name 
snacks_fertiggerichte$Product_name_c <- gsub("Knorr", "", snacks_fertiggerichte$Product_name)
snacks_fertiggerichte$Product_name_c <- gsub("[0-9]", "", snacks_fertiggerichte$Product_name_c)
snacks_fertiggerichte$Product_name_c <- gsub("ml", "", snacks_fertiggerichte$Product_name_c)

#for each unique product name, calculate the mean rating
avg_rating_snacks_fertiggerichte <- aggregate(snacks_fertiggerichte$Rating, by = list(snacks_fertiggerichte$Product_name_c), FUN = mean)

#count how many unique in group.1
length(unique(avg_rating_snacks_fertiggerichte$Group.1))
  #61 products

#show all unique in group.1
unique(avg_rating_snacks_fertiggerichte$Group.1)

#recode x to Rating and Group.1 to Product_name
names(avg_rating_snacks_fertiggerichte)[names(avg_rating_snacks_fertiggerichte) == "x"] <- "Rating"
names(avg_rating_snacks_fertiggerichte)[names(avg_rating_snacks_fertiggerichte) == "Group.1"] <- "Product_name"

#merge with a count of these unique product_names
avg_rating_snacks_fertiggerichte <- merge(avg_rating_snacks_fertiggerichte, 
                                          aggregate(snacks_fertiggerichte$Product_name, by = list(snacks_fertiggerichte$Product_name_c), FUN = length), 
                                          by.x = "Product_name", by.y = "Group.1")

#recode x to occurences
names(avg_rating_snacks_fertiggerichte)[names(avg_rating_snacks_fertiggerichte) == "x"] <- "occurences"

#export avg_rating_snacks_fertiggerichte as csv
write.csv(avg_rating_snacks_fertiggerichte, "avg_rating_per_product_snacks_fertiggerichte.csv")

# products that have lower mean rating DESC limit to 10 & show product name
top_10_lowest_mean_rating <- avg_rating_snacks_fertiggerichte %>%
  arrange(desc(Rating)) %>%
  head(10)

  #     Product                                                      | Rating | Occurrences |
  #   ---|--------------------------------------------------------------|--------|-------------|
  #    1 | Vollkorn Pasta Snack Spinat & Käse                          | 1.5    | 2           |
  #    2 | Taste the World Reis Snack Chili con Carne Style            | 2.03   | 36          |
  #    3 | Taste the World Pasta Snack Mac & Cheese Jalapeño           | 2.96   | 74          |
  #    4 | Sweety Milchreis mit Vanillegeschmack                       | 3.09   | 43          |
  #    5 | Asia Noodles Instant Nudeln Huhn-Geschmack                  | 3.26   | 42          |
  #    6 | Asia Snack Tom Kha Gai Noodles                               | 3.35   | 34          |
  #    7 | Asia Noodles Instant Nudeln Curry-Geschmack                 | 3.37   | 41          |
  #    8 | Asia Snack Green Curry Noodles                               | 3.4    | 20          |
  #    9 | Asia Noodles Instant Nudeln Beef Taste                       | 3.48   | 190         |
  #    10| Asia Noodles Instant Nudeln Duck Taste                       | 3.48   | 190         |

#----------------------------------
# analyzing 5 star reviews - soups
#----------------------------------

#filter on GPT_estimated_cleaned == Suppen
soups <- subset(knorr_hellmans_cat_clean, GPT_estimated_cleaned == "Suppen")

#filter out product_names with less than 30 reviews
soups <- subset(soups, Product_name %in% names(sort(table(soups$Product_name), decreasing = TRUE)[1:30]))

#summarize soup ratings
summary(soups$Rating)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #1.000   4.000   5.000   4.267   5.000   5.000

#plot soup ratings distribution
ggplot(soups, aes(x = Rating)) +
  geom_histogram(binwidth = 1,
                 fill = "skyblue",
                 color = "black") +
  labs(title = "Histogram of Ratings for Soups", x = "Rating", y = "Frequency")

#word cloud of soups NEED FIX
soups$text <- as.character(soups$Text)
wordcloud(soups$text, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#-----------------------------------------------
# pre-processing for translation & text analysis
#-----------------------------------------------

processed <- textProcessor(knorr_hellmans_cat_clean$Text,
                           metadata = knorr_hellmans_cat_clean)
    # Building corpus... 
    # Converting to Lower Case... 
    # Removing punctuation... 
    # Removing stopwords... 
    # Removing numbers... 
    # Stemming... 
    # Creating Output..

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)

    # Removing 16447 of 29264 terms (16447 of 572339 tokens) due to frequency 
    # Removing 30 Documents with No Words 
    # Your corpus now has 13252 documents, 12817 terms and 555892 tokens.


