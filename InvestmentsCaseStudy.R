
library(dplyr)
library(tidyr)
library(stringr)

# Loading and Cleaning Data
companies <- read.delim("companies.txt", header = TRUE, stringsAsFactors = FALSE)
str(companies)
names(companies)

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
str(rounds2)
names(rounds2)

# Converting the unique ID of company permalink in both data frames to lower case as R is case sensitive
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

#renaming the permalink column to company_permalink in companies data frame to enable simple merging
names(companies)[1] <- "company_permalink"
names(companies)

#Checkpoint 1: Data Cleaning 1
## Table 1.1 - Understanding the dataset
### 1. How many unique companies are present in the companies file?
count(distinct(companies, company_permalink))

### 2. How many unique companies are present in the rounds file?
count(distinct(rounds2, company_permalink))

### 4. Are there any companies in the rounds2 file which are not present in companies ? 
sum(!is.element(rounds2$company_permalink, companies$company_permalink))

### 5. Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
###    Name the merged frame master_frame.
###   How many observations are present in master_frame 
master_frame <- merge(rounds2, companies, by = "company_permalink")
names(master_frame)

#Checkpoint 2: Funding Type Analysis
## Table 2.1 - Average Values of Investments for Each of Funding Types

### Using "group_by" function for Grouping the master_frame based in funding type
funding_type <- group_by(master_frame, funding_round_type)

### Using "summarise" function for calculating the fund wise average fund invested
funding_type_average <- summarise(funding_type, avg_fund = mean(raised_amount_usd, na.rm = TRUE))

### Using "arrange" function to sort the fund wise investments in decreasing order
funding_type_average <- arrange(funding_type_average, desc(avg_fund))

### Using filter function to filter only venture, angel, seed and private equity fund type
funding_type_average <- filter(funding_type_average, funding_round_type %in% c("venture", "angel", "seed", "private_equity"))

### Using filter funtion to which investment is suitable by applying condition of avg fund >= 5000000 and <= 15000000
filter(funding_type_average, avg_fund >= 5000000 & avg_fund <= 15000000)

# Checkpoint 3: Country Analysis
# It is mentioned that Spark Industries want to invest in only English speaking countries (list given as link in assingment)
# As per the list downloaded the short code of english speaking countries from https://wits.worldbank.org/wits/wits/witshelp/Content/Codes/Country_Codes.htm
# Prepared a new CSV file called "countries.csv" with two columns country and country_code and loaded to data frame "eng_countries"
eng_countries <- read.csv("countries.csv", stringsAsFactors = FALSE)
str(eng_countries)

# Created a new master frame by filtering master_frame with only english speaking countries and funding type "Venture"
master_frame <- filter(master_frame, tolower(funding_round_type) == "venture", tolower(country_code) %in% tolower(eng_countries$Country_Code))
## Table 3.1 - Analysing the Top 3 English-Speaking Countries

### Using "group_by" function for Grouping the master_frame1 based on english speaking country code
country_grouping <- group_by(master_frame, country_code)

### Using "summarise" function for calculating the country wise total fund invested
country_grouping_sum <- summarise(country_grouping, total_fund = sum(raised_amount_usd, na.rm = TRUE))

### Using "arrange" function to sort the country wise investments in 
top9 <- head(arrange(country_grouping_sum, desc(total_fund)),9)
top9

# Checkpoint 4: Sector Analysis 1
## Loading "mapping.csv" and converting the same from wide to long format using "gather" function
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
names(mapping)
mapping <- gather(mapping, main_sector, val, 2:10)
mapping <- mapping[!(mapping$val==0),]
mapping <- mapping[,-3]

## Correcting the category_list of mapping as "na" is replaced with "0"
mapping$category_list <- gsub("0", "na", mapping$category_list)
mapping$category_list <- gsub("2.na", "2.0", mapping$category_list)

## As the category_list contain values separated by "|", creating primary sector by using "separate" function
master_frame <- separate(master_frame, category_list, into=c("primary_sector", "sec_sector"), sep="\\|")
master_frame <- select(master_frame, -sec_sector)

## Renaming "category_list" to "primary_sector" in mapping dataframe for easy merger
names(mapping)[1] <- "primary_sector"

## Merged data frame with each primary sector mapped to its main sector 
master_frame <- merge(master_frame, mapping, by = "primary_sector")

# Checkpoint 5: Sector Analysis 2
## Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range.
## To acheive this, filtered the master_frame by selecting top 3 countries from top9 data frame and applied additional constraint that raised_amount_usd is between 5 million and 15 million.
D1 <- filter(master_frame, country_code %in% top9[1,1], raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)
D2 <- filter(master_frame, country_code %in% top9[2,1], raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)
D3 <- filter(master_frame, country_code %in% top9[3,1], raised_amount_usd>=5000000 & raised_amount_usd <= 15000000)

## The total number (or count) of investments for each main sector in a separate column
## The total amount invested in each main sector in a separate column
## To acheive this grouping the three data frames by "main_sector" and summarizing the same based on count of investment and total sum of investment i.e., funds raised
## Arranged the data in descending order of count of investments and total investments
D1_inv <- group_by(D1, main_sector)
D1_inv <- summarise(D1_inv, countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd))
D1_inv <- arrange(D1_inv, desc(countof_inv), desc(total_inv))
## or
D1_inv <- arrange(summarise(group_by(D1, main_sector), countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd)), desc(countof_inv), desc(total_inv))

D2_inv <- group_by(D2, main_sector)
D2_inv <- summarise(D2_inv, countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd))
D2_inv <- arrange(D2_inv, desc(countof_inv), desc(total_inv))
## or
D2_inv <- arrange(summarise(group_by(D2, main_sector), countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd)), desc(countof_inv), desc(total_inv))

D3_inv <- group_by(D3, main_sector)
D3_inv <- summarise(D3_inv, countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd))
D3_inv <- arrange(D3_inv, desc(countof_inv), desc(total_inv))
## or 
D3_inv <- arrange(summarise(group_by(D3, main_sector), countof_inv = sum(!is.na(main_sector)), total_inv = sum(raised_amount_usd)), desc(countof_inv), desc(total_inv))

### Total number of Investments (count) in C1, C2, C3
sum(D1_inv$countof_inv) # or nrow(D1)
sum(D2_inv$countof_inv) # or nrow(D2)
sum(D3_inv$countof_inv) # or nrow(D3)

### Total amount of investment (USD)
sum(D1_inv$total_inv) # or sum(D1$raised_amount_usd)
sum(D2_inv$total_inv) # or sum(D2$raised_amount_usd)
sum(D3_inv$total_inv) # or sum(D3$raised_amount_usd)

### Top Sector name (no. of investment-wise)
### Second Sector name (no. of investment-wise)
### Third Sector name (no. of investment-wise)
head(D1_inv, 3)
head(D2_inv, 3)
head(D3_inv, 3)

### For point 3 (top sector count-wise), which company received the highest investment?
### Filtered D1, D2 and D3 based on main sector which is highest invested, 
### then grouped the same based on  company name and then arranged in decreasing order sum of raised amount
arrange(summarise(group_by(filter(D1, main_sector %in% D1_inv[1,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))
arrange(summarise(group_by(filter(D2, main_sector %in% D2_inv[1,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))
arrange(summarise(group_by(filter(D3, main_sector %in% D3_inv[1,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))

### For point 4 (second best sector count-wise), which company received the highest investment? solution similar to above point
arrange(summarise(group_by(filter(D1, main_sector %in% D1_inv[2,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))
arrange(summarise(group_by(filter(D2, main_sector %in% D2_inv[2,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))
arrange(summarise(group_by(filter(D3, main_sector %in% D3_inv[2,1]), tolower(name)), fund = sum(raised_amount_usd)), desc(fund))
