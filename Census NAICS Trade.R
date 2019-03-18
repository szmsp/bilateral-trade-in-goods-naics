###############################################################################################################################

# README
# Project:    Goods trade by three-digit NAICS code between the US and other countries
# Objective:  From the Census API, download two years of the US Census Bureau bilateral trade by three-digit NAICS codes

# US Census API Setup
# Census API Key request here:          http://api.census.gov/data/key_signup.html

# In the API call: months must be specified as "\d\d" ("01", "02")
# The as.yearqtr function converts the underlying date value to the first day-month of the quarter 

# For help on Census international trade data API
# https://www.census.gov/foreign-trade/reference/guides/Guide%20to%20International%20Trade%20Datasets.pdf

# To run, replace "[key]" and "[path]" below

###############################################################################################################################

# Run before every program to clear your workspace
rm(list=ls())
cat("\014") 

# Root paths
folder <- "[path]"
analysis <- paste(folder, "Analysis\\", sep = "")

# Set dates to download
# First date
first_year <- "2013"
first_month <- "01"

# Final date
final_year  <- "2018"
final_month <- "12"

### PATHS AND PARAMETERS ###

# Census key, obtained from Census website above
census_key <- "[key]"

# Set analysis parameters and vars you want to grab. Currently set to Germany
# Parameters
ctry_code   <- 4280    
# Vars
import_vars <- paste("CTY_CODE","CTY_NAME","NAICS","NAICS_LDESC","GEN_VAL_MO","GEN_VAL_YR", sep=",")
export_vars <- paste("CTY_CODE","CTY_NAME","NAICS","NAICS_LDESC","ALL_VAL_MO","ALL_VAL_YR", sep=",")

###############################################################################################################################

### PULL DATA FROM CENSUS API###

# Set location
Sys.setlocale("LC_ALL","C")

# Set working directory
setwd(folder)

# Load libraries
library("tidyverse")  
library("RJSONIO")    
library("sqldf")      
library("ggthemes")   
library("zoo")        
library("dplyr")      
library("tools")
library("lubridate") 
library("reshape2")
library("formattable")

#### PULL DATA ####

# Function to extract imports data

getImports <- function(census_key, year_month, import_vars, ctry_code) {
  
  imp_resURL <- paste("https://api.census.gov/data/timeseries/intltrade/imports/",
                      "naics?get=",import_vars,"&COMM_LVL=","&COMM_LVL=NA3","&time=",year_month, 
                      "&CTY_CODE=",ctry_code,"&key=",census_key,sep="")
  
  imp_lJSON <- fromJSON(imp_resURL) # convert JSON content to R objects
  
  imp_lJSON <- imp_lJSON[2:length(imp_lJSON)]         # keep everything but the 1st element (var names) in lJSON
  imp_lJSON.cc <- sapply(imp_lJSON,function(x) x[1])  # extract country code
  imp_lJSON.cn <- sapply(imp_lJSON,function(x) x[2])  # extract country name
  imp_lJSON.nc <- sapply(imp_lJSON,function(x) x[3])  # extract three-digit NAICS code
  imp_lJSON.nd <- sapply(imp_lJSON,function(x) x[4])  # extract NAICS description
  imp_lJSON.avm <- sapply(imp_lJSON,function(x) x[5]) # extract all value month
  imp_lJSON.avy <- sapply(imp_lJSON,function(x) x[6]) # extract all value year
  imp_lJSON.t <- sapply(imp_lJSON,function(x) x[8])   # extract time
  
  imp_df <- data.frame(as.Date(paste(imp_lJSON.t,"-02", sep=""), format="%Y-%m-%d"), imp_lJSON.cc, 
                       as.character(imp_lJSON.cn), imp_lJSON.nc, imp_lJSON.nd, 
                       as.numeric(imp_lJSON.avm),as.numeric(imp_lJSON.avy))
  # put in dataframe
  names(imp_df) <- c("year_month", "country_code", "country_name", "naics_code", "naics_desc",
                     "monthly_import_value", "ytd_import_value")
  
  # name the vars in the data frame
  return(imp_df)
}

# API calls split by year for efficiency

date_list <- seq(as.Date(paste(first_year, "/", first_month, "/1", sep = "")), as.Date(paste(final_year, "/", final_month, "/1", sep = "")), "years")
year_list <- year(date_list)

# Call imports data

for (y in year_list){
  
  if((y == first_year)){
    month_imports <- getImports(census_key, year_month = (paste("from+", y,"-01+to+", y,"-12", sep = "")), import_vars, ctry_code)
  }
  
  if(exists("month_imports") && (y != first_year) && (y != final_year)){
    temp_data <- getImports(census_key, year_month = (paste("from+", y,"-01+to+", y,"-12", sep = "")), import_vars, ctry_code)
    month_imports <- rbind(month_imports, temp_data)
    rm(temp_data)
  }
  
  if(exists("month_imports") && (y == final_year)){
    temp_data <- getImports(census_key, year_month = (paste("from+", y,"-01+to+", y,"-",final_month, sep = "")), import_vars, ctry_code)
    month_imports <- rbind(month_imports, temp_data)
    rm(temp_data)
  }
  
}

View(month_imports)
check_imports <- sqldf("select year_month, naics_code, count(country_code) as naics_count from month_imports group by 1, 2")
View(check_imports)
# Check for one observation per three-digit NAICS code per month
stopifnot(check_imports$naics_count == 1)

month_imports <- month_imports[order(month_imports$naics_desc, month_imports$year_month),]

# Function to extract exports data

getExports <- function(census_key, year_month, export_vars, ctry_code) {
  
  exp_resURL <- paste("https://api.census.gov/data/timeseries/intltrade/exports/",
                      "naics?get=",export_vars,"&COMM_LVL=","&COMM_LVL=NA3","&time=",year_month,
                      "&CTY_CODE=",ctry_code,"&key=",census_key,sep="")
  
  exp_lJSON <- fromJSON(exp_resURL) # convert JSON content to R objects
  
  exp_lJSON <- exp_lJSON[2:length(exp_lJSON)]         # keep everything but the 1st element (var names) in lJSON
  exp_lJSON.cc <- sapply(exp_lJSON,function(x) x[1])  # extract country code
  exp_lJSON.cn <- sapply(exp_lJSON,function(x) x[2])  # extract country name
  exp_lJSON.nc <- sapply(exp_lJSON,function(x) x[3])  # extract three-digit NAICS code
  exp_lJSON.nd <- sapply(exp_lJSON,function(x) x[4])  # extract NAICS description
  exp_lJSON.avm <- sapply(exp_lJSON,function(x) x[5]) # extract all value month
  exp_lJSON.avy <- sapply(exp_lJSON,function(x) x[6]) # extract all value year
  exp_lJSON.t <- sapply(exp_lJSON,function(x) x[8])   # extract time
  
  exp_df <- data.frame(as.Date(paste(exp_lJSON.t,"-02", sep=""), format="%Y-%m-%d"), exp_lJSON.cc, 
                       as.character(exp_lJSON.cn), exp_lJSON.nc, exp_lJSON.nd, as.numeric(exp_lJSON.avm),
                       as.numeric(exp_lJSON.avy))
  # put in dataframe
  names(exp_df) <- c("year_month", "country_code", "country_name", "naics_code", "naics_desc",
                     "monthly_export_value", "ytd_export_value") 
  
  # name the vars in the data frame
  return(exp_df)
}

# Call exports data

for (y in year_list){
  
  if(!exists("month_exports") && (y == first_year)){
    month_exports <- getExports(census_key, year_month = (paste("from+", y,"-01+to+", y,"-12", sep = "")), export_vars, ctry_code)
  }
  
  if(exists("month_exports") && (y != first_year) && (y != final_year)){
    temp_data <- getExports(census_key, year_month = (paste("from+", y,"-01+to+", y,"-12", sep = "")), export_vars, ctry_code)
    month_exports <- rbind(month_exports, temp_data)
    rm(temp_data)
  }
  
  if(exists("month_exports") && (y == final_year)){
    temp_data <- getExports(census_key, year_month = (paste("from+", y,"-01+to+", y,"-",final_month, sep = "")), export_vars, ctry_code)
    month_exports <- rbind(month_exports, temp_data)
    rm(temp_data)
  }
  
}

View(month_exports)
check_exports <- sqldf("select year_month, naics_code, count(country_code) as naics_count from month_exports group by 1, 2")
View(check_exports)
# Check for one observation per three-digit NAICS code per month
stopifnot(check_exports$naics_count == 1)

month_exports <- month_exports[order(month_exports$naics_desc, month_exports$year_month),]

#### CREATE LAGGED VALUES ####

# Merge import and export dataset to single dataset to create single set
trade_data <- merge(month_imports, month_exports, by = c("year_month", "country_code", "country_name", "naics_code", "naics_desc"), all = TRUE)

# Proper case NAICS fields
trade_data$naics_desc <- tolower(trade_data$naics_desc)
trade_data$naics_desc <- toTitleCase(trade_data$naics_desc)

# Create quarterly date-time variable in trade data
trade_data$yq <- as.yearqtr(trade_data$year_month, format = "%Y-%m-%d")
format(trade_data$yq, format = "%y 0%q")
View(trade_data)

# Create new dataset to sum trade by quarter-NAICS
quarterly_data <- sqldf("select country_name, yq, naics_code, naics_desc, sum(monthly_import_value) as yq_import_value, sum(monthly_export_value) as yq_export_value from trade_data group by 1, 2, 3, 4")

# YTD lagged values

ytd_data <- sqldf("select naics_code, naics_desc, year_month, ytd_import_value, ytd_export_value from trade_data group by 1, 2, 3")

ytd_data <- 
  ytd_data %>%
  group_by(naics_desc) %>%
  mutate(lag.ytd_import_value = dplyr::lag(ytd_import_value, n = 12, order_by = naics_desc, default = NA))

ytd_data <- 
  ytd_data %>%
  group_by(naics_desc) %>%
  mutate(lag.ytd_export_value = dplyr::lag(ytd_export_value, n = 12, order_by = naics_desc, default = NA))

# Quarterly lagged values

quarterly_data <- sqldf("select country_name, naics_code, naics_desc, yq, yq_import_value, yq_export_value from quarterly_data group by 1, 2, 3, 4")

quarterly_data <- 
  quarterly_data %>%
  group_by(naics_desc) %>%
  mutate(lag.yq_import_value = dplyr::lag(yq_import_value, n = 4, order_by = naics_desc, default = NA))

quarterly_data <- 
  quarterly_data %>%
  group_by(naics_desc) %>%
  mutate(lag.yq_export_value = dplyr::lag(yq_export_value, n = 4, order_by = naics_desc, default = NA))

#### FIND YEAR-ON-YEAR CHANGE ####

pct_change <- function(new, old) {(new - old)/old}

quarterly_data$change_m_yoy <- pct_change(quarterly_data$yq_import_value, quarterly_data$lag.yq_import_value)
quarterly_data$change_x_yoy <- pct_change(quarterly_data$yq_export_value, quarterly_data$lag.yq_export_value)

ytd_data$change_m_yoy <- pct_change(ytd_data$ytd_import_value, ytd_data$lag.ytd_import_value)
ytd_data$change_x_yoy <- pct_change(ytd_data$ytd_export_value, ytd_data$lag.ytd_export_value)

#### VIEW CURRENT QUARTER AND YTD SUBSETS ####

current_quarter <- as.yearqtr(paste(final_year, "-", final_month, "-02", sep=""), format = "%Y-%m-%d")
format(current_quarter, format = "20%y Q%q")
current_month <- as.Date(paste(final_year, "-", final_month, "-02", sep=""), format = "%Y-%m-%d")

quarter_subset <- subset(quarterly_data, yq == current_quarter)
ytd_subset <- subset(ytd_data, year_month == current_month)

#### CHART TRENDS BY INDUSTRY ####

# Set theme
theme_set(theme_bw())

gross_charts <- function(df, output_loc){
  
  # Create list of industries
  naics_list <- unique(df$naics_desc)
  
  # Produce by-industry plots
  for(i in seq_along(naics_list)){
    
    # Create plots
    plot <- 
      ggplot(subset(df, naics_desc == naics_list[i]), aes(x = year_month, y = gross_value, color = trade_type)) +
      geom_line() +
      xlab("Period") +
      scale_y_continuous("Value ($)", labels = scales::dollar) +
      ggtitle(paste("U.S.-Germany Trade in ", naics_list[i], " Products, 1/2013 to 12/2018", sep = "")) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      theme(legend.spacing.x = unit(0.25, 'cm')) +
      labs(caption = paste("Source: U.S. Census, U.S. Trade in Goods with Germany.", sep = ""))
    
    # Save plots
    ggsave(filename = paste(naics_list[i], " Gross Chart.pdf", sep = ""), plot, path = output_loc)
    
    # Print to screen
    print(plot)
    
  }
}

# Monthly Gross Charts

monthly_gross_trends <- trade_data[, c(1, 3, 5:6, 8)]
monthly_gross_trends$ex_less_im <- monthly_gross_trends$monthly_export_value - monthly_gross_trends$monthly_import_value
names(monthly_gross_trends) <- c("year_month", "country_name", "naics_desc", "U.S. Monthly Imports", "U.S. Monthly Exports", "U.S. Monthly Exports Less Imports")
monthly_gross_trends$year_month <- as.Date(monthly_gross_trends$year_month)
monthly_gross_trends2 <- melt(monthly_gross_trends, id.vars = c("year_month", "country_name", "naics_desc"), variable.name = "trade_type", value.name = "gross_value")

gross_charts(df = monthly_gross_trends2, output_loc = paste(analysis, "Monthly Charts\\", sep = ""))


