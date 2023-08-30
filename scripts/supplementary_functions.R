################################################################################
#
# Packages
#
################################################################################

suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("tidyr"))
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("yaml"))
suppressPackageStartupMessages(library("jsonlite"))
suppressPackageStartupMessages(library("cowplot"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("ggpubr"))

################################################################################
#
# Function: rolling average
#
################################################################################

#' Calculate a rolling average over a window of x days
#' 
#' @param dates
#' @param values
#' @param window
#' @param direction
#' @return directory list
#' @example
#' rolling_average(
#'   dates = c("2022-01-01","2022-01-02","2022-01-11","2022-01-12", "2022-02-01", "2022-03-02"),
#'   values = c(1, 3, 3, 3, 3, 5),
#'   window = 10,
#'   direction = "rv"
#' )
rolling_average <- function(dates, values, window, direction){
  if(!direction %in% c('fw', 'rv', 'bi')) stop('Input should be either fw (forward), rv (reverse) or bi (bi-directional)')
  
  output_list <- vector(mode = "numeric")
  for(date in dates){
    
    n_days = ifelse(
      test = direction == "bi",
      yes  = (window-1)/2,
      no   = window-1
    )
    
    lower_date = ifelse(
      test = direction != "fw", 
      yes  = date - n_days,
      no   = date
    )
    
    upper_date = ifelse(
      test = direction != "rv", 
      yes  = date + n_days,
      no   = date
    )
    
    output_list <- c(output_list, mean(values[dates >= lower_date & dates <= upper_date]))
    
  }
  return(output_list)
}

return_period <- function(date, period_dataframe){
  for(period in period_dataframe$period){
    if(between(
      x     = date, 
      left  = period_dataframe$start[period_dataframe$period == period],
      right = period_dataframe$end[period_dataframe$period == period]
    )
    ){
      return(as.character(period))
    }
  }
  return("none")
}

#' Return Alias
#'
#' Function to return pango alias from a prefix
#' Alias list can be downloaded from "https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json"
return_alias <- function(prefix, alias_list){
  if(is.na(prefix) | startsWith(prefix, "X") | prefix == "B" | prefix == "A" | prefix == "not_reported" | prefix == "Recombinants"){
    return(prefix)
  } else{
    return(alias_list[[prefix]])
  }
}

#' Return lineage of interest
#'
#' Return the name of a named list, where the value is part of the user-supplied string
#' @param string A string containing a full Pango lineage
#' @param lineage_list A named list, where names contain binning values and values are the Pango lineage prefixes
#' @return Binning value, 'Other' by default
return_lineage_of_interest <- function(string, lineage_list){
  # Skip NA values
  if(is.na(string)){
    return(NA)
  }
  if(string %in% c("not_reported", "Recombinants")){
    return(NA)
  }
  # Set default for non-NA values
  out <- "Other"
  # Loop over list
  
  for(i in seq(1:length(lineage_list))){
    if(str_detect(string, fixed(names(lineage_list)[i]))){
      out <- lineage_list[i]
    }
  }
  return(out)
}