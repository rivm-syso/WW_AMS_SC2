################################################################################
#
# Title: Table 1
# Authors: Anne-Merel van der Drift & Auke Haver
# Department: NRS, CIb, RIVM
# Creation date: 2023-03-01
#
################################################################################

# This script was created to re-create tables used in the publication:Long-term 
# wastewater monitoring of SARS-CoV-2 viral loads and variants at the major 
# international passenger hub Amsterdam Schiphol Airport: a valuable addition to
# COVID-19 surveillance

################################################################################
#
# Setup Environment
#
################################################################################

# Load packages and functions
print("Loading packages and functions")
source("scripts/supplementary_functions.R")

################################################################################
#
# Parsing Arguments
#
################################################################################

option_list = list(
  # 
  make_option(
    c("-w", "--wastewater_variant_data"), 
    default="https://zenodo.org/record/11282406/files/wastewater_variant_frequencies.tsv", 
    help="",
  ),
  # 
  make_option(
    c("-c", "--clinical_variant_data"), 
    default="data/clinical_variant_frequencies.tsv", 
    help="",
  ),
  # 
  make_option(
    c("-o", "--output"), 
    default="results/table_1.csv", 
    help="",
  )
)

opt_parser = OptionParser(option_list=option_list); 

opt = parse_args(opt_parser)

################################################################################
#
# FUNCTIONS
#
################################################################################

return_formatted_ww_estimate <- function(a,b,c){
  return(
    paste0(
      format(round(a, 3), nsmall = 3),
      " (",
      format(round(b, 3), nsmall = 3),
      "-",
      format(round(c, 3), nsmall = 3),
      ")"
    )
  )
}

return_formatted_cl_estimate <- function(a,b,c){
  return(
    paste0(
      format(round(a, 3), nsmall = 3),
      " (",
      b,
      "/",
      c,
      ")"
    )
  )
}

################################################################################
#
# WW DATA
#
################################################################################

# load data
df.abundances_binned_filtered <- read_tsv(
  file = opt$wastewater_variant_data,
  show_col_types = FALSE
)

# Summarize initial observations
df.ww_bin_data <- 
  df.abundances_binned_filtered %>% 
  filter(abundance > 0, bin != "Other") %>% 
  group_by(bin) %>% 
  group_nest() %>% 
  mutate(data = lapply(data, function(x) filter(x, sample_date == min(sample_date)))) %>% 
  unnest(data) %>% 
  group_by(bin, sample_date, round(abundance, 8), round(confint_025, 8), round(confint_975, 8)) %>%
  reframe(
    init_det_ww = sample_date,
    det_lin     = paste0(lineage, collapse = "; "),
    abundance   = sum(abundance),
    confint_025 = sum(confint_025),
    confint_975 = sum(confint_975),
    .groups = "drop"
  ) %>%
  mutate(est_ww = return_formatted_ww_estimate(abundance, confint_025, confint_975)) %>% 
  select(bin, init_det_ww, est_ww, det_lin)

################################################################################
#
# CL DATA
#
################################################################################

# Load data
df.clinical_detections <- read_tsv(
  file = opt$clinical_variant_data,
  show_col_types = FALSE
)

# Summarize initial observations
df.cl_bin_data<-
  df.clinical_detections %>%
  filter(corrected_cases >0, bin != "Other") %>%
  group_by(bin) %>%
  group_nest() %>% 
  mutate(data = lapply(data, function(x) filter(x, week_floor == min(week_floor)))) %>% 
  unnest(data) %>%
  mutate(
    init_det_cl = week_floor,
    perc_cases = return_formatted_cl_estimate(week_bin_average, corrected_cases, sample_size),
  ) %>%
  select(bin, init_det_cl, perc_cases)

################################################################################
#
# Combine
#
################################################################################

df.joined_data <-
  full_join(
    df.cl_bin_data,
    df.ww_bin_data,
    by = "bin"
  ) %>% distinct()

################################################################################
#
# Output
#
################################################################################

write_csv(
  x = df.joined_data,
  file = opt$output,
)