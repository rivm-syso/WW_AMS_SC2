################################################################################
#
# Title: Tidy NGS results
# Authors: Auke Haver
# Department: NRS, CiB, RIVM
# Creation date: 2023-01-19
#
################################################################################

# Load environment
source("scripts/supplementary_functions.R")

################################################################################
#
# Parse Arguments
#
################################################################################

option_list = list(
  # 
  make_option(
    c("-a", "--alias"), 
    default="https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json",
    help="",
  ),
  # 
  make_option(
    c("-b", "--binning"), 
    default="parameters/VOI_VOC_bins.yaml", 
    help="",
  ),
  # 
  make_option(
    c("-k", "--kiemsurveillance"), 
    default="https://data.rivm.nl/covid-19/COVID-19_varianten.csv", 
    help="",
  ),
  # 
  make_option(
    c("-o", "--output"), 
    default="data/clinical_variant_frequencies.tsv", 
    help="",
  )
)

opt_parser = OptionParser(option_list=option_list); 

opt = parse_args(opt_parser)


################################################################################
#
# DATA
#
################################################################################

ls.lineage_aliases <- read_json(opt$alias)
ls.lineage_bins <- read_yaml(opt$binning)

print("Processing clinical data...")
# Load data
df.kiemsurveillance_raw <- read_delim(file = opt$kiemsurveillance, show_col_types = FALSE, delim = ";")

# KIEMSURVEILLANCE DATA
# The kiemsurveillance data has an untidy format due to the phylogeny rules.
# Some case values occur more than once in the table. This can be confusing.
# Instead, we will only use paraphyletic and monophyletic clades.
# Furthermore, counts will occur only once in the table.
df.kiemsurveillance_tidy <- df.kiemsurveillance_raw %>% 
  group_by(Date_of_statistics_week_start) %>%  
  mutate(Is_subvariant_of = replace_na(Is_subvariant_of, "B.1")) %>%
  filter(!str_detect(Variant_code, "\\+"), !str_detect(Is_subvariant_of, "\\+")) %>%
  mutate(
    # Deduct all lower branch variant from higher branch variants
    corrected_cases = sapply(X = Variant_code, FUN = function(x) Variant_cases[Variant_code == x] - sum(replace_na(Variant_cases[Is_subvariant_of == x],0))),
    # Remove +mutation notation
    lineage = gsub("\\+.*", "", Variant_code)
  ) %>%
  rename(
    "week_floor" = "Date_of_statistics_week_start",
    "sample_size" = "Sample_size"
  ) %>%
  select(week_floor, sample_size, lineage, corrected_cases) %>%
  # Calculate not-reported cases
  group_by(week_floor) %>%
  mutate(not_reported = unique(sample_size) - sum(corrected_cases)) %>%
  pivot_wider(names_from = lineage, values_from = corrected_cases, values_fill = 0) %>%
  pivot_longer(cols = !c(week_floor, sample_size), names_to = "lineage", values_to = "corrected_cases") %>%
  ungroup() %>%
  # Add lineage alias
  mutate(
    # Convert to full branch traceback
    lineage_alias = sapply(X = lineage, FUN = function(x) ifelse(
      test = x %in% c("not_reported", "Recombinants"),
      yes  = x,
      no   = gsub(
        pattern = "[A-Z]+",
        replacement = return_alias(
          gsub(
            pattern = "\\..*",
            replacement = "", 
            x = x
          ),
          ls.lineage_aliases),
        x))),
    bin = replace_na(unlist(lapply(lineage_alias, function(x) return_lineage_of_interest(x, ls.lineage_bins))), "Other"),
    bin = ifelse(
      test = bin %in% c("Omicron - B.1.1.529", "Omicron - BA.1"),
      yes  = "Omicron - BA.1/B.1.1.529",
      no   = bin
    )
  ) %>%
  # Reorder columns
  select(week_floor, sample_size, lineage, lineage_alias, bin, corrected_cases) %>% 
  group_by(week_floor, bin) %>%
  mutate(week_bin_average = sum(corrected_cases) / unique(sample_size)) %>%
  ungroup()

# Write to file
write_tsv(df.kiemsurveillance_tidy , file.path(opt$output))

print("Finished! Exiting...")



