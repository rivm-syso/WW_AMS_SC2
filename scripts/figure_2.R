################################################################################
#
# Title: Figure 2
# Authors: Anne-Merel van der Drift & Auke Haver
# Department: NRS, CIb, RIVM
# Creation date: 2023-03-01
#
################################################################################

# This script was created to re-create plots used in the publication:Long-term 
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

# Generate arguments
option_list = list(
  # Collection directory
  make_option(
    c("-s", "--schiphol_data"), 
    default="https://zenodo.org/record/8298280/files/viral_load_data.tsv",
    help=".TSV file containing viral load data for Amsterdam Schiphol Airport",
  ),  # Collection directory
  make_option(
    c("-n", "--national_data"), 
    default="https://data.rivm.nl/data/covid-19/COVID-19_rioolwaterdata_landelijk.csv",#data/resources/COVID-19_rioolwaterdata_landelijk.csv",# https://data.rivm.nl/data/covid-19/COVID-19_rioolwaterdata_landelijk.csv",
    help=".CSV file containing viral load data for the Netherlands",
  ),
  # Output directory
  make_option(
    c("-o", "--output"), 
    default="results/", 
    help="output file",
  )
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

################################################################################
#
# Parameters
#
################################################################################

start_date  <- as.Date("2021-06-01")
center_date <- as.Date("2022-05-01")
end_date    <- as.Date("2022-09-04")

################################################################################
#
# Load data
#
################################################################################

df.vl_schiphol_raw <- read_tsv(opt$schiphol_data, show_col_types = FALSE) 

df.vl_national_raw <- read_delim(opt$national_data, delim = ";", show_col_types = FALSE) 

################################################################################
#
# Process
#
################################################################################

df.vl_schiphol_clean <-
  df.vl_schiphol_raw %>%
  reframe(
    viral_load = viral_load,
    sample_date = as.Date(sample_date),
    rolling_avg = rolling_average(
      dates = sample_date,
      values = viral_load,
      window = 7,
      direction = "bi"
    )
  ) %>%
  filter(
    between(
      x     = sample_date,
      left  = start_date, 
      right = end_date
    )
  )%>%
  mutate(
    type = ifelse(
      test = sample_date < center_date,
      yes  = "AMS_CD",
      no   = "AMS_E"
    ),
    viral_load = ifelse(
      test = sample_date < center_date,
      yes  = viral_load / (5*10^11),
      no   = viral_load / (10^12)
    ),
    rolling_avg=ifelse(
      test = sample_date < center_date,
      yes  = rolling_avg /(5*10^11),
      no   = rolling_avg/ (10^12)
    )
  )

df.vl_national_clean <- 
  df.vl_national_raw %>%
  reframe(
    sample_date = as.Date(Date_measurement),
    viral_load  = RNA_flow_per_100000 / (2.5*10^12),
    rolling_avg = rolling_average(
      dates = sample_date,
      values = viral_load,
      window = 7,
      direction = "rv"
    )
  ) %>%
  filter(
    between(
      x     = sample_date,
      left  = start_date, 
      right = end_date
    )
  )

################################################################################
#
# Plot
#
################################################################################



plt.national_vs_schiphol <-
  ggplot() +
  geom_area(
    data = df.vl_national_clean,
    mapping = aes(
      x = sample_date,
      y= viral_load,
    ),
    fill = "grey",
    alpha = .8
  )+
  geom_line(
    data = df.vl_schiphol_clean,
    mapping = aes(
      x = sample_date,
      y = rolling_avg,
      color = type
    )
  ) +
  geom_point(
    pch     = 21,
    color   = "black",
    data    = df.vl_schiphol_clean,
    mapping = aes(
      x = sample_date,
      y = viral_load,
      fill = type
    )
  ) +
  geom_vline(
    mapping = aes(
      xintercept = center_date
    ),
    linetype = "dashed"
  ) +
  # Scales
  ## colors
  scale_color_manual(
    name = "",
    values = c(
      "AMS_CD" = "#21918c",
      "AMS_E"  = "#440154"
    ),
    labels = c(
      "AMS_CD" = expression(paste("WWTP-AMS FC-VL period C+D (",5,"*",10^11, " particles)")),
      "AMS_E"  = expression(paste("WWTP-AMS FC-VL period E (", 10^12, " particles)"))
    )
  ) +
  scale_fill_manual(
    name = "",
    values = c(
      "AMS_CD" = "#21918c",
      "AMS_E"  = "#440154"
    ),
    labels = c(
      "AMS_CD" = expression(paste("WWTP-AMS FC-VL period C+D (",5,"*",10^11, " particles)")),
      "AMS_E"  = expression(paste("WWTP-AMS FC-VL period E (", 10^12, " particles)"))
    )
  ) +
  scale_x_date(
    date_labels = c("%b-%y"),
    date_breaks = "1 month",
    limits      = c(start_date, end_date),
    expand      = c(0,0),
    name        = "Date (Month-Year)"
  ) +
  scale_y_continuous(
    limits     = c(0,125),
    expand     = c(0,0),
    breaks     = c(0,20,40,60,80,100, 120),
    name       = "WWTP-AMS FC-VL (virus particles per 24h)",
    sec.axis = sec_axis(
      trans = ~.*.25,
      breaks = c(0,5,10,15,20,25,30),
      name  = expression(paste("National FIC-VL (",10^14," virus particles per ",10^5," i.e. per 24h)")),
    )
  ) +
  theme_bw() +
  # Theme
  theme(
    legend.position       = c(0.05,0.9),
    legend.justification  = "left",
    legend.background     = element_blank(),
    legend.box.background = element_blank(),
    axis.text.x           = element_text(angle = 45, hjust = 1),
    legend.text.align     = 0,
    axis.title.y.right    = element_text(hjust = .5)
  ) +
  # Make a custom legend for background fill
  annotation_custom(
    # Take the legend from a new plot
    grob = get_legend(
      ggplot(mapping = aes(x=1, y=1, fill = "National FIC-VL"))+
        geom_col()+
        scale_fill_manual(values = "grey", name = "") +
        theme(
          legend.background     = element_blank(),
          legend.box.background = element_blank(),)
    ),
    # Place it under the old legend
    xmin = as.Date("2021-07-15"),
    xmax = as.Date("2021-09-01"),
    ymin = 87,
    ymax = 107
  )

ggsave(
  plot     = plt.national_vs_schiphol,
  filename = file.path(opt$output, "figure_2.pdf"),
  width    = 8,
  height   = 5
)
