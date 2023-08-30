################################################################################
#
# Title: Figure 3
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
    c("-s", "--start"), 
    default="2021-06-01", 
    help="Start date for plotting",
  ),
  make_option(
    c("-e", "--end"), 
    default="2022-09-04", 
    help="End date for plotting",
  ),
  make_option(
    c("-a", "--admissions"),
    help=".csv file containing weekly hospital admissions worldwide",
  ),
  make_option(
    c("-v", "--viral_load"), 
    default="https://zenodo.org/record/8298280/files/viral_load_data.tsv",
    help=".tsv file containing viral load measurments",
  ),
  # Output directory
  make_option(
    c("-o", "--output"), 
    default="results/figure_3.pdf", 
    help="output file",
  )
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

################################################################################
#
# Load Data
#
################################################################################

if(!"admissions" %in% names(opt)){
  print("No hospital admission file found. If you want to include this in figure 3, please download it from https://ourworldindata.org/grapher/weekly-hospital-admissions-covid and supply it as an argument to this script.")
  print("Continuing without hospital admissions")
  df.worlddata_weekly_hosp <- tibble(
    Entity = c(
      "France",
      "Italy",
      "Spain",
      "United Kingdom",
      "United States",
      "Netherlands"
    ),
    Code = c( "FRA","ITA", "ESP","GBR","USA","NLD"),
    Day = as.Date("2022-02-06"),
    `Weekly new hospital admissions per million` = 10
    
    
  )
} else {
  df.worlddata_weekly_hosp <- read_csv(opt$admissions, show_col_types = FALSE)
}


df.viral_load <- read_tsv(opt$viral_load, show_col_types = FALSE) %>%
  # Add rolling average
  mutate(
    rolling_avg = rolling_average(
      dates     = sample_date,
      values    = viral_load,
      window    = 7,
      direction = "bi"
    ),
    corrected_avg = ifelse(
      test = sample_date < as.Date("2022-05-01"),
      yes  = rolling_avg* 2,
      no   = rolling_avg 
    ),
    correction_label = ifelse(
      test = sample_date < as.Date("2022-05-01"),
      yes  = "period_AD",
      no   = "period_E"
    )
  ) %>%
  # Select appropriate date
  filter(
    between(
      x     = sample_date,
      left  = as.Date(opt$start),
      right = as.Date(opt$end)
    )
  ) 

################################################################################
#
# Process Data
#
################################################################################

df.worlddata_weekly_hosp_tidy <-
  df.worlddata_weekly_hosp %>%
  filter(
    between(Day, as.Date(opt$start), as.Date(opt$end)),
    Code %in% c("NLD", "USA", "FRA", "GBR", "ITA", "ESP")
  ) %>%
  mutate(
    Entity = factor(
      x      = Entity,
      levels = c(
        "France",
        "Italy",
        "Spain",
        "United Kingdom",
        "United States",
        "Netherlands"
      )
    )
  )

################################################################################
#
# Plot Data
#
################################################################################

df.labels <- 
  data.frame(
    x_start = c(as.Date("2022-04-20"),  as.Date("2022-05-10")),
    y_start = 250
  ) %>%
  mutate(
    labels = ifelse(
      test = x_start == as.Date("2022-05-10"),
      yes  = "~~~~~~10^12~Particles~per~24~h",
      no   = "5%*%10^11~Particles~per~24~h"
    )
  )


plt.worlddata.weekly.hosp <- 
  ggplot() +
  geom_area(
    data = df.viral_load,
    mapping = aes(
      x = sample_date,
      y = corrected_avg / (2.5*10^11),
      fill = "area_fill"
    ),
    alpha = .8,
  ) +  geom_line(
    data    = df.worlddata_weekly_hosp_tidy,
    mapping = aes(
      colour = Entity,
      x      = Day,
      y      = `Weekly new hospital admissions per million`,
      linetype = Entity
    ),
    linewidth = 1.1
  )+
  geom_vline(
    mapping  = aes(xintercept = as.Date("2022-05-01")),
    linetype = "dashed"
  ) +
  geom_text(
    data    = df.labels,
    mapping = aes(
      x = x_start,
      y = y_start,
      angle = 90,
      label = labels
    ),
    hjust = 0,
    parse = "T"
  ) +
  # Scales
  scale_fill_manual(
    values = "grey",
    labels = "WWTP-AMS FC-VL"
  )+
  ## Color
  scale_colour_manual(
    values = c(
      "France"         = "#fde725",
      "Italy"          = "#5ec962",
      "Spain"          = "#21918c",
      "United Kingdom" = "#3b528b",
      "United States"  = "#440154",
      "Netherlands"    = "black"
    )
  ) +
  ## Linetype
  scale_linetype_manual(
    values = c(
      "France"         = 1,
      #"Greece"         = 1,
      "Italy"          = 1,
      "Spain"          = 1,
      "United Kingdom" = 1,
      "United States"  = 1,
      "Netherlands"    = 1
    )
  )+
  ## Axis
  scale_x_date(
    name        = "Date (Month-Year)",
    date_breaks = "2 months", 
    date_labels = "%b-%y", 
    expand = c(0,0), 
    limits = c(as.Date(opt$start), as.Date(opt$end))
  ) +
  scale_y_continuous(
    # name   = "Weekly Admissions per million inhabitants",
    name   = expression(paste("Weekly Admissions per ",10^6," inhabitants")),
    limits = c(0,480),
    expand = c(0,0),
    breaks = c(0, 120, 240, 360, 480),
    sec.axis = sec_axis(
      trans = ~./4,
      name = "WWTP-AMS FC-VL (virus particles per 24h)",
      breaks = c(0, 30, 60, 90, 120)
    )
  ) +
  # Themes
  theme_bw() +
  theme( 
    axis.text.x = element_text(),
    axis.text.y  = element_text(color = 'black'),
    axis.title.y = element_text(color='black'),
    legend.title = element_blank(),
    strip.background = element_rect(color = "black", fill = "white"),
    legend.spacing.y = unit(0,'cm'),
    panel.spacing.x = unit(0, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    legend.position = c(.11,.77),
    legend.background = element_blank()
  )

ggsave(
  opt$output, 
  plt.worlddata.weekly.hosp,
  width    = 8,
  height   = 5
)