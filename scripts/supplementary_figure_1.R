################################################################################
#
# Title: Figure 1
# Authors: Anne-Merel van der Drift & Auke Haver
# Department: NRS, CIb, RIVM
# Creation date: 2024-04-15
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
    default="2020-02-16", 
    help="Start date for plotting",
  ),
  make_option(
    c("-e", "--end"), 
    default="2022-09-04", 
    help="End date for plotting",
  ),
  make_option(
    c("-p", "--pandemic_daily_passenger_counts"), 
    default="https://zenodo.org/record/11282406/files/pandemic_daily_passenger_counts.tsv",
    help=".TSV file containing passenger data",
  ),
  make_option(
    c("-v", "--viral_load_data"), 
    default= "https://zenodo.org/record/11282406/files/viral_load_data.tsv",
    help=".TSV file containing viral load data",
  ),
  # Output directory
  make_option(
    c("-o", "--output"), 
    default="results/supplementary_figure_1", 
    help="output file",
  )
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

################################################################################
#
# Load Data
#
################################################################################

df.viral_load <- read_tsv(
  file = opt$viral_load_data,
  col_types = "cDddd"
)

df.passengers <- read_tsv(
  file = opt$pandemic_daily_passenger_counts,
  col_types = "cDd"
)

df.viral_load_passengers <-
  right_join(
    x = df.passengers %>%
      group_by(record_date) %>%
      reframe(passengers = sum(total)),
    y = df.viral_load,
    by = c("record_date" = "sample_date")
  ) 

correlation <- cor.test(
  x = df.viral_load_passengers$passengers,
  y = df.viral_load_passengers$flow,
  method = "pearson"
)

model = lm(formula = flow~passengers, data = df.viral_load_passengers) 

plt.flow_passengers <-
  ggplot() +
  geom_point(
    data    = df.viral_load_passengers,
    mapping = aes(x = passengers/10^4, y = flow/1000, fill = strftime(record_date, format = "%Y")),
    pch     = 21
  ) +
  geom_abline(
    intercept = model$coefficients[[1]]/1000,
    slope     = model$coefficients[[2]]*10,
    linetype  = "dashed"
  ) +
  geom_label(
    mapping = aes(x = 1, y = 5, label = ifelse(
      correlation$p.value >= 0.001,
      "p-value >= 0.001",
      "p-value < 0.001")
    ),
    hjust = 0
  ) +
  geom_label(
    mapping = aes(x = 1, y = 4.5, label = paste0("r = ", round(correlation$estimate, 3))),
    hjust = 0
  ) +
  scale_fill_viridis_d( name = "Year", begin = .3) +
  scale_y_continuous(limits = c(0, 6), expand = c(0,0), name =bquote('Flow'~ (10^3~m^3)))+
  scale_x_continuous(limits = c(0, 12), expand = c(0,0), breaks = seq(0,12, 2), name =bquote('Arrivals'~ (10^4))) +
  theme_bw() +
  theme(
    legend.position = c(.80,.20),
    legend.box.background = element_rect(fill = NA)
  )

ggsave(
  plot     = plt.flow_passengers,
  filename = paste0(opt$output, ".png"),
  width    = 5,
  height   = 5
)
ggsave(
  plot     = plt.flow_passengers,
  filename = paste0(opt$output, ".pdf"),
  width    = 5,
  height   = 5
)

