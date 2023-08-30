################################################################################
#
# Title: Figure 1
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
    default="https://zenodo.org/record/8298280/files/pandemic_daily_passenger_counts.tsv",
    help=".TSV file containing passenger data",
  ),
  make_option(
    c("-m", "--pre_pandemic_daily_passenger_averages.tsv"), 
    default="https://zenodo.org/record/8298280/files/pre-pandemic_daily_passenger_averages.tsv",
    help=".TSV file containing passenger data",
  ),
  make_option(
    c("-v", "--viral_load_data"), 
    default="https://zenodo.org/record/8298280/files/viral_load_data.tsv",
    help=".TSV file containing viral load data",
  ),
  make_option(
    c("-w", "--wastewater_variant_frequency_data"), 
    default="https://zenodo.org/record/8298280/files/wastewater_variant_frequencies.tsv", 
    help=".TSV file containing relative frequencies of variants from ww samples",# Replace with Zenodo
  ),
  # Collection directory
  make_option(
    c("-c", "--clinical_variant_frequency_data"), 
    default="data/clinical_variant_frequencies.tsv", 
    help=".TSV file containing relative frequencies of variants from clincal sampling",
  ),
  # Output directory
  make_option(
    c("-o", "--output"), 
    default="results/figure_1.pdf", 
    help="output file",
  )
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);


################################################################################
#
# Data type: Measures
#
################################################################################

df.periods <- tibble(
  period = LETTERS[1:5],
  start  = c(
    as.Date("2020-02-16"), 
    as.Date("2020-12-01"), 
    as.Date("2021-06-01"), 
    as.Date("2021-12-01"), 
    as.Date("2022-05-01")
  ),
  end    = c(
    as.Date("2020-11-30"), 
    as.Date("2021-05-31"), 
    as.Date("2021-11-30"), 
    as.Date("2022-04-30"), 
    as.Date("2022-09-04")
  ),
)

################################################################################
#
# Data type: Measures
#
################################################################################

# Load data
df.timeline <- tribble( ~start_date, ~event, ~displ, ~period,
                        as.Date("2020-02-27"), "First confirmed Dutch case",                              0.15, "A",
                        as.Date("2020-03-13"), "Cancellation flights high-risk areas",                    0.11, "A",
                        as.Date("2020-03-18"), "EU closes its external borders for non-essential travel", 0.07, "A",
                        as.Date("2020-04-17"), "Mandatory health certificate",                            0.03, "A",
                        as.Date("2020-12-01"), "Mandatory facemask",                                      0.15, "A",
                        as.Date("2020-12-15"), "Mandatory negative test on flights",                      0.11, "B",
                        as.Date("2020-12-18"), "Lockdown",                                                0.07, "B",
                        as.Date("2021-06-01"), "Dutch government lifts\nmost restrictions",               0.02, "C",
)

# Plot data
plt.timeline <- 
  # Set data and base aesthetics equal for all geoms
  ggplot() +
  # background
  geom_rect(
    data    = df.periods,
    alpha   = .2,
    mapping = aes(
      xmin = start,
      xmax = end+1,
      ymin = 0,
      ymax = .2,
      fill = period
    ),
    show.legend = FALSE
  )+
  geom_text(
    data    = df.periods,
    mapping = aes(
      x     = start + (end-start)/2,
      y     = .02,
      label = period
    )
  ) +
  # Add point as tip of lollipop
  geom_point(
    data = df.timeline,
    aes(x = start_date, y = displ),
    color = "#444444",
    size = 1) + 
  # Add segment as branch of lollipop
  geom_segment(
    data = df.timeline,
    aes(
      x = start_date, y = displ,
      xend = start_date,  
      yend = 0
    ),
    color = "#444444"
  )+
  # Add text to lollipops
  geom_text(
    data = df.timeline,
    aes(
      x = start_date, 
      y = displ, 
      label = event
    ), 
    hjust = 0, 
    vjust = -0.5,
    size = 3.5
  ) +
  # Axis
  scale_x_date(
    date_breaks = "1 months", 
    date_labels = "%b-%y", 
    expand = c(0,0), 
  ) +
  scale_y_continuous(limits = c(0, .20), expand = c(0,0))+ 
  scale_fill_viridis_d()+
  # Theme 
  theme_bw() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(),
    axis.text.x = element_text(size = 8),
    panel.background = element_blank()
  ) 

################################################################################
#
# Data type: Passengers
#
################################################################################

# If file is not given, skip passenger plot
# Load data
df.passengers <- read_tsv(
  file = opt$pandemic_daily_passenger_counts,
  show_col_types = FALSE
) %>%
  mutate(continent = factor(
    continent, 
    levels = c(
      "Europe",
      "Asia",
      "Oceania",
      "North America",
      "South America",
      "Africa"
    )))

df.pre_pandemic_daily_passenger_averages <- 
  read_tsv(
    file           = opt$pre_pandemic_daily_passenger_averages.tsv,
    show_col_types = FALSE
  ) %>%
  group_by(month_day, average_arrival_2017_2019) %>%
  reframe(year = list(2020, 2021, 2022))%>%
  mutate(
    date = as.Date(paste0(year, "-", month_day), format = "%Y-%b-%d")
  ) %>%
  select(date, average_arrival_2017_2019) %>%
  filter(between(date, left = as.Date(opt$start), right = as.Date(opt$end)))


# Generate plot
plt.passengers <-
  ggplot() +
  geom_col(
    data    = df.passengers,
    mapping = aes(
      x = record_date,
      y = total/10000,
      fill = continent
    ),
    width = 1
  ) +
  geom_line(
    data    = df.pre_pandemic_daily_passenger_averages %>%
      mutate(label = "Mean Daily Arrivals\n2017-2019"),
    mapping = aes(
      x   = date,
      y   = average_arrival_2017_2019/10000,
      group = 1,
      color = label
    )
  )+
  theme_bw() +
  scale_fill_viridis_d()+
  scale_colour_manual(values = c("black"))+
  scale_x_date(
    date_breaks = "1 months", 
    date_labels = "%b-%y", 
    limits      = c(as.Date(opt$start)-1, as.Date(opt$end)+1),
    expand      = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0,12.5), 
    expand = c(0,0),
    breaks = c(0, 4, 8, 12)
  )+
  labs(
    y = expression(paste("Passenger Arrivals (", 10^5, ")")),
    fill = "",
    color = ""
  ) +
  theme(axis.title.x = element_blank())






################################################################################
#
# Data type: Viral load
#
################################################################################


# Load data
df.viral_load <- read_tsv(
  file           = opt$viral_load_data,
  show_col_types = FALSE
) %>%
  mutate(
    label_rolling_average = ifelse(
      test = sample_date < as.Date("2021-06-01"),
      yes  = "1e11 Particles",
      no   = ifelse(
        test = sample_date < as.Date("2022-05-01"),
        yes  = "5*1e11 Particles",
        no   = "1e12 Particles"
      )
    ), 
    label_points = ifelse(
      test = viral_load == 0,
      yes  = "Not Detected",
      no   = label_rolling_average
    ),
    viral_load = ifelse(
      test = sample_date < as.Date("2021-06-01"),
      yes  = viral_load / 10^11,
      no   = ifelse(
        test = sample_date < as.Date("2022-05-01"),
        yes  = viral_load / (5*10^11),
        no   = viral_load / (10^12)
      )
    ),
    rolling_avg = rolling_average(dates = sample_date, values = viral_load, window = 7, direction = "bi"),
    label_points = factor(label_points, levels = c("1e11 Particles","5*1e11 Particles","1e12 Particles", "Not Detected")),
    label_rolling_average = factor(label_points, levels = c("1e11 Particles","5*1e11 Particles","1e12 Particles"))
  )

#####
# Generate plot
plt.viral_load <-
  ggplot() +
  # 7 Day Rolling Average
  geom_line(
    data    = df.viral_load %>%
      mutate(rol_avg = "Rolling Average\n(7-day window)"),
    mapping = aes(
      x    = sample_date,
      y    = rolling_avg,
      group = 1,
      color = label_rolling_average
    ),
    show.legend = FALSE,
    #alpha = .5,
  )+
  # Detection
  geom_point(
    data = df.viral_load,
    mapping = aes(
      x    = sample_date,
      y    = viral_load,
      fill = label_points,
    ),
    color = "black",
    pch   = 21
  )+
  # Theme
  theme_bw() +
  theme(
    axis.title.x     = element_blank(),
    legend.title = element_blank()
  ) +
  # Scales
  scale_fill_manual(
    #begin = .25,
    labels = list(
      "1e11 Particles"   = expression(paste(10^11, " Particles")),
      "5*1e11 Particles" = expression(paste(5, "*", 10^11, " Particles")),
      "1e12 Particles"   = expression(paste(10^12, " Particles")),
      "Not Detected"     = "Not Detected"
    ),
    values = c("#5ec962", "#21918c", "#3b528b","#fde725")
  )  +
  # Scales
  scale_color_manual(
    #begin = .25,
    labels = list(
      "1e11 Particles"   = expression(paste(10^11, " Particles")),
      "5*1e11 Particles" = expression(paste(5, "*", 10^11, " Particles")),
      "1e12 Particles"   = expression(paste(10^12, " Particles"))
    ),
    values = c("#5ec962", "#21918c", "#3b528b")
  )+
  scale_x_date(
    date_breaks = "1 months", 
    date_labels = "%b-%y", 
    limits      = c(as.Date(opt$start)-1, as.Date(opt$end)+1),
    expand      = c(0,0)
  )+
  scale_y_continuous(
    limits = c(-5, 120),
    breaks = c(0, 20, 40, 60, 80, 100),
    expand = c(0,0)
  ) +
  # Labels
  labs(
    y = "FC-VL (Particles per 24h)",
    fill = "",
    color = ""
  )  

################################################################################
#
# Data type: Variants
#
################################################################################

ls.new_bins <- list(
  "Omicron - BA.1"      = "Omicron - B.1.1.529/BA.1", 
  "Omicron - B.1.1.529" = "Omicron - B.1.1.529/BA.1",
  "Omicron - BA.4"      = "Omicron - BA.4/BA.5", 
  "Omicron - BA.5"      = "Omicron - BA.4/BA.5", 
  "Beta - B.1.351"      = "Other",
  "Gamma - P.1"         = "Other",
  "Mu - B.1.621"        = "Other",
  "Lambda - C.37"       = "Other"
)


# Load data
df.abundance_data_combined <- 
  full_join(
    read_tsv(
      file = opt$wastewater_variant_frequency_data, show_col_types = FALSE) %>%
      mutate(source = "ww"),
    read_tsv(opt$clinical_variant_frequency_data, show_col_types = FALSE) %>%
      mutate(source = "cd"),
    by = c("week_floor", "source", "lineage", "lineage_alias", "bin", "week_bin_average")
  ) %>%
  select("week_floor", "source", "bin", "week_bin_average") %>%
  mutate(
    week_floor = as.Date(week_floor),
    week_bin_average = as.numeric(week_bin_average)
  ) %>%
  distinct(week_floor, source, bin, week_bin_average) %>%
  filter(between(week_floor, left = as.Date(opt$start), right = as.Date(opt$end))) %>%
  mutate(
    bin = sapply(bin, function(x) ifelse(
      test = x %in% names(ls.new_bins),
      yes  = ls.new_bins[x],
      no = x
    ))
  ) %>% unnest(bin) %>%
  group_by(source, week_floor, bin) %>%
  reframe(week_bin_average = sum(week_bin_average)) %>%
  mutate(bin = factor(
    bin, 
    levels = c(
      "Alpha - B.1.1.7",
      "Delta - B.1.617.2",
      "Omicron - BA.1/B.1.1.529",
      "Omicron - BA.2",
      "Omicron - BA.4/BA.5",
      "Other")
  )
  )

# Generate Plot
plt.variants_combined <- 
  ggplot()+
  geom_col(
    data    = df.abundance_data_combined,
    mapping = aes(
      x    = week_floor,
      y    = week_bin_average,
      fill = bin
    ),
    width = 7
  )+
  geom_col(
    data    = df.abundance_data_combined,
    mapping = aes(
      x    = week_floor,
      y    = week_bin_average,
      fill = bin,
    ),
    width = 7,
    color = "black",
    show.legend = FALSE
  ) +
  geom_text(
    data = tibble(
      source = c("ww", "cd"),
      label  = c("Wastewater Sequencing", "Reported Clinical Sequencing"),
      pos_x  = c(as.Date("2020-03-01")),
      pos_y  = c(.75)
    ),
    mapping = aes(
      x = pos_x,
      y = pos_y,
      label = label
    ),
    hjust = 0
  )+
  facet_grid(
    row = vars(source)
  )+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(
    date_breaks = "1 months", 
    date_labels = "%b-%y", 
    limits      = c(as.Date(opt$start)-1, as.Date(opt$end)+1),
    expand      = c(0,0)
  ) +
  theme_bw()+ 
  theme(
    strip.background = element_blank(),
    strip.text       = element_blank(),
    panel.spacing    = unit(1, "lines"),
    axis.title.x     = element_blank()
  ) +
  labs(
    y = "\nVariant Proportion (Weekly average)",
    fill = ""
  ) +
  scale_fill_manual(
    values =c(
      "#440154",
      "#414487",
      "#2a788e",
      "#22a884",
      "#7ad151",
      "#fde725"
    )
  )

################################################################################
#
# Combined plot
#
################################################################################


# COMBINE PLOTS
plt_combined_overview <-
  # Plot two grids, the overarching grid is two side-by-side columns
  cowplot::plot_grid(
    # The first column is the actual plots without legends
    cowplot::plot_grid(
      # Timeline plot (remove legend)
      plt.timeline ,
      # FILL IN FOR EXTRA PLOTS
      plt.passengers + theme(legend.position = "none") + geom_vline(xintercept = df.periods$end[1:4], linetype = "dashed", color = "grey", show.legend = FALSE), 
      plt.viral_load + theme(legend.position = "none") + geom_vline(xintercept = df.periods$end[1:4], linetype = "dashed", color = "grey", show.legend = FALSE), 
      # Variant plot (remove legend)
      plt.variants_combined + theme(legend.position = "none", axis.title.x = element_blank()) + geom_vline(xintercept = df.periods$end[1:4], linetype = "dashed", color = "grey", show.legend = FALSE), 
      # Set grid as column
      ncol = 1, 
      # Add Full Caps labels for publication
      labels = "AUTO",
      # Align left and right axes
      axis = "lr",
      # Align horizontally and vertically
      align = "v", rel_heights = c(1,2,2,3)
    ), 
    # The second column is the legend
    cowplot::plot_grid(
      # Since the first plot doesn't have a legend, just add a white empty plot
      ggplot() + theme_void(), 
      # FILL IN FOR EXTRA PLOTS
      cowplot::get_legend(plt.passengers), 
      cowplot::get_legend(plt.viral_load), 
      # The second plot does have a legend
      cowplot::get_legend(plt.variants_combined),
      # Set grid as column
      ncol =1, rel_heights = c(1,2,2,3)
    ), 
    # Set widths of columns
    rel_widths = c(6,1)
  ) %>%
  annotate_figure(bottom = "Date (Month-Year)")

# Add background (The ggdraw flattens the image and adds a white background)
plt_combined_overview_draw <- 
  cowplot::ggdraw(plt_combined_overview) + 
  theme(plot.background = element_rect(fill="white", color = NA))

# Save


ggsave(
  opt$output, # Name of output file
  plt_combined_overview_draw, # Name of object
  # Modify size to make overall text smaller
  height = 12, # Add 3 height for every subplot
  width = 18
)
