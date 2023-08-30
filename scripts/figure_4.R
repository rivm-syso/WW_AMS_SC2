################################################################################
#
# Title: Figure 4
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
    c("-s", "--settings"), 
    default="data/parameters/study_periods.yaml", 
    help="Study periods file",
  ),
  make_option(
    c("-p", "--passenger_data"), 
    default="https://zenodo.org/record/8298280/files/pandemic_daily_passenger_counts.tsv",
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
    help=".TSV file containing relative frequencies of variants from ww samples",
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
    default="results/", 
    help="output file",
  )
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

################################################################################
#
# Data type: Study period
#
################################################################################

df.periods <- tibble(
  period = c("D", "E"),
  # 'period D: Dec 2021 - April 2022',
  # 'period E: May 2022 - September 2022'),
  start  = c(
    as.Date("2021-11-29"), 
    as.Date("2022-05-02")
  ),
  end    = c(
    as.Date("2022-05-01"), 
    as.Date("2022-09-04")
  ),
)

################################################################################
#
# Data type: Viral load
#
################################################################################

# Load data
df.viral_load <- read_tsv(
  file           = opt$viral_load_data,
  show_col_types = FALSE
) %>% mutate(
  viral_load = viral_load,
  period     = sapply(sample_date, function(x) return_period(x, df.periods)),
  vl_rol_avg =rolling_average(dates = sample_date, values = viral_load, window = 7, direction = "bi")
)%>%
  filter(!period == "none")

################################################################################
#
# Data type: Variants
#
################################################################################

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
  mutate(
    week_floor = as.Date(week_floor),
    week_bin_average = as.numeric(week_bin_average),
    period = sapply(week_floor, function(x) return_period(x, df.periods)),
  ) %>%
  ungroup() %>%
  group_by(bin) %>%
  filter(
    !period == "none",
    week_bin_average > 0
  ) %>%
  ungroup() %>%
  distinct()%>%
  mutate(
    bin = factor(
      bin, levels = c(
        "Other",
        "Delta - B.1.617.2",
        "Omicron - BA.1/B.1.1.529",
        "Omicron - BA.2",
        "Omicron - BA.4",
        "Omicron - BA.5"
      )
    )
  )

################################################################################
#
# Plot
#
################################################################################


plot_subplot <- function(
    abundance_dataframe, 
    viral_load_dataframe, 
    required_period, 
    conversion_factor,
    y_axis_label
){
  
  # Split dataframes
  
  ## Wastewater data
  ww_dataframe <- abundance_dataframe %>%
    filter(source == "ww", period == required_period) %>%
    group_by(sample_date, bin, period) %>%
    summarize(
      abundance = sum(abundance),
      .groups = "drop"
    )
  
  ## Clinical data dataframe
  cd_dataframe <- abundance_dataframe %>%
    filter(source == "cd", period == required_period) %>%
    distinct(bin, week_floor, week_bin_average, period)
  
  ## VL dataframe
  vl_dataframe <- viral_load_dataframe %>% 
    filter(period == required_period)
  
  return(
    ggplot() +
      geom_col(
        data    = cd_dataframe,
        mapping = aes(
          x    = week_floor+3,
          y    = week_bin_average,
          group = bin,
          fill = bin
        ),
        alpha   = .5,
        width = 7
      ) +
      geom_col(
        data    = ww_dataframe,
        mapping = aes(
          x = sample_date,
          y = abundance,
          fill  = bin
        ),
        width = 1,
        alpha = .75,
        color = "black"
      ) +
      geom_line(
        data    = vl_dataframe,
        mapping = aes(
          x = sample_date,
          y = vl_rol_avg/conversion_factor,
          group = 1
        ),
        color = "black",
        linewidth = 1.1,
        alpha = .9
      )+
      geom_line(
        data    = vl_dataframe,
        mapping = aes(
          x = sample_date,
          y = vl_rol_avg/conversion_factor,
          group = 1,
          color = ""
        ),
        linewidth = 1,
        alpha = .9
      )+
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_date(
        date_breaks = "1 week",
        name        = "",
        date_labels = "%d-%b",
        expand      = c(0,0)
      ) +
      scale_y_continuous(
        limits      = c(0,1.0005),
        expand      = c(0,0),
        name        = "",
        sec.axis = sec_axis(
          name =  y_axis_label,
          trans = ~.*100,
        )
      ) +
      scale_color_manual(
        values = "grey",
        labels = "WWTP-AMS FC-VL"
        
      )+
      scale_fill_manual(
        values = c(
          "#440154",
          "#414487",
          "#2a788e",
          "#22a884",
          "#7ad151",
          "#fde725"
        )
      )+
      labs(
        x = "Date (Day-Month)",
        fill = "Variant of Concern",
        color = ""
      ) +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"),
        legend.spacing.x = unit(.5, 'cm'),
        plot.margin = unit(c(0.2,0,0,0), 'lines')
      ) 
  )
}


plot_grid(
  plotlist = list(
    ggarrange(
      plot_subplot(
        df.abundance_data_combined, 
        df.viral_load, 
        "D",
        conversion_factor = 5*10^13,
        expression(paste(5, "*", 10^11, "particles"))
      ),
      plot_subplot(
        df.abundance_data_combined, 
        df.viral_load, 
        "E",
        conversion_factor = 10^14,
        expression(paste(10^12, "particles"))
      ),
      ncol = 1,
      common.legend = TRUE,
      legend = "none"
    ) %>%
      annotate_figure(
        right = "WWTP-AMS FC-VL (virus particles per 24h)",
        left  = "Variant Frequency",
        bottom = "Sample Date (Day-Month)"
      ),
    get_legend(plot_subplot(
      df.abundance_data_combined, 
      df.viral_load, 
      "D",
      conversion_factor = 5*10^13,
      expression(paste(5, "*", 10^11, "particles"))
    ))),
  ncol = 1, rel_heights = c(8,1))



ggsave(
  filename = file.path(opt$output, "figure_4.pdf"),
  width = 10,
  height = 6,
  dpi = 300
)
