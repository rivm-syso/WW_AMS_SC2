# Wastewater monitoring of SARS-CoV-2 at Schiphol Airport  
---
Preprint available at [Preprints with THE LANCET](https://dx.doi.org/10.2139/ssrn.4558626)

## ABSTRACT  
**Background:** Wastewater-based epidemiological surveillance at municipal wastewater 
treatment plants has proven to play an important role in COVID-19 surveillance. Since,
international passenger hubs contribute extensively to global transmission of viruses, 
wastewater surveillance at this type of location may be of added value as well. The aim 
of this study is to explore the potential of long-term wastewater surveillance at a large 
passenger hub as an additional tool for public health surveillance during different stages 
of a pandemic.   

**Methods:** Here, we present an analysis of SARS-CoV-2 viral loads in airport 
wastewater by reverse-transcription quantitative polymerase chain reaction (RT-qPCR) 
from the beginning of the COVID-19 pandemic in Feb 2020, and an analysis of SARS-CoV-2 variants by whole-genome next-generation sequencing from Sep 2020, both 
until Sep 2022, in the Netherlands. Results are contextualized using (inter)national 
measures and data sources such as passenger numbers, clinical surveillance data and 
national wastewater surveillance data.   

**Findings:** Our findings show that wastewater surveillance was possible throughout the 
study period, irrespective of measures. Viral loads rose simultaneously with the 
emergence of new variants. Furthermore, trends in viral load and variant detection in
airport wastewater closely followed, and in some cases preceded, trends in national 
daily average viral load in wastewater and variants detected in clinical surveillance.   

**Interpretation:** Wastewater-based epidemiology at a large international airport is a 
valuable addition to classical COVID-19 surveillance and the developed expertise can 
be applied in pandemic preparedness plans for other (emerging) pathogens in the future.

---

## Description
This repository contains the code required to reproduce the plots in the preprint: [*Long-term wastewater monitoring of SARS-CoV-2 viral loads and variants at the major international passenger hub Amsterdam Schiphol Airport: a valuable addition to COVID-19 surveillance*](https://dx.doi.org/10.2139/ssrn.4558626).  

---  

## Usage
The code in this repository has been tested with *R* 4.2.2. with *Tidyverse* v2.0.0. Packages required are listed in `scripts/supplementary_functions.R`. The figures can be produced by running the `run_analysis.R` script, which automatically sources other scripts and gathers data from online databases and the [Zenodo data set](https://zenodo.org/record/8298280)  associated with the [preprint](https://dx.doi.org/10.2139/ssrn.4558626), available under the [Creative Commons Attribution 4.0 License](https://creativecommons.org/licenses/by/4.0/legalcode). 

---  

## External data
A list of external data used for plotting is given below.

### Automatically sourced
 - https://data.rivm.nl/covid-19/COVID-19_varianten.csv: This data is made available by the Kiemsurveillance RIVM under the [CC BY 4.0 Licence](https://creativecommons.org/licenses/by/4.0/deed.en).
 - https://data.rivm.nl/covid-19/COVID-19_rioolwaterdata_landelijk.csv: This data is made available by the Waterschappen, CBS & RIVM under the [Public Domain Mark 1.0 Licence](https://creativecommons.org/publicdomain/mark/1.0/deed.en).
 - https://raw.githubusercontent.com/cov-lineages/pango-designation/master/pango_designation/alias_key.json: This data part of the Pango-Designation GitHub repository made available under the [CC BY-NC 4.0 Licence](https://creativecommons.org/licenses/by-nc/4.0/).

### Hospitalization data
To include the hospitalization data in figure 3, please download the weekly hospital admission covid per million data from [Our World in Data](https://ourworldindata.org/grapher/weekly-hospital-admissions-covid-per-million?tab=map), and supply the `.csv` table to the `figure_3.R` script using the `-a` option. This data is made available by Our World in Data under the [CC BY Licence](https://creativecommons.org/licenses/by-nc/4.0/).

---  

## Authorship
### Code
Auke Haver & Anne-Merel van der Drift

### Manuscript
* [Anne-Merel van der Drift](https://orcid.org/0000-0001-9258-3781)
* [Auke Haver](https://orcid.org/0000-0002-6711-2205)
* [Astrid Kloosterman](https://orcid.org/0000-0002-8702-1247)
* [Ruud van der Beek](https://orcid.org/0009-0000-3012-9161)
* Erwin Nagelkerke
* [Jeroen Laros](https://orcid.org/0000-0002-8715-7371)
* Consortium NRS
* [Jaap van Dissel](https://orcid.org/0000-0002-3857-331X)
* [Ana Maria de Roda Husman](https://orcid.org/0000-0001-9651-0504)
* [Willemijn Lodder](https://orcid.org/0009-0006-4795-5404)

---  

## Funding
This research was funded by the Dutch Ministry of Health, Welfare and Sport as part of the Dutch National Sewage Surveillance program.
