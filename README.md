# Wastewater monitoring of SARS-CoV-2 at Schiphol Airport  
---
Publication accepted at [Science of the Total Environment](https://www.sciencedirect.com/journal/science-of-the-total-environment)

---

## Description
This repository contains the code required to reproduce the plots in the publication: [*Long-term wastewater monitoring of SARS-CoV-2 viral loads and variants at the major international passenger hub Amsterdam Schiphol Airport: a valuable addition to COVID-19 surveillance*](https://www.sciencedirect.com/journal/science-of-the-total-environment).  

---  

## Usage
The code in this repository has been tested with *R* 4.2.2. with *Tidyverse* v2.0.0. Packages required are listed in `scripts/supplementary_functions.R`. The figures can be produced by running the `run_analysis.R` script, which automatically sources other scripts and gathers data from online databases and the [Zenodo data set](https://zenodo.org/record/11282406)  associated with the [preprint](https://dx.doi.org/10.2139/ssrn.4558626), available under the [Creative Commons Attribution 4.0 License](https://creativecommons.org/licenses/by/4.0/legalcode). 

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
* [Dirk Eggink](0000-0002-4248-6008)
* [Jeroen Laros](https://orcid.org/0000-0002-8715-7371)
* Consortium NRS
* [Jaap van Dissel](https://orcid.org/0000-0002-3857-331X)
* [Ana Maria de Roda Husman](https://orcid.org/0000-0001-9651-0504)
* [Willemijn Lodder](https://orcid.org/0009-0006-4795-5404)

---  

## Funding
This research was funded by the Dutch Ministry of Health, Welfare and Sport as part of the Dutch National Sewage Surveillance program.
