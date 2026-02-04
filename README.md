# Replication package for "Environmental impacts of Indigenous land restitution in Chile"

---

## Overview

This repository provides all the necessary data and Stata code required to replicate the analyses and results presented in the paper titled **"Environmental impacts of Indigenous land restitution in Chile"**.

> **Note:** The replicator should expect the code to run for approximately **1 hour** to complete the full analysis.

---

## Data Availability and Provenance Statements

All databases are provided in the folder `/data`. No additional permission is required to use them. All data files are provided in `.dta` format. Further details regarding these data are provided in the "Data" section of the paper.

We certify that the authors of the manuscript have legitimate access to and permission to use the data used in this manuscript.

### Data on land restitution and historical reservations

The data for the land restitution program **FTAI-20B** were created by the authors, based on data from the **Corporación Nacional Indígena (CONADI)** available at [siic.conadi.cl](https://siic.conadi.cl).

* **Shapefiles selected:** `Cubiertas Descargables/Compras de Tierras Art. 20B`
* **Administrative data selected:** `Bases de Datos Descargables/Archivo histórico de compras Art. 20b`
* **Historical Mapuche reservations:** Geocoded data via shapefile `Cubiertas Descargables/Títulos de Merced`.

The data were originally accessed in **July 2024**. During the last check on **December 20, 2025**, a new version of these data was available.

### Land use outcomes

Land use data were created using two main sources:

1. **Temporally consistent land cover classes:** Based on the database by Graesser et al. (2022), provided directly by the authors.
2. **Tree cover identification (Natural forests vs. plantations):** Combined with data from the **MapBiomas Chile project (v1.0)** available at [chile.mapbiomas.org](https://chile.mapbiomas.org/).

### Environmental and productivity outcomes

* **Carbon capture and biodiversity:** Constructed based on data by Heilmayr et al. (2020).
* **Erodibility:** Based on highly-erodible land information from the **Centro de Información de Recursos Naturales de Chile (CIREN, 2010)**.
* **Enhanced Vegetation Index (EVI):** Extracted from **Landsat Collection 2** using **Google Earth Engine** as a proxy for productivity.

### Additional data

* **Weather variables:** High-resolution precipitation and temperature datasets from the **Center for Climate and Resilience Science**, available at [Zenodo](https://zenodo.org/records/7529682).
* **Municipality-level controls:** Constructed from the **Chilean household survey (CASEN)** via the Ministerio de Desarrollo Social y Familia.
* **Conflict events:** Geocoded data from the **MACEDA database (v3.0)**, available at [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KTOUDQ).
* **Wheat suitability:** Based on **FAO’s GAEZ v.4** potential rain-fed wheat production.

---

## Computational Requirements

The code was last tested on the following setup:

* **Hardware:** Intel(R) Core(TM) Ultra 7 155U (1.70 GHz) laptop.
* **Storage:** 800GB of free space.
* **OS:** Windows 11 Home Single Language (version 24H2).

### Software Requirements

* **Stata SE version 18.0** or later.
* **Required Stata packages:** `estout`, `pallete`, `colrspace`.

---

## Instructions to Replicators

1. **Download and unzip** the replication package from the project repository.
2. **Modify the path:** Open the file `Replication_FTAI_2026.do`. Find the placeholder `"YOUR PATH"` and replace it with the actual directory path where you extracted the files.
3. **Run the analysis:** Execute `Replication_FTAI_2026.do` to replicate all figures and tables.

---

## References

* **Boisier, P.** (2023). *Cr2met: A high-resolution precipitation and temperature dataset for the period 1960-2021 in continental Chile (v2.5)* [Data set]. [https://doi.org/10.5281/zenodo.7529682](https://doi.org/10.5281/zenodo.7529682)
* **Cayul, P., Corvalán, A., Jaimovich, D., & Pazzona, M.** (2022). Introducing MACEDA: New micro-data on an indigenous self-determination conflict. *Journal of Peace Research*, 59(6), 903–912.
* **Cayul, P., Corvalán, A., Duran-Micco, E., & Jaimovich, D.** (2025). El conflicto de autodeterminación entre el estado de Chile y el pueblo Mapuche: Una descripción usando microdatos georreferenciados de MACEDA (1990-2021). *Revista de Ciencia Política*.
* **Centro de Información de Recursos Naturales de Chile (CIREN).** (2010). *Determinación de la erosión actual y potencial de los suelos de Chile*. Technical report.
* **Graesser, J., Stanimirova, R., Tarrio, K., Copati, E. J., Volante, J. N., Verón, S. R., Banchero, S., Elena, H., Abelleyra, D. d., & Friedl, M. A.** (2022). Temporally-consistent annual land cover from Landsat time series in the southern cone of South America. *Remote Sensing*, 14(16), 4005.
* **Heilmayr, R., Echeverría, C., & Lambin, E. F.** (2020). Impacts of Chilean forest subsidies on forest cover, carbon and biodiversity. *Nature Sustainability*, 3(9), 701–709. [https://doi.org/10.1038/s41893-020-0547-0](https://doi.org/10.1038/s41893-020-0547-0)
* **MapBiomas.** (2024). *MapBiomas Chile, version 1.0*. [https://chile.mapbiomas.org](https://chile.mapbiomas.org)

