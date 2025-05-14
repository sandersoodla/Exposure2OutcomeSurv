# Exposure2OutcomeSurv: Eestikeelne kokkuvõte 
English below

`Exposure2OutcomeSurv` on R tarkvarapakett diagnooside (ekspositsioonide ja tulemite) vaheliste seoste uurimiseks elukestusanalüüsiga ehk statistiliste meetoditega, mis analüüsivad aega mingi uuritava sündmuse toimumiseni.
Paketil on Shiny graafiline kasutajaliides ning see töötab OMOP CDM vormingus terviseandmetega.

Rakendus võimaldab:

* Ühenduda OMOP CDM andmebaasiga, mis võib asuda erinevatel platvormidel või kohalikes DuckDB failides.
* Valida ekspositsiooni ja tulemusseisundi kontseptsioone.
* Luua soo ja vanuse põhjal sobitades ekspositsioonita võrdlusgrupp.
* Visualiseerida elukestuserinevusi Kaplan-Meieri graafikute abil.
* Hinnata riskide suhteid (Hazard Ratios) Coxi võrdeliste riskide mudelite abil.
* Salvestada ja laadida analüüsi tulemusi.

Pakett loodi Tartu Ülikooli bakalaureusetöö raames 2025. aastal.

# Exposure2OutcomeSurv: Survival Analysis on Exposure and Outcome Condition Pairs

[![License: Apache 2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

`Exposure2OutcomeSurv` is an R Shiny application designed for exploring survival relationships between user-defined exposure and outcome condition cohorts using data from an OMOP Common Data Model (CDM) database.

The application allows users to:
* Connect to an OMOP CDM hosted on various platforms or via local DuckDB files.
* Select exposure and outcome condition concepts.
* Perform matching to create comparable patient groups.
* Visualize survival differences using Kaplan-Meier plots (`survminer`).
* Estimate Hazard Ratios using Cox Proportional Hazards models (`survival`).
* Save and load analysis results.

## Features

* Connects to OMOP CDM databases supported by `DatabaseConnector` (e.g., PostgreSQL, Microsoft SQL Server, Oracle, Google BigQuery, Snowflake, Spark, Redshift, Netezza) and local DuckDB files.
* Interactively define Exposure and Outcome cohorts using condition concepts (search or file upload).
* Performs Incidence Density Matching controlling for age (year of birth), gender, and time.
* Generates Kaplan-Meier plots with risk tables.
* Calculates Hazard Ratios from Cox models, accounting for the matched design.
* Displays results in interactive tables (`DT`) that can be filtered, sorted, and exported.
* Allows saving (`.rds`) and loading of complete analysis session results.

## Installation

### Prerequisites

* **R Environment:** R version 4.4.0 or higher recommended. A properly configured R environment is necessary, especially for database connectivity. See the [OHDSI Hades R Setup Guide](https://ohdsi.github.io/Hades/rSetup.html) for detailed instructions on setting up R, RTools (Windows), and potentially Java for database drivers.
* **Database Access:** Access to an OMOP CDM v5.3+ database.
* **DuckDB (Optional):** If you intend to connect to local `.duckdb` files, you must install the `duckdb` package separately: `install.packages("duckdb")`.

### Steps

1.  **Install Package:**
    * **A) From GitHub:**
        ```R
        # install.packages("remotes") # If not already installed
        remotes::install_github("sandersoodla/Exposure2OutcomeSurv")
        ```
    * **B) From Cloned Repo:**
        ```R
        # install.packages("remotes") # If not already installed
        remotes::install_local("path/to/your/cloned/Exposure2OutcomeSurv")
        ```

3.  **Configure Database Connection:** Before first use, you **must** set up your database connection details in an `.Renviron` file. See the **Configuration** section below for details.

## Configuration

1.  **Edit `.Renviron`:** Create or edit the `.Renviron` file (in your project root or home directory) using the following template. Fill in the values specific to your setup.

    ```bash
    # Java options (increase memory if needed for some drivers)
    _JAVA_OPTIONS='-Xmx4g'

    # Path to folder containing database driver JAR files
    DATABASECONNECTOR_JAR_FOLDER='C:/path/to/jdbc_drivers'

    # --- Database Type ---
    # Set to "duckdb" for local files, or the DatabaseConnector name for others
    # Examples: "postgresql", "sql server", "oracle", "redshift", "bigquery"
    
    DBMS="postgresql"
    
    # --- Connection Details ---
    # Option A: If DBMS = "duckdb"
    # DB_HOST should be the FOLDER containing the .duckdb file
    # DB_NAME should be the FILENAME of the .duckdb file (e.g., "cdm.duckdb")

    # Option B: If DBMS is NOT "duckdb" (using DatabaseConnector)
    # DB_HOST: Server address (e.g., "your_server.com", "your_server\\instance")
    # DB_NAME: Database name on the server (e.g., "cdm_prod", "omop_db")

    DB_HOST=your-db-host
    DB_NAME=your-db-name
    DB_PORT=db-port
    DB_USERNAME=your-username
    DB_PASSWORD=your-password

    # Schema containing the OMOP CDM tables
    CDM_SCHEMA=schema-where-CDM-is-located
    # Schema where the application can write temporary tables (requires WRITE access)
    WRITE_SCHEMA=schema-for-temporary-writing-by-CDMConnector

    # --- Results Directory ---
    SURV_RESULTS_DIR=""

    ```

2.  **Restart R Session:** After creating or modifying your `.Renviron` file, **you must restart your R session** for the environment variables to be loaded correctly. (In RStudio: Session -> Restart R).

## Usage

1.  **Load Package:**
    ```R
    library(Exposure2OutcomeSurv)
    ```
2.  **Launch App:**
    ```R
    launchApp()

    # Example with optional arguments (see function documentation for details):
    # launchApp(port = 1234, launch.browser = FALSE)
    ```
3.  **Workflow:**
    * Connect to your database (using configured credentials).
    * Select exposure and outcome concepts.
    * Run the analysis, providing a filename for saving results.
    * Explore the summary table and Kaplan-Meier plots.
    * Optionally, load previously saved results.

## Author Information
The package was developed as part of a Bachelor's thesis at the University of Tartu in 2025.
* **Author and Maintainer:** Sander Soodla (<sandersoodla@gmail.com>)
