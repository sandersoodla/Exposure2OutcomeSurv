# Exposure2OutcomeSurv: siia eesti keeles kirjeldus

eesti

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

### Steps

## Usage

## Configuration

## Author Information
* **Author and Maintainer:** Sander Soodla (<sandersoodla@gmail.com>)