% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgx_auto_explore.R
\name{xgx_auto_explore}
\alias{xgx_auto_explore}
\title{Produce an xgx-styled report the given dataset using xgx R markdown templates,
or a user-provided R markdown template.  (Note: The R markdown template provided must
be formatted in a similar manner to that of the xgx R markdown templates to work.)
The working directory will contain a new directory (`xgx_autoexplore_output`) after running this function,
 which will contain a directory for the dataset, and futher a directory for the type of analysis / R markdown template.}
\usage{
xgx_auto_explore(
  data_path = NULL,
  mapping = list(),
  author_name = NULL,
  multiple_dosing = FALSE,
  pk_cmt = NULL,
  pd_cmt = NULL,
  pd_data_type = NULL,
  dose_cmt = NULL,
  steady_state_day = NULL,
  time_between_doses = NULL,
  rmd_template_name = NULL,
  rmd_template_path = NULL,
  rmd_output_path = NULL,
  pdf_output_path = NULL,
  html_output_path = NULL,
  add_datetime = TRUE,
  show_explanation = TRUE
)
}
\arguments{
\item{data_path}{Path (as a string) to the dataset that is to be analyzed}

\item{mapping}{A list of column name mappings from the
original (template) dataset column names
to the corresponding columns in the new dataset.}

\item{author_name}{The name of the author to be displayed on the template}

\item{multiple_dosing}{Whether or not to use a "Multiple" or "Single" Ascending dose template}

\item{pk_cmt}{An integer denoting the "compartment" containing the PK data. The "CMT" column will typically
have these integers, where each row may contain PK, PD, dosing or other events/observations data}

\item{pd_cmt}{An integer denoting the "compartment" containing the PD data, 
of the desired type  (continuous, ordinal, etc.). The "CMT" column will typically
have these integers, where each row may contain PK, PD, dosing or other events/observations data}

\item{pd_data_type}{The type of PD data - acceptable values exist in the following list: 
["binary","continuous","count","ordinal","real_example","receptor_occupancy","time_to_event"]}

\item{dose_cmt}{Integer denoting the compartment for dosing records}

\item{steady_state_day}{used to denote the day of rich sampling of PK at steady state}

\item{time_between_doses}{dosing interval, has units to match the time variable of the dataset}

\item{rmd_template_name}{A custom output name for the generated Rmd file}

\item{rmd_template_path}{A user provided custom template (as a string)}

\item{rmd_output_path}{A custom output path for the generated Rmd file
(This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))}

\item{pdf_output_path}{A custom output path for the generated PDF file
(This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))}

\item{html_output_path}{A custom output path for the generated HTML file
(This is typically left as `NULL` in order to maintain the hierarchical directory structure of `xgx_autoexplore_output`))}

\item{add_datetime}{Boolean indicating additon of a date stamp to the beginnning of the Rmd file}

\item{show_explanation}{Boolean indicating if the additional explanations (text in between figures) are needed for the user.}
}
\description{
\code{xgx_auto_explore} returns an HTML and PDF document with plots
describing the provided dataset
}
\details{
This function can be used quickly to explore your data by generating 
overview plots before constructing non-linear mixed effects models.
}
\examples{

author_name = "Your Name Here"
show_explanation = FALSE

\dontrun{
# Try out the nonlinear_pkpd dataset with the
# Multiple Ascending Dose PK Rmd template
data_path <- "~/nonlinear_pkpd.csv"

# Specify the mapping of column names
mapping <- list(
  "TIME" = "TIM2",
  "NOMTIME" = "NT",
  "EVID" = 0,
  "CENS" = 0,
  "DOSE" = "MGKG",
  "TRTACT" = "TRT",
  "LIDV_NORM" = "LIDV/MGKG",
  "LIDV_UNIT" = "UNIT",
  "PROFDAY" = 1,
  "SEX" = 0,
  "WEIGHTB" = 0)


# 5 contains the PK Concentration in this dataset
pk_cmt = 5
# We don't need PD right now
pd_cmt = NULL
pd_data_type = NULL


dose_cmt = 1
steady_state_day = c(0, 6)
time_between_doses = 24
multiple_dosing = TRUE

output_directory = tempdir()

xgx_auto_explore(data_path = data_path,
                 mapping = mapping,
                 author_name = author_name,
                 pk_cmt = pk_cmt,
                 pd_cmt = pd_cmt,
                 dose_cmt = dose_cmt,
                 steady_state_day = steady_state_day,
                 time_between_doses = time_between_doses,
                 multiple_dosing = multiple_dosing,
                 pd_data_type = pd_data_type,
                 rmd_output_path = output_directory,
                 show_explanation = show_explanation)
}
    
}
