\name{umf_UssingAnalysis}
\alias{umf_UssingAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
umf_UssingAnalysis
}
\description{
This function takes a file_path leading to an xls or xlsx file containing Ussing chamber data, a list of drugs used in the protocol, the number of values to average, and methods for determining values and returns an object of class 'Ussing' containing the appropriate analysis. Methods have been set to print(), summarize(), and plot() the objects of class 'Ussing'. A custom method: view.plots(), takes an object of class 'Ussing' and returns each plot individually.
}
\usage{
umf_UssingAnalysis(file_path, drugs=c("Amil", "FSK", "VX770", "CFTRinh", "UTP"), n=6, method=c("delta", "deltaMin"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_path}{
The full path to the xls or xlsx containing Ussing chamber data. File must be in the correct format for the analysis to be performed correctly: There must be three sheets ("Isc", "TER", and "Protocol"). "Time" must be in cell A1 for each sheet. Time units must be in seconds and be integers. There must not be blank values in the data.
}
  \item{drugs}{
  List of drugs in the order that they were used in the protocol. Must match the number of values in the protocol. Suggested: c("Amil", "FSK", "VX770", "CFTRinh", "UTP").
  }
   \item{n}{
 The number of values to average when computing values in the analysis. Suggested n=6.
  }
  \item{method}{
 The method used to compute the delta values. For the lowest value in the defined range use 'deltaMin', for the final values of the range use 'delta'.
  }
}
\details{
This function takes a file_path leading to an xls or xlsx file containing Ussing chamber data, a list of drugs used in the protocol, the number of values to average, and methods for determining values and returns an object of class 'Ussing' containing the appropriate analysis. Methods have been set to print(), summarize(), and plot() the objects of class 'Ussing'. A custom method: view.plots(), takes an object of class 'Ussing' and returns each plot individually.
}
\value{
The value returned is an object of class 'Ussing'.
}
\references{
@Manual{,
    title = {readxl: Read Excel Files},
    author = {Hadley Wickham and Jennifer Bryan},
    year = {2017},
    note = {R package version 1.0.0},
    url = {https://CRAN.R-project.org/package=readxl},
  }
}
\author{
Eric S. Davis
}
\note{
Required package: "readxl"
}
