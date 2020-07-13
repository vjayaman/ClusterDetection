# ClusterDetection

To see sample outputs, open *cluster_module.html* (first) and *identifying_distribution.html* (second). Note that knitting the rmarkdown docs will replace these html files.

Run *setup_script.R* first, to install the necessary packages and process some of the data into formats the other scripts can use. This includes generating the lookup table of heights, for example.

Then, run *cluster_module.Rmd*, which generates and saves a file "both_timepoints.Rds", which is required to run the next document, *identifying_distribution.Rmd*. 

This setup has not been extensively tested yet, so bugs are expected at this point.

