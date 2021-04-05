# ChromosomePaintingTool
#### Daniel Farid (Yale 2023), Gabriel Conte Cortez Martins (Yale 2020)

## Tool Overview and Description

This tool, developed by members of Yale University's Gerstein Lab, allows visualization of chromosome signal density plots for ENTEx experiments. Choose a combination of individual, assay, and tissue, and view the signal track, or chromosome painting map, for that specific experiment. Also available are some advanced stats, such as changing the plot type from heatmap to histogram to scatter plot, specifying the chromosome region of interest (<b>work in progress</b>), and specifying which chromosomes to zoom in on (<b>work in progress</b>).

## Instructions for Use

Download the "chromoPaintingBundle" directory on this repository, which has the app (app.R) and all necessary files for use of the tool. Opening app.R within this directory locally <b>in RStudio</b> will ensure that all file paths used in the code will work correctly. A commented version of the app code is available within the "appCommented.R" file.

## Important Notes

#### Missing Data Errors

The signal tracks used in this tool are generated using publically available BAM files from the ENCODE online portal. Some combinations of individual/tissue/assay did not have experiments completed, and thus data for these experiments does not exist. As a result, selecting these specific combinations when using the tool may result in an error.   
