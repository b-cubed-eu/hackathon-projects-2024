# Project 6: Phenological Diversity trends by remote sensing-related datacubes

## Abstract

Spectral diversity is a proxy for vegetation diversity. But is also a measure of trait diversity of the plant community and their geological surroundings. Taking a Critical Zone perspective, it describes the trait expression of the plant community taking in account the taxonomic composition and the abiotic factors linked to the availability of nutrients and water. To best emphasize this perspective of trait response of the plant community to change in abiotic factor, it is important to include the temporal dimension and take a phenological stance. The R package rasterdiv allows the implementation of spectral diversity analysis over satellite estimates of vegetation pigments (NDVI, MCARI, ...), in particular with Rao'Q , a diversity index that takes in account both species abundances and their trait distances. The package allows to deal with multidimensional traits, assuming that they are uncorrelated which is not the case for vegetation index time series. So, we propose to implement a dedicated method in rasterdiv to handle time series to extract phenological diversity. We plan two different alternative methods. 1.	Dynamic Time Warping (DTW) of time series trajectory. Rao Q requires identifying a distance between the traits. The output metrics is the sum of the residual difference and the penalty of the time warping change. DWT has an exact slow algorithm and a fast one. 2.	Summary Statistics approach. Time series are summarized using phenological statistics. Different options are possible.  The easier solution is to use HR-VPP (https://www.eea.europa.eu/en/datahub/datahubitem-view/b6cc3b37-0686-4bb1-b8c3-3c08520743c3) available on WEKEO. Use cases: we plan to work on three small (⋍30 hectares) grassland areas along an elevation gradient, where in situ biodiversity information is available. More in detail: 1) Gran Paradiso National Park at 2200m; 2) Sila National Park at 1700m; 3) Alta Murgia grasslands at 500m.

## More information


## Lead(s)

Saverio Vicario, Matteo Marcantonio

## Project Outcomes
