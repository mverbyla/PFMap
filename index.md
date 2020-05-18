---
layout: default
---

## Installation

The latest version of pathogenflows can be installed from github (if you don't have the package **devtools**, you will need to install it first):

``` r
# install.packages("devtools")
devtools::install_github('mverbyla/pathogenflows')
```

## About

The `getLoadings` function allows you to predict annual loadings of water pathogens from the onsite sanitation system technologies used in a given region of the world. 

| Pathogen Loadings               | Description          |
|:--------------------------|:---------------------------|
| Excreted                       | Number of pathogens excreted per year   |
| To groundwater             | Number of pathogens emitted to groundwater each year   |
| To the surface                 | Number of pathogens emitted to the surface each year      |
| Retained in subsurface    | Number of pathogens retained in the subsurface each year |
| Decayed/inactivated        | Number of pathogens inactivated per year | 
| Conveyed in fecal sludge to treatment   | Number of pathogens conveyed each year to treatment in fecal sludge |
| Conveyed in sewerage to treatment   | Number of pathogens conveyed each year to treatment in sewerage | 

The `getloadings` function requires the following three inputs:

### Inputs:

*   onsiteData
*   pathogenType

The input `onsiteData` should refer to a data input file from the K2P Data Portal (data.waterpathogens.org). This file must be in a very specific format. An example file can be found [here](http://data.waterpathogens.org/dataset/5374462b-5bb5-456f-bfc0-816ea572666d/resource/4d9e5fba-9280-4b8b-acce-d1c87952acc1/download/onsitedata_example.csv).

The input **`pathogenType`** should be equal to either one of the following strings: c("Virus","Bacteria","Protozoa","Helminth"). 


## Example

The following shows the use of the getLoadings function for data from Kampala, Uganda, grouping the data by 'region' and showing results for the 'Virus' group.

``` r
getLoadings(onsiteData,pathogenType="Virus")

#>     region     excreted to_groundwater   to_surface retained_in_soil      decayed In_Fecal_Sludge    In_Sewage  stillViable Onsite_LRV Onsite_PR
#> 1  Central 3.470412e+16   7.208736e+14 1.241347e+15     6.487862e+15 1.696317e+16    4.204900e+13 9.142680e+15 1.114695e+16       0.49    0.6788
#> 2  Kawempe 1.828207e+17   2.923315e+15 3.819070e+15     2.630984e+16 1.427235e+17    1.714675e+14 6.738060e+15 1.365191e+16       1.13    0.9253
#> 3 Makindye 2.115370e+17   5.784264e+15 7.404292e+15     5.205838e+16 1.396717e+17    1.877473e+14 6.042660e+15 1.941896e+16       1.04    0.9082
#> 4   Nakawa 1.740476e+17   4.848475e+15 5.834954e+15     4.363628e+16 1.023125e+17    1.109438e+14 1.716174e+16 2.795611e+16       0.79    0.8394
#> 5   Rubaga 2.103805e+17   4.278101e+15 5.257688e+15     3.850291e+16 1.602164e+17    2.221318e+14 9.808800e+14 1.073880e+16       1.29    0.9490

```

## Learn More

For more information, please see [our website](https://www.waterpathogens.org/). 
