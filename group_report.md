Group report
================
Group 4
2022-08-02

# Cleaning

## Fluxes

A flux that requires cleaning:

<img src="group_report_files/figure-gfm/unnamed-chunk-1-1.png" style="display: block; margin: auto auto auto 0;" />

A flux that does not require cleaning:

<img src="group_report_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto auto auto 0;" />

## PAR values

Problems with PAR values during ER measurements:

<img src="group_report_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto auto auto 0;" />

Problems corrected. The negative PAR values were replaced by 0 (because
it is ER):

<img src="group_report_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto auto auto 0;" />

# Data dictionaries

## Fluxes

| Variable name | Description                                                              | Variable type | Variable range or levels | Unit           | How measured                                                             |
|:--------------|:-------------------------------------------------------------------------|:--------------|:-------------------------|:---------------|:-------------------------------------------------------------------------|
| turfID        | The ID of the plot of the carbon flux measurement                        | numeric       | Inf - -Inf               | NA             | defined                                                                  |
| pairID        | NA                                                                       | numeric       | 1 - 137                  | NA             | NA                                                                       |
| type          | Types of the data that were collected with (ER) and without tarps (NEE). | numeric       | Inf - -Inf               | NA             | defined                                                                  |
| flux          | The value of the measured carbon flux                                    | numeric       | -55.047 - 68.556         | mmol/sqm/h     | calculated                                                               |
| temp_soil     | NA                                                                       | numeric       | 0.825 - 21.608           | NA             | NA                                                                       |
| PARavg        | The mean of the Photosynthetic Active Radiation (PAR) value              | numeric       | 0.046 - 1818.049         | micromol/s/sqm | Automatically measured by the PAR sensor and recorded to the data logger |
| datetime      | Date and time of the measured carbon flux                                | numeric       | Inf - -Inf               | NA             | defined                                                                  |

# Calculations

## Fluxes

<img src="https://render.githubusercontent.com/render/math?math=\color{violet}flux=slope\times \frac{P\times V}{R\times T\times A}">

Where:

-   flux: the flux of CO<sub>2</sub> at the surface of the plot
    (![\\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu "\mu")mol
    m<sup>-2</sup> s<sup>-1</sup>)
-   slope: slope of linear regression fitting the CO<sub>2</sub>
    concentration versus time
    (![\\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu "\mu")mol
    mol<sup>-1</sup>)
-   P: pressure, assumed 1 atm
-   V: volume of the chamber and tubing (L)
-   R: gas constant (0.082057 L atm K<sup>-1</sup> mol<sup>-1</sup>)
-   T: chamber air temperature (K)
-   A: area of chamber frame base (m<sup>2</sup>)

## Gross primary productivity

GPP = NEE - ER
