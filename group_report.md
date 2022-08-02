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

# Calculations

## Fluxes

<img src="https://render.githubusercontent.com/render/math?math=\color{violet}flux=slope\times \frac{P\times V}{R\times T\times A}" width = "800" height="200">

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

# Figures

## Vikesland

<img src="group_report_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto auto auto 0;" />
Fluxes over 24 hours at Vikesland (data not fully cleaned yet, might
need some more work).

# Data dictionaries

## Fluxes

| Variable name | Description                                                              | Variable type | Variable range or levels                                                                                                        | Unit           | How measured                                                                 |
|:--------------|:-------------------------------------------------------------------------|:--------------|:--------------------------------------------------------------------------------------------------------------------------------|:---------------|:-----------------------------------------------------------------------------|
| turfID        | The ID of the plot of the carbon flux measurement                        | categorical   | 85 WN1C 162, 105 WN3C 173, 158 WN2C 199, TTC 146, TTC 140, TTC 141, TTC 101, TTC 110, TTC 115, 4 AN1C 4, 27 AN3C 27, 77 AN2C 77 | NA             | defined                                                                      |
| type          | Types of the data that were collected with (ER) and without tarps (NEE). | categorical   | ER, NEE, GPP                                                                                                                    | NA             | defined                                                                      |
| flux          | The value of the measured carbon flux                                    | numeric       | -212.288 - 126.234                                                                                                              | mmol/sqm/h     | calculated                                                                   |
| temp_soil     | The mean of the soil temperature in the outer plot.                      | numeric       | 0.825 - 21.608                                                                                                                  | celsius        | Automatically measured by the thermal sensor and recorded to the data logger |
| PARavg        | The mean of the Photosynthetic Active Radiation (PAR) value              | numeric       | -2.819 - 1818.049                                                                                                               | micromol/s/sqm | Automatically measured by the PAR sensor and recorded to the data logger     |
| datetime      | Date and time of the measured carbon flux                                | date_time     | 2022-07-23 21:45:15 - 2022-07-31 08:12:45                                                                                       | NA             | defined                                                                      |
