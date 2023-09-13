Readme
================
Joseph Gaudard
2022-06-14

# Introduction

We study how trait composition influences ecosystem functioning by
measuring CO2-flux within and across plant communities, mainly focus on
ecosystem carbon dynamics in response to climate change, but also
nitrogen deposition and grazing. Twenty-four hours of carbon Flux
measurement was conducted along the elevational gradients in the four
different sites, which are Vikesland (408 masl, 6 plots), Hogsete (697
masl, 3 plots), Joasete (888 masl, 6 plots), and Liahovden (1.267 masl,
3 plots). Net Ecosystem Exchange (NEE) and Ecosystem Respiration (ER)
along with Photosynthetic Active Radiation (PAR) were measured using
Li-cor LI-840A.

# Data dictionnaries

## CO<sub>2</sub> fluxes

<!-- ### Vikesland -->

| Variable name | Description                                                              | Variable type | Variable range or levels                                                                                                        | Unit           | How measured                                                                 |
|:--------------|:-------------------------------------------------------------------------|:--------------|:--------------------------------------------------------------------------------------------------------------------------------|:---------------|:-----------------------------------------------------------------------------|
| turfID        | The ID of the plot of the carbon flux measurement                        | categorical   | 85 WN1C 162, 105 WN3C 173, 158 WN2C 199, TTC 146, TTC 140, TTC 141, TTC 101, TTC 110, TTC 115, 4 AN1C 4, 27 AN3C 27, 77 AN2C 77 | NA             | defined                                                                      |
| type          | Types of the data that were collected with (ER) and without tarps (NEE). | categorical   | ER, NEE, GPP                                                                                                                    | NA             | defined                                                                      |
| flux          | The value of the measured carbon flux                                    | numeric       | -212.288 - 126.234                                                                                                              | mmol/sqm/h     | calculated                                                                   |
| temp_soil     | The mean of the soil temperature in the outer plot.                      | numeric       | 0.825 - 21.608                                                                                                                  | celsius        | Automatically measured by the thermal sensor and recorded to the data logger |
| PARavg        | The mean of the Photosynthetic Active Radiation (PAR) value              | numeric       | -2.819 - 1818.049                                                                                                               | micromol/s/sqm | Automatically measured by the PAR sensor and recorded to the data logger     |
| datetime      | Date and time of the measured carbon flux                                | date_time     | 2022-07-23 21:45:15 - 2022-07-31 08:12:45                                                                                       | NA             | defined                                                                      |

## Microclimate

<!-- # Figures -->
<!-- ```{r, echo=FALSE, fig.align='left', fig.cap=""} -->
<!-- # graph -->
<!-- ``` -->

# Future potential of the data

Note: those data are representative of a single point in the season
(close to peak productivity), but not necessarily representative of the
entire season or annual carbon balance.
<!-- ## Comparison with daily point measurements -->

-   How is the mean and/or median comparing to the calculated 24h
    fluxes?
-   How much is the peak measurement (usual method) deviating from the
    calculated 24h fluxes?
-   Temperature response curves, species composition of turfs (group 3)
-   SLA / LMA to explain GPP with transplant treatment (or any other
    proxy of biomass)
-   Impacts of traits on flux through 24h
-   Greater diversity leading to greater productivity? Does this hold
    for transplants?
-   Legacy effect versus adaptation on fluxes: is the transplant
    behaving (fluxes wise) more similarly to the local ambient or the
    origin ambient?
-   Fluxes related to conditions and altitude gradient:
    -   gradient of temperature
    -   gradient of PAR?? (group 3, we need answers!)
    -   how are those gradients affecting c fluxes? (using ambient
        turfs)
    -   or is that only an effect of the species composition?
-   Soil respiration: is photosynthesis or soil respiration the main
    driver in variation of NEE accross the altitude gradient?
-   How much are the traits explaining the variability in fluxes? Can
    the altitude gradient and the transplanting explain more of that
    variation?

# Field measurements

## Start of 24h campaign

-   Connect the chamber to the licor and the pump.
-   Place PAR sensor and air temperature in the chamber and connect them
    to the logger.
    <!-- - Connect the soil temperature sensor to the logger. -->
-   Connect the battery and see that the licor is working normally
    (there should be a green light).
-   Connect and arm the logger.
-   Check that the watch you will use to write down starting time is
    synchronized with the logger.
-   Turn on the fan and the pump.
-   Check the value from the licor (should be around 400ppm).

## NEE measurement

-   Check that the fan and pump are working and that all the tubes are
    connected.
-   Check that the logger is armed.
-   Air the chamber.
    <!-- - Put the soil temperature probe in the ground on the edge of the inner plot. -->
-   Put the chamber on the inner plot and apply the chain on the skirt
    of the chamber for airtightness.
-   Write down the exact time at which the measurement started (with a
    10 seconds resolution).
-   Leave it undisturbed for 3 minutes. Be careful to not move around
    (might push gases out of the ground) and to not shade the chamber.
-   Write down when the measurement ended.
-   Remove the chain and the chamber, and air the chamber.
-   To save battery, you can turn off the fan and the pump in between
    measurements (but do not turn off the licor!)

## ER measurment

Cover the chamber with the dark tarp before measuring and do the same as
for NEE.

## Environmental measurements

Air temperature and PAR values will be logged directly in the same
dataset as the cflux. Soil moisture and temperature will be measured
using TMS microclimate loggers.
<!-- Should we measure soil moisture after each measurements? I think yes, but it might be destructive (because of the pins we have to put in the plot each time)? -->

## At the end of the 24h campaign

-   Let the logger and licor run 1 minute after the last measurement
-   Turn off the fan and pump
-   Disarm the logger.
-   Disconnect everything for transport.
-   Take a picture of the field data sheet.

## Changing batteries of the licor (scooter battery)

<!-- The licor will have an orange light when it needs a new battery (and you will hear the pump struggling too). You can finish the current measurement but will have to change the battery afterwards. -->

The battery has a button that you can briefly press to see the battery
state. You should change the battery when it reaches 25% (the last
liht).

1.  Turn off the fan and pump.
2.  Disconnect both cables from the old battery.
3.  Connect the new battery (Connect the correct colors or walk down the
    mountain to fetch the other licor!)
4.  Turn on the fan and pump and check that everything works normally.
5.  Wait a minute before starting a new measurement.

## Changing batteries of the logger

1.  Turn off the fan and pump
2.  Disarm the logger
3.  Disconnect the logger
4.  Change the batteries
5.  Connect the logger
6.  Check the logger’s clock again
7.  Arm the logger
