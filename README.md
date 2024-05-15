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

| Variable name  | Description                                                                                                                | Variable type | Variable range or levels                  | Unit                | How measured |
|:---------------|:---------------------------------------------------------------------------------------------------------------------------|:--------------|:------------------------------------------|:--------------------|:-------------|
| turfID         | Unique ID of vegetation turf as origSiteID, treatments and destSiteID                                                      | categorical   | 105 WN3C 173 - TTC 146                    | NA                  | defined      |
| type           | Types of the CO2 flux data: GPP = Gross Primary Productivity, NEE = Net ecosystem exchange, and ER = ecosystem respiration | categorical   | ER - NEE                                  | NA                  | defined      |
| flux           | Value for CO2 flux with recommendations from flag applied. Used to calculate GPP.                                          | numeric       | -97.022 - 130.504                         | mmol m-2hr-1        | calculated   |
| temp_soil      | Mean soil temperature measured during flux measurements.                                                                   | numeric       | 0.84 - 32.13                              | °C                  | recorded     |
| temp_airavg    | Mean air temperature measured during the flux measurments. Not recommended for microclimate analyses.                      | numeric       | 0.807 - 32.454                            | °C                  | recorded     |
| PARavg         | Mean Photosynthetic Active Radiation (PAR)                                                                                 | numeric       | -2.843 - 1840.27                          | µmol s-1sqm-1       | recorded     |
| datetime       | Date and time of the carbon flux measurment                                                                                | date_time     | 2022-07-23 21:45:15 - 2022-07-31 08:12:45 | yyyy-mm-dd hh:mm:ss | recorded     |
| fluxID         | Unique identifier for each flux measurement                                                                                | numeric       | 1 - 288                                   | NA                  | defined      |
| flag           | Flagging missing or unreliable data. This information can be used to sort unwanted observations.                           | categorical   | discard - zeroNEE                         | NA                  | defined      |
| flux_noflag    | Flux as calculated with the slopes. No value replaced with flags. No GPP calculated.                                       | numeric       | -4931.3 - 28252.31                        | mmol m-2 hr-1       | calculated   |
| time           | Time of the carbon flux measurment (independent of date)                                                                   | NA            | NA                                        | hh:mm:ss            | recorded     |
| flux_corrected | Flux corrected for CO2 accumulation in canopy at night. We recommend using that one.                                       | numeric       | -141.365 - 130.504                        | mmol /m^-2 /hr-1    | calculated   |
| warming        | Warming treatment with W for warming or A for ambient                                                                      | categorical   | A - W                                     | NA                  | defined      |
| origSiteID     | Unique site ID of origin site                                                                                              | categorical   | Hog - Vik                                 | NA                  | defined      |
| destSiteID     | Unique site ID of destination site (the site they were measured at)                                                        | categorical   | Hog - Vik                                 | NA                  | defined      |

## Microclimate

| Variable name    | Description                                                                                              | Variable type | Variable range or levels           | Unit                            | How measured |
|:-----------------|:---------------------------------------------------------------------------------------------------------|:--------------|:-----------------------------------|:--------------------------------|:-------------|
| datetime         | Date and time of measurement                                                                             | date_time     | 2022-07-23 00:15:00 - 2022-08-08   | yyyy-mm-dd hh:mm:ss             | recorded     |
| destSiteID       | Unique site ID of destination site                                                                       | categorical   | Hogsete - Vikesland                | NA                              | defined      |
| loggerID         | Unique climate logger ID                                                                                 | numeric       | 94194604 - 95221150                | NA                              | defined      |
| turfID           | Unique ID of vegetation turf as originplotID, treatments and destinationplotID                           | categorical   | 100 AN5M 100 - TTC 146             | NA                              | defined      |
| warming          | Warming treatment with W for warming or A for ambient                                                    | categorical   | A - W                              | NA                              | defined      |
| climate_variable | Microclimate variable including air_temperature, ground_temperature, soil_temperature, and soil_moisture | categorical   | air_temperature - soil_temperature | NA                              | defined      |
| value_original   | Temperature or moisture reading including values later flagged as suspect                                | numeric       | -1 - 35.625                        | °C, (m3 water × m−3 soil) × 100 | recorded     |
| value            | Temperature or moisture reading with suspect values replaced with NA                                     | numeric       | -1 - 34.625                        | °C, (m3 water × m−3 soil) × 100 | recorded     |
| flag             | Warning for values that exceed expected parameters                                                       | categorical   | cut_max_moist - keep               | NA                              | defined      |

<!-- # Figures -->
<!-- ```{r, echo=FALSE, fig.align='left', fig.cap=""} -->
<!-- # graph -->
<!-- ``` -->

# Future potential of the data

Note: those data are representative of a single point in the season
(close to peak productivity), but not necessarily representative of the
entire season or annual carbon balance.
<!-- ## Comparison with daily point measurements -->

- How is the mean and/or median comparing to the calculated 24h fluxes?
- How much is the peak measurement (usual method) deviating from the
  calculated 24h fluxes?
- Temperature response curves, species composition of turfs (group 3)
- SLA / LMA to explain GPP with transplant treatment (or any other proxy
  of biomass)
- Impacts of traits on flux through 24h
- Greater diversity leading to greater productivity? Does this hold for
  transplants?
- Legacy effect versus adaptation on fluxes: is the transplant behaving
  (fluxes wise) more similarly to the local ambient or the origin
  ambient?
- Fluxes related to conditions and altitude gradient:
  - gradient of temperature
  - gradient of PAR?? (group 3, we need answers!)
  - how are those gradients affecting c fluxes? (using ambient turfs)
  - or is that only an effect of the species composition?
- Soil respiration: is photosynthesis or soil respiration the main
  driver in variation of NEE accross the altitude gradient?
- How much are the traits explaining the variability in fluxes? Can the
  altitude gradient and the transplanting explain more of that
  variation?

# Field measurement instructions

## Start of 24h campaign

- Connect the chamber to the licor and the pump.
- Place PAR sensor and air temperature in the chamber and connect them
  to the logger.
  <!-- - Connect the soil temperature sensor to the logger. -->
- Connect the battery and see that the licor is working normally (there
  should be a green light).
- Connect and arm the logger.
- Check that the watch you will use to write down starting time is
  synchronized with the logger.
- Turn on the fan and the pump.
- Check the value from the licor (should be around 400ppm).

## NEE measurement

- Check that the fan and pump are working and that all the tubes are
  connected.
- Check that the logger is armed.
- Air the chamber.
  <!-- - Put the soil temperature probe in the ground on the edge of the inner plot. -->
- Put the chamber on the inner plot and apply the chain on the skirt of
  the chamber for airtightness.
- Write down the exact time at which the measurement started (with a 10
  seconds resolution).
- Leave it undisturbed for 3 minutes. Be careful to not move around
  (might push gases out of the ground) and to not shade the chamber.
- Write down when the measurement ended.
- Remove the chain and the chamber, and air the chamber.
- To save battery, you can turn off the fan and the pump in between
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

- Let the logger and licor run 1 minute after the last measurement
- Turn off the fan and pump
- Disarm the logger.
- Disconnect everything for transport.
- Take a picture of the field data sheet.

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
