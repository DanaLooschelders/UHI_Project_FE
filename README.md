# UHI_Project_FE
These scripts build a random forest model to model the air temperature in the city of muenster.

the air temperature measurements of Thermochron iButtons and Netamto weather stations were used as training data

The following predictors are used
    * Building height
    * Sd of building height
    * Sky View Factor
    * Albedo
    * Water and Wetness (https://land.copernicus.eu/pan-european/high-resolution-layers/water-wetness/status-maps/water-wetness-2018)
    * NDVI
    * Tree cover density (https://land.copernicus.eu/pan-european/high-resolution-layers/forests/tree-cover-density/status-maps/tree-cover-density-2018)
    * Impervious Density 2018 (https://land.copernicus.eu/pan-european/high-resolution-layers/imperviousness/status-maps/imperviousness-density-2018)
    * Temperature (Weather station Steinfurter Str. Münster)
    * Relative Humidity (Weather station Steinfurter Str. Münster)
    * Stability (Weather station Steinfurter Str. Münster)
    * Windspeed (Weather station Steinfurter Str. Münster)
    * Wind direction (Weather station Steinfurter Str. Münster)
    * Cloud cover (Weather station GeoDach Münster)
