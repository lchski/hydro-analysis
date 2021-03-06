
## Downloading data

Hydro: <https://secure.hydroottawa.com/Usage/Secure/TOU/DownloadMyData.aspx>

For the hourly climate data (adapted from [ECCC instructions](https://drive.google.com/drive/folders/1WJCDEU34c60IfOnG4rv5EPZ4IhhW9vZH)), run this in `data/source/climate.weather.gc.ca/`:

```
for year in `seq 2017 2020`;do for month in `seq 1 12`;do wget --content-disposition "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=49568&Year=${year}&Month=${month}&Day=14&timeframe=1&submit= Download+Data" ;done;done
```

`seq 2017 2020` sets the start/end years. `stationID=49568` gives us the YOW weather data.

There'll be a bunch of parsing errors for any months not yet arrived in the year.
