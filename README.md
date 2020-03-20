[![abaplint](https://app.abaplint.org/badges/Resulto-Consultoria/abap_covid19_api_reader)](https://app.abaplint.org/project/Resulto-Consultoria/abap_covid19_api_reader)
# Reader for Covid19 Rest APIs

Abstract class zcl_covid19_rest_api will be used as base of rest api client to read from different data sources.

Class generate client from defined RFC Destination (HTTP Connections to External Server)

## Data sources
This repository has partial implementations for data sources:

| Reference | RFC Destination | Host | Path prefix |
| ------------- | ------------- | ------------- | ------------- |
| https://github.com/javieraviles/covidAPI  | COVID19_GLOBAL  | coronavirus-19-api.herokuapp.com | / |
| https://github.com/ExpDev07/coronavirus-tracker-api  | COVID19_TRACKER  | coronavirus-tracker-api.herokuapp.com | /v2/ |
| https://rapidapi.com/astsiatsko/api/coronavirus-monitor  | COVID19_RAPIDAPI  | coronavirus-monitor.p.rapidapi.com | /coronavirus/ |
