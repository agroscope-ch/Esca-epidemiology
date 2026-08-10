# Esca_CH_2001-2022.csv — Variable Description

Dataset of Esca disease observations in Swiss vineyards (2001–2022), with 1328 plot-year records.

Source: <https://zenodo.org/records/21704548>



## Plot identification

| Variable | Description |
|---|---|
| `plot` | Unique plot identifier |
| `region` | Wine-growing region (Chablais, La Cote, Lavaux, Neuchatel, Nord Vaudois, Valais) |
| `commune` | Municipality where the plot is located |
| `E` | Swiss easting coordinate (CH1903/LV03) |
| `N` | Swiss northing coordinate (CH1903/LV03) |

## Observation

| Variable | Description |
|---|---|
| `date_observation` | Date of field observation (YYYY-MM-DD) |
| `Year` | Year of observation (2001, 2004, then biennially 2004–2014, then annually 2015–2022) |

## Vineyard characteristics

| Variable | Description |
|---|---|
| `variety` | Grape variety (e.g. chasselas, gamay, pinot noir, gamaret, …) |
| `age` | Vine age class: jeune (young), moyenne (medium), vieille (old) |
| `training_system` | Vine training system: guyot, gobelet, or Cordon (Co) |

Note that the `age` and `training_system` variables were not collected in the last years of the monitoring. 

## Disease counts per row (4 rows per plot)

Each plot is divided into 4 rows of ~100 vine stocks. The `_1` to `_4` suffixes refer to rows 1–4. Values are counts of affected stocks in each row.

| Variable | Description |
|---|---|
| `Apoplexy_1` … `Apoplexy_4` | Number of stocks showing apoplexy (sudden vine death) |
| `Symptoms_1` … `Symptoms_4` | Number of stocks showing foliar symptoms |
| `Holes_1` … `Holes_4` | Number of dead/missing stocks (holes in the row) |
| `Eutypa_1` … `Eutypa_4` | Number of stocks showing eutypa dieback symptoms |

Note that the variables `Holes_X` and `Eutypa_X` were added not used in the analysis and were not collected in the last years of the monitoring. 

## Aggregated percentages

Percentages computed across the 4 rows (400 stocks per plot).

| Variable | Description |
|---|---|
| `percentage_apoplexy` | Percentage of stocks with apoplexy |
| `percentage_symptoms` | Percentage of stocks with chronic Esca symptoms |
| `percentage_eutypa` | Percentage of stocks with Eutypa dieback |
| `percentage_holes` | Percentage of dead/missing stocks |

## Grouping

| Variable | Description |
|---|---|
| `stable_group` | Year range for which the plot belongs to a stable observation group (plots observed continuously over that period) |
| `group_year` | Year range for which a domain was observed (but some changes in the plots may be possible; see paper for details about the difference with `stable_group`) |


_Note: this file serves as a description of the dataset variables on Zenodo._