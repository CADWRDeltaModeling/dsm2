# Update DSM2 Historical Simulation

## Introduction

The historical simulation of Delta Simulation Model II (DSM2) simulates the ground truth of Sacramento-San Joaquin Delta hydrodynamics and water quality. It requires collecting observed flows, stages, and water quality at model boundaries, as well as actual gate operations. Missing inputs must be filled to ensure DSM2 runs successfully. This document outlines the procedures for preparing inputs for DSM2 historical simulation. All required scripts and information are included in the package located at `\\nasbdo\Delta_Mod\Share\lanliang\Update_DSM2_package`.

## Prerequisites

1. **Command Window Access**: Follow [these instructions](https://www.windowscentral.com/add-open-command-window-here-back-context-menu-windows-10) to add the 'Open command window here' option to Windows Explorer.
2. **HEC-DSSVue**: Download and install [HEC-DSSVue](http://www.hec.usace.army.mil/software/hec-dssvue/downloads.aspx).
3. **Vtools**: Download and install [Vtools](http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/models/vtools/install_vtools.html#prerequisites).
4. **Python 2.7**: Download and install Python 2.7 from [Python](https://www.python.org/downloads/) or [Anaconda](https://www.anaconda.com/download/).

## Schematic of Updating Procedures

The process involves four major steps:

1. **DICU Update**: Retrieve CIMIS data, prepare precipitation and evapotranspiration data, and update input files in the DICU model.
2. **DSM2 Boundary Inputs Update**: Retrieve boundary data from CDEC and ancillary data for QA/QC.
3. **Martinez Boundaries Update**: Update stage and EC data for Martinez.
4. **Final QA/QC**: Conduct quality checks and finalize updates.

Refer to the detailed steps in the shared document `Update DSM2 Historical Simulation.docx` for comprehensive guidance.

## Step 1: DICU Update (under the folder /DICU)

### 1. Retrieve CIMIS data

- Log in to [CIMIS](http://wwwcimis.water.ca.gov/Auth/Login.aspx) with the user name `wildej` and password `delta`, and click `DATA` and then `My Reports`.
- Under the list `Quick Reports`, click `Execute monthly Report`, `List 1`, and then the report is loaded in an Excel spreadsheet. This report includes the precipitation and reference ET and other climatic data at stations Davis(6), Brentwood(47), Manteca(70), Twitchell(140), Lodi West(166), Tracy(167). Right now, the data at Lodi West are missing on that spreadsheet.
- Log off CIMIS and close the browser.

### 2. Prepare precipitation and evapotranspiration data in DICU-YYYYMM.xlsm

- Delete the second and third columns in the downloaded spreadsheet, and copy the data into [DICU-YYYYMM.xlsm](file:///Z:/lliang/cdec_data_retrieve/DSM2_updates/Update_DSM2/DICU/DICU-201712.xlsm) on `CIMISData` sheet with the same format.
- Check data on `PrecipLookupToDSS` sheet are correctly linked to the data on `CIMISData` sheet. Sometimes, the data is not available in some months or some stations on `CIMISData`. When the downloaded spreadsheet is pasted on the `CIMISData` sheet, the precipitation and reference ET on `CIMISData` will be automatically copied to another sheet `PrecipLookupToDSS`. In order to make the data automatically transfer from `CIMISData` to `PrecipLookupToDSS`, the rows of the missing data in `CIMISData` have to be filled with blanks.
- Extend the data on `TimeSeries` sheet, and keep the same formulas on each column. The long-term mean values in the columns, such as “Mean Evap-ET”, “DICU Ave Evap(mm)” and “Hist Ave Evap”, could repeat the same values as those in the previous year.
- Save the spreadsheet. Do not close the file. It will be used for updating the precipitation and reference ET for the DICU model.

### 3. Update the input files in DICU model

- Go to [DICU/Precip/7STATION-DAT-Y2K](file:///D:/delta/dsm2_v8/studies/hist2015/DICU/DICU/PRECIP/7STATION-DAT-Y2K), open the Precip input file 7STAPREC.WY20XX. The file saves the monthly precipitation in the water year 20XX at seven stations: Davis, Rio Vista, Brentwood, Tracy, Stockton, Lodi and Galt. CIMIS has not collected the precipitation data at Galt for recent several decades. Update the precipitation with the same station names from the spreadsheet `PrecipLookupToDSS`, and copy the same precipitation at Lodi into the column of Galt.
- Starting from WY2015, the Lodi West data from CIMIS is missing, so the Lodi precipitation after April 2015 is downloaded from [the National Centers for Environmental Information](http://ipm.ucanr.edu/calludt.cgi/PCSTATIONDATA?MAP=&STN=LODI-01.P1440&SCALE=Daily), University of California Agriculture & Natural Resources (UCANR). Pasted the downloaded Lodi precipitation on the spreadsheet `PrecipLookupToDSS`.
- Update the Precip file located at `DICU/PRECIP/7STATION-DAT-Y2K`. If some of the downloaded Lodi data are missing, the precipitation at Stockton can be taken as the substitute, since Stockton is the nearest available station to Lodi.
- Save the Precip file. (If starting a new water year, make sure to copy the file for the next water year with the appropriate name of the next water year + 1 so the average precip information will not be lost!)
- Water year type. Go to [the website](http://cdec.water.ca.gov/cgi-progs/iodir/WSIHIST) to get the water year type, and go to the folder `DICU/DICU5IN1/1922-20**(the current year to update)`, and update WYTYPES file with textpad.
- Pan evaporation. Go to [DICU/PAN_EVAP](file:///D:/delta/dsm2_v8/studies/hist2015/DICU/DICU/PAN-EVAP), update README-2YYY.txt file with the Manteca pan evap (in and mm), AVE EVAP of DICU_YYYYMM.xlsm and ET ADJ FACTR from the “TimeSeries” worksheet of DICU_YYYYMM.xlsm to keep a record of the data used.
- Update DICU5.5 txt file. Go to `DICU/DICU5/1922-20**`, update DICU5.5 txt file at the bottom. You will want to update the water year type and the ET adjustment factors which are in rows where the values are from column in the “TimeSeries” worksheet of DICU_YYYYMM.xlsm. Also remember this is by water year and the data spacing must not change.

### 4. Run DICU

- Go to `DICU/DICU5IN1/1922-20**`, update dicu5in1.py. The lines marked with “Update here!” must be updated. Open the command window, and run:

        python dicu5in1.py

- Go to `DICU/DICU5/1922-20**`, update and run the python script run-dicu5.py. The lines marked with “update folder name” must be updated. Open the command window, and run:

        python run-dicu5.py

- Go to `DICU/NODCU/NODCU12/1922-20**`, update and run the python script bat1922-20**.py. The lines marked with “update folder name”, “update the year”, and “update the month” must be updated. Open the command window, and run:

        python bat1922-20**.py

- Copy the file DICU_YYYYMM.dss from `/DICU/NODCU/NODCU12/1922-20**/` to the folder `/timeseries` of the DSM2 historical simulation.

## Step 2: DSM2 Boundary Inputs Update (under the folder /DSM2_flow_stage_EC_input)

### 1. Boundary inputs from CDEC

- Most DSM2 boundary data can be retrieved from CDEC. The python script Retrieve_data_fromCDEC.py can retrieve the data of CDEC stations as an input text file defines, and write the retrieved data into a DSS file.

The input text file is a queue of the information of the requested CDEC stations, and each line in the file contains the station ID, sensor number, and duration of one CDEC station.

The data in the DSS file is the raw data with data gaps frequently. To conduct writing the timeseries in the DSS file without interruptions, they are defined as irregular timeseries.

Get a command prompt window under the folder /DSM2_flow_stage_EC_input, and type the line below to retrieve the DSM2 boundary data.

        Python Retrieve_data_fromCDEC.py arg1 arg2 arg3 arg4

where

Arg1 – The first argument, the text file of the CDEC station information. To retrieve DSM2 boundaries, Arg1 is `Delta_boundaries.txt`. The DSM2 boundary flows, stages, and ECs, from CDEC are included in this text file. Table 1 lists the DSM2 boundaries, and their corresponding CDEC stations and related information, which the text file has included. CDEC does not have the DSM2 boundary, Mokelumne River inflow.

Arg2 – The second argument, the starting date of the data, formatted as mm/dd/yyyy.

Arg3 – The third argument, the ending date of the data, formatted as mm/dd/yyyy.

Arg4 – The fourth argument, the name of the DSS file that stores the retrieved data.

Below is an example to download the DSM inputs from 1/1/2017 through 12/31/2017 and write the retrieved data into a DSS file named as update201712.dss.

        Python Retrieve_data_fromCDEC.py Delta_boundaries.txt 1/1/2017 12/31/2017 update201712.dss

#### Table 1 The CDEC stations to retrieve data as the DSM2 inputs

| CDEC station | RKI name | DSM2 input type | Input location | Time interval |
|--------------|----------|-----------------|----------------|---------------|
| LIS          | BYOLO040 | flow            | Yolo Bypass    | 1 DAY         |
| YBY          | BYOLO040 | flow            | Yolo Bypass    | 1 DAY         |
| VNS          | RSAN112  | flow            | San Joaquin River at Vernalis | 1 DAY |
| FPT          | RSAC155  | flow            | Sacramento River at Freeport | 1 DAY |
| NHG          | RCAL009  | flow            | Calaveras River at Stockton | 1 DAY |
| MHB          | RCSM075  | flow            | Cosumnes River at Michigan Bar | 1 DAY |
| HRO          | CHSWP003 | export          | Banks pumping | 1 DAY |
| TRP          | CHDMC004 | export          | Jones (Tracy) pumping | 1 DAY |
| BKS          | SLBAR002 | export          | North Bay Aqueduct | 1 DAY |
| CCW          | CHVCT001 | export          | Middle River pumping | 1 DAY |
| IDB          | ROLD034  | export          | Old River pumping near Discovery Bay | 1 DAY |
| INB          | CHCCC006 | export          | Rock Slough pumping near Brentwood | 1 DAY |
| MRZ          | RSAC054  | stage           | Martinez | 15 MIN |
| MRZ          | RSAC054  | EC              | Martinez | 1 HOUR |
| SRH          | RSAC139  | EC              | Sacramento River at Hood | 1 DAY |
| VER          | RSAN112  | EC              | Vernalis | 1 DAY |

- QA/QC of all the data in the DSS file except Martinez stage and EC. Conduct QA/QC and remove errors for those timeseries. Martinez stage and EC will be processed independently after Step 2.
- Use HEC-DSSVue to fill the data gaps. HEC-DSSVue can automatically fill the gaps with several time steps missing by interpolation when converting irregular to regular timeseries. There is another way to fill the gaps with longer intervals. Go to the menu of HEC-DSSVue, and click: Tools -> Math Functions -> General -> Estimate missing values.
- Use HEC-DSSVue to convert the irregular timeseries into regular timeseries. Go to the menu of HEC-DSSVue, and click: Tools -> Math Functions -> Time Functions -> select operator: min/max/avg/…over period -> select function type: average over period -> select new period intervals as Table 1 -> Compute -> save with the default pathnames.
- Copy and paste the file update201712.dss with regular timeseries into the folder /merge_data.

### 2. Retrieve ancillary data from CDEC

- Call the same Python script Retrieve_data_fromCDEC.py to retrieve extra data for QA/QC, filling data gaps at Martinez, and checking the gates operation times. The CDEC stations for fulfilling those functions are listed in the text file, ancillary_stations.txt. Below is an example to download the ancillary data from 1/1/2017 through 12/31/2017 and write the retrieved data into a DSS file named as ancillary201712.dss.

        Python Retrieve_data_fromCDEC.py ancillary_stations.txt 1/1/2017 12/31/2017 ancillary201712.dss

### 3. Download data from other sources

- San Francisco stage for updating Martinez stage

Go to the website: [NOAA Tides and Currents](http://tidesandcurrents.noaa.gov/waterlevels.html?id=9414290)

Choose the options:

        Units: Feet
        Timezone: LST
        Datum: MLLW
        Interval: Hourly
        Update: Data Only

When the data list shows on the screen, click the button: Export to CSV, to save the data in a csv file. Load the csv data in HEC-DSSVue, and convert its datum from MLLW to NGVD by NGVD = MLLW - 2.64 feet

- Yolo Bypass Flow

Yolo Bypass is a wide-open area, so it is hard to investigate the actual flows in this region. There is no flow station at the DSM2 boundary location. As a boundary flow in DSM2, Yolo Bypass flow has been assumed to equal the aggregation of the flows collected from those stations near Yolo Bypass, like the Yolo Bypass flow (QYOLO) from [DAYFLOW](https://www.water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/Environmental-Services/Compliance-Monitoring--Assessment/Dayflow/Files/Publications/Current-Dayflow-Documentation.pdf) as

        QYOLO = Yolo Bypass flow at Woodland + Sacramento Weir Spill + South Fork Putah Creek flow

These three flows can be retrieved from CDEC station YBY, USGS station 11426000 (SACRAMENTO WEIR SPILL TO YOLO BYPASS NR SAC CA), and CDEC station PUT. The last two stations have been inactive for recent years, so YBY is the unique effective station collecting the Yolo Bypass flow.

However, DSM2 v6.0 historical simulation update tool took the flow at CDEC station RUM (Cache Creek at Rumsey Bridge) as the Yolo Bypass flow. It is not appropriate, especially the inflow to the Sacramento River during summers. Generally, during summers and falls Yolo Bypass has toe drain instead of inflow to the Sacramento River, and during winters and springs it functions as a diversion to reduce the Sacramento River floods.

DSM2 input data version is based on the timeseries ending time. Starting from the version December 2017 (12/2017), the RUM flow from 2006 through current has been replaced by the available observed flows at YBY and LIS. The old input data versions keep the RUM flow.

Furthermore, from the version 12/2017, CDEC station LIS flow is taken as the Yolo Bypass flow from June to November every year, while CDEC station YBY flow is taken as the Yolo Bypass flow from December to next May. If the Sacramento River floods diverted into Yolo Bypass come earlier than December or after May, YBY flow might be accounted as the Yolo Bypass flow of the DSM2 inputs in those months.

The combination of YBY and LIS flows in the file `update201712.dss` under the folder /DSM2_flow_stage_EC_input is conducted in HEC-DSSVue. Open `update201712.dss` in HEC-DSSVue, set the time window December 1st - May 31st, select the daily YBY flow, duplicate the daily YBY flow and rename it as `/CDEC/LIS/FLOW//1DAY/20_E/`. It overwrites the daily LIS flow from December through May. After that, the daily LIS flow represents Yolo Bypass flow and will be merged with the previous version of DSM2 inputs.

- Mokelumne River flow

Contact the staff in East Bay Municipal Utility District (EBMUD) directly. Their website only presents the flow at Mokelumne River below WID for the last seven days:

[EBMUD Daily Water Supply Report](http://www.ebmud.com/water-and-drought/about-your-water/water-supply/water-supply-reports/daily-water-supply-report/)

Kevin Fung [kevin.fung@ebmud.com](mailto:kevin.fung@ebmud.com) has been contacted for the past several years and provided the raw data of the current year.

The QA/QC’d Mokelumne River flow of the previous years, which EBMUD sent to USGS, can be downloaded from USGS website.

Once Mokelumne River flow is received, use HEC-DSSVue to load and save it in the DSS file `/DSM2_flow_stage_EC_input/updateYYYYMM.dss`. Here MM and YY/YYYY are the month and year of the updated version, and set the pathname of this time series as

        /FILL+CHAN/RMIL070/FLOW//1DAY/DWR-DMS-YYYYMM/

## Step 3: Martinez Boundaries Update

### 1. Martinez Stage (DATUM: NAVD88; under the folder /MTZ_stage_EC/fill_stage)

- Remove stage data errors. Copy Martinez CDEC stage from `/DSM2_flow_stage_EC_inputs/update201712.dss` to a new DSS file, for example `MTZ_201712.dss`, under the folder `/MTZ_stage_EC/fill_stage`. Check the data in the file `MTZ_201712.dss`, remove errors in the stage timeseries, record the time window of each data gap at the end of the text file `input.txt`, and delete the out-of-date time windows in the file `input.txt`.
- One hour shift of Daylight saving time. Compare the astronomical tide and CDEC retrieved stage at Martinez, select the time window of one hour shift due to Daylight saving time, and shift one hour to match the phases of astronomical tides. The time shift can be conducted in HEC-DSSVue under the menu Tools -> Math Functions -> Time Functions -> Operator: Shift in Time. Record the data gaps in March and November at the end of `input.txt` because of the time shift.
- Prepare the input file. Prepare the input text file (`input.txt`) for the python script (`fillgaps.py`) to fill the stage gaps. Below is an example of the file `input.txt`. It includes the data version (tmark), names of input and output DSS files, the pathnames of timeseries used in this gap filing, and the time windows of the stage data gaps found. The time windows are suggested to be longer than one day or two days to fill the gaps smoothly.

![sample_file_1.png](attachments/87228633/87228635.png)

- Run the python script:

        Python fillgaps.py input.txt

### 2. Martinez EC (under the folder /MTZ_stage_EC /fill_EC)

- Remove the errors in the observed 15-minute EC data. Copy Martinez 15-minute EC data from `/DSM2_flow_stage_EC_inputs/update201712.dss` to a new DSS file, for example `MTZ_201712.dss`, under the folder `/MTZ_stage_EC/fill_EC`. Open `MTZ_201712.dss`, find and remove the errors in the Martinez 15-minute EC data, and record the data gaps in the text files `.\timewindows_ec_ave.txt`.
- Average the filtered 15min MRZ EC to hourly MRZ EC and save part F of the pathname as `/100_E_AVE/`. Then the hourly MRZ EC has the pathname as

        /CDEC/MRZ/EC//1HOUR/100_E_AVE/

- Copy Mallard hourly and daily EC data from `/DSM2_flow_stage_EC_inputs/ ancillary201712.dss` to the same DSS file `/MTZ_stage_EC/fill_EC/MTZ_201712.dss`.
- Calculate NDOI by combining 6 inflows, 6 exports and Delta consumptive use.

1) In the file `/DSM2_flow_stage_EC_input/updateYYYYMM.dss`, use HEC-DSSVue to sum up the six inflows into one total inflow with the pathname

        Path1: /CDEC/TOTAL/FLOW//1DAY/20_H/.

The pathnames of the inflows are listed below,

        Sacramento inflow: /CDEC/FPT/FLOW//1DAY/20_H/
        San Joaquin inflow: /CDEC/VNS/FLOW//1DAY/20_E/
        Yolo Bypass inflow: /CDEC/LIS/FLOW//1DAY/20_E/
        Calaveras inflow: /CDEC/NHG/FLOW//1DAY/23_H/
        Cosumnes inflow: /CDEC/MHB/FLOW//1DAY/20_H/
        Mokelumne inflow: /FILL+CHAN/RMIL070/FLOW//1DAY/DWR-DMS-YYYYMM/

2) Also sum up the six exports into one total export with the pathname

        Path2: /CDEC/TOTAL/FLOW_EXPORT//1DAY/70_D/.

The pathnames of the six exports are listed below,

        SWP: /CDEC/HRO/FLOW_EXPORT//1DAY/70_D/
        CVP: /CDEC/TRP/FLOW_EXPORT//1DAY/70_D/
        North Bay aqueduct: /CDEC/BKS/EXPORT//1DAY/70_D/
        Old River near Brentwood: /CDEC/INB/FLOW_EXPORT//1DAY/70_D/
        Old River near discovery bay: /CDEC/IDB/FLOW_EXPORT//1DAY/70_D/
        Middle River: /CDEC/CCW/FLOW_EXPORT//1DAY/70_D/

3) Calculate the total inflow Path1 minus the total export Path2, and save it as the timeseries with the pathname

        Path3: /CDEC/TOTAL/FLOW //1DAY/INFLOWS-EXPORTS/

4) Copy the timeseries with Path3 to the DSS file `/MTZ_stage_EC/fill_EC/MTZ_201712.dss`

5) Copy the latest version of dicu_YYYYMM.dss from the folder `/DICU/NODCU/NODCU12/1922-2017` to the folder `/MTZ_stage_EC/fill_EC`.

6) Use HEC-DSSVue to open file `/MTZ_stage_EC/fill_EC/dicu_YYYYMM.dss`, and sum up all the timeseries with part C DIV-FLOW to one timeseries with

        Path 4: /DICU-HIST+NODE/TOTAL/DIV-FLOW//1MON/DWR-BDO/

7) Sum up all the timeseries with part C DRAIN-FLOW to one timeseries with

        Path 5: /DICU-HIST+NODE/TOTAL/DRAIN-FLOW//1MON/DWR-BDO/

8) Sum up all the timeseries with part C SEEP-FLOW to one timeseries with

        Path 6: /DICU-HIST+NODE/TOTAL/SEEP-FLOW//1MON/DWR-BDO/

9) Calculate Delta consumptive use by Path 4+Path6-Path5, and save it as one timeseries with the pathname

        Path 7: /DICU-HIST+NODE/TOTAL/FLOW//1MON/DWR-BDO/

10) Convert the timeseries with path 7 into daily data,

        Path 8: /DICU-HIST+NODE/TOTAL/FLOW//1DAY/DWR-BDO/

and copy the daily one to the DSS file `MTZ_stage_EC/fill_EC/MTZ_201712.dss`

11) Open file `MTZ_201712.dss`, and calculate NDOI by subtracting the timeseries with Path8 from that with Path 3. The NDOI data has the pathname

        Path 9: "/FILL+CHAN/NDOI/FLOW//1DAY/ DWR-DMS-YYYYMM/

Here YYYY and MM are the year and month of the updated version.

- Prepare the input file. Prepare the input text file, such as `timewindows_ec_ave.txt`, for the python script (`fillgaps_ec.py`) to fill the EC gaps. Below is an example of the file `timewindows_ec_ave.txt`. It includes the data version (tmark), names of input and output DSS files, and the time windows of the data gaps found.

![MRZ_EC_input.png](attachments/87228633/87228634.png)

- Run the Python script, and obtain the output file filled.dss.

        Python fillgaps_ec.py timewindows_ec_ave.txt

### 3. Merge the updated data and the previous version of DSM2 input data

- After the data gaps have been filled, copy QA/QC’d Martinez 15-minute stage and 1-hour EC from the folders `/MTZ_stage_EC/fill_EC` and `/MTZ_stage_EC/fill_stage` into the file `/DSM2_flow_stage_EC_inputs/updateYYYYMM.dss`.
- When all the input timeseries have been QA/QC’d, the previous version of DSM2 input data must be extended with the updated data in the file `/DSM2_flow_stage_EC_inputs/updateYYYYMM.dss`. Copy the previous version of DSM2 input data, `histMMYY.dss`, into the folder `/merge_data`, update the names of merged files, the name of output file, and the versions in the file `pathnames.txt`, and run the Python script:

        Python merge_data.py pathnames.txt

- Check all the pathnames in the latest version match those in DSM2 inp files.

## Step 4: Gate operations update (under the folder \gateoperations)

### 1. Delta Cross Channel

Download the gate operations from [USBR](https://www.usbr.gov/mp/cvo/vungvari/Ccgates.pdf)

### 2. Clifton Court gate

Ask Liu, Siqing (Siqing.Liu@water.ca.gov) from O&M. O&M collects the inputs without QA/QC and updates DSM2 monthly, so their inputs can be taken as the preliminary inputs.

### 3. South Delta temporary barriers and Montezuma Slough gate

Go to [South Delta Temporary Barriers](http://baydeltaoffice.water.ca.gov/sdb/tbp/web_pg/tempbsch.cfm), or ask Michal Burn, South Delta Section to get the temporary barriers gate operations, or ask O&M. The available values of the gate parameters are listed in the file `\gateoperations\barriers_values_03082012.xlsx`. The Vertical Datum in the Excel file is NGVD29, while that in the gate operation DSS file is NAVD88. When the DSS file is updated, the Datum difference must be counted.

### 4. Tune the gate operation times

All the information of gate operations collected above are added in `\gateoperations\gate-v8-YYYYMM.dss`. The collected gate operation schedules are normally not the actual gate operations. After all the inputs of DSM2 historical simulation are prepared, pre-run DSM2 HYDRO and check if the simulated upstream and downstream stages/flows of each gate match the observed stages/flows. If not, tune the gate operation times until the simulated stage variations in time reflect the gate operation schedules accurately. Table 2 is the list of CDEC stations to check the gate operation schedules. CDEC stages or flows in the table have been downloaded and saved in the file `ancillary201712.dss` in Step 2.

#### Table 2 The CDEC stations to check gate operations

| Barriers            | Upstream   | RKI or Channel No          | CDEC station |
|---------------------|------------|----------------------------|--------------|
| Old River @ Head    | Upstream   | Mossdale                   | MSD          |
|                     | Downstream | Channel 55, ROLD074        | OH1          |
| Old River at Tracy  | Upstream   | ROLD047                    | OAD          |
|                     | Downstream | ROLD046                    | OBD          |
| Grant Line Canal    | Upstream   | Channel 205                | DGL          |
|                     | Downstream | CHGRL009                   | GLC          |
| Middle River        | Upstream   | RMID027                    | MTB          |
|                     | Downstream | Channel 135                | No station   |
| Delta Cross Channel | Downstream | Channel 365                | DLC          |
| Montezuma Slough    | Upstream   | Collinsville at Sac. River | CSE          |
|                     | Downstream | Roaring River              | MSL          |

## Attachments:

![plots_for_chapters.png](attachments/87228633/87228632.png) (image/png)  
![MRZ_EC_input.png](attachments/87228633/87228634.png) (image/png)  
![sample_file_1.png](attachments/87228633/87228635.png) (image/png)
