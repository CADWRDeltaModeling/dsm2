# Verify new North Delta survival equations by comparing to the original survival probabilities
# Doug Jackson
# doug@QEDAconsulting.com
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import datetime as dt
####################################################################################################
# Constants
####################################################################################################
workingDir = "/Users/djackson/Documents/QEDA/DWR/programs/dsm2_master/dsm2/dsm2/src/ptm/development/verifyNorthDeltaSurvEquations/"

# Specify dates to simulate
startDate = "1999-02-01"
endDate = "2017-07-01"
freq = "2W"

figWidth = 6
figHeight = 6
####################################################################################################
# Run
####################################################################################################
os.chdir(workingDir)

with open("ptmConfig_NorthDelta.yaml", "r") as fH:
    configTemplate = fH.read()

dates = pd.date_range(start=startDate, end=endDate, freq=freq).to_pydatetime().tolist()

datesList = list()
oldSurvList = list()
newSurvList = list()
for thisDate in dates:
    thisEndDate = thisDate + dt.timedelta(weeks=20)
    thisInsertionDate = thisDate + dt.timedelta(weeks=2)

    thisStartDate = dt.datetime.strftime(thisDate, "%d%b%Y").upper()
    thisEndDate = dt.datetime.strftime(thisEndDate, "%d%b%Y").upper()
    thisInsertionDate = dt.datetime.strftime(thisInsertionDate, "%m/%d/%Y").upper()

    print(f"Running {thisStartDate} to {thisEndDate}. Insertion date: {thisInsertionDate}")

    config = configTemplate.replace("PTM_START_DATE_PLACEHOLDER", f"{thisStartDate}")
    config = config.replace("PTM_END_DATE_PLACEHOLDER", f"{thisEndDate}")
    config = config.replace("RELEASE_DATE_PLACEHOLDER", f"{thisInsertionDate}")

    with open("ptmConfig.yaml", "w") as fH:
        print(config, end="", file=fH)

    javaCommand = "java -jar ECO_PTM_SouthDelta_01apr25.jar ptmConfig.yaml"

    ret = os.system(javaCommand)

    oldSurv = pd.read_csv(os.path.join(workingDir, "output", "survival.csv"))
    newSurv = pd.read_csv(os.path.join(workingDir, "output", "routeSurvival.csv"))

    datesList.append(thisDate)
    oldSurvList.append(oldSurv["Combined_suv"].values[0])
    newSurvList.append(newSurv["overall"].values[0])

    # Save the results after every loop in case the script is aborted
    results = pd.DataFrame({"dates":datesList,
                            "oldSurv":oldSurvList,
                            "newSurv":newSurvList})
    results.to_csv("compareSurv.csv", index=False)

fig, ax = plt.subplots(figsize=[figWidth, figHeight])
ax.plot(results["oldSurv"], results["newSurv"], "o", alpha=0.75)
ax.plot([0, 1], [0, 1])
ax.set_xlabel("original ECO-PTM survival")
ax.set_ylabel("new survival equations")
plt.savefig("compareSurv.png")
plt.close("all")