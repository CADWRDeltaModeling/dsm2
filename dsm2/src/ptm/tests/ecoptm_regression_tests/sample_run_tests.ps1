# Activate an environment with pytest
conda activate ecoptm_regression_tests

$JAVA_CODE_ROOT_DIR = "C:/delta/DSM2-8.3.2-win64/bin"
$JAVA_HOME = "C:/delta/DSM2-8.3.2-win64/bin/jre"
$CONFIG_FILE = "D:/QEDA/DWR/ECO_PTM_runs/test_DWR_ECO_PTM_24apr22/ptm_config_ecoptm_regression_tests.inp"

# Test ECO-PTM
pytest -s --javaCodeRootDir=$JAVA_CODE_ROOT_DIR --javaHome=$JAVA_HOME --configFile=$CONFIG_FILE .\tests\test_dsm2_regression_ecoptm.py

# Regenerate test outputs
# pytest -s --javaCodeRootDir=$JAVA_CODE_ROOT_DIR --javaHome=$JAVA_HOME --configFile=$CONFIG_FILE --force-regen .\tests\test_dsm2_regression_ecoptm.py

# Deactivate the environment
conda deactivate
