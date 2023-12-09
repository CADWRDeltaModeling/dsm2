""" Test configurations"""
__author__ = "Doug Jackson, QEDA Consulting, LLC"
__email__ = "doug@QEDAconsulting.com"

# Add custom regression fixture
pytest_plugins = ("surv_regression", "dss_regression")


# Add command line options
def pytest_addoption(parser):
    parser.addoption("--javaCodeRootDir", action="store")
    parser.addoption("--javaHome", action="store")
    parser.addoption("--configFile", action="store")
    parser.addoption("--no_run", action="store_true")
