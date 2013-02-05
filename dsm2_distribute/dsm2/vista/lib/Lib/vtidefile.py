from vista.db.hdf5 import HDF5DataReference, HDF5Group
def opentidefile(filename):
    """
    opens a tidefile with the given name. The tidefile is a group
    just like the dss files and contains references to time series data
    """
    tidefile = HDF5Group(filename)
    return tidefile
#