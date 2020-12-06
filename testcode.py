import numpy as np
import pandas as pd
import os
from os.path import join
import h5py

os.chdir(r"C:\Users\llavi\Desktop\test11.16\PRAS_files")

filename = "VRE0.4_wind_2012base100%_8760_25%tx_18%IRM_30GWstorage_addgulfsolar.pras"

with h5py.File(filename, "r") as f:
    # List all groups
    print("Keys: %s" % f.keys())
    print(f["regions"]["load"][:].sum(axis=1).max())  # grab the loads, sum zonally
    # a_group_key = list(f.keys())[0]

