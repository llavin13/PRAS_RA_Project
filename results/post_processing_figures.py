# general imports
import os
import warnings
from os.path import join
import pandas as pd
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D
import time
import h5py
import re
import seaborn as sns
import descartes
from pylab import text
import sys
from mpl_toolkits.axes_grid1 import make_axes_locatable
from shapely.geometry import Point, LineString, MultiPoint
from shapely.ops import cascaded_union
from geovoronoi.plotting import (
    subplot_for_map,
    plot_voronoi_polys_with_points_in_area,
    plot_voronoi_polys,
    plot_points,
)
from geovoronoi import voronoi_regions_from_coords, points_to_coords

folder = "PRAS3.12.21"
data = join(os.environ["HOMEPATH"], "Desktop", folder)
sys.path.insert(0, data)
from MISO_data_utility_functions import LoadMISOData, NRELEFSprofiles

NREL = False
NREL_year, NREL_profile = 2012, ""

casename = "VRE0.4_wind_2012base100%_8760_100%tx_18%IRM_0GWstorage_addgulfsolar_"
#VRE0.4_wind_2012base100%_8760_100%tx_18%IRM_0GWstorage_addgulfsolar
miso_datapath = join(os.environ["HOMEPATH"], "Desktop", folder, "VREData")
hifld_datapath = join(os.environ["HOMEPATH"], "Desktop", folder, "HIFLD_shapefiles")
shp_path = os.environ["CONDA_PREFIX"] + r"\Library\share\gdal"
results = join(data, "results")

miso_data = LoadMISOData(data, miso_datapath, hifld_datapath, shp_path)
miso_data.convert_CRS()


class plotter(object):
    def __init__(self, data_folder, results_folder, casename, miso_data):
        print("loading plotting data...")
        self.miso_map = miso_data.miso_map
        self.iso_map = miso_data.iso_map
        self.states_map = miso_data.states_map
        self.utilities_map = miso_data.utilities_map
        self.casename = casename

        miso_data.utility_territory_mapping()
        self.map_dict = miso_data.map_dict
        assert (type(casename)) == str
        # loads data, mostly
        # data loads
        self.miso_map = pd.read_excel(
            join(data_folder, "NREL-Seams Model (MISO).xlsx"), sheet_name="Mapping"
        )
        self.miso_loads = pd.read_excel(
            join(data_folder, "NREL-Seams Model (MISO).xlsx"), sheet_name="Load"
        )
        self.miso_tx = pd.read_excel(
            join(data_folder, "NREL-Seams Model (MISO).xlsx"),
            sheet_name="Transmission",
        )

        # new loads
        self.miso_busmap = pd.read_csv(
            os.path.join(data_folder, "MISO_data", "Bus Mapping Extra Data.csv")
        )
        self.miso_bus_zone = pd.read_excel(
            join(data_folder, "MISO_data", "Bus_to_SeamsRegion.xlsx")
        )

        # create attributes of stuff we want later
        self.results_folder = results_folder
        #self.miso_map = miso_map
        #self.miso_loads = miso_loads
        #self.miso_tx = miso_tx

        print("...plotting data loaded")

    def remove_zones(self,zone):
        #print(self.miso_busmap)
        #print(self.miso_busmap.columns)
        #print(self.miso_bus_zone)
        idx = self.miso_map[self.miso_map['CEP Bus Name']==zone].index[0]
        #print(idx)
        #print(zone)
        busID = self.miso_map.at[idx,'CEP Bus ID']#grab the zone bus ID
        #print(busID)
        del self.miso_loads[zone]
        self.miso_map.drop(idx,inplace=True)
        self.miso_tx.drop(self.miso_tx[self.miso_tx['From']==float(busID)].index,inplace=True)
        self.miso_tx.drop(self.miso_tx[self.miso_tx['To']==float(busID)].index,inplace=True)
        self.miso_tx.reset_index(inplace=True)
        self.miso_bus_zone.drop(self.miso_bus_zone[self.miso_bus_zone['Seams Region']==zone].index,inplace=True)
        return None

    def format_data(self):
        miso_busmap = self.miso_busmap.merge(self.miso_bus_zone, left_on="Name", right_on="Bus")
        miso_busmap = miso_busmap[
            ~miso_busmap["Seams Region"].isin(["PJM-C", "CSWS+", "MDU", "MN-NW","WAPA_DK","BREC","AECIZ","CBPC-NIPCO"])
        ]
        miso_busmap = miso_busmap.rename(columns={"Seams Region": "Seams_Region"})
        miso_seam_zone = pd.DataFrame(columns=["Seams_Region", "geometry"])
        for i in list(miso_busmap.Seams_Region.unique()):
            tmp = miso_busmap[miso_busmap.Seams_Region == i]
            tmp_Lon = list(tmp.Lon)
            tmp_Lat = list(tmp.Lat)
            Seams_loc = MultiPoint(list(zip(tmp_Lon, tmp_Lat)))
            miso_seam_zone = miso_seam_zone.append(
                [{"Seams_Region": i, "geometry": Seams_loc,}], ignore_index=True,
            )
        miso_seam_zone_gdf = gpd.GeoDataFrame(miso_seam_zone)
        miso_seam_zone_gdf["centroid"] = miso_seam_zone_gdf.centroid
        miso_seam_zone_gdf = miso_seam_zone_gdf.set_geometry("centroid")
        miso_seam_zone_gdf.crs = "EPSG:4326"
        self.miso_seam_zone_gdf = miso_seam_zone_gdf  # for later use

        # results loads
        region_lole = pd.read_csv(
            join(self.results_folder, self.casename + "regionlole.csv"), header=None
        )
        region_eue = pd.read_csv(
            join(self.results_folder, self.casename + "regioneue.csv"), header=None
        )
        region_period_eue = pd.read_csv(
            join(self.results_folder, self.casename + "regionperiodeue.csv"), header=None
        )
        period_eue = pd.read_csv(
            join(self.results_folder, self.casename + "periodeue.csv"), header=None
        )
        period_lolp = pd.read_csv(
            join(self.results_folder, self.casename + "periodlolp.csv"), header=None
        )

        utilization = pd.read_csv(
            join(self.results_folder, self.casename + "utilizations.csv"), header=None
        )

        flow = pd.read_csv(join(self.results_folder, self.casename + "flows.csv"), header=None)

        # clean and reformat some of the loaded info
        region_lole.index, region_eue.index = (
            list(self.miso_map["CEP Bus ID"]),
            list(self.miso_map["CEP Bus ID"]),
        )
        region_lole.columns, region_eue.columns = ["LOLE"], ["EUE"]
        region_df = pd.concat([region_lole, region_eue], axis=1)
        tmps = len(region_period_eue.columns)
        region_df["load"] = list(self.miso_loads.iloc[:tmps, 1:].sum(axis=0))
        region_df["names"] = self.miso_map["CEP Bus Name"].values
        # clean and reformat transmission info

        #convert to objs
        self.region_df = region_df
        self.region_lole = region_lole
        self.region_eue = region_eue
        self.region_period_eue = region_period_eue
        self.period_eue = period_eue
        self.period_lolp = period_lolp
        self.utilization = utilization
        self.flow = flow

    def data_check(self):
        # runs some checks on data formatting from uploads
        return self.region_df

    def create_month_hour_df(self, df, month="ALL", hour="ALL"):
        df["Date"] = list(self.miso_loads["Date"])
        df["Month"] = [
            int(re.findall(r"-(\d+)-", str(d))[0]) for d in df["Date"].values
        ]
        df["HourBegin"] = df.index.values % 24
        if month != "ALL":
            df = df[df.Month == month]
        if month != "ALL":
            df = df[df.HourBegin == hour]

        return df

    def format12x24(self, df, mean=False):

        # does some formatting for use with seaborn heatmaps
        df["Date"] = list(self.miso_loads["Date"])
        df["Month"] = [
            int(re.findall(r"-(\d+)-", str(d))[0]) for d in df["Date"].values
        ]
        df["HourBegin"] = df.index.values % 24
        np_12x24 = np.zeros((12, 24))
        for r in range(np_12x24.shape[0]):
            for c in range(np_12x24.shape[1]):
                if mean:
                    np_12x24[r, c] += df[0][
                        (df.Month == r + 1) & (df.HourBegin == c)
                    ].mean()
                else:
                    np_12x24[r, c] += df[0][
                        (df.Month == r + 1) & (df.HourBegin == c)
                    ].sum()
        insert_row = np.zeros((1, 24))
        np_12x24 = np.vstack((insert_row, np_12x24))  # helps to fix indexing
        return np_12x24

    def heatmap(self, attribute_string, show=False, mean=False):
        # plots heatmap
        assert (type(attribute_string)) == str
        attribute = getattr(self, attribute_string)
        fig, ax = plt.subplots(1, figsize=(8, 5))
        ax = sns.heatmap(
            self.format12x24(attribute, mean=mean), linewidth=0.5, cmap="Reds"
        )
        ax.set_ylim(1, 13)
        ax.set_ylabel("Month", fontsize=20)
        ax.set_xlabel("Hour Beginning", fontsize=20)
        ax.set_title(attribute_string)
        plt.savefig(
            join(self.results_folder, attribute_string + self.casename + "heatmap.jpg"),
            dpi=300,
        )
        if show:
            plt.show()
        return None

    def panel_tx_heatmap(self, attribute_string, show=False):
        assert (type(attribute_string)) == str

        # create plot
        rows = 12
        cols = 5
        fig, axs = plt.subplots(rows, cols, sharex=True, sharey=True, figsize=(40, 20))
        # fig.suptitle("All my friends")

        # iterate
        maxval = 1.0
        for i, v in enumerate(list(self.miso_tx.Line.unique())[:-1]):
            if i % 10 == 0:
                print(
                    str(i)
                    + " out of "
                    + str(len(list(self.miso_tx.Line.unique())))
                    + " lines are plotted"
                )
            df_index = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))].index[0]
            from_label = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))][
                "From"
            ].values[
                0
            ]  # [0]
            to_label = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))][
                "To"
            ].values[
                0
            ]  # [0]
            from_name = self.miso_map[self.miso_map["CEP Bus ID"] == from_label][
                "CEP Bus Name"
            ].values[0]
            to_name = self.miso_map[self.miso_map["CEP Bus ID"] == to_label][
                "CEP Bus Name"
            ].values[0]
            attribute = getattr(self, attribute_string)
            attribute_df = pd.DataFrame(attribute.loc[df_index, :])
            attribute_df.columns = [0]  # overwrite so matching works
            sns.heatmap(
                self.format12x24(attribute_df, mean=True),
                vmin=0,
                vmax=maxval,
                linewidth=0.5,
                cmap="Reds",
                cbar=False,
                ax=axs[int(i / cols), i % cols],
            )
            axs[int(i / cols), i % cols].set_ylim(1, 13)
            axs[int(i / cols), i % cols].set_title(from_name + " to " + to_name)

        # write plot
        plt.savefig(
            join(
                self.results_folder,
                "ALL" + attribute_string + self.casename + "heatmap.jpg",
            ),
            dpi=300,
        )
        return None

    def tx_heatmap(self, zone_label, attribute_string, show=False):
        assert (type(attribute_string)) == str

        # print(float(zone_label))
        df_index = self.miso_tx[self.miso_tx["Line"] == float(zone_label)].index[0]

        # grab label for plotting
        from_label = self.miso_tx[self.miso_tx["Line"] == float(zone_label)][
            "From"
        ].values[
            0
        ]  # [0]
        to_label = self.miso_tx[self.miso_tx["Line"] == float(zone_label)]["To"].values[
            0
        ]  # [0]
        from_name = self.miso_map[self.miso_map["CEP Bus ID"] == from_label][
            "CEP Bus Name"
        ].values[0]
        to_name = self.miso_map[self.miso_map["CEP Bus ID"] == to_label][
            "CEP Bus Name"
        ].values[0]

        attribute = getattr(self, attribute_string)
        attribute_df = pd.DataFrame(attribute.loc[df_index, :])
        attribute_df.columns = [0]  # overwrite so matching works
        fig, ax = plt.subplots(1, figsize=(8, 5))
        ax = sns.heatmap(
            self.format12x24(attribute_df, mean=True), linewidth=0.5, cmap="Reds",
        )
        ax.set_ylim(1, 13)
        ax.set_ylabel("Month")
        ax.set_xlabel("Hour Beginning")
        ax.set_title(attribute_string + " " + from_name + " to " + to_name)
        plt.savefig(
            join(
                self.results_folder,
                attribute_string
                + from_name
                + "to"
                + to_name
                + self.casename
                + "heatmap.jpg",
            ),
            dpi=300,
        )
        if show:
            plt.show()
        return None

    def updated_geography_plot(
        self,
        CRS=4326,
        attribute="EUE",
        line_attribute="utilization",
        plot_type="fills",
        hours="ALL",
        months="ALL",
        subset_lines=True,
    ):
        plotTitle = "MISO Base Case \n Regional Resource Adequacy"
        if attribute != "LOLE" and attribute != "EUE":
            raise ValueError("can only plot LOLE or EUE")
        boundary = self.iso_map[self.iso_map["NAME"] == self.iso_map.at[0, "NAME"]]
        boundary = boundary.to_crs(epsg=CRS)
        gdf_proj = self.miso_seam_zone_gdf.to_crs(boundary.crs)
        # re-assignment due to different zone naming conventions
        #gdf_proj.at[
        #    gdf_proj[gdf_proj.Seams_Region == "WAPA_DK"].index.values[0], "Seams_Region"
        #] = "CBPC-NIPCO"  # [0] = "CBPC-NIPCO"
        #gdf_proj.at[
        #    gdf_proj[gdf_proj.Seams_Region == "BREC"].index.values[0], "Seams_Region"
        #] = "AECIZ"
        gdf_proj.at[
            gdf_proj[gdf_proj.Seams_Region == "LA-Gulf"].index.values[0], "Seams_Region"
        ] = "LA-GULF"
        # end re-assignment
        gdf_merge = pd.merge(
            gdf_proj,
            self.region_df,
            how="left",
            left_on="Seams_Region",
            right_on="names",
        )
        self.gdf_merge = gdf_merge
        line_gdf = self.create_lines(line_attribute, month=months, hour=hours)
        labs = list(gdf_merge["Seams_Region"])
        attribute_max = gdf_merge[attribute].max()
        boundary.geometry = boundary.geometry.buffer(0)
        boundary_shape = cascaded_union(boundary.geometry)
        coords = points_to_coords(gdf_proj.geometry)
        poly_shapes, pts,poly_to_pt_assignments = voronoi_regions_from_coords(
            coords, boundary_shape
        )
        #print(poly_shapes)
        #print(type(poly_shapes))

        # run plotting
        fig, ax = subplot_for_map()
        myaxes = plt.axes()
        myaxes.set_ylim([20, 50])
        myaxes.set_xlim([-100, -82])
        myaxes.set_yticklabels((" "))
        myaxes.set_xticklabels((" "))
        # for i,s in enumerate(poly_shapes):
        #    gdf_merge.at[i,'geometry'] = s
        divider = make_axes_locatable(myaxes)
        cax = divider.append_axes("bottom", size="5%", pad=0.1)
        if plot_type == "bubbles":
            gdf_merge.plot(
                ax=myaxes,
                column=attribute,
                cmap="Blues",
                legend=True,
                cax=cax,
                alpha=1.0,
                markersize=100,
                legend_kwds={
                    "label": attribute + " (MWh)",
                    "orientation": "horizontal",
                },
            )
            # (MWh (EUE) or Hours (LOLE) /y)
            plot_points(
                myaxes, pts, 2, labels=labs, alpha=0.0, label_fontsize=16
            )  # mostly just adds the zonal labels
        elif plot_type == "fills":
            for i, s in enumerate(poly_shapes):
                #print(i,s)
                plot_voronoi_polys(
                    myaxes,
                    s,
                    color="g",
                    alpha=gdf_merge.at[i, attribute] / attribute_max,
                )
            gdf_merge.plot(
                ax=myaxes,
                column=attribute,
                cmap="Greens",
                legend=True,
                cax=cax,
                alpha=0.0,
                legend_kwds={
                    "label": attribute + " (MWh)",
                    "orientation": "horizontal",
                },
            )
            # (MWh (EUE) or Hours (LOLE) /y)
            plot_points(
                myaxes, pts, 2, labels=labs
            )  # mostly just adds the zonal labels
        else:
            raise ValueError("plot_type must be either fills or bubbles")

        # subset line gdf, if desired, to get rid of lines without flows
        if subset_lines:
            fromindices = list(
                line_gdf.index[
                    line_gdf.from_name.isin(
                        ["AECIZ", "LA-GULF", "MEC", "SIPC", "IL-C", "MISO-MS"]
                    )
                ]
            )
            toindices = list(
                line_gdf.index[
                    line_gdf.to_name.isin(
                        ["AECIZ", "LA-GULF", "MEC", "SIPC", "IL-C", "MISO-MS"]
                    )
                ]
            )
            retained = list(set(fromindices + toindices))
            line_gdf = line_gdf[line_gdf.index.isin(retained)]
            plotTitle = "Retained Lines in " + plotTitle

        linewidths = list(line_gdf.MW)
        linewidths_2 = list(line_gdf.capacity)
        # finally, add the tx lines
        for lw, lw2 in zip(linewidths, linewidths_2):
            line_gdf[line_gdf.MW == lw].plot(
                lw=lw2 * 0.001, ax=myaxes, color="k", zorder=2, alpha=0.3
            )
            line_gdf[line_gdf.MW == lw].plot(
                lw=lw * 0.001, ax=myaxes, color="r", zorder=3
            )

        # could also add a MISO boundary if it seems useful
        self.iso_map[self.iso_map["NAME"] == self.iso_map.at[0, "NAME"]].plot(
            ax=myaxes, facecolor="b", edgecolor="y", alpha=0.04, linewidth=2, zorder=1
        )
        # last big thing would be a helpful legend....
        self.states_map.plot(ax=myaxes, edgecolor="k", facecolor="None", alpha=0.3)
        # states_map.plot(ax=myaxes, edgecolor="k", facecolor="None")
        # myaxes.set_title("MISO regions polygons \n (fill based on " + attribute + ")")
        # plt.suptitle("MISO Regional Resource Adequacy", fontsize=16, y=1)
        # plt.title("100% Tx Capacity, 0 GW Storage ICAP", fontsize=12, y=0.95)
        myaxes.set_title(
            plotTitle, fontsize=12,
        )
        # \n 25% Tx Capacity, 0 GW Storage ICAP
        # add manual legends to help interpret plot
        cap_1 = round(max(linewidths_2), -3)
        cap_2 = round(max(linewidths_2), -3) * 2.0 / 3.0
        cap_3 = round(max(linewidths_2), -3) * 1.0 / 3.0

        utilization_1 = round(max(linewidths), -2)
        utilization_2 = round(max(linewidths), -2) * 2.0 / 3.0
        utilization_3 = round(max(linewidths), -2) * 1.0 / 3.0

        custom_capacity_lines = [
            Line2D([0], [0], color="k", lw=cap_1 * 0.001, alpha=0.3),
            Line2D([0], [0], color="k", lw=cap_2 * 0.001, alpha=0.3),
            Line2D([0], [0], color="k", lw=cap_3 * 0.001, alpha=0.3),
            Line2D([0], [0], color="r", lw=utilization_1 * 0.001),
            Line2D([0], [0], color="r", lw=utilization_2 * 0.001),
            Line2D([0], [0], color="r", lw=utilization_3 * 0.001),
        ]
        myaxes.legend(
            custom_capacity_lines,
            [
                str(int(cap_1)) + " MW",
                str(int(cap_2)) + " MW",
                str(int(cap_3)) + " MW",
                str(int(utilization_1)) + " MW",
                str(int(utilization_2)) + " MW",
                str(int(utilization_3)) + " MW",
            ],
            loc="lower left",
            title="Line Capacity   Average Flow",
            fontsize="x-small",
            title_fontsize="small",
            frameon=False,
            ncol=2,
        )
        # title="Line Capacity   Line " + line_attribute.capitalize()

        # custom_utilization_lines = []
        # myaxes.legend(custom_utilization_lines, [],
        # loc="lower right",title="Line "+line_attribute, fontsize="x-small",title_fontsize="small",frameon=False)
        if subset_lines:
            r = "RETAINED_"
        else:
            r = ""
        print("plotted")
        plt.savefig(
            os.path.join(
                self.results_folder,
                r
                + "voronoi"
                + plot_type
                + self.casename
                + "_h="
                + str(hours)
                + "_m="
                + str(months)
                + ".jpg",
            ),
            dpi=500,
        )
        # eventually create values for loading EUE, lole, etc
        return None

    def create_lines(self, attribute_string, CRS=4326, month=7, hour=18):
        capacity_list = list(
            self.miso_tx.iloc[: len(self.miso_tx.Line.unique()) - 1, :].FW
        )
        line_utilization = pd.DataFrame(
            columns=["from_name", "to_name", "line_loc", "expected_utilization"]
        )
        print("NOTE: ignoring CRS-related geopandas warnings")
        for i, v in enumerate(list(self.miso_tx.Line.unique())[:-1]):
            if i % 20 == 0:
                print(
                    str(i)
                    + " out of "
                    + str(len(list(self.miso_tx.Line.unique())))
                    + " lines are plotted"
                )
            df_index = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))].index[0]
            from_label = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))][
                "From"
            ].values[
                0
            ]  # [0]
            to_label = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))][
                "To"
            ].values[
                0
            ]  # [0]
            from_name = self.miso_map[self.miso_map["CEP Bus ID"] == from_label][
                "CEP Bus Name"
            ].values[0]
            to_name = self.miso_map[self.miso_map["CEP Bus ID"] == to_label][
                "CEP Bus Name"
            ].values[0]
            warnings.simplefilter(action="ignore", category=UserWarning)
            from_name_loc = self.gdf_merge[
                self.gdf_merge.Seams_Region == from_name
            ].centroid.values[0]
            to_name_loc = self.gdf_merge[
                self.gdf_merge.Seams_Region == to_name
            ].centroid.values[0]
            line_loc = LineString([from_name_loc, to_name_loc])  # ok, have the string
            attribute = getattr(self, attribute_string)
            attribute_df = pd.DataFrame(attribute.loc[df_index, :])
            attribute_df.columns = [0]  # overwrite so matching works
            attribute_df = self.create_month_hour_df(
                attribute_df, month=month, hour=hour
            )
            expected_utilization = attribute_df[0].mean()
            line_utilization = line_utilization.append(
                [
                    {
                        "from_name": from_name,
                        "to_name": to_name,
                        "line_loc": line_loc,
                        "expected_utilization": expected_utilization,
                    }
                ],
                ignore_index=True,
            )
        line_utilization_gdf = gpd.GeoDataFrame(
            line_utilization, geometry=line_utilization.line_loc
        )
        line_utilization_gdf["capacity"] = capacity_list
        line_utilization_gdf["MW"] = (
            line_utilization_gdf.capacity * line_utilization_gdf.expected_utilization
        )
        line_utilization_gdf.crs = "EPSG:" + str(CRS)
        line_utilization_gdf.to_crs(epsg=CRS, inplace=True)
        return line_utilization_gdf

    def geography_tx_plot(self, attribute_string, CRS=4326, month="ALL", hour="ALL"):
        capacity_list = list(
            self.miso_tx.iloc[: len(self.miso_tx.Line.unique()) - 1, :].FW
        )  # grabs the fw capacity of lines
        zone_loc = pd.read_csv(
            os.path.join(
                os.environ["HOMEPATH"],
                "Desktop",
                folder,
                "miso_locs_to_SEAMS_zones.csv",
            )
        )
        zone_centroid = pd.DataFrame(columns=["Lat", "Lon", "Zone"])
        for i in list(zone_loc["FINAL_SEAMS_ZONE"].unique()):
            centroid_Lat = zone_loc[zone_loc["FINAL_SEAMS_ZONE"] == i]["Lat"].mean()
            centroid_Lon = zone_loc[zone_loc["FINAL_SEAMS_ZONE"] == i]["Lon"].mean()
            zone_centroid = zone_centroid.append(
                [{"Lat": centroid_Lat, "Lon": centroid_Lon, "Zone": i}],
                ignore_index=True,
            )
        zone_centroid_gdf = gpd.GeoDataFrame(
            zone_centroid,
            geometry=gpd.points_from_xy(zone_centroid["Lon"], zone_centroid["Lat"]),
        )
        zone_centroid_gdf.crs = "EPSG:" + str(CRS)
        zone_centroid_gdf.to_crs(epsg=CRS, inplace=True)
        # create zone centroid
        assert (type(attribute_string)) == str
        line_utilization = pd.DataFrame(
            columns=["from_name", "to_name", "line_loc", "expected_utilization"]
        )
        for i, v in enumerate(list(self.miso_tx.Line.unique())[:-1]):
            if i % 10 == 0:
                print(
                    str(i)
                    + " out of "
                    + str(len(list(self.miso_tx.Line.unique())))
                    + " lines are plotted"
                )
            df_index = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))].index[0]
            from_label = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))][
                "From"
            ].values[
                0
            ]  # [0]
            to_label = self.miso_tx[self.miso_tx["Line"] == float(str(int(v)))][
                "To"
            ].values[
                0
            ]  # [0]
            from_name = self.miso_map[self.miso_map["CEP Bus ID"] == from_label][
                "CEP Bus Name"
            ].values[0]
            to_name = self.miso_map[self.miso_map["CEP Bus ID"] == to_label][
                "CEP Bus Name"
            ].values[0]
            try:
                from_name_loc = Point(
                    zone_centroid[zone_centroid.Zone == from_name].Lon,
                    zone_centroid[zone_centroid.Zone == from_name].Lat,
                )
                to_name_loc = Point(
                    zone_centroid[zone_centroid.Zone == to_name].Lon,
                    zone_centroid[zone_centroid.Zone == to_name].Lat,
                )
            except TypeError:
                print("No unit in", from_name, "or", to_name, "...")
            line_loc = LineString([from_name_loc, to_name_loc])
            attribute = getattr(self, attribute_string)
            attribute_df = pd.DataFrame(attribute.loc[df_index, :])
            attribute_df.columns = [0]  # overwrite so matching works
            attribute_df = self.create_month_hour_df(
                attribute_df, month=month, hour=hour
            )
            expected_utilization = attribute_df[0].mean()
            line_utilization = line_utilization.append(
                [
                    {
                        "from_name": from_name,
                        "to_name": to_name,
                        "line_loc": line_loc,
                        "expected_utilization": expected_utilization,
                    }
                ],
                ignore_index=True,
            )
        line_utilization_gdf = gpd.GeoDataFrame(
            line_utilization, geometry=line_utilization.line_loc
        )
        line_utilization_gdf["capacity"] = capacity_list
        line_utilization_gdf["MW"] = (
            line_utilization_gdf.capacity * line_utilization_gdf.expected_utilization
        )
        line_utilization_gdf.crs = "EPSG:" + str(CRS)
        line_utilization_gdf.to_crs(epsg=CRS, inplace=True)
        # create lines expected utilization dataframe
        fig, ax = plt.subplots(1, figsize=(8, 8))
        myaxes = plt.axes()
        myaxes.set_ylim([28, 50])
        myaxes.set_xlim([-100, -82])
        myaxes.set_title(
            "Line Utilization \n (Hours=" + str(hour) + ", Month=" + str(month) + ")"
        )
        zone_centroid_gdf.plot(ax=myaxes, color="b")
        # line_utilization_gdf.plot(ax=myaxes, color="r")
        self.states_map.plot(ax=myaxes, edgecolor="k", facecolor="None")

        linewidths = list(line_utilization_gdf.MW)
        linewidths_2 = list(line_utilization_gdf.capacity)
        for lw, lw2 in zip(linewidths, linewidths_2):
            line_utilization_gdf[line_utilization_gdf.MW == lw].plot(
                lw=lw2 * 0.001, ax=myaxes, color="k", zorder=1, alpha=0.3
            )
            line_utilization_gdf[line_utilization_gdf.MW == lw].plot(
                lw=lw * 0.001, ax=myaxes, color="r", zorder=2
            )

        plt.savefig(
            join(
                self.results_folder,
                "line_utilization_m="
                + str(month)
                + "_h="
                + str(hour)
                + "_"
                + self.casename
                + ".jpg",
            ),
            dpi=300,
        )
        return None

    def load_utility_df(self, attribute_string):

        col = attribute_string[attribute_string.find("_") + 1 :].upper()
        for k in self.map_dict.keys():
            self.map_dict[k].append(
                self.region_df[col][self.region_df["names"] == k].values[0]
            )

        index_list = []
        for k, v in self.map_dict.items():
            for utility in v[0]:
                index_list.append(
                    self.utilities_map.index[
                        self.utilities_map["NAME"] == utility
                    ].values[0]
                )
        utilities_plot_df = self.utilities_map[
            self.utilities_map.index.isin(index_list)
        ]

        l = []
        if "lole" in attribute_string.lower():
            list_index = 2
        elif "eue" in attribute_string.lower():
            list_index = 3
        else:
            raise ValueError("attribute must be either LOLE or EUE")
        for utility in utilities_plot_df["NAME"]:
            for k, v in self.map_dict.items():
                for u in v[0]:
                    if u == utility:
                        l.append(v[list_index])
        pd.set_option("mode.chained_assignment", None)  # for now to suppress warnings
        utilities_plot_df[col] = l
        return (utilities_plot_df, col)

    def geography_plot(self, attribute_string):
        # plots onto map of MISO
        fig, ax = plt.subplots(1, figsize=(8, 8))
        myaxes = plt.axes()
        myaxes.set_ylim([18, 50])
        myaxes.set_xlim([-108, -82])
        self.iso_map[self.iso_map["NAME"] == self.iso_map.at[0, "NAME"]].plot(
            ax=myaxes, facecolor="b", edgecolor="r", alpha=0.1, linewidth=2
        )
        self.states_map.plot(ax=myaxes, edgecolor="k", facecolor="None")

        divider = make_axes_locatable(myaxes)
        cax = divider.append_axes("bottom", size="5%", pad=0.1)

        utilities_plot_df, label = self.load_utility_df(attribute_string)
        utilities_plot_df.plot(
            ax=myaxes,
            column=label,
            cmap="Reds",
            legend=True,
            cax=cax,
            legend_kwds={
                "label": label + " (MWh (EUE) or Hours (LOLE) /y)",
                "orientation": "horizontal",
            },
        )
        plt.savefig(join(self.results_folder, self.casename + label + ".jpg"), dpi=300)
        return None

    def plot_polygons(
        self,
        CRS=4326,
        attribute="EUE",
        line_attribute="utilization",
        plot_type="fills",
        hours="ALL",
        months="ALL",
        label="polygons",
    ):
        boundary = self.iso_map[self.iso_map["NAME"] == self.iso_map.at[0, "NAME"]]
        boundary = boundary.to_crs(epsg=CRS)
        gdf_proj = self.miso_seam_zone_gdf.to_crs(boundary.crs)
        # re-assignment due to different zone naming conventions
        #gdf_proj.at[
        #    gdf_proj[gdf_proj.Seams_Region == "WAPA_DK"].index.values[0], "Seams_Region"
        #] = "CBPC-NIPCO"  # [0] = "CBPC-NIPCO"
        #gdf_proj.at[
        #    gdf_proj[gdf_proj.Seams_Region == "BREC"].index.values[0], "Seams_Region"
        #] = "AECIZ"
        gdf_proj.at[
            gdf_proj[gdf_proj.Seams_Region == "LA-Gulf"].index.values[0], "Seams_Region"
        ] = "LA-GULF"
        # end re-assignment
        gdf_merge = pd.merge(
            gdf_proj,
            self.region_df,
            how="left",
            left_on="Seams_Region",
            right_on="names",
        )
        self.gdf_merge = gdf_merge
        line_gdf = self.create_lines(line_attribute, month=months, hour=hours)
        labs = list(gdf_merge["Seams_Region"])
        # attribute_max = gdf_merge[attribute].max()
        boundary.geometry = boundary.geometry.buffer(0)
        boundary_shape = cascaded_union(boundary.geometry)
        coords = points_to_coords(gdf_proj.geometry)
        poly_shapes, pts, poly_to_pt_assignments = voronoi_regions_from_coords(
            coords, boundary_shape
        )

        fig, ax = plt.subplots(1, figsize=(8, 8))
        myaxes = plt.axes()
        myaxes.set_xlabel("Longitude", fontsize=20)
        myaxes.set_ylabel("Latitude", fontsize=20)
        myaxes.tick_params(axis="both", which="major", labelsize=14)
        # myaxes.set_xticklabels(fontsize=14)
        # myaxes.set_yticklabels(fontsize=14)
        # myaxes.set_ylim([18, 50])
        # myaxes.set_xlim([-108, -82])
        plot_voronoi_polys_with_points_in_area(
            myaxes,
            boundary_shape,
            poly_shapes,
            pts,
            poly_to_pt_assignments,
            points_color="blue",
            points_markersize=12,
            point_labels=labs,
            point_label_fontsize=12,
            point_label_color="k",
        )
        # self.iso_map[self.iso_map["NAME"] == self.iso_map.at[0, "NAME"]].plot(
        #    ax=myaxes, facecolor="b", edgecolor="r", alpha=0.1, linewidth=2
        # )
        # also add the linegdf

        linewidths = list(line_gdf.MW)
        linewidths_2 = list(line_gdf.capacity)
        # finally, add the tx lines
        for lw, lw2 in zip(linewidths, linewidths_2):
            line_gdf[line_gdf.MW == lw].plot(
                lw=lw2 * 0.001, ax=myaxes, color="k", zorder=2, alpha=0.3
            )
        # finally, add the tx lines
        # and, do I need a line legend?

        cap_1 = round(max(linewidths_2), -3)
        cap_2 = round(max(linewidths_2), -3) * 2.0 / 3.0
        cap_3 = round(max(linewidths_2), -3) * 1.0 / 3.0

        custom_capacity_lines = [
            Line2D([0], [0], color="k", lw=cap_1 * 0.001, alpha=0.4),
            Line2D([0], [0], color="k", lw=cap_2 * 0.001, alpha=0.4),
            Line2D([0], [0], color="k", lw=cap_3 * 0.001, alpha=0.4),
        ]
        myaxes.legend(
            custom_capacity_lines,
            [str(int(cap_1)) + " MW", str(int(cap_2)) + " MW", str(int(cap_3)) + " MW"],
            loc="lower left",
            title="Line Capacity",
            fontsize="large",
            title_fontsize="large",
            frameon=False,
            ncol=2,
        )

        plt.tight_layout()
        # plt.show()
        plt.savefig(join(self.results_folder, "polymap_" + label + ".png"), dpi=300)
        return None

    def aggregate_zones(self, df, tmps=24, monthly=False):
        if monthly == False:
            df = df.iloc[:, 1:]
        else:
            df["Month"] = [
                int(re.findall(r"-(\d+)-", str(d))[0]) for d in df["Date"].values
            ]
            df = df.iloc[:, 1:]
        df["hour"] = list(range(tmps)) * int(len(df.index) / tmps)
        if monthly == False:
            df = df.groupby("hour").mean()
        else:
            df = df.groupby(["Month", "hour"]).mean()
        return df

    def modify_load_year(self, year, new_load_magnitude, new_load_profile):
        scalar = new_load_magnitude / (self.miso_loads.iloc[:, 1:].sum(axis=1).mean())
        self.miso_loads.iloc[:, 1:] = self.miso_loads.iloc[:, 1:] * scalar
        seams_profile = (
            self.miso_loads.iloc[:, 1:].sum(axis=1)
            / self.miso_loads.iloc[:, 1:].sum(axis=1).sum()
        )
        self.miso_loads.iloc[:, 1:] = self.miso_loads.iloc[:, 1:].mul(
            (new_load_profile.LoadMW.reset_index().LoadMW / seams_profile), axis=0
        )

    def plot_zonal_loads(
        self, monthly=True, show=False, NREL=False, year_lab=2012, scenario_lab=""
    ):
        miso_loads_zones = self.aggregate_zones(self.miso_loads, monthly=monthly)
        color_list = [self.map_dict[k][1] for k in miso_loads_zones.columns]
        fig, ax = plt.subplots(1, figsize=(10, 6))
        miso_loads_zones.plot.area(ax=ax, color=color_list)

        ax.set_ylabel("MWh", fontsize=24)
        if monthly:
            ax.set_xlabel("(Month-Hour) Average", fontsize=16)
        else:
            ax.set_xlabel("Hour Beginning", fontsize=16)
        for index, label in enumerate(
            [
                "Jan",
                "Feb",
                "Mar",
                "Apr",
                "May",
                "Jun",
                "Jul",
                "Aug",
                "Sep",
                "Oct",
                "Nov",
                "Dec",
            ]
        ):
            plt.axvline(x=(index + 1) * 24, color="k")
            text(
                (index + 1) * 24 - 17,
                miso_loads_zones.sum(axis=1).max() * 1.01,
                label,
                fontsize=12,
            )
        # draw vertical line from (70,100) to (70, 250)
        # plt.plot([24, 0], [24, 100000], 'k-', lw=2)

        lgd = plt.legend(bbox_to_anchor=(1.2, 1.01), loc="upper right")
        if NREL:
            label = "NREL" + str(scenario_lab) + str(year_lab) + "load.png"
        else:
            label = "MISOload.png"
        plt.savefig(
            join(self.results_folder, label),
            bbox_extra_artists=(lgd,),
            bbox_inches="tight",
            dpi=300,
        )
        if show:
            plt.show()
        return None


class ELCCplotter(object):
    def __init__(self, folder, pct_cap=False, rankplot=False):
        self.folder = folder
        self.results_folder = join(folder, "results")
        self.pras_folder = join(folder, "PRAS_files")
        self.elcc_folder = join(self.results_folder, "ELCCresults")
        self.casename = "VRE"
        self.pct_cap = pct_cap
        self.rankplot = rankplot

    def storage_case_plot(
        self,
        vary_str,
        *args,
        print_plot=False,
        case_label="100%tx",
        cname="storageELCC_"
    ):
        self.label_list = [case_label]
        self.color_list, self.line_list, self.linestyle_list, self.alpha_list = (
            [],
            [],
            [],
            [],
        )
        arglist = []
        self.plot_title_list = [
            "VRE penetration (%energy)",
            "casetype (n)",
            "load (MW)",
            "hours (/y)",
            "tx (%)",
            "IRM (%)",
            "Storage ICAP (GW)",
            "Storage ICAP (% Peak Load)",
        ]
        for counter, i in enumerate(args):
            if type(i) == list:
                argiter = i
                place = counter
            else:
                arglist.append(i)
        for a in argiter:
            arglist.insert(place, a)
            self.storage_df = self.storage_case_load(
                arglist, a, "storage_df", cname=cname
            )
            arglist.pop(place)

        if self.pct_cap:
            place += 1
        self.storage_df["minelcc%"] = (
            self.storage_df.minelcc * 100.0 / max(self.storage_df.maxelcc)
        )
        self.storage_df["maxelcc%"] = (
            self.storage_df.maxelcc * 100.0 / max(self.storage_df.maxelcc)
        )
        self.storage_df["avgelcc%"] = (
            self.storage_df["maxelcc%"] + self.storage_df["minelcc%"]
        ) * 0.5

        self.storage_df["case_num"] = [1.0 for i in self.storage_df.index]

        rows = 8
        cols = 3
        fig, axs = plt.subplots(rows, cols, sharex=True, sharey=True, figsize=(12, 12))
        axs[rows - 1, cols - 2].set_visible(
            False
        )  # bottom r axis is off for visibility
        axs[rows - 1, cols - 1].set_visible(
            False
        )  # bottom r axis is off for visibility
        fig.suptitle(
            "ELCC as function of "
            + self.plot_title_list[place][: self.plot_title_list[place].find("(")],
            fontsize=30,
        )
        if hasattr(self, "ranks"):
            sorted_list = [
                x
                for _, x in sorted(
                    zip(list(self.ranks), self.storage_df.resourcename.unique())
                )
            ]
        else:
            sorted_list = self.storage_df.resourcename.unique()
        for i, zone in enumerate(sorted_list):
            self.storage_df[self.storage_df.resourcename == zone].plot.line(
                x="xval",
                y="avgelcc%",
                c="r",
                ax=axs[int(i / cols), i % cols],
                legend=False,
            )
            subsetdf = self.storage_df[self.storage_df.resourcename == zone]
            """
            axs[int(i / cols), i % cols].text(
                subsetdf.xval.values[int(len(subsetdf.xval) / 2.0)],
                3.0 + subsetdf["avgelcc%"].values[int(len(subsetdf.xval) / 2.0)],
                "Tx=100%",
                color="r",
            )
            """
            # axs[int(i / cols), i % cols].text(
            #    subsetdf.xval.mean(), subsetdf["avgelcc%"].mean(), "Tx=100%"
            # )
            axs[int(i / cols), i % cols].fill_between(
                subsetdf.xval,
                subsetdf["minelcc%"],
                subsetdf["maxelcc%"],
                color="k",
                alpha=0.2,
            )
            axs[int(i / cols), i % cols].set_title(
                zone[: zone.find(re.findall(r"\d+", zone)[0])]
            )
            axs[int(i / cols), i % cols].set_ylabel("ELCC (%)", fontsize=16)
            axs[int(i / cols), i % cols].set_xlabel(
                self.plot_title_list[place], fontsize=16
            )

        # add a manual legend to your plot, if desired
        if print_plot:
            colors = ["black", "red"]
            linewidths = [12, 4]
            linestyles = ["-", "-"]
            alphas = [0.2, 1]
            lines = [
                Line2D([0], [0], color=c, linewidth=lw, linestyle=ls, alpha=a)
                for c, lw, ls, a in zip(colors, linewidths, linestyles, alphas)
            ]
            labels = ["80%CI", case_label]
            plt.figlegend(
                lines,
                labels,
                fontsize=24,
                frameon=False,
                bbox_to_anchor=(0.92, 0.15),
                ncol=2,
            )
            plt.tight_layout()
            plt.subplots_adjust(top=0.9)
        # store some objects, if desired
        self.fig = fig
        self.axs = axs
        self.cols = cols
        self.rows = rows
        # write plot
        if print_plot:
            filename = "_".join([str(elem) for elem in arglist])
            plt.savefig(
                join(self.results_folder, vary_str + "_ELCC_" + filename + ".jpg",),
                dpi=300,
            )

    def add_storage_line_to_existing_plot(
        self,
        vary_str,
        case_num,
        *args,
        print_plot=False,
        case_label="25%tx",
        cname="storageELCC_"
    ):
        self.label_list.append(case_label)
        pd.set_option("mode.chained_assignment", None)  # for now to suppress warnings
        linecolor_list = [
            "r",
            "b",
            "g",
            "yellow",
            "purple",
            "cyan",
            "lime",
            "red",
            "blue",
            "g",
            "yellow",
            "purple",
            "cyan",
            "lime",
        ]
        linestyle_list = [
            "-",
            "-",
            "-",
            "-",
            "-",
            "-",
            "-",
            ":",
            ":",
            ":",
            ":",
            ":",
            ":",
            ":",
            ":",
        ]
        arglist = []
        # create attribute from pre-existing df, but clear it
        self.storage_df_2 = pd.DataFrame(columns=self.storage_df.columns)
        for counter, i in enumerate(args):
            if type(i) == list:
                argiter = i
                place = counter
            else:
                arglist.append(i)
        for a in argiter:
            arglist.insert(place, a)
            # setattr(x, attr, 'magic')
            self.storage_df = self.storage_case_load(
                arglist, a, "storage_df", n=case_num, cname=cname
            )
            # setattr(self, attr_ID,self.storage_case_load(arglist, a, attr_ID))
            arglist.pop(place)
        if self.pct_cap:
            place += 1
        case_storage_df = self.storage_df[self.storage_df.case_num == case_num]
        case_storage_df["minelcc%"] = (
            case_storage_df.minelcc * 100.0 / max(case_storage_df.maxelcc)
        )
        case_storage_df["maxelcc%"] = (
            case_storage_df.maxelcc * 100.0 / max(case_storage_df.maxelcc)
        )
        case_storage_df["avgelcc%"] = (
            case_storage_df["maxelcc%"] + case_storage_df["minelcc%"]
        ) * 0.5
        if hasattr(self, "ranks"):
            sorted_list = [
                x
                for _, x in sorted(
                    zip(list(self.ranks), case_storage_df.resourcename.unique())
                )
            ]
        else:
            sorted_list = case_storage_df.resourcename.unique()
        for i, zone in enumerate(sorted_list):
            subsetdf = case_storage_df[case_storage_df.resourcename == zone]
            """
            if subsetdf.cname.values[0] == "solarELCC_":
                storagezone = re.findall(r"\b[A-Z]+-*[A-Z]*", zone)[0][:-1] + "6hour"
                subsetdf_2 = self.storage_df[
                    (self.storage_df.case_num == (case_num - 3))
                    & (self.storage_df.resourcename == storagezone)
                ]
                #get avg elcc for sub2
                subsetdf_2["minelcc%"] = (
                    subsetdf_2.minelcc * 100.0 / max(case_storage_df.maxelcc)
                )
                subsetdf_2["maxelcc%"] = (
                    subsetdf_2.maxelcc * 100.0 / max(case_storage_df.maxelcc)
                )
                subsetdf_2["avgelcc%"] = (
                    subsetdf_2["maxelcc%"] + subsetdf_2["minelcc%"]
                ) * 0.5
                # find the delta
                
                subsetdf_2["deltaelcc%"] = subsetdf["avgelcc%"] - subsetdf_2["avgelcc%"]
                print(subsetdf_2["deltaelcc%"])
                subsetdf_2.plot.line(
                    x="xval",
                    y="deltaelcc%",
                    c=linecolor_list[case_num - 1],
                    linestyle="--",
                    ax=self.axs[int(i / self.cols), i % self.cols],
                    legend=False,
                )
                # plot the delta!
            """
            subsetdf.plot.line(
                x="xval",
                y="avgelcc%",
                c=linecolor_list[case_num - 1],
                linestyle=linestyle_list[case_num - 1],
                ax=self.axs[int(i / self.cols), i % self.cols],
                legend=False,
            )

            """
            self.axs[int(i / self.cols), i % self.cols].text(
                subsetdf.xval.mean(),
                subsetdf["avgelcc%"].mean() - 12.0,
                "Tx=25%",
                color=linecolor_list[case_num - 1],
            )
            """
            self.axs[int(i / self.cols), i % self.cols].fill_between(
                subsetdf.xval,
                subsetdf["minelcc%"],
                subsetdf["maxelcc%"],
                color="k",
                alpha=0.2,
            )
            self.axs[int(i / self.cols), i % self.cols].set_xlabel(
                self.plot_title_list[place]
            )
        # append data
        self.color_list.append(linecolor_list[case_num - 1])
        self.line_list.append(4)
        self.linestyle_list.append(linestyle_list[case_num - 1])
        self.alpha_list.append(1.0)
        if print_plot:
            # try to grab and print the deltas
            colors = ["black", "red"] + self.color_list
            linewidths = [12, 4] + self.line_list
            linestyles = ["-", "-"] + self.linestyle_list
            alphas = [0.2, 1.0] + self.alpha_list
            # colors = ["black", "red", "blue", "green", "red", "blue"]
            # linewidths = [12, 4, 4, 4, 4, 4]
            # linestyles = ["-", "-", "-", "-", ":", ":"]
            # alphas = [0.2, 1, 1, 1, 1, 1]
            lines = [
                Line2D([0], [0], color=c, linewidth=lw, linestyle=ls, alpha=a)
                for c, lw, ls, a in zip(colors, linewidths, linestyles, alphas)
            ]
            labels = ["80%CI"] + self.label_list
            plt.figlegend(
                lines,
                labels,
                fontsize=16,
                frameon=False,
                bbox_to_anchor=(0.92, 0.15),
                ncol=2,
            )
            plt.tight_layout()
            plt.subplots_adjust(top=0.9)
            filename = "_".join([str(elem) for elem in arglist])
            plt.savefig(
                join(self.results_folder, vary_str + "_ELCC_" + filename + ".jpg",),
                dpi=300,
            )

    def storage_case_load(
        self, arglist, colname, attr_string, n=1, cname="storageELCC_"
    ):
        # out of curiosity, what can be done with the casename?

        casename = cname + self.casename
        pras_filename = self.casename
        for i in arglist:
            casename = self.handler(casename, i)
            pras_filename = self.handler(pras_filename, i)
        casename += "addgulfsolar"
        pras_filename += "addgulfsolar.pras"

        # get the loads so you can determine the normalized EUE and rank cases

        # if you wish to convert storage GW to % of peak load, you need to know peak load, so grab it
        if self.pct_cap == True:
            with h5py.File(join(self.pras_folder, pras_filename), "r") as f:
                peakload = f["regions"]["load"][:].sum(axis=1).max()
        df = pd.read_csv(join(self.elcc_folder, casename + ".csv"))
        if not hasattr(self, "zonal_loads") and self.rankplot:
            with h5py.File(join(self.pras_folder, pras_filename), "r") as f:
                print("grabbing loads from " + pras_filename)
                self.zonal_loads = f["regions"]["load"][:].sum(axis=0)
                normeue = np.asarray(
                    [
                        float(a) / float(b)
                        for a, b in zip(df.ZoneEUE, list(self.zonal_loads))
                    ]
                )
                temp = normeue.argsort()
                self.ranks = np.empty_like(temp)
                self.ranks[temp] = np.arange(len(normeue))
        df["caseID"] = [colname for i in df.index]
        colval = re.search(r"\d+\.?\d*", colname).group()
        # may want to modify colvalue to be a %
        if self.pct_cap and float(colval) > 1.0:
            print("oldcolval is: " + str(colval))
            colval = (1000.0 * float(colval)) / float(peakload)  # with conversion to GW
            print("new is: " + str(colval))
        df["xval"] = [
            int(colval) if float(colval) > 1.0 else int(float(colval) * 100.0)
            for i in df.index
        ]
        if n != 1:
            df["case_num"] = [n for i in df.index]
        df["cname"] = [cname for i in df.index]
        # df["original_load"] = list(self.zonal_loads)
        # df["NormEUE"] = [
        #    float(a) / float(b) for a, b in zip(df.ZoneEUE, df.original_load)
        # ]
        if hasattr(self, attr_string):
            return pd.concat([self.storage_df, df], sort=True)
        return df

    def solar_case_plot(self, vary_str, *args, print_plot=False, case_label="100%tx"):
        self.label_list = [case_label]
        arglist = []
        for counter, i in enumerate(args):
            if type(i) == list:
                argiter = i
                place = counter
            else:
                arglist.append(i)
        for a in argiter:
            arglist.insert(place, a)
            self.solar_df = self.solar_case_load(arglist, a, "solar_df")
            arglist.pop(place)
        self.solar_df["minelcc%"] = self.solar_df.minelcc * (
            100.0 / max(self.solar_df.maxelcc)
        )
        self.solar_df["maxelcc%"] = self.solar_df.maxelcc * (
            100.0 / max(self.solar_df.maxelcc)
        )
        self.solar_df["avgelcc%"] = (
            self.solar_df["maxelcc%"] + self.solar_df["minelcc%"]
        ) * 0.5

        self.solar_df["case_num"] = [1.0 for i in self.solar_df.index]
        rows = 6
        cols = 4
        solar_fig, solar_axs = plt.subplots(
            rows, cols, sharex=True, sharey=True, figsize=(15, 8)
        )
        solar_axs[rows - 1, cols - 2].set_visible(
            False
        )  # bottom r axis is off for visibility
        solar_axs[rows - 1, cols - 1].set_visible(
            False
        )  # bottom r axis is off for visibility
        solar_fig.suptitle("Solar ELCC as function of Storage ICAP", fontsize=30)
        for i, zone in enumerate(self.solar_df.resourcename.unique()):
            self.solar_df[self.solar_df.resourcename == zone].plot.line(
                x="xval",
                y="avgelcc%",
                c="r",
                ax=solar_axs[int(i / cols), i % cols],
                legend=False,
            )
            subsetdf = self.solar_df[self.solar_df.resourcename == zone]
            # solar_axs[int(i / cols), i % cols].text(
            #    subsetdf.xval.values[int(len(subsetdf.xval) / 2.0)],
            #    3.0 + subsetdf["avgelcc%"].values[int(len(subsetdf.xval) / 2.0)],
            #    "Tx=100%",
            #    color="r",
            # )
            # axs[int(i / cols), i % cols].text(
            #    subsetdf.xval.mean(), subsetdf["avgelcc%"].mean(), "Tx=100%"
            # )
            solar_axs[int(i / cols), i % cols].fill_between(
                subsetdf.xval,
                subsetdf["minelcc%"],
                subsetdf["maxelcc%"],
                color="k",
                alpha=0.2,
            )
            solar_axs[int(i / cols), i % cols].set_title(
                zone[: zone.find(re.findall(r"Utility", zone)[0])]
            )
            solar_axs[int(i / cols), i % cols].set_ylabel("ELCC (%)", fontsize=16)
            solar_axs[int(i / cols), i % cols].set_xlabel("ICAP (GW)", fontsize=16)

        # add a manual legend to your plot, if desired
        if print_plot:
            colors = ["black", "red"]
            linewidths = [12, 4]
            alphas = [0.2, 1]
            lines = [
                Line2D([0], [0], color=c, linewidth=lw, alpha=a)
                for c, lw, a in zip(colors, linewidths, alphas)
            ]
            labels = ["80%CI", "AvgELCC(%)"]
            plt.figlegend(
                lines,
                labels,
                fontsize=24,
                frameon=False,
                bbox_to_anchor=(0.88, 0.2),
                ncol=2,
            )
        # store some objects, if desired
        self.solar_fig = solar_fig
        self.solar_axs = solar_axs
        self.solar_cols = cols
        self.solar_rows = rows
        # write plot
        if print_plot:
            filename = "_".join([str(elem) for elem in arglist])
            plt.savefig(
                join(
                    "SOLAR",
                    self.results_folder,
                    vary_str + "_ELCC_" + filename + ".jpg",
                ),
                dpi=300,
            )

    def solar_case_load(self, arglist, colname, attr_string, n=1):
        casename = "solarELCC_" + self.casename
        for i in arglist:
            casename = self.handler(casename, i)
        casename += "addgulfsolar"
        df = pd.read_csv(join(self.elcc_folder, casename + ".csv"))
        df["caseID"] = [colname for i in df.index]
        df["xval"] = [int(re.search(r"\d+", colname).group()) for i in df.index]
        if n != 1:
            df["case_num"] = [n for i in df.index]
        if hasattr(self, attr_string):
            return pd.concat([self.solar_df, df], sort=True)
        return df

    def add_solar_line_to_existing_plot(
        self, vary_str, case_num, *args, print_plot=False, case_label="25%tx"
    ):
        self.label_list.append(case_label)
        pd.set_option("mode.chained_assignment", None)  # for now to suppress warnings
        linecolor_list = ["r", "b", "g"]
        arglist = []
        # create attribute from pre-existing df, but clear it
        self.solar_df_2 = pd.DataFrame(columns=self.solar_df.columns)
        for counter, i in enumerate(args):
            if type(i) == list:
                argiter = i
                place = counter
            else:
                arglist.append(i)
        for a in argiter:
            arglist.insert(place, a)
            # setattr(x, attr, 'magic')
            self.solar_df = self.solar_case_load(arglist, a, "solar_df", n=case_num)
            # setattr(self, attr_ID,self.storage_case_load(arglist, a, attr_ID))
            arglist.pop(place)
        case_solar_df = self.solar_df[self.solar_df.case_num == case_num]
        case_solar_df["minelcc%"] = (
            case_solar_df.minelcc * 100.0 / max(case_solar_df.maxelcc)
        )
        case_solar_df["maxelcc%"] = (
            case_solar_df.maxelcc * 100.0 / max(case_solar_df.maxelcc)
        )
        case_solar_df["avgelcc%"] = (
            case_solar_df["maxelcc%"] + case_solar_df["minelcc%"]
        ) * 0.5

        for i, zone in enumerate(case_solar_df.resourcename.unique()):
            case_solar_df[case_solar_df.resourcename == zone].plot.line(
                x="xval",
                y="avgelcc%",
                c=linecolor_list[case_num - 1],
                ax=self.solar_axs[int(i / self.solar_cols), i % self.solar_cols],
                legend=False,
            )
            subsetdf = case_solar_df[case_solar_df.resourcename == zone]
            # self.solar_axs[int(i / self.solar_cols), i % self.solar_cols].text(
            #    subsetdf.xval.mean(),
            #    subsetdf["avgelcc%"].mean() - 12.0,
            #    "Tx=25%",
            #    color=linecolor_list[case_num - 1],
            # )
            self.solar_axs[int(i / self.solar_cols), i % self.solar_cols].fill_between(
                subsetdf.xval,
                subsetdf["minelcc%"],
                subsetdf["maxelcc%"],
                color="k",
                alpha=0.2,
            )
            self.solar_axs[int(i / self.solar_cols), i % self.solar_cols].set_xlabel(
                "StorageICAP (GW)", fontsize=16
            )

        if print_plot:
            colors = ["black", "red", "blue", "green", "orange", "purple"]
            linewidths = [12, 4, 4, 4, 4, 4]
            alphas = [0.2, 1, 1, 1, 1, 1]
            lines = [
                Line2D([0], [0], color=c, linewidth=lw, alpha=a)
                for c, lw, a in zip(colors, linewidths, alphas)
            ]
            labels = ["80%CI"] + self.label_list
            plt.figlegend(
                lines,
                labels,
                fontsize=24,
                frameon=False,
                bbox_to_anchor=(0.88, 0.2),
                ncol=2,
            )
            filename = "_".join([str(elem) for elem in arglist])
            plt.savefig(
                join(
                    "SOLAR",
                    self.results_folder,
                    vary_str + "_ELCC_" + filename + ".jpg",
                ),
                dpi=300,
            )

    def handler(self, casename, obj):
        if type(obj) == str:
            casename += obj
        else:
            raise ValueError("casename objects must be strings")
        casename += "_"
        return casename


class casemetricplotter(object):
    def __init__(self, folder):
        miso_map = pd.read_excel(
            join(folder, "NREL-Seams Model (MISO).xlsx"), sheet_name="Mapping"
        )
        self.zones = list(miso_map["CEP Bus Name"])
        self.results_folder = join(folder, "results")
        self.metric_folder = join(self.results_folder, "metricresults")
        self.casename = "VRE"
        self.label_list = []

    def load_case(
        self,
        casename,
        n_iter,
        arglist,
        metric1="eue",
        metric2="lole",
        spatiotemporal="region",
    ):
        end_str_1 = spatiotemporal + metric1
        end_str_2 = spatiotemporal + metric2
        load_str = self.casename
        for a in arglist:
            load_str += a
            load_str += "_"

        str_1 = load_str + end_str_1
        str_2 = load_str + end_str_2
        df = pd.read_csv(join(self.metric_folder, str_1 + ".csv"), header=None)
        df2 = pd.read_csv(join(self.metric_folder, str_2 + ".csv"), header=None)
        df["casename"] = [casename for i in df.index]
        df.columns = [metric1, "casename"]
        df[metric2] = list(df2[0])
        df["xval"] = [int(re.findall(r"\d+", i)[0]) for i in df.casename]
        df["zone"] = self.zones
        df["n_iter"] = [n_iter for i in df.index]
        if hasattr(self, "case_df"):
            df = pd.concat([self.case_df, df], sort=True)
        self.metric = metric1
        return df

    def plot_case(self, *args, n_iter=0, txtitle="Tx=100%", add_legend=False):
        self.label_list.append(txtitle)
        arglist = []
        for i, a in enumerate(args):
            if type(a) == str:
                arglist.append(a)
            else:
                caselist = a
                place = i
        for c in caselist:
            arglist.insert(place, c)
            self.case_df = self.load_case(c, n_iter, arglist)
            arglist.pop(place)

        self.case_df["ratio"] = [
            0 if lole == 0.0 else eue / lole
            for eue, lole in zip(self.case_df["eue"], self.case_df["lole"])
        ]
        # with cases zipped, plotting lines should now be possible for both eue and lole
        if n_iter == 0:
            self.rows = 6
            self.cols = 4
            self.fig, self.axs = plt.subplots(
                self.rows, self.cols, sharex=True, sharey=True, figsize=(15, 8)
            )
            self.axs[self.rows - 1, self.cols - 2].set_visible(
                False
            )  # bottom r axis is off for visibility
            self.axs[self.rows - 1, self.cols - 1].set_visible(
                False
            )  # bottom r axis is off for visibility
            self.fig.suptitle(
                "Region EUE/LOLE ratio as function of Storage ICAP", fontsize=30,
            )
        color_list = ["r", "b", "g", "y"]
        linewidth_list = [8, 8, 8, 8]
        # self.case_df.to_csv(join(self.results_folder, "mydf.csv"))
        for i, z in enumerate(self.case_df.zone.unique()):
            subsetdf = self.case_df[
                (self.case_df.zone == z) & (self.case_df.n_iter == n_iter)
            ]
            subsetdf.plot.line(
                x="xval",
                y="ratio",
                c=color_list[n_iter],
                linewidth=2,
                ax=self.axs[int(i / self.cols), i % self.cols],
                legend=False,
            )
            # self.axs[int(i / self.cols), i % self.cols].text(
            #    subsetdf.xval.mean(),
            #    subsetdf["ratio"].mean(),
            #    txtitle,
            #    color=color_list[n_iter],
            # )
            self.axs[int(i / self.cols), i % self.cols].set_title(z)
            self.axs[int(i / self.cols), i % self.cols].set_ylabel("Ratio", fontsize=16)
            self.axs[int(i / self.cols), i % self.cols].set_xlabel(
                "ICAP (GW)", fontsize=16
            )

        lines = [
            Line2D([0], [0], color=color_list[i], linewidth=linewidth_list[i])
            for i in range(len(self.label_list))
        ]
        labels = self.label_list
        if add_legend:
            plt.figlegend(
                lines,
                labels,
                fontsize=20,
                frameon=False,
                bbox_to_anchor=(0.88, 0.2),
                ncol=2,
            )
        plt.xticks(fontsize=12)
        plt.yticks(fontsize=12)
        plt.tight_layout()
        plt.subplots_adjust(top=0.9)
        # name and save file
        filename = "_".join([str(elem) for elem in arglist])
        filename = "RATIO" + "_" + filename  # self.metric.upper()
        # plt.show()

        plt.savefig(
            join(self.results_folder, filename + ".jpg",), dpi=300,
        )

    def add_plot_line(self, *args):
        return None

# storageELCC_VRE0.2_wind_2012base100%_8760_0%tx_18%IRM_nostorage_addgulfsolar

test = plotter(data, join(results, "metricresults"), casename, miso_data)

#here we need to do a few things to make modifications
test.remove_zones("AECIZ")
test.remove_zones("CBPC-NIPCO")
test.format_data()


if NREL:
    nreltester = NRELEFSprofiles(data, NREL_profile)
    load, normprofile = nreltester.run_all(NREL_year)
    test.modify_load_year(NREL_year, load, normprofile)
    l = [m for m in re.finditer("_", NREL_profile)]
    scenario_label = (
        NREL_profile[l[0].end() : l[1].start()] + NREL_profile[l[1].end() :]
    )
else:
    scenario_label = ""

test.updated_geography_plot()
test.updated_geography_plot(subset_lines=False)
test.plot_polygons()
# test.geography_tx_plot("utilization", month=7, hour=16)
# test.geography_plot("region_lole")
# test.geography_plot("region_eue")
#test.heatmap("period_eue")
#test.heatmap("period_lolp")
# test.panel_tx_heatmap("utilization")  # takes awhile
# test.tx_heatmap("15", "utilization")
# test.tx_heatmap("15", "flow")
# test.heatmap("period_lolp", mean=True)

test.plot_zonal_loads(NREL=NREL, year_lab=NREL_year, scenario_lab=scenario_label)

"""

case_obj = casemetricplotter(data)
case_obj.plot_case(
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    "addgulfsolar",
)
case_obj.plot_case(
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "50%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    "addgulfsolar",
    n_iter=1,
    txtitle="Tx=50%",
)
case_obj.plot_case(
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    "addgulfsolar",
    n_iter=2,
    txtitle="Tx=25%",
    add_legend=True,
)


### ELCC plotting as fxn of storage ICAP ###

elcc_obj = ELCCplotter(data, pct_cap=True, rankplot=True)

elcc_obj.storage_case_plot(
    "varystoragecapacity",
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    case_label="100%tx",
)

elcc_obj.add_storage_line_to_existing_plot(
    "varystoragecapacity",
    2,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="25%tx",
)

elcc_obj.add_storage_line_to_existing_plot(
    "varystoragecapacity",
    3,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "50%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="50%tx",
)


elcc_obj.add_storage_line_to_existing_plot(
    "varystoragecapacity",
    4,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "10%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="10%tx",
)

elcc_obj.add_storage_line_to_existing_plot(
    "varystoragecapacity",
    5,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "0%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="0%tx",
)

elcc_obj.add_storage_line_to_existing_plot(
    "varystoragecapacity",
    6,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "150%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="150%tx",
)

elcc_obj.add_storage_line_to_existing_plot(
    "varystoragecapacity",
    7,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "200%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=True,
    case_label="200%tx",
)

# the next plot should include solar
solar_elcc_obj = ELCCplotter(data, pct_cap=True, rankplot=True)

solar_elcc_obj.storage_case_plot(
    "SOLARvarystoragecapacity",
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    case_label="100%tx",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    2,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="25%tx",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    5,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "0%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="0%tx",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    7,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "200%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="200%tx",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    8,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="solar100%tx",
    cname="solarELCC_",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    9,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    ["0GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="solar25%tx",
    cname="solarELCC_",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    12,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "0%tx",
    "18%IRM",
    ["0GWstorage", "12GWstorage", "30GWstorage", "100GWstorage"],
    print_plot=False,
    case_label="solar0%tx",
    cname="solarELCC_",
)

solar_elcc_obj.add_storage_line_to_existing_plot(
    "SOLARvarystoragecapacity",
    14,
    "0.4",
    "wind",
    "2012base100%",
    "8760",
    "200%tx",
    "18%IRM",
    ["0GWstorage", "30GWstorage", "60GWstorage", "100GWstorage"],
    print_plot=True,
    case_label="solar200%tx",
    cname="solarELCC_",
)


VREplot = ELCCplotter(data, rankplot=True)

VREplot.storage_case_plot(
    "varyVREcapacity",
    ["0.2", "0.4", "0.6", "0.8"],
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "30GWstorage",
    case_label="100%tx",
)

VREplot.add_storage_line_to_existing_plot(
    "varyVREcapacity",
    2,
    ["0.2", "0.4", "0.6", "0.8"],
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "30GWstorage",
    print_plot=False,
    case_label="25%tx",
)

VREplot.add_storage_line_to_existing_plot(
    "varyVREcapacity",
    8,
    ["0.2", "0.4", "0.6", "0.8"],
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "30GWstorage",
    print_plot=False,
    case_label="solar100%tx",
    cname="solarELCC_",
)

VREplot.add_storage_line_to_existing_plot(
    "varyVREcapacity",
    9,
    ["0.2", "0.4", "0.6", "0.8"],
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "30GWstorage",
    print_plot=True,
    case_label="solar25%tx",
    cname="solarELCC_",
)


VREplot0GW = ELCCplotter(data, rankplot=True)

VREplot0GW.storage_case_plot(
    "0GWvaryVREcapacity",
    ["0.2", "0.4", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "0GWstorage",
    case_label="100%tx",
)

VREplot0GW.add_storage_line_to_existing_plot(
    "0GWvaryVREcapacity",
    2,
    ["0.2", "0.4", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "0GWstorage",
    print_plot=False,
    case_label="25%tx",
)

VREplot0GW.add_storage_line_to_existing_plot(
    "0GWvaryVREcapacity",
    8,
    ["0.2", "0.4", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "0GWstorage",
    print_plot=False,
    case_label="solar100%tx",
    cname="solarELCC_",
)

VREplot0GW.add_storage_line_to_existing_plot(
    "varyVREcapacity",
    9,
    ["0.2", "0.4", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "0GWstorage",
    print_plot=True,
    case_label="solar25%tx",
    cname="solarELCC_",
)


VREplot60GW = ELCCplotter(data, rankplot=True)

VREplot60GW.storage_case_plot(
    "60GWvaryVREcapacity",
    ["0.2", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "60GWstorage",
    case_label="100%tx",
)

VREplot60GW.add_storage_line_to_existing_plot(
    "60GWvaryVREcapacity",
    2,
    ["0.2", "0.4", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "60GWstorage",
    print_plot=False,
    case_label="25%tx",
)

VREplot60GW.add_storage_line_to_existing_plot(
    "60GWvaryVREcapacity",
    8,
    ["0.2", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "60GWstorage",
    print_plot=False,
    case_label="solar100%tx",
    cname="solarELCC_",
)


VREplot60GW.add_storage_line_to_existing_plot(
    "60GWvaryVREcapacity",
    9,
    ["0.2", "0.4", "0.6"],
    "wind",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "60GWstorage",
    print_plot=True,
    case_label="solar25%tx",
    cname="solarELCC_",
)


balancedVREplot30GW = ELCCplotter(data, rankplot=True)

balancedVREplot30GW.storage_case_plot(
    "balanced30GWvaryVREcapacity",
    ["0.2", "0.4", "0.6", "0.8"],
    "balanced",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "30GWstorage",
    case_label="100%tx",
)

balancedVREplot30GW.add_storage_line_to_existing_plot(
    "balanced30GWvaryVREcapacity",
    2,
    ["0.2", "0.4", "0.6", "0.8"],
    "balanced",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "30GWstorage",
    print_plot=False,
    case_label="25%tx",
)

balancedVREplot30GW.add_storage_line_to_existing_plot(
    "balanced30GWvaryVREcapacity",
    8,
    ["0.2", "0.4", "0.6"],
    "balanced",
    "2012base100%",
    "8760",
    "100%tx",
    "18%IRM",
    "30GWstorage",
    print_plot=False,
    case_label="solar100%tx",
    cname="solarELCC_",
)

balancedVREplot30GW.add_storage_line_to_existing_plot(
    "balanced30GWvaryVREcapacity",
    9,
    ["0.2", "0.4", "0.6"],
    "balanced",
    "2012base100%",
    "8760",
    "25%tx",
    "18%IRM",
    "30GWstorage",
    print_plot=True,
    case_label="solar25%tx",
    cname="solarELCC_",
)
"""