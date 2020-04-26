# Upper_Mekong_capacity_check
Code used to check how much capacity China has to control Mekong river flow at Chiang Saen.
Note 26th April 2020: The code depends on hydrostreamer v0.5, currently only available in the hydrostreamer github repository dev-branch!

If using this code, or the figures, please cite the following article: {cit}

The repository does NOT include the data (apart from the sub-basin areas) required to run this code.

To replicate the results, you will need:

* The WLE Greater Mekong Dam Observatory dataset: https://wle-mekong.cgiar.org/changes/our-research/greater-mekong-dams-observatory/. Mekong Region Futures Institute (2020), Dataset on the Dams of the Greater Mekong. Bangkok, Mekong Region Futures Institute.

* Observerd streamflow at Chiang Saen hydrological station. https://portal.mrcmekong.org

* GRUN runoff data. Ghiggi, Gionata, Vincent Humphrey, Sonia I. Seneviratne, and Lukas Gudmundsson. 2019. ‘GRUN: An Observations-Based Global Gridded Runoff Dataset from 1902 to 2014’. Earth System Science Data Discussions, March, 1–32. https://doi.org/10.5194/essd-2019-32.

* LORA runoff data. Hobeichi, Sanaa, Gab Abramowitz, Jason Evans, and Hylke E. Beck. 2019. ‘Linear Optimal Runoff Aggregate (LORA): A Global Gridded Synthesis Runoff Product’. Hydrology and Earth System Sciences 23 (2): 851–70. https://doi.org/10.5194/hess-23-851-2019.

* Runoff (mrro) products from the ISIMIP 2a project with variable social forcing scenario. We used models caraib, dlem, dbh, h08, lpjml, matsiro, pcr-globwb, vegas, vic and watergap2 forced with PGFv2, GSWP3, and WFDEI. Gosling, Simon, Hannes Müller Schmied, Richard Betts, Jinfeng Chang, Philippe Ciais, Rutger Dankers, Petra Döll, et al. 2017. ‘ISIMIP2a Simulation Data from Water (Global) Sector’. GFZ Data Services. https://doi.org/10.5880/pik.2017.010 

We used 26 years of data for each, from 1985 to 2010 to estimate runoff volume. All datasets were averaged to a monthly timeseries.

Code licence: MIT 
Figure licence: CC BY-SA 4.0
