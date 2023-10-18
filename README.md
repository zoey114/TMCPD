# TMCPD: Time-Varying Network Community Structure Change-Point Detection

### Introduction:
TMCPD, or Time-Varying Network Community Structure Change-Point Detection, is an approach designed to identify change-points where the community structure within a sequence of time-varying networks undergoes alterations.

The overall workflow is presented below: (a) Construct $T$ sets of time-varying networks $`{G_s^G_s^11,s=1,..,T}`$, Then find common modules for each set of time-varying networks and record the maximized modularity values. (b) Detect the first change point based on the series of modularity. (c) Build sets of time-varying networks starting at the last “change-point” snapshot. Iterate steps (a)&(b) until no change-point. 

<img width="800" alt="workflow" src="https://github.com/zoey114/TMCPD/assets/56131629/9ee3a28e-fcc4-44d3-be9c-9ea20f7eed03">




### Requirements: 


### Example:
