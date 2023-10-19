# TMCPD: Time-Varying Network Community Structure Change-Point Detection

### Introduction:
TMCPD, or Time-Varying Network Community Structure Change-Point Detection, is an approach designed to identify change-points where the community structure within a sequence of time-varying networks undergoes alterations.

The overall workflow is presented below: (a) Construct $T$ sets of time-varying networks $`\{G_s^1,s=1,..,T\}`$. Then find common modules for each set of time-varying networks and record the maximized modularity values. (b) Detect the first change point based on the series of modularity. (c) Build sets of time-varying networks starting at the last “change-point” snapshot. Iterate steps (a)&(b) until no change-point. 

<img width="800" alt="workflow" src="https://github.com/zoey114/TMCPD/assets/56131629/9ee3a28e-fcc4-44d3-be9c-9ea20f7eed03">

### Tutorial:
TMCPD requires a sequence of networks represented in the form of adjacency matrices as input, denoted as $`\{ A(G_{s}),s=1,..., T\}`$. The output has two parts, one contains the community membership along with their corresponding maximum modularity values at each time point. The other part contains the first detected change point.  

The instructions for implementing TMCPD are available in [example](https://github.com/zoey114/TMCPD/blob/main/code/example/example.md). 


### Requirements: 
R version 4.0 or greater is required.

Matlab version R2021b or greater is required. 

The detailed requirements can be found [here](https://github.com/zoey114/TMCPD/blob/main/requirements). 

### Data: 
The DCA data used in this paper can be found [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UPMOHW). 
