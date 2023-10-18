README - R Toolbox for Identifying Common Functional Modules
Oct 2016


This package has four components:


data_example: this folder contains an example dataset. The code used to generate the example dataset is also included in this folder under name ‘generateBlock.R’;


‘find_community.R’: this file contains scripts for finding common community structure. The output has two parts; one part is the community membership and the other part is the maximum modularity value. An example is included at the end of the file;

‘significance_test.R’: this file contains scripts for performing significance test on the identified common community structure. The output is the empirical p-value. An example is included at the end of the file;

‘robustness_test.R’: this file contains scripts for performing robustness test on the identified common community structure. The output is a vector containing the normalized mutual information between the identified common community structure and the community structures from perturbed networks. An example is included at the end of the file;


GenLouvain2.0: this folder contains the Matlab package for efficient Louvain method application developed by Jutla et al. (2011-2014). Please refer to:   
Inderjit S. Jutla, Lucas G. S. Jeub, and Peter J. Mucha, "A generalized Louvain method for community detection implemented in MATLAB," http://netwiki.amath.unc.edu/GenLouvain (2011-2014).