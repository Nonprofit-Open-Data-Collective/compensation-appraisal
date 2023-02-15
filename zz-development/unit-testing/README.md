## Instructions for running weights comparision report - V04

These are the instructions for comparing different sets of weights when calculating distance between a user inputted organization and all other organizations in the comparison set. 

1. Open unit-testing/run-comparisons-v04.R. 

2. On line 4, give `folder.name` the name of the folder you would like to store the results in. e.g. `folder.name <- "v04-test14"`

3. Run lines 1-30 to initialize storage. 

4. For each set of weight you want to run,  change the following code to define each set of weights. 

I would recommend not doing more that 2 or 3 sets of weights. Any more than that and the plots in the generated report will be hard to interpret. 

- `weight.set.ID` represents the ID for the set of weights. 
- `geo.weights` is the geography weights. There are 3 levels: `c(LEVEL1, LEVEL2, LEVEL3)`. 
- `r.mission.weights` is the regular mission weights. There are 4 levels: `c(LEVEL1, LEVEL2, LEVEL3, LEVEL4)`. 
- `s.mission.weights` is the regular mission weights. There are 4 levels: `c(LEVEL1, LEVEL2, LEVEL3, LEVEL4, LEVEL5)`. 

See file unit-testing/CodeBooks/code-book-v03.docx (yes version 03) for details on level meanings. 

```
#set 1
weight.set.ID <- 1
geo.weights[weight.set.ID, ] <- c(5,2,1)
r.mission.weights[weight.set.ID, ] <- c(5,2,1,1)
s.mission.weights[weight.set.ID, ] <- c(5,2,1,1,0.5)

#set 2
weight.set.ID <- 2
geo.weights[weight.set.ID, ] <- c(2,2,1)
r.mission.weights[weight.set.ID, ] <- c(3,2,1,1)
s.mission.weights[weight.set.ID, ] <- c(3,2,1,1,0.5)

```

5. Once you have defined your weights set, run the rest of the document. (Everything below the comment "Run the Scripts-"). It take about 5 minutes to generate the report.  Your report and data will store in "unit-testing/test-results/YourFolderName/"
- "graphs.html" is a report of summary graphs/tables of the results
- "full-data.rds" is all data of all comparison orgs used in every comparison
- "test-orgs.rds" is the test orgs that were used (the ones we pretending are user input orgs). This table is included on "graphs.html".
- "weights.rds" is a table of the weights you put in. This table is included on "graphs.html".


