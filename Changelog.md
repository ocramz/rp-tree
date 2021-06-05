0.4

- add function to compute the forest construction parameters from the dataset dimensions (rpTreeCfg)
- add type parameter to RPTree to label tree branching points

0.3.6

- relax lower dependency bounds to ensure it builds with GHC 8.6.5 (== stackage lts 14.27) as well

0.2

- fix 'candidates' such that 'knn' now does the right thing
- now 'knn' accepts a distance function as parameter as well
- add I/O functionality
- some time benchmarks
