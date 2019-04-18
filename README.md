# fantasy-baseball
This repo sets out to optimize a fantasy baseball draft. It considers how best to spend a budget given roster constraints, league scoring rules, estimates of player performance, and "keeper" players. This uses a (very) complex objective function with uncertain inputs, and searches over a relatively large space of possibilities quickly.

Some of the components are: 

* Scrape projections
* estimate inputs (machine learning!)
* rank players in league scoring
* keeper draft advice
* trade evaluation
* standings projection
