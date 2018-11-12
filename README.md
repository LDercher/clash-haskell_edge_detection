# clash-haskell_edge_detection

goals:
	1. Take existing haskell implementation of edge detection alg and implement in clash.
	2. benchmark performance and test output vrs. VHDL impl not generated in Haskell. 
	
note: compile with clash -XViewPatterns -XUndecidableInstances -XBangPatterns -XUndecidableSuperClasses -XMultiParamTypeClasses -XRankNTypes guassian_hip1.5.3toclash.hs
