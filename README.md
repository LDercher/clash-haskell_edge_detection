# clash-haskell_edge_detection

The purpose of this project is to take an existing implementation of a image edge detection algorithm in Haskell and convert it to code that can be compiled by the clash compiler (see https://clash-lang.org/ for details) 
	
note: compile with 
clash -XViewPatterns -XUndecidableInstances -XBangPatterns -XUndecidableSuperClasses -XMultiParamTypeClasses -XRankNTypes guassian_hip1.5.3toclash.hs
