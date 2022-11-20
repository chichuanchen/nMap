It contains data for a frontal component (frontal N2) and a parietal component (parietal P2p)  Both occur from 375-475 ms after stimulus onset, but are taken from different channel groups/over different parts of the head. These components should be analyzed SEPARATELY. 

Each row is an individual trial level data (there are >7000 trials total)
First column =subject number
Second column = knower-level (0-8), 
Third column = trial type (1= 2:3, 2= 3:2, 3= 1:2, 4= 2:1, 5= 1:3, and 6=3:1)
Fourth column = Parietal P2p (positive going, microvolts)
Fifth column = Frontal N2 (negative going component, microvolts)
Sixth column = pre- or post- data (1 or 2)

I noticed one subject with NaNs--missing values for KL.  It is probably good to be able to deal with NaNs in your code, so I left it in.