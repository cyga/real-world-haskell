-- file: ch23/PodCabalMain.hs
module Main where

import qualified PodMainGUI
import Paths_pod(getDataFileName)

main = 
    do gladefn <- getDataFileName "podresources.glade"
       PodMainGUI.main gladefn
