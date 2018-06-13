{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           PrettyPrint
import           Project
import           Reporting

someProject :: Project
someProject = ProjectGroup "Sweden" [stockholm, gottenburg, malmo]
  where
    stockholm = Project 1 "Stockholm"
    gottenburg = Project 2 "GottenBurg"
    malmo = ProjectGroup "Malmo" [city, limhman]
    city = Project 3 "Malmo City"
    limhman = Project 4 "Lihman"
