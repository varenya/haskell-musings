{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import           PrettyPrint
import           Project
import           Reporting

someProject :: Project ProjectId
someProject = ProjectGroup "Sweden" [stockholm, gottenburg, malmo]
  where
    stockholm = Project "Stockholm" 1
    gottenburg = Project  "GottenBurg" 2
    malmo = ProjectGroup "Malmo" [city, limhman]
    city = Project "Malmo City" 3
    limhman = Project "Lihman" 4
