import "hint" HLint.HLint

ignore "Redundant bracket" = Data.MonadicStreamFunction
ignore "Eta reduce"        = Control.Monad.Trans.MStreamF.transG1
