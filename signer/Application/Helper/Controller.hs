module Application.Helper.Controller where

import IHP.ControllerPrelude


-- Here you can add functions which are available in all your controllers


dateGregorianIo :: IO (Integer, Int, Int) -- :: (year, month, day)
dateGregorianIo = getCurrentTime >>= return . toGregorian . utctDay

currentDayIo :: IO Day
currentDayIo = getCurrentTime >>= return . utctDay
