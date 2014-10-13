import Control.Applicative
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

main = print $ length sundays

sundays = filter isSunday firsts

firsts = (,,) <$> [1901..2000] <*> [1..12] <*> [1]

isSunday (y,m,d) = (snd $ sundayStartWeek $ fromGregorian y m d) == 0
