{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Time.Orphans () where

import Data.Orphans ()

import Control.DeepSeq (NFData (..))
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.Format

#if MIN_VERSION_time(1,8,0)
import Data.Time.Clock.System
#endif

#if !MIN_VERSION_time(1,11,0)
import Data.Fixed (Pico)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
#endif

#if MIN_VERSION_time(1,11,0)
import Data.Ix (Ix (..))
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
#endif

#if !MIN_VERSION_time(1,4,0)
instance NFData Day where
    rnf (ModifiedJulianDay d) = rnf d

instance NFData UniversalTime where
    rnf (ModJulianDate d) = rnf d

instance NFData DiffTime where
    rnf d = d `seq` ()

instance NFData AbsoluteTime where
    rnf d = d `seq` ()

instance NFData UTCTime where
    rnf (UTCTime d t) = rnf d `seq` rnf t

instance NFData NominalDiffTime where
    rnf d = d `seq` ()

instance NFData LocalTime where
    rnf (LocalTime d tod) = rnf d `seq` rnf tod

instance NFData ZonedTime where
    rnf (ZonedTime lt tz) = rnf lt `seq` rnf tz

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = rnf h `seq` rnf m `seq` rnf s

instance NFData TimeZone where
    rnf (TimeZone a b c) = rnf a `seq` rnf b `seq` rnf c
#endif

#if !MIN_VERSION_time(1,6,0)
instance ParseTime UniversalTime where
    -- substituteTimeSpecifier _ = timeSubstituteTimeSpecifier
    -- parseTimeSpecifier _ = timeParseTimeSpecifier
    buildTime l xs = localTimeToUT1 0 (buildTime l xs)

instance FormatTime UniversalTime where
    formatCharacter c = fmap (\f tl fo t -> f tl fo (ut1ToLocalTime 0 t)) (formatCharacter c)

instance Show UniversalTime where
    show t = show (ut1ToLocalTime 0 t)

instance Read UniversalTime where
    readsPrec n s = [ (localTimeToUT1 0 t, r) | (t,r) <- readsPrec n s ]
#endif


#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,11,0)
deriving instance Ord DayOfWeek
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,10,0)
#if __GLASGOW_HASKELL__ <710
deriving instance Typeable DayOfWeek
#endif
deriving instance Data DayOfWeek
#endif

#if MIN_VERSION_time(1,8,0) && !MIN_VERSION_time(1,10,0)
#if __GLASGOW_HASKELL__ <710
deriving instance Typeable SystemTime
#endif

deriving instance Data SystemTime
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,11,1)
instance NFData DayOfWeek where
    rnf !_ = ()

instance NFData CalendarDiffTime where
    rnf (CalendarDiffTime x y) = rnf x `seq` rnf y

instance NFData CalendarDiffDays where
    rnf (CalendarDiffDays x y) = rnf x `seq` rnf y
#endif

#if !MIN_VERSION_time(1,11,0)

instance Read DiffTime where
    readPrec = do
        t <- readPrec :: ReadPrec Pico
        _ <- lift $ char 's'
        return $ realToFrac t

instance Read NominalDiffTime where
    readPrec = do
        t <- readPrec :: ReadPrec Pico
        _ <- lift $ char 's'
        return $ realToFrac t

#endif

#if MIN_VERSION_time(1,11,0) && !MIN_VERSION_time(1,11,1)
instance NFData Month where
    rnf (MkMonth m) = rnf m

instance Enum Month where
    succ (MkMonth a) = MkMonth (succ a)
    pred (MkMonth a) = MkMonth (pred a)
    toEnum = MkMonth . toEnum
    fromEnum (MkMonth a) = fromEnum a
    enumFrom (MkMonth a) = fmap MkMonth (enumFrom a)
    enumFromThen (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromThen a b)
    enumFromTo (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromTo a b)
    enumFromThenTo (MkMonth a) (MkMonth b) (MkMonth c) =
        fmap MkMonth (enumFromThenTo a b c)

instance Ix Month where
    range (MkMonth a, MkMonth b) = fmap MkMonth (range (a, b))
    index (MkMonth a, MkMonth b) (MkMonth c) = index (a, b) c
    inRange (MkMonth a, MkMonth b) (MkMonth c) = inRange (a, b) c
    rangeSize (MkMonth a, MkMonth b) = rangeSize (a, b)

instance NFData QuarterOfYear where
    rnf Q1 = ()
    rnf Q2 = ()
    rnf Q3 = ()
    rnf Q4 = ()

instance NFData Quarter where
    rnf (MkQuarter m) = rnf m

instance Enum Quarter where
    succ (MkQuarter a) = MkQuarter (succ a)
    pred (MkQuarter a) = MkQuarter (pred a)
    toEnum = MkQuarter . toEnum
    fromEnum (MkQuarter a) = fromEnum a
    enumFrom (MkQuarter a) = fmap MkQuarter (enumFrom a)
    enumFromThen (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromThen a b)
    enumFromTo (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromTo a b)
    enumFromThenTo (MkQuarter a) (MkQuarter b) (MkQuarter c) =
        fmap MkQuarter (enumFromThenTo a b c)

instance Ix Quarter where
    range (MkQuarter a, MkQuarter b) = fmap MkQuarter (range (a, b))
    index (MkQuarter a, MkQuarter b) (MkQuarter c) = index (a, b) c
    inRange (MkQuarter a, MkQuarter b) (MkQuarter c) = inRange (a, b) c
    rangeSize (MkQuarter a, MkQuarter b) = rangeSize (a, b)
#endif
