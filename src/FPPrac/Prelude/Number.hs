module FPPrac.Prelude.Number
  ( Number(..)
  , atoi
  , atof
  )
where

-- | Combined integral and floating number type
data Number
  = I Integer
  | F Float

instance Eq Number where
  (I i1) == (I i2) = i1 == i2
  (F f1) == (F f2) = f1 == f2
  (I i1) == (F f2) = fromIntegral i1 == f2
  (F f1) == (I i2) = f1 == fromIntegral i2

instance Ord Number where
  compare (I i1) (I i2) = compare i1 i2
  compare (F f1) (F f2) = compare f1 f2
  compare (I i1) (F f2) = compare (fromIntegral i1) f2
  compare (F f1) (I i2) = compare f1 (fromIntegral i2)

instance Show Number where
  show (I i) = show i
  show (F f) = show f

instance Num Number where
  (I i1) + (I i2) = I (i1 + i2)
  (F f1) + (F f2) = F (f1 + f2)
  (I i1) + (F f2) = F ((fromInteger i1) + f2)
  (F f1) + (I i2) = F (f1 + (fromInteger i2))
  (I i1) * (I i2) = I (i1 * i2)
  (F f1) * (F f2) = F (f1 * f2)
  (I i1) * (F f2) = F ((fromInteger i1) * f2)
  (F f1) * (I i2) = F (f1 * (fromInteger i2))
  negate (I i)    = I (negate i)
  negate (F f)    = F (negate f)
  abs (I i)       = I (abs i)
  abs (F f)       = F (abs f)
  signum (I i)    = I (signum i)
  signum (F f)    = F (signum f)
  fromInteger     = I

instance Real Number where
  toRational (I i) = toRational i
  toRational (F f) = toRational f

instance Enum Number where
  toEnum         = I . toInteger
  fromEnum (I i) = fromEnum i
  fromEnum (F f) = fromEnum f

instance Integral Number where
  quotRem (I i1) (I i2) = let (i1',i2') = quotRem i1 i2 in (I i1', I i2')
  quotRem (F _)      _  = error "quotRem: first argument is not an integer"
  quotRem _      (F _)  = error "quotRem: second argument is not an integer"
  divMod  (I i1) (I i2) = let (i1',i2') = divMod i1 i2 in (I i1', I i2')
  divMod (F _)      _   = error "divMod: first argument is not an integer"
  divMod _      (F _)   = error "divMod: second argument is not an integer"
  toInteger (I i)       = i
  toInteger (F _)       = error "Can not use 'toInteger' to convert float to integer"

instance Fractional Number where
  (/) (I i1) (I i2) = F $ (fromInteger i1) / (fromInteger i2)
  (/) (F d1) (F d2) = F $ d1 / d2
  (/) (F d1) (I i2) = F $ d1 / (fromInteger i2)
  (/) (I i1) (F d2) = F $ (fromInteger i1) / d2
  fromRational      = F . fromRational

instance RealFrac Number where
  properFraction (F f) = let (b,a) = properFraction f in (b, F a)
  properFraction (I i) = let (b,a) = properFraction (fromIntegral i) in (b, F a)
  truncate (F f)       = truncate f
  truncate (I i)       = truncate ((fromIntegral i) :: Float)
  round (F f)          = round f
  round (I i)          = round ((fromIntegral i) :: Float)
  ceiling (F f)        = ceiling f
  ceiling (I i)        = ceiling ((fromIntegral i) :: Float)
  floor (F f)          = floor f
  floor (I i)          = floor ((fromIntegral i) :: Float)

instance Floating Number where
  pi                    = F pi
  exp (F f)             = F (exp f)
  exp (I i)             = F (exp $ fromIntegral i)
  sqrt (F f)            = F (sqrt f)
  sqrt (I i)            = F (sqrt $ fromIntegral i)
  log (F f)             = F (log f)
  log (I i)             = F (log $ fromIntegral i)
  (F f1) ** (F f2)      = F (f1 ** f2)
  (I i1) ** (I i2)      = F ((fromIntegral i1) ** (fromIntegral i2))
  (F f1) ** (I i2)      = F (f1 ** (fromIntegral i2))
  (I i1) ** (F f2)      = F ((fromIntegral i1) ** f2)
  logBase (F f1) (F f2) = F (logBase f1 f2)
  logBase (I i1) (I i2) = F (logBase (fromIntegral i1) (fromIntegral i2))
  logBase (F f1) (I i2) = F (logBase f1 (fromIntegral i2))
  logBase (I i1) (F f2) = F (logBase (fromIntegral i1) f2)
  sin (F f)             = F (sin f)
  sin (I i)             = F (sin $ fromIntegral i)
  tan (F f)             = F (tan f)
  tan (I i)             = F (tan $ fromIntegral i)
  cos (F f)             = F (cos f)
  cos (I i)             = F (cos $ fromIntegral i)
  asin (F f)            = F (asin f)
  asin (I i)            = F (asin $ fromIntegral i)
  atan (F f)            = F (atan f)
  atan (I i)            = F (atan $ fromIntegral i)
  acos (F f)            = F (acos f)
  acos (I i)            = F (acos $ fromIntegral i)
  sinh (F f)            = F (sinh f)
  sinh (I i)            = F (sinh $ fromIntegral i)
  tanh (F f)            = F (tanh f)
  tanh (I i)            = F (tanh $ fromIntegral i)
  cosh (F f)            = F (cosh f)
  cosh (I i)            = F (cosh $ fromIntegral i)
  asinh (F f)           = F (asinh f)
  asinh (I i)           = F (asinh $ fromIntegral i)
  atanh (F f)           = F (atanh f)
  atanh (I i)           = F (atanh $ fromIntegral i)
  acosh (F f)           = F (acosh f)
  acosh (I i)           = F (acosh $ fromIntegral i)

-- | Converts a String to an Integral Number
atoi :: String -> Number
atoi = I . read

-- | Converts a String to a Floating Number
atof :: String -> Number
atof = F . read
