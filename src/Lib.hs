module Lib
    ( someFunc,
      minMaxNormalization,
      zScoreStandardization,
      zeroOneError,
      meanAbsoluteError,
      sumSquaredError,
      worstCaseError,

    ) where
import Control.Applicative (ZipList)
import Foreign (fromBool)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- lecture 10 slide 25
minMaxNormalization :: RealFloat a => ZipList a -> ZipList a
minMaxNormalization xs = (/) <$> numerator <*> denominator
    where
        numerator = (-) <$> xs <*> pure (minimum xs)
        denominator = pure $ maximum xs - minimum xs

-- lecture 10 slide 26
zScoreStandardization :: RealFloat a => ZipList a -> ZipList a
zScoreStandardization xs = (/) <$> numerator <*> denominator
    where
        numerator = (-) <$> xs <*> pure (mean xs)
        denominator = pure $ standardDeviation xs

-- lecture 10 slide 26
mean :: RealFloat a => ZipList a -> a
mean xs = sum xs / fromIntegral (length xs)

-- lecture 10 slide 26
standardDeviation :: RealFloat a => ZipList a -> a
standardDeviation xs =
    sqrt ( 1 / fromIntegral (length xs) * sum ((**2) . (-) (mean xs) <$> xs))

-- lecture 7 slide 18
zeroOneError actual predicted examples =
    sum errors
        where
            errors = (\x -> fromBool $ actual x == predicted x) <$> examples

-- lecture 7 slide 18
meanAbsoluteError actual predicted examples = 
    sum errors
        where
            errors = (\x -> actual x - predicted x) <$> examples

-- lecture 7 slide 18
sumSquaredError actual predicted examples = 
    sum errors
        where
            errors = (\x -> (actual x - predicted x) ** 2) <$> examples

-- lecture 7 slide 18
worstCaseError actual predicted examples = 
    maximum errors
        where
            errors = (\x -> actual x - predicted x) <$> examples