module Lib
    ( someFunc,
      minMaxNormalization,
      zScoreStandardization,
    ) where
import Control.Applicative (ZipList)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

minMaxNormalization :: RealFloat a => ZipList a -> ZipList a
minMaxNormalization xs = (/) <$> numerator <*> denominator
    where
        numerator = (-) <$> xs <*> pure (minimum xs)
        denominator = pure $ maximum xs - minimum xs

zScoreStandardization :: RealFloat a => ZipList a -> ZipList a
zScoreStandardization xs = (/) <$> numerator <*> denominator
    where
        numerator = (-) <$> xs <*> pure (mean xs)
        denominator = pure $ standardDeviation xs

mean :: RealFloat a => ZipList a -> a
mean xs = sum xs / fromIntegral (length xs)

standardDeviation :: RealFloat a => ZipList a -> a
standardDeviation xs =
    sqrt ( 1 / fromIntegral (length xs) * sum ((**2) . (-) (mean xs) <$> xs))