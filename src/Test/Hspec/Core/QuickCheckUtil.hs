module Test.Hspec.Core.QuickCheckUtil where

import           Control.Applicative
import           Control.Exception
import           Data.IORef
import           Data.Word
import           Data.Bits
import           Test.QuickCheck hiding (Result(..))
import           Test.QuickCheck as QC
import           Test.QuickCheck.Property hiding (Result(..))
import qualified Test.QuickCheck.Property as QCP
import           Test.QuickCheck.IO ()
import           Test.QuickCheck.Random
import           System.Random.TF.Gen

aroundProperty :: (IO () -> IO ()) -> Property -> Property
aroundProperty action (MkProperty p) = MkProperty $ MkProp . aroundRose action . unProp <$> p

aroundRose :: (IO () -> IO ()) -> Rose QCP.Result -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action (reduceRose r >>= writeIORef ref)
  readIORef ref

isUserInterrupt :: QC.Result -> Bool
isUserInterrupt r = case r of
  QC.Failure {theException = me} -> (me >>= fromException) == Just UserInterrupt
  _ -> False

seedToInteger :: (Word64, Word64, Word64, Word64) -> Integer
seedToInteger (a, b, c, d) = pos 0 a + pos 1 b + pos 2 c + pos 3 d
  where
    pos e n = toInteger n `shift` (64 * e)

integerToSeed :: Integer -> (Word64, Word64, Word64, Word64)
integerToSeed n = (pos 0, pos 1, pos 2, pos 3)
  where
    pos e = fromInteger (n `shiftR` (64 * e))

integerToGen :: Integer -> QCGen
integerToGen = QCGen . seedTFGen . integerToSeed
