{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

module Num where

import GHC.Real (Ratio((:%)))
import Data.Ratio
import Control.Applicative
import Parser


newtype ScmNum = ScmNum Rational deriving (Eq, Ord, Num)

instance Show ScmNum where
  show (ScmNum (num :% 1)) = show num
  show (ScmNum (num :% den)) = show num ++ "/" ++ show den

instance Read ScmNum where
  readsPrec _ input =
    case runParser parseNum input of
      Succeed (num, rest) -> [(num, rest)]
      _ -> []
    where
      parseNum :: Parser ScmNum
      parseNum = do
        spaces
        sign  <- maybeOneOf "+-"
        num   <- read <$> some digit
        let num' =
              case sign of 
                Just '-' -> -num
                _        -> num
        slash <- maybeChar '/'
        case slash of
          Nothing -> return $ ScmNum $ num' % 1
          Just _  -> do
            den <- read <$> some digit
            return $ ScmNum $ num' % den

instance Fractional ScmNum where
  ScmNum (num :% den) / ScmNum (num' :% den') = ScmNum $ (num * den') % (den * num')

  fromRational (num :% den) = ScmNum $ num % den