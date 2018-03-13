module Value where

newtype ScmNum = ScmNum Integer deriving (Eq, Ord, Read)

instance Show ScmNum where
  show (ScmNum n) = show n


data ScmVal =
    VNil
  | VChar Char
  | VTrue
  | VFalse
  | VNum ScmNum
  | VStr String
  | VSym String
  | VCons { car :: ScmVal
          , cdr :: ScmVal }
  | VClo { closureName :: String
         , closureParams :: [String]
         , closureBody :: ScmVal
         , closureEnv :: Env }
  -- TPrim { primFunc :: }
  deriving Eq

instance Show ScmVal where
  show VNil = "()"
  show (VChar '\n') = "#\\newline"
  show (VChar ' ') = "#\\space"
  show (VChar c) = "#\\" ++ [c]
  show VTrue = "#t"
  show VFalse = "#f"
  show (VNum n) = show n
  show (VStr s) = "\"" ++ s ++ "\""
  show (VSym s) = s
  show (VCons a0 d0) = "(" ++ go a0 d0 ++ ")"
    where
      go a VNil = show a
      go a (VCons a' d') = show a ++ " " ++ go a' d'
      go a d = show a ++ " . " ++ show d


newtype Env = Env [StoreIx] deriving (Eq, Show)


type StoreIx = Int