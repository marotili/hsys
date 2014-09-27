{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module ASP 
(
) where
  
import Text.PrettyPrint
import Control.Applicative hiding (empty)
       
import Data.Char
  
data Fact = Fact
     
data Rule = Rule

-- starts lowercase
data Constant = ConstNot Constant | Constant String deriving Show
     
-- starts uppercase
data Variable = VarNot Variable | Variable String deriving Show
     
data Simpleterm a where
  SimplInt :: Int -> Simpleterm Int
  SimplConst :: Constant -> Simpleterm Constant
  SimplVar :: Variable -> Simpleterm Variable
  SimplNot :: Simpleterm a -> Simpleterm ()
deriving instance Show (Simpleterm a)
  
type family Inv f where
  Inv (Simpleterm Int) = Int
  Inv (Simpleterm ()) = ()
  Inv (Simpleterm Constant) = Constant
  Inv (Simpleterm Variable) = Variable
  
instance Num (Simpleterm Int) where
  (SimplInt a) + (SimplInt b) = SimplInt (a + b)
  abs (SimplInt a) = SimplInt (abs a)
  signum (SimplInt a) = SimplInt (signum a)
  (SimplInt a) * (SimplInt b) = SimplInt (a * b)
  negate (SimplInt a) = SimplInt (-a)
  fromInteger (a) = SimplInt (fromInteger a)
  
data Function = 
  Function 
  { fConst :: Constant 
  , fTerms :: [Simpleterm']
  }
  deriving (Show)
   
data Simpleterm' where
  Simpleterm :: Simpleterm a -> Simpleterm'
  
data Term' where
  T :: Term a -> Term'

deriving instance Show Term'
deriving instance Show Simpleterm'
    
data Term a = TermSimple (Simpleterm a) | TermFunc Function
     deriving (Show)
     
data ASP = ASP ([Term']) deriving Show
    
     
ppSimpleterm :: Simpleterm a -> Doc
ppSimpleterm (SimplInt i) = int i
ppSimpleterm (SimplConst const) = ppConst const
ppSimpleterm (SimplVar var) = ppVar var
ppSimpleterm (SimplNot simpleterm) = text "-" <> ppSimpleterm simpleterm
             
ppSimpleterm' (Simpleterm s) = ppSimpleterm s

ppVar (VarNot var) = text "-" <> ppVar var
ppVar (Variable str) = text str

ppTerm (TermSimple _) = text "Simpleterm"
ppTerm (TermFunc func) = ppFunc func
       
ppTerm' (T t) = ppTerm t
       
ppFunc (Function const terms) = ppConst const <> parens (hcat $ punctuate comma $ map ppSimpleterm' terms)
       
ppConst (ConstNot const) = text "-" <> ppConst const
ppConst (Constant str) = text str
       
ppASP (ASP terms) = foldr ($+$) empty (map (\d -> d <> text ".") $ map ppTerm' terms)

class Negatable v where
  not :: v -> v
  
instance Negatable Constant where
  not constant = ConstNot constant
instance Negatable Variable where
  not var = VarNot var
    
const name | length name == 0 = error "Constant without name"
           | isLower (head name) == False = error "Constant must start with lowercase letter"
           | otherwise = Constant name
      
var name | length name == 0 = error "Variable without name"
         | isLower (head name) == True = error "Variable must start with uppercase letter"
         | otherwise = Variable name
    
    
function :: String -> [Simpleterm'] -> Function
function = Function . ASP.const
         
         
def :: ([Simpleterm'] -> Function) -> [Simpleterm'] -> Term'
def a b = T $ TermFunc (a b)
    
n :: Int -> Simpleterm Int
n x = SimplInt x
  
instance Num Simpleterm' where
  (Simpleterm (SimplInt x)) + (Simpleterm (SimplInt y)) = Simpleterm (SimplInt (x + y))
  _ + _ = error "can't add non integers"
  fromInteger (x) = Simpleterm (SimplInt (fromInteger x))
  
c = Simpleterm . SimplConst . ASP.const
v = Simpleterm . SimplVar . ASP.var
        
t :: [Term']
t = do
  let polygon = function "polygon"
  let a = function "a"
  [def a [c "atom", v "X"],
   def polygon [0, 20, 40]]

  
