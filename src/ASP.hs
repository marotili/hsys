{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
module ASP 
(
) where
  
import Text.PrettyPrint
       
import Data.Char
  
data Fact = Fact
     
data Rule = Rule

-- starts lowercase
data Constant = ConstNot Constant | Constant String
     
-- starts uppercase
data Variable = VarNot Variable | Variable String
     
data Simpleterm a where
  SimplInt :: Int -> Simpleterm Int
  SimplConst :: Constant -> Simpleterm Constant
  SimplVar :: Variable -> Simpleterm Variable
  SimlpNot :: Simpleterm ()
  
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
     
data Term a = TermSimple (Simpleterm a) | TermFunc Function
     
data ASP = ASP (forall a. [Term a])

ppTerm (TermSimple _) = text "Simpleterm"
ppTerm (TermFunc _) = text "Function"
ppASP (ASP terms) = foldr ($+$) empty (map ppTerm terms)

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
  
data Simpleterm' where
  Simpleterm :: Simpleterm a -> Simpleterm'
  
data Term' where
  T :: Term a -> Term'
        
t :: [Term']
t = do
  let polygon = function "polygon"
  [def polygon [0, 20, 40]]

