{-# OPTIONS_GHC -F -pgmF htfpp #-}
    
import Test.Framework
       
prop_function = monadicIO

main = htfMain htf_thisModuleTests
