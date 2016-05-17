module Main where

import Parser
import Data.Attoparsec.Text
import Data.Text(pack)
import Data.Monoid
import Data.Array
import Control.Monad
import Text.Printf
import Data.List
import System.Environment

generateFArray :: [Seqs] -> Array (SLC,SLC) Integer 
generateFArray s = accumArray (+) 1 ((A,A),(V,V)) seqs
                  where sd         = sData <$> s
                        slcToArray :: [Maybe SLC] -> [Maybe SLC] -> [((SLC,SLC),Integer)]
                        slcToArray (Just a:as) (Just b:bs)
                             | a == b    = ((a,b),1):slcToArray as bs
                             | otherwise = ((a,b),1):((b,a),1):slcToArray as bs
                        slcToArray (_:as)      (_:bs)      = slcToArray as bs
                        slcToArray _           _           = []
                        seqs :: [((SLC,SLC),Integer)]
                        seqs = do
                               (i,a) <- zip [1..] sd
                               (j,b) <- zip [1..] sd
                               guard $ i < j
                               slcToArray a b

generateQArray :: Array (SLC,SLC) Integer -> Array (SLC,SLC) Double
generateQArray a = array (bounds a) dat
               where dat     = [(i,f i) | i <- indices a]
                     f (i,j) = (fromIntegral (a!(i,j))) / s
                     s       = fromIntegral $ sum [a!(i,j) | i <- [A .. V], j <- [A .. V], i <= j]

generateEArray :: Array SLC Double -> Array (SLC, SLC) Double
generateEArray a = array ((A,A),(V,V)) dat
               where dat     = [((i,j), f i j) | i <- [A .. V], j <- [A .. V]]
                     f i j 
                             | i == j    = (a!i)*(a!j) 
                             | otherwise = (a!i)*(a!j)*2

prettyPrintArray :: (Enum i, Ix i, Show a, Show i, PrintfArg a) => String -> Array (i,i) a -> String
prettyPrintArray fmt a = mat
                where ((x1,y1),(x2,y2)) = bounds a
                      d                 = [[printf fmt (a!(e,f)) | e <- [x1..x2]] | f <- [y1..y2]]
                      rows              = intercalate "|" <$> d
                      mat               = unlines (zipWith (<>) captionCol $ caption:rows)
                      caption           = intercalate "|" ((printf "%8s" . show <$> [x1..x2]) :: [String])
                      captionCol        = "        |":((++ "|") . printf "%8s" . show <$> [y1..y2]) :: [String]

generatePArray :: Array (SLC,SLC) Double -> Array SLC Double
generatePArray a = array (A,V) dat
               where dat = [(i, f i) | i <- [A .. V]]
                     f i = a!(i,i)/2 + (sum $ do
                                              j <- [A .. V]
                                              return $ a!(i,j) / 2)

generateSArray :: Array (SLC,SLC) Double -> Array (SLC,SLC) Double -> Array (SLC,SLC) Int
generateSArray q e = array ((A,A),(V,V)) dat
                 where dat     = [(i, f i)| i <- indices q]
                       f (i,j) = round $ (logBase 2 (q!(i,j) / e!(i,j)))*2
                                          

main :: IO ()
main = do
       [f] <- getArgs
       i <- readFile f
       case parseOnly parseSequence (pack i) of
         Left  e    -> print $ "Error parsing: " <> e
         Right seqs -> do
            let fa = generateFArray $ seqData seqs
                qa = generateQArray fa
                pa = generatePArray qa
                ea = generateEArray pa
                sa = generateSArray qa ea
            print "Array f_ij:"
            putStrLn $ prettyPrintArray "%8d" fa
            print "Array q_ij:"
            putStrLn $ prettyPrintArray "%1.6f" qa
            print "Array p_i:"
            print pa
            print "Array e_ij"
            putStrLn $ prettyPrintArray "%1.6f" ea
            print "BLOSUM-Array s_ij"
            putStrLn $ prettyPrintArray "%8d" sa

{--sumBlock :: (Ix a, Ix b, Integer c) => [Text] -> Array (a, b) c 
sumBlock       [] = []
sumBlock (x:y:xs) = a
                     where a = array --}
                

