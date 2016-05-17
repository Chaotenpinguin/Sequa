module Parser where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text(Text, unpack)
import Text.Read(readMaybe)
import Data.Array
--import Debug.Trace

data SLC = A | R | N | D | C | Q | E | G | H | I | L | K | M | F | P | S | T | W | Y | V deriving (Show, Eq, Bounded, Enum, Ix, Read, Ord)

data Sequence = Sequence
              { header  :: SequenceHeader
              , seqData :: [Seqs]
              } deriving (Show, Eq)

data SequenceHeader = SequenceHeader
                    { name    :: Text
                    , width   :: Int
                    , numSeqs :: Int
                    } deriving (Show, Eq)
data Seqs = Seqs
          { seqId   :: Text
          , foo     :: Int
          , sData   :: [Maybe SLC]
          , bar     :: Int 
          } deriving (Show, Eq)

parseSeq :: Parser Seqs
parseSeq = do
           skipMany endOfLine
           id <- takeTill (\c -> c == ' ' || c == '(' || c == '\n')
           skipSpace 
           char '('
           skipSpace
           foo' <- signed decimal
           char ')'
           skipSpace
           seq <- takeTill (\c -> c == ' ' || c == '\n')
           skipSpace
           bar' <- signed decimal
           endOfLine
           return $ Seqs id foo' ((\c -> readMaybe [c]) <$> (unpack seq)) bar'

parseSeqHeader :: Parser SequenceHeader
parseSeqHeader = do
                 takeTill isEndOfLine
                 endOfLine
                 string "AC"
                 skipSpace
                 idName <- takeTill (== ';')
                 takeTill isEndOfLine
                 endOfLine
                 takeTill isEndOfLine
                 endOfLine
                 string "BL"
                 takeTill (== '=')
                 char '='
                 wid <- decimal
                 takeTill (== '=')
                 char '='
                 number <- decimal
                 takeTill isEndOfLine
                 endOfLine
                 return $ SequenceHeader idName wid number

parseSequence :: Parser Sequence
parseSequence = do 
                h    <- parseSeqHeader
                seqs <- count (numSeqs h) parseSeq 
                return $ Sequence h seqs
