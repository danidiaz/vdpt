{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VDPT.Parser 
    (
        traceTreeParser
    ,   parseIntoEither 
    ,   module VDPT.Types
    ) where

import Data.Monoid
import Data.Tree
import qualified Data.Map as M
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Text.Lazy as AL
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Applicative

import VDPT.Types

traceTreeParser :: Parser (Tree (NodeId -> Attributes))
traceTreeParser = optional (char '\xfeff') *> skipSpace *> go 
  where 
    go = mkNode <$> nodeBegin <*> many1 attribute <*> many go <* nodeEnd
    mkNode nodeType' attrList' children' = 
        Node (\nodeId' -> Attributes nodeId' nodeType' (M.fromList attrList')) children'            

notCharOrEOL :: Char -> Char -> Bool
notCharOrEOL x = \c -> c /= x && not (isEndOfLine c)

nodeBegin :: Parser T.Text  
nodeBegin = T.strip <$> (takeWhile1 (notCharOrEOL '(') <* char '(' <* skipWhile isHorizontalSpace <* skip isEndOfLine <* skipWhile isEndOfLine)

nodeEnd :: Parser ()
nodeEnd = skipWhile isHorizontalSpace <* char ')' <* skipWhile isHorizontalSpace <* ((skip isEndOfLine <* skipWhile isEndOfLine) <|> endOfInput)

attribute :: Parser (T.Text, JSON.Value)
attribute = (,) <$> (T.strip <$> takeWhile1 (notCharOrEOL '=') <* char '=') <*> (skipWhile isHorizontalSpace *> attributeValue <* skip isEndOfLine <* skipWhile isEndOfLine)

attributeValue :: Parser JSON.Value 
attributeValue = 
   (pure JSON.Null <* char '-' <* skipWhile isHorizontalSpace)
   <|>
   (pure (JSON.Bool True) <* string "true" <* skipWhile isHorizontalSpace)
   <|>
   (pure (JSON.Bool False) <* string "false" <* skipWhile isHorizontalSpace)
   <|>
   (JSON.Number <$> scientific <* skipWhile isHorizontalSpace)
   <|>
   (JSON.toJSON <$> (char '[' *> sepBy' listElement (char ',') <* char ']' <* skipWhile isHorizontalSpace))
   <|>
   (JSON.String . T.strip <$> takeWhile1 (not . isEndOfLine))

listElement :: Parser JSON.Value
listElement = JSON.String . T.strip <$> (topFragment1 <|> topFragment2)
  where
    topFragment1 = (\x xs->mconcat (x:xs)) <$> A.takeWhile1 elemChar <*> many topFragmentTrailed 
    topFragment2 = (\xs->mconcat xs) <$> many1 topFragmentTrailed 
    topFragmentTrailed = (<>) <$> parenFragment <*> A.takeWhile elemChar
    elemChar c = c /= ',' && c/= '[' && c/= ']' && c/= '(' && c/= ')' && not (isEndOfLine c)        

    parenFragment = (\x t z -> T.cons x (T.snoc t z)) <$> char '(' <*> insideParentFragment <*> char ')'
    insideParentFragment = (\x xs->mconcat (x:xs)) <$> A.takeWhile nonParen <*> many parenFragmentTrailed 
    parenFragmentTrailed = (<>) <$> parenFragment <*> A.takeWhile nonParen   
    nonParen c = c/= '(' && c/= ')' && not (isEndOfLine c)        

parseIntoEither :: Parser a -> TL.Text -> Either String a
parseIntoEither p lat = AL.eitherResult (AL.parse p lat)
