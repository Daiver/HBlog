module HtmlTemplater where

import Data.Maybe
import qualified Data.Map as Map

data RenderState = RSToken | RSLetter

renderTemplate :: Map.Map String String -> String -> String
renderTemplate dict = inner "" RSLetter 
    where
        getOrSpace key = fromMaybe "" $ Map.lookup key dict
        inner _   _ (x:[])          = [x]
        inner acc st ('{':'{':xs)   = inner "" RSToken xs
        inner acc st ('}':'}':xs)   = getOrSpace (reverse acc) ++ inner "" RSLetter xs
        inner acc RSToken  (x:y:xs) = inner (x:acc) RSToken (y:xs)
        inner acc RSLetter (x:y:xs) = x : inner "" RSLetter (y:xs)

renderTemplateFlipped = flip renderTemplate

inheritTemplate :: Map.Map String String -> String -> String
inheritTemplate dict = inner "" RSLetter 
    where
        getOrSpace key = fromMaybe "" $ Map.lookup key dict
        inner _   _ (x:[])          = [x]
        inner acc st ('{':'%':xs)   = inner "" RSToken xs
        inner acc st ('%':'}':xs)   = getOrSpace (reverse acc) ++ inner "" RSLetter xs
        inner acc RSToken  (x:y:xs) = inner (x:acc) RSToken (y:xs)
        inner acc RSLetter (x:y:xs) = x : inner "" RSLetter (y:xs)


