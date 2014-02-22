#!/usr/bin/env runhaskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Numeric (showFFloat)
import Control.Exception (catch)
import Data.Maybe (listToMaybe)
import Data.Char (isSpace)

newtype DollarAmount = DollarAmount Float
  deriving (Eq, Ord, Num)

instance Show DollarAmount where
  show (DollarAmount f) =
    "$" ++ (showFFloat (Just 2) f "")

data PhoneLine = PhoneLine {
  name :: String,
  cost :: DollarAmount
  }

instance Show PhoneLine where
  show (PhoneLine {name=n, cost=c}) =
    n ++ "'s bill: " ++ (show c)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

getAmount :: Maybe Float -> IO (Maybe DollarAmount)
getAmount default' = do
  flt <- getLine
  let maybeFlt = case flt of
                 "" -> case default' of
                       Nothing -> Just 0
                       x -> x
                 _ -> (maybeRead ("0" ++ flt))
  return $ case maybeFlt of
           (Just f) -> Just (DollarAmount f)
           Nothing -> Nothing

promptAmount msg default' = do
  let msg' = case default' of
             (Just f) -> msg ++ " (" ++ (show (DollarAmount f)) ++ ")"
             Nothing -> msg
  putStrLn msg'
  amt <- getAmount default'
  case amt of (Just d) -> return d
              Nothing -> do
                putStrLn "Invalid input, try again"
                promptAmount msg default'

getPhoneLine n = do
  c <- promptAmount (n ++ "'s costs") Nothing
  return PhoneLine {name = n, cost = c}

getPhoneLines = do
  let lines = map getPhoneLine ["Paul", "Rob", "Lindsay", "Steve"]
  lines' <- sequence lines
  return lines'

calculatePhoneLine ceach cshared (PhoneLine {name=n, cost=c}) =
  PhoneLine {name = n, cost = c + ceach + cshared}

calculateCosts lines ceach (DollarAmount cshared) =
  map (calculatePhoneLine ceach csharedPerLine) lines
  where
    numLines = length lines
    csharedPerLine = DollarAmount (cshared / (fromIntegral numLines))

joinPhoneLines :: [PhoneLine] -> String
joinPhoneLines [] = ""
joinPhoneLines [ln] = (show ln)
joinPhoneLines (ln:xs) = (joinPhoneLines [ln]) ++ "\n" ++ (joinPhoneLines xs)

main = do
  ctotal <- promptAmount "Total bill amount:" Nothing
  cline <- promptAmount "Mobile Share" (Just 30)
  csurcharges <- promptAmount "Surcharges and fees" Nothing
  ctaxes <- promptAmount "Government fees & taxes" Nothing
  cother <- promptAmount "Any other costs that apply to everyone equally"
                         Nothing
  cdata <- promptAmount "Data cost (Mobile Share 10GB with Unlimited Talk & Text)"
                        (Just 120)
  cdiscount <- promptAmount "National Account Discount" (Just 26.40)
  lines <- getPhoneLines
  let lines' = (calculateCosts lines
                               (cline + csurcharges + ctaxes + cother)
                               (cdata - cdiscount))
  let calcTotal = foldl (+) (DollarAmount 0) (map cost lines')
  let result = case ((show ctotal) == (show calcTotal)) of
               True  -> Right lines'
               False -> Left ("Your given total " ++
                           (show ctotal) ++
                           " does not equal the computed total " ++
                           (show calcTotal))
  putStrLn $ case result of (Right ls) -> joinPhoneLines ls
                            (Left s) -> s
