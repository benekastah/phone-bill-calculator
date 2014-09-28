{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import Numeric (showFFloat)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Char (isSpace)

newtype DollarAmount = DollarAmount Float
  deriving (Eq, Ord, Num)

instance Show DollarAmount where
  show (DollarAmount f) = "$" ++ (showFFloat (Just 2) f "")

data PhoneLine = PhoneLine
  { name :: String
  , cost :: DollarAmount
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

promptAmount msg lineName default' = do
  putStrLn $ foldr1 (++) $ catMaybes [ fmap (++ ": ") lineName
                                     , Just $ msg ++ " "
                                     , fmap (("(" ++) . (++ ")") . show . DollarAmount) default'
                                     ]
  amt <- getAmount default'
  maybe (promptAmount msg lineName default') return amt

getPhoneLine defaultCost (n, costs, useDefaults) = do
  c1 <- if useDefaults
    then return defaultCost
    else getCosts $ Just n
  c2 <- promptAmount "Other costs" (Just n) costs
  return PhoneLine {name = n, cost = c1 + c2}

getPhoneLines defaultCost = do
  let lines = map (getPhoneLine defaultCost) [ ("Paul", Just 6.99, True)
                                             , ("Steve", Nothing, True)
                                             , ("Lindsay", Nothing, True)
                                             , ("Rob", Nothing, True)
                                             , ("Chris", Nothing, False)
                                             ]
  lines' <- sequence lines
  return lines'

calculatePhoneLine cshared (PhoneLine {name=n, cost=c}) =
  PhoneLine {name = n, cost = c + cshared}

calculateCosts lines (DollarAmount cshared) =
  map (calculatePhoneLine csharedPerLine) lines
  where
    csharedPerLine = DollarAmount (cshared / (fromIntegral $ length lines))

joinPhoneLines :: [PhoneLine] -> String
joinPhoneLines [] = ""
joinPhoneLines [ln] = (show ln)
joinPhoneLines (ln:xs) = (joinPhoneLines [ln]) ++ "\n" ++ (joinPhoneLines xs)

getCosts lineName = do
  cline <- promptAmount "Mobile Share" lineName (Just 40)
  clineDiscount <- promptAmount "Mobile Share discount" lineName (Just 25)
  csurcharges <- promptAmount "Surcharges and fees" lineName Nothing
  ctaxes <- promptAmount "Government fees & taxes" lineName Nothing
  return $ (cline - clineDiscount) + csurcharges + ctaxes

main = do
  ctotal <- promptAmount "Total bill amount:" Nothing Nothing
  cdata <- promptAmount "Data cost (Mobile Share 10GB with Unlimited Talk & Text)"
                        Nothing
                        (Just 100)
  cdiscount <- promptAmount "National Account Discount" Nothing (Just 22)
  lines <- getCosts Nothing >>= getPhoneLines
  let lines' = calculateCosts lines (cdata - cdiscount)
  let calcTotal = foldl (+) (DollarAmount 0) (map cost lines')
  let result = case ((show ctotal) == (show calcTotal)) of
               True  -> Right lines'
               False -> Left ("Your given total " ++
                           (show ctotal) ++
                           " does not equal the computed total " ++
                           (show calcTotal))
  putStrLn $ case result of (Right ls) -> joinPhoneLines ls
                            (Left s) -> s

-- vim: ts=2 sts=2 sw=2
