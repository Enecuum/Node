module Node.Extra where

whenRight :: Show a => Either a t -> (t -> IO ()) -> IO ()
whenRight aElem aFunc = case aElem of
    Left  aError    -> putStrLn $ "Error: " ++ show aError
    Right aJustElem -> aFunc aJustElem
