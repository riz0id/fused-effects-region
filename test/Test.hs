{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Carrier.Region
import Control.Effect.Lift
import Control.Monad

till :: Has Region sig io => io Bool -> io a -> io ()
till condition iteration = loop
  where loop = do b <- condition
                  if b then return ()
                       else iteration >> loop

test3_internal :: ( Has Region    sig m
                  , Has (Lift IO) sig m
                  )
               => Handle -> m Handle
test3_internal h1 = do
  h2 <- hOpenFile "./test/ex-file.conf" ReadMode
  fname <- hGetLine h2
  h3 <- hOpenFile fname WriteMode
  hPutStrLn h3 fname
  till (liftM2 (||) (hIsEof h2) (hIsEof h1))
       (hGetLine h2 >>= hPutStrLn h3 >>
        hGetLine h1 >>= hPutStrLn h3)
  sendIO (putStrLn "Finished zipping h1 and h2")
  return h3

test3 :: ( Effect sig
         , Has Region sig m
         , Has (Lift IO) sig m
         )
      => m ()
test3 = do
  h1 <- hOpenFile "./test/fname1.txt" ReadMode
  h3 <- region (test3_internal h1)
  till (hIsEof h1)
       (hGetLine h1 >>= hPutStrLn h3)
  sendIO (putStrLn "test3 done")

main :: IO ()
main = do
  runRegion test3

  return () -- runRegion test1
