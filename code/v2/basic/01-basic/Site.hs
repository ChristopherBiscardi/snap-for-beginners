{-# LANGUAGE OverloadedStrings #-}
import Snap.Core -- (writeBS, Snap)
import Snap.Http.Server -- (quickHttpServe)

main :: IO ()
main = quickHttpServe handler

handler :: Snap ()
handler = writeBS "Handled"

