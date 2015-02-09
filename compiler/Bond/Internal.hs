module Bond.Internal (
    module X,
    putInt32le
  ) where

import Bond.Types as X
import Bond.Default as X
import Bond.FastBinary as X
import Bond.Wire as X
import Data.Binary.Put (putWord32le)

putInt32le :: Int32 -> FastBinaryM
putInt32le = putWord32le . fromIntegral
