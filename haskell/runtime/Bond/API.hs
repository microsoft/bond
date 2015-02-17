module Bond.API (
    bondGet,
    bondPut,
    runFastBinaryGet,
    runFastBinaryPut
  ) where

import Bond.BinaryProto (bondGet, bondPut)
import Bond.FastBinary (runFastBinaryGet, runFastBinaryPut)
