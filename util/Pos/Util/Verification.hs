{- |

Machinery to support pure datatype verification.

1. Datatypes can have and not have internal pure consistency. If
datatype doesn't have any implicit rules, it's called safe. 'Integer'
is safe, 'Either Text String' is safe too. Otherwise, it's called
unsafe. 'Coin' is unsafe, because it has implicit upper bound on the
value, which is lower than 'Word64' bound. Newtype over string that is
supposed to contain only strings of length less than 30 symbols is
unsafe.

2. Unsafe types' constructors should have prefix \"Unsafe\" (for
example, 'UnsafeProxySecretKey'). They should also have description of
why are they unsafe. They also should provide an easy way to build a
correct valid type, it can be @mkX@ method or anything similar.

3. If datatype @S@ is a part of @T@ (e.g. record field), then @T@
is not called unsafe.

4. @T@ is called verifiable if it is unsafe or any of its parts are
verifiable.

5. Every verifiable type must have 'PVerifiable' instance.

-}
module Pos.Util.Verification
    (
      PVer
    , pverFail
    , pverField

    , PVerifiable(..)
    , runPVerify
    , runPVerifyFail
    ) where

import           Universum

import qualified Data.Text as T

import           Pos.Util.Util (eitherToThrow)

-- Prefix P is annoying, but we need to distinguish between pure
-- verification and VAR somehow.

-- | Pure verification monad. It accumulates errors along the
-- verification (AST) path.
newtype PVer a = PVer
    { getPVer :: Either [Text] a
    } deriving (Show, Eq, Functor, Applicative, Monad)

data VerError = VerError Text deriving (Show)

instance Exception VerError

-- | Fail inside the 'PVer' monad.
pverFail :: Text -> PVer ()
pverFail t = PVer (Left [t])

-- | Verifies some field, prefixing with the text value in case of
-- error. Prefix is supposed to be the record field name. Use it when
-- you want to specify which field/component are you verifying.
pverField :: Text -> PVer () -> PVer ()
pverField p (PVer v) = PVer $ first (\x -> p:x) v


-- | Things that can be (purely) verified
class PVerifiable a where
    pverify :: a -> PVer ()

runPVerify :: (PVerifiable a) => a -> Either VerError ()
runPVerify a = first (VerError . T.intercalate ".") . getPVer $ pverify a

runPVerifyFail :: (MonadThrow m, PVerifiable a) => a -> m ()
runPVerifyFail = eitherToThrow . runPVerify
