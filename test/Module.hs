module Module (
       x, y, z
) where
import qualified Data.List as C
-- I want this comment
-- And this one too.
import qualified Util -- cmt right
import qualified Extra as Biz {- block cmt -}
import Data.Map (a,
       b)

f :: Util.One -> Midi.New.Two -> Util.Foo -> C.Result
f x = x * C.z
