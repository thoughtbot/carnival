module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import System.Random (newStdGen, randomRs)
import Text.Markdown (Markdown(..))
import Yesod.Text.Markdown ()
import Stripe.Instances ()

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Web.Stripe.Customer as S

type Token = Text
type USD = Int

type Validated a = Either [Text] a
type Validation a = a -> Validated a

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

freePlanId :: S.PlanId
freePlanId = S.PlanId "personal"

newToken :: IO Token
newToken = do
    g <- newStdGen

    return $ T.pack $ take 30 $ map toChar $ randomRs (0, 61) g

  where
    toChar i
        | i < 26 = toEnum $ i + fromEnum 'A'
        | i < 52 = toEnum $ i + fromEnum 'a' - 26
        | otherwise = toEnum $ i + fromEnum '0' - 52

unMarkdown :: Markdown -> TL.Text
unMarkdown (Markdown t) = t
