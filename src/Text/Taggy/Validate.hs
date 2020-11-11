module Validate where

import Text.Taggy.Types (Tag(..))
import qualified Text.Taggy as Tag
import Text.Taggy (run, taggyWith, Node(..), Element(..), nodeChildren)
import qualified Data.Text.Lazy.IO             as TL
import qualified Data.Text.Lazy                as TL
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text.Lazy.Encoding       as TL
import Data.Attoparsec.Text.Lazy (Result(..))
import qualified Data.Text as T
import Data.Semigroup ((<>))
import Text.Pretty.Simple (pPrint)

data ValidationErr a
  = StartsWithClosingTag [a]
  | TagMismatch { _expected :: a
                , _found :: a
                , _unconsumedStream :: [a]}
  deriving (Eq, Show)

showValidationErr :: Int -> ValidationErr a -> ValidationErr a
showValidationErr n verr = case verr of
                             StartsWithClosingTag xs -> StartsWithClosingTag (take (n+10) xs)
                             TagMismatch exp fnd rst -> TagMismatch exp fnd (take n rst)

-- add open tags to stack
-- on close tag, if matches top of stack then pop stack, else err
allTagsClose' :: [Tag] -> Either (ValidationErr Tag) [Tag]
allTagsClose' = flip go []
  where
    go :: [Tag] -> [Tag] -> Either (ValidationErr Tag) [Tag]
    go [] _ = Right [] -- no input
    go (TagOpen name attrs True:t) stack = go t stack -- self-closing tag, ignore
    go (topen@(TagOpen name attrs False):t) stack = go t (topen : stack) -- open tag, add to stack
    go (tclose@(TagClose name):t) [] = Left $ StartsWithClosingTag (tclose:t)
    go (found@(TagClose name):t) (expected@(TagOpen name' attrs _):restOpen)
      | name == name' = go t restOpen -- pop stack on match
      | otherwise = Left $ TagMismatch expected found t
    -- ignore comment, script, & style
    go (_: t) stack = go t stack


-- TODO validate all cide files. Q: make edits to sanitized files or to those
-- in ../gcide ? remember to log all edit (searchably) in ../gcide-typos.md
--
-- * CIDE.A fails
-- * FIXME !!! we can't parse cideSanitized/CIDE.P past "Polyconic"
--   COMPLETED: "polycosmic" had an unclosed <note> element which went
--   unnoticed by 'validateTags'.
--
validateTags
  :: Char -- CIDE letter in [A..Z]
  -> IO (Either (ValidationErr Tag) [Tag])
validateTags cideLetter = do
  resultNodes <- run True . TL.decodeLatin1 <$> BL.readFile
    ("/home/aporia/gcide-hs/gcide-0.51/CIDE." <> [cideLetter])
  case resultNodes of
    Done remainingInput nodes -> return $ allTagsClose' nodes
    Fail _ _ _                -> fail "taggy: attoparsec error"

getExcerpt tags = go tags (Nothing, [])
    where go [] ret = ret
          go (tag:rest) (maybeLastOpen, satisfactory) =
              case tag of
                TagText t -> if T.length t > 12 then go rest (maybeLastOpen, TagText t : satisfactory)
                                                else go rest (maybeLastOpen, satisfactory)
                TagOpen elementName _ _ -> go rest (Just elementName, satisfactory)
                _ -> go rest (maybeLastOpen, satisfactory)



-- TODO show atticus how to run this, examine typo, & log typo in
-- ../gcide-typos.md
--
-- validated cide files: A
validateCide cideLetter = do
  Left verr <- validateTags cideLetter
  pPrint $ showValidationErr 15 verr
  --validationErrToTypoTemplate (Just cideLetter) verr

validateCide' cideLetter = do
  err <- validateTags cideLetter
  return $ case err of
    Left  verr -> [showValidationErr 15 verr]
    Right _    -> []
          --validationErrToTypoTemplate (Just cideLetter) verr

test = validateCide 'F'
