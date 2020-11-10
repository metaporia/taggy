{-# LANGUAGE OverloadedStrings #-}

module Text.Taggy.Entities
  (convertEntities, convertGcideEntities, sgmlEntity) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as Atto
import Data.Attoparsec.Text ((<?>))

-- | Convert all the (currently supported)
--   HTML entities to their corresponding
--   unicode characters.
convertEntities :: T.Text -> T.Text
convertEntities t =
    either (const t) T.concat
  $ Atto.parseOnly entityConverter t

convertGcideEntities :: T.Text -> T.Text
convertGcideEntities t =
  either (const t) T.concat $ Atto.parseOnly gcideEntityConverter t

test = convertEntities . convertGcideEntities $  "&quot;\\'d7 &quot;hello orld\\'d6&quot; aoeu \\'d3\\'d5&quot;"

gcideEntityConverter :: Atto.Parser [T.Text]
gcideEntityConverter = do
  t <- Atto.manyTill Atto.anyChar (Atto.string "\\'" <|> (const "" <$> Atto.endOfInput))
  eof <- Atto.atEnd
  let t' = T.pack t
  if eof
     then return [t']
     else do e <- gcideEntity
             (T.concat [t' , e] :) <$> gcideEntityConverter

--sgmlEntityConverter :: Atto.Parser [T.Text]
--sgmlEntityConverter = do
--  t <- Atto.manyTill Atto.any

entityConverter :: Atto.Parser [T.Text]
entityConverter = do
  t <- Atto.takeTill (=='&')
  eof <- Atto.atEnd
  if eof
    then return [t]
    else do amp       <- Atto.char '&'
            mnextChar <- Atto.peekChar
            mentity   <- maybe (return Nothing) pickParser mnextChar

            case mentity of
              Nothing -> (T.concat [t, T.singleton amp] :)
                           `fmap` entityConverter
              Just ent -> (T.concat [t, ent] :)
                           `fmap` entityConverter

  where pickParser c = if c == '#' then numericEntity else entity

numericEntity :: Atto.Parser (Maybe T.Text)
numericEntity = fmap Just go <|> return Nothing

  where go = do Atto.char '#'
                e <- fmap (T.singleton . chr) (hexa <|> decim)
                Atto.char ';'
                return e
        hexa = do Atto.satisfy (\c -> c == 'x' || c == 'X')
                  Atto.hexadecimal
        decim = Atto.decimal


entity :: Atto.Parser (Maybe T.Text)
entity = (flip HM.lookup htmlEntities <$> go "" 8)
           <|> return Nothing
  where
    go :: T.Text -> Int -> Atto.Parser T.Text
    go _ 0 = mzero
    go s n = do c <- Atto.anyChar
                if c == ';'
                  then return $! s
                  else do done <- Atto.atEnd
                          if done
                            then mzero
                            else go (s <> T.singleton c) (n-1)

-- | Expects entity prefix to have already been discarded/parsed.
gcideEntity :: Atto.Parser T.Text
gcideEntity = do
--  Atto.string "\\'" <?> "gcide entity prefix"
  hex <- Atto.take 2
  case HM.lookup hex gcideEntities of
    Just unicode -> return unicode
    Nothing      -> return $ "\\'" <> hex

sgmlEntity :: Atto.Parser T.Text
sgmlEntity = do
  Atto.char '<'
  entityName <- Atto.takeWhile1 (\c -> c /= ' ' && c /= '<' && c /= '\\' && c /= '/' && c /= '>')
  Atto.char '/'
  mnext <- Atto.peekChar
  case mnext of
    Just '>' -> fail "expected SGML-entity but found self-closing html tag"
    Nothing -> mempty
  case HM.lookup entityName sgmlEntities of
    Just unicode -> return unicode
    Nothing -> fail $ "expected valid sgml entity: found" <> T.unpack entityName




htmlEntities :: HM.HashMap T.Text T.Text
htmlEntities = HM.fromList $
  [ ("quot", "\34")
  , ("amp", "\38")
  , ("apos", "\39")
  , ("lt", "\60")
  , ("gt", "\62")
  , ("nbsp", "\160")
  , ("iexcl", "\161")
  , ("cent", "\162")
  , ("pound", "\163")
  , ("curren", "\164")
  , ("yen", "\165")
  , ("brvbar", "\166")
  , ("sect", "\167")
  , ("uml", "\168")
  , ("copy", "\169")
  , ("ordf", "\170")
  , ("laquo", "\171")
  , ("not", "\172")
  , ("shy", "\173")
  , ("reg", "\174")
  , ("macr", "\175")
  , ("deg", "\176")
  , ("plusmn", "\177")
  , ("sup2", "\178")
  , ("sup3", "\179")
  , ("acute", "\180")
  , ("micro", "\181")
  , ("para", "\182")
  , ("middot", "\183")
  , ("cedil", "\184")
  , ("sup1", "\185")
  , ("ordm", "\186")
  , ("raquo", "\187")
  , ("frac14", "\188")
  , ("frac12", "\189")
  , ("frac34", "\190")
  , ("iquest", "\191")
  , ("Agrave", "\192")
  , ("Aacute", "\193")
  , ("Acirc", "\194")
  , ("Atilde", "\195")
  , ("Auml", "\196")
  , ("Aring", "\197")
  , ("AElig", "\198")
  , ("Ccedil", "\199")
  , ("Egrave", "\200")
  , ("Eacute", "\201")
  , ("Ecirc", "\202")
  , ("Euml", "\203")
  , ("Igrave", "\204")
  , ("Iacute", "\205")
  , ("Icirc", "\206")
  , ("Iuml", "\207")
  , ("ETH", "\208")
  , ("Ntilde", "\209")
  , ("Ograve", "\210")
  , ("Oacute", "\211")
  , ("Ocirc", "\212")
  , ("Otilde", "\213")
  , ("Ouml", "\214")
  , ("times", "\215")
  , ("Oslash", "\216")
  , ("Ugrave", "\217")
  , ("Uacute", "\218")
  , ("Ucirc", "\219")
  , ("Uuml", "\220")
  , ("Yacute", "\221")
  , ("THORN", "\222")
  , ("szlig", "\223")
  , ("agrave", "\224")
  , ("aacute", "\225")
  , ("acirc", "\226")
  , ("atilde", "\227")
  , ("auml", "\228")
  , ("aring", "\229")
  , ("aelig", "\230")
  , ("ccedil", "\231")
  , ("egrave", "\232")
  , ("eacute", "\233")
  , ("ecirc", "\234")
  , ("euml", "\235")
  , ("igrave", "\236")
  , ("iacute", "\237")
  , ("icirc", "\238")
  , ("iuml", "\239")
  , ("eth", "\240")
  , ("ntilde", "\241")
  , ("ograve", "\242")
  , ("oacute", "\243")
  , ("ocirc", "\244")
  , ("otilde", "\245")
  , ("ouml", "\246")
  , ("divide", "\247")
  , ("oslash", "\248")
  , ("ugrave", "\249")
  , ("uacute", "\250")
  , ("ucirc", "\251")
  , ("uuml", "\252")
  , ("yacute", "\253")
  , ("thorn", "\254")
  , ("yuml", "\255")
  , ("OElig", "\338")
  , ("oelig", "\339")
  , ("Scaron", "\352")
  , ("scaron", "\353")
  , ("Yuml", "\376")
  , ("fnof", "\402")
  , ("circ", "\710")
  , ("tilde", "\732")
  , ("Alpha", "\913")
  , ("Beta", "\914")
  , ("Gamma", "\915")
  , ("Delta", "\916")
  , ("Epsilon", "\917")
  , ("Zeta", "\918")
  , ("Eta", "\919")
  , ("Theta", "\920")
  , ("Iota", "\921")
  , ("Kappa", "\922")
  , ("Lambda", "\923")
  , ("Mu", "\924")
  , ("Nu", "\925")
  , ("Xi", "\926")
  , ("Omicron", "\927")
  , ("Pi", "\928")
  , ("Rho", "\929")
  , ("Sigma", "\931")
  , ("Tau", "\932")
  , ("Upsilon", "\933")
  , ("Phi", "\934")
  , ("Chi", "\935")
  , ("Psi", "\936")
  , ("Omega", "\937")
  , ("alpha", "\945")
  , ("beta", "\946")
  , ("gamma", "\947")
  , ("delta", "\948")
  , ("epsilon", "\949")
  , ("zeta", "\950")
  , ("eta", "\951")
  , ("theta", "\952")
  , ("iota", "\953")
  , ("kappa", "\954")
  , ("lambda", "\955")
  , ("mu", "\956")
  , ("nu", "\957")
  , ("xi", "\958")
  , ("omicron", "\959")
  , ("pi", "\960")
  , ("rho", "\961")
  , ("sigmaf", "\962")
  , ("sigma", "\963")
  , ("tau", "\964")
  , ("upsilon", "\965")
  , ("phi", "\966")
  , ("chi", "\967")
  , ("psi", "\968")
  , ("omega", "\969")
  , ("thetasym", "\977")
  , ("upsih", "\978")
  , ("piv", "\982")
  , ("ensp", "\8194")
  , ("emsp", "\8195")
  , ("thinsp", "\8201")
  , ("zwnj", "\8204")
  , ("zwj", "\8205")
  , ("lrm", "\8206")
  , ("rlm", "\8207")
  , ("ndash", "\8211")
  , ("mdash", "\8212")
  , ("lsquo", "\8216")
  , ("rsquo", "\8217")
  , ("sbquo", "\8218")
  , ("ldquo", "\8220")
  , ("rdquo", "\8221")
  , ("bdquo", "\8222")
  , ("dagger", "\8224")
  , ("Dagger", "\8225")
  , ("bull", "\8226")
  , ("hellip", "\8230")
  , ("permil", "\8240")
  , ("prime", "\8242")
  , ("Prime", "\8243")
  , ("lsaquo", "\8249")
  , ("rsaquo", "\8250")
  , ("oline", "\8254")
  , ("frasl", "\8260")
  , ("euro", "\8364")
  , ("image", "\8465")
  , ("weierp", "\8472")
  , ("real", "\8476")
  , ("trade", "\8482")
  , ("alefsym", "\8501")
  , ("larr", "\8592")
  , ("uarr", "\8593")
  , ("rarr", "\8594")
  , ("darr", "\8595")
  , ("harr", "\8596")
  , ("crarr", "\8629")
  , ("lArr", "\8656")
  , ("uArr", "\8657")
  , ("rArr", "\8658")
  , ("dArr", "\8659")
  , ("hArr", "\8660")
  , ("forall", "\8704")
  , ("part", "\8706")
  , ("exist", "\8707")
  , ("empty", "\8709")
  , ("nabla", "\8711")
  , ("isin", "\8712")
  , ("notin", "\8713")
  , ("ni", "\8715")
  , ("prod", "\8719")
  , ("sum", "\8721")
  , ("minus", "\8722")
  , ("lowast", "\8727")
  , ("radic", "\8730")
  , ("prop", "\8733")
  , ("infin", "\8734")
  , ("ang", "\8736")
  , ("and", "\8743")
  , ("or", "\8744")
  , ("cap", "\8745")
  , ("cup", "\8746")
  , ("int", "\8747")
  , ("there4", "\8756")
  , ("sim", "\8764")
  , ("cong", "\8773")
  , ("asymp", "\8776")
  , ("ne", "\8800")
  , ("equiv", "\8801")
  , ("le", "\8004")
  , ("ge", "\8805")
  , ("sub", "\8834")
  , ("sup", "\8835")
  , ("nsub", "\8836")
  , ("sube", "\8838")
  , ("supe", "\8839")
  , ("oplus", "\8853")
  , ("otimes", "\8855")
  , ("perp", "\8869")
  , ("sdot", "\8901")
  , ("vellip", "\8942")
  , ("lceil", "\8968")
  , ("rceil", "\8969")
  , ("lfloor", "\8970")
  , ("rfloor", "\8971")
  , ("lang", "\9001")
  , ("rang", "\9002")
  , ("loz", "\9674")
  , ("spades", "\9824")
  , ("clubs", "\9827")
  , ("hearts", "\9829")
  , ("diams", "\9830")
  ]

-- | Table generated from gcide-0.51/webfont.txt (specifically the table of
-- custom escape sequences).
--
-- The keys are hex numbers and the values are their corresponding unicode
-- characters.
gcideEntities :: HM.HashMap T.Text T.Text
gcideEntities =
  HM.fromList
    $ [ ("80", "Ç")
      , ("81", "ü")
      , ("82", "é")
      , ("83", "â")
      , ("84", "ä")
      , ("85", "à")
      , ("86", "å")
      , ("87", "ç")
      , ("88", "ê")
      , ("89", "ë")
      , ("8a", "è")
      , ("91", "æ")
      , ("92", "Æ")
      , ("93", "ô")
      , ("94", "ö")
      , ("a4", "ñ")
      , ("a6", "⅔")
      , ("a7", "⅓")
      , ("a9", "˝")
      , ("ab", "½")
      , ("ac", "¼")
      , ("b0", " ")
      , ("b4", "´")
      , ("b5", "☞")
      , ("b6", "˝")
      , ("b7", "´")
      , ("b8", "”")
      , ("ba", "‖")
      , ("bc", "§")
      , ("bd", "“")
      , ("be", "ā")
      , ("bf", "‘")
      , ("c0", "ṉ")
      , ("c1", "♯")
      , ("c2", "♭")
      , ("c3", "–")
      , ("c4", "―")
      , ("c5", "t")
      , ("c6", "ī")
      , ("c7", "ē")
      , ("c8", "ḍ")
      , ("c9", "ṇ")
      , ("ca", "ṭ")
      , ("cb", "ĕ")
      , ("cc", "ĭ")
      , ("ce", "ŏ")
      , ("cf", "‐")
      , ("d0", "—")
      , ("d2", "œ")
      , ("d3", "ō")
      , ("d4", "ū")
      , ("d5", "ǒ")
      , ("d6", "ǣ")
      , ("d7", "ō")
      , ("d8", "‖")
      , ("dc", "ŭ")
      , ("dd", "ă")
      , ("de", "˘")
      , ("df", "ȳ")
      , ("e5", "ȧ")
      , ("e7", "h")
      , ("eb", "ð")
      , ("ed", "þ")
      , ("ee", "ã")
      , ("f4", "ȝ")
      , ("f7", "≈")
      , ("f8", "°")
      , ("f9", "•")
      , ("fa", "·")
      , ("fb", "√")
      ]

-- also for gcide
sgmlEntities :: HM.HashMap T.Text T.Text
sgmlEntities = HM.fromList
  [ ("Cced"  , "Ç")
  , ("uum"   , "ü")
  , ("eacute", "é")
  , ("acir"  , "â")
  , ("aum"   , "ä")
  , ("agrave", "à")
  , ("aring" , "å")
  , ("cced"  , "ç")
  , ("ecir"  , "ê")
  , ("eum"   , "ë")
  , ("egrave", "è")
  , ("ium"   , "ï")
  , ("icir"  , "î")
  , ("igrave", "ì")
  , ("Aum"   , "Ä")
  , ("Eacute", "É")
  , ("ae"    , "æ")
  , ("AE"    , "Æ")
  , ("ocir"  , "ô")
  , ("oum"   , "ö")
  , ("ograve", "ò")
  , ("ucir"  , "û")
  , ("ugrave", "ù")
  , ("yum"   , "ÿ")
  , ("Oum"   , "Ö")
  , ("Uum"   , "Ü")
  , ("pound" , "£")
  , ("aacute", "á")
  , ("iacute", "í")
  , ("oacute", "ó")
  , ("uacute", "ú")
  , ("ntil"  , "ñ")
  , ("Ntil"  , "Ñ")
  , ("frac23", "⅔")
  , ("frac13", "⅓")
  , ("sec"   , "˝")
  , ("frac12", "½")
  , ("frac14", "¼")
  , ("?"     , "(?)")
  , ("hand"  , "☞")
  , ("bprime", "˝")
  , ("prime" , "´")
  , ("rdquo" , "”")
  , ("sect"  , "§")
  , ("ldquo" , "“")
  , ("amac"  , "ā")
  , ("lsquo" , "‘")
  , ("nsm"   , "ṉ")
  , ("sharp" , "♯")
  , ("flat"  , "♭")
  , ("imac"  , "ī")
  , ("emac"  , "ē")
  , ("dsdot" , "ḍ")
  , ("nsdot" , "ṇ")
  , ("tsdot" , "ṭ")
  , ("ecr"   , "ĕ")
  , ("icr"   , "ĭ")
  , ("ocr"   , "ŏ")
  , ("OE"    , "Œ")
  , ("oe"    , "œ")
  , ("omac"  , "ō")
  , ("umac"  , "ū")
  , ("ocar"  , "ǒ")
  , ("aemac" , "ǣ")
  , ("oemac" , "ō")
  , ("ucr"   , "ŭ")
  , ("acr"   , "ă")
  , ("cre"   , "˘")
  , ("ymac"  , "ȳ")
  , ("adot"  , "ȧ")
  , ("edh"   , "ð")
  , ("thorn" , "þ")
  , ("atil"  , "ã")
  , ("ndot"  , "ṅ")
  , ("rsdot" , "ṛ")
  , ("yogh"  , "ȝ")
  , ("mdash" , "—")
  , ("divide", "÷")
  , ("deg"   , "°")
  , ("middot", "•")
  , ("root"  , "√")
  , ("th"    , "t") -- FIXME should depict "th" sound as in "the"
  , ("asl"   , "a")
  , ("esl"   , "e")
  , ("isl"   , "i")
  , ("usl"   , "u")
  , ("osl"   , "o")
  , ("adot"  , "ȧ")
  , ("br"    , "\n") -- FIXME replace with "" or "\n"?
  ]
