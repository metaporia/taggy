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
  entityName <- Atto.takeWhile1 (\c -> c /= ' ' && c /= '<' && c /= '\\' && c /= '/' && c /= '>') <?> "sgmlEntity: takeWhile1"
  Atto.char '/'
  mnext <- Atto.peekChar
  case mnext of
    Just '>' -> fail "expected SGML-entity but found self-closing html tag"
    _ -> case HM.lookup entityName (sgmlEntities <> htmlEntities) of
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
  , ("frac1x12", "1/12")
  , ("frac34", "\190")
  , ("frac37", "3/7")
  , ("iquest", "\191")
  , ("Agrave", "\192")
  , ("Aacute", "\193")
  , ("Acirc", "\194")
  , ("Atilde", "\195")
  , ("Auml", "\196")
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
  , ("Aring", "\197")
  , ("aring", "\229")
  , ("uring", "u\778")
  , ("xring", "x\778")
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
  , ("div", "\247")
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
  , ("ALPHA", "\913")
  , ("BETA", "\914")
  , ("GAMMA", "\915")
  , ("DELTA", "\916")
  , ("EPSILON", "\917")
  , ("ZETA", "\918")
  , ("ETA", "\919")
  , ("THETA", "\920")
  , ("IOTA", "\921")
  , ("KAPPA", "\922")
  , ("LAMBDA", "\923")
  , ("MU", "\924")
  , ("NU", "\925")
  , ("XI", "\926")
  , ("OMICRON", "\927")
  , ("PI", "\928")
  , ("RHO", "\929")
  , ("SIGMA", "\931")
  , ("TAU", "\932")
  , ("UPSILON", "\933")
  , ("PHI", "\934")
  , ("CHI", "\935")
  , ("PSI", "\936")
  , ("OMEGA", "\937")
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
-- <min/: same as <prime/; see units for latitude & longitude (viz. degrees, minutes, & seconds)
-- TODO fix mathex (probably need to write a preprocessor that renders them to
-- unicode before the main parse pass). For now operators &c. will be replaced
-- naively.
--  - <lbrace2/
-- FIXME
--  - <ocrl/
--  - <colret/: tables are broken atm (and in dico's gcide parser as well)
--  - <colbreak/: more table formatting
--  - <bar/: in table in CIDE.T
--  - <ai/: can't find unicode ai ligature
--  - <als/: ??
--  - <Crev/: ?? abbr for a thousand units
--  - <sb/: ?? dico omits it
--  - repair <fracxy/ to use a single unicode char instead of "x/y"
--  - <ect/: pronunciation diacritic
--  - <ecrl/: all <xcrl/ are pron. diacritics but I don't know what. Are they
--    typos or merely undocumented?
--  - <ecer/: as above

sgmlEntities :: HM.HashMap T.Text T.Text
sgmlEntities = HM.fromList
  [ ("omega"  , "\969")
  , ("omega"  , "\969")
  , ("chi"    , "\967")
  , ("psi"    , "\968")
  , ("Psi"    , "\936")
  , ("phi"    , "\966")
  , ("Phi"    , "\934")
  , ("upsilon", "\965")
  , ("Upsilon", "\933")
  , ("tau"    , "\964")
  , ("Tau"    , "\932")
  , ("sigma"  , "\963")
  , ("Sigma"  , "\931")
  , ("rho"    , "\961")
  , ("Rho"    , "\929")
  , ("pi"     , "\960")
  , ("Pi"     , "\928")
  , ("omicron", "\959")
  , ("Omicron", "\927")
  , ("xi"     , "\958")
  , ("Xi"     , "\926")
  , ("nu"     , "\957")
  , ("Nu"     , "\925")
  , ("Mu"     , "\924")
  , ("mu"     , "\956")
  , ("lambda" , "\955")
  , ("kappa"  , "\954")
  , ("Kappa"  , "\922")
  , ("iota"   , "\953")
  , ("Iota"   , "\921")
  , ("theta"  , "\952")
  , ("Theta"  , "\920")
  , ("eta"    , "\951")
  , ("Eta"    , "\919")
  , ("zeta"   , "\950")
  , ("Zeta"   , "\918")
  , ("epsilon", "\949")
  , ("Epsilon", "\917")
  , ("delta"  , "\948")
  , ("Delta"  , "\916")
  , ("gamma"  , "\947")
  , ("Gamma"  , "\915")
  , ("beta"   , "\946")
  , ("Beta"   , "\914")
  , ("alpha"  , "\945")
  , ("Alpha"  , "\913")
  , ("Cced"   , "Ç")
  , ("uum"    , "ü")
  , ("eacute" , "é")
  , ("acir"   , "â")
  , ("aum"    , "ä")
  , ("agrave" , "à")
  , ("aring"  , "å")
  , ("cced"   , "ç")
  , ("ecir"   , "ê")
  , ("eum"    , "ë")
  , ("xum"    , "x\776")
  , ("egrave" , "è")
  , ("ium"    , "ï")
  , ("icir"   , "î")
  , ("igrave" , "ì")
  , ("Aum"    , "Ä")
  , ("Eacute" , "É")
  , ("ae"     , "æ")
  , ("AE"     , "Æ")
  , ("ocir"   , "ô")
  , ("oum"    , "ö")
  , ("ograve" , "ò")
  , ("ucir"   , "û")
  , ("ugrave" , "ù")
  , ("yum"    , "ÿ")
  , ("Oum"    , "Ö")
  , ("Uum"    , "Ü")
  , ("pound"  , "£")
  , ("aacute" , "á")
  , ("iacute" , "í")
  , ("oacute" , "ó")
  , ("uacute" , "ú")
  , ("frac23" , "⅔")
  , ("frac13" , "⅓")
  , ("sec"    , "˝")
  , ("frac12" , "½")
  , ("frac14" , "¼")
  , ("?"      , "(?)")
  , ("hand"   , "☞")
  , ("bprime" , "˝")
  , ("prime"  , "´")
  , ("rdquo"  , "”")
  , ("rdqo"   , "”")
  , ("sect"   , "§")
  , ("ldquo"  , "“")
  , ("ldqo"   , "“")
  , ("amac"   , "ā")
  , ("lsquo"  , "‘")
  , ("nsm"    , "ṉ")
  , ("sharp"  , "♯")
  , ("flat"   , "♭")
  , ("imac"   , "ī")
  , ("emac"   , "ē")
  , ("Omac"   , "\332")
  , ("dot"    , "\729")
  , ("dsdot"  , "ḍ")
  , ("nsdot"  , "ṇ")
  , ("tsdot"  , "ṭ")
  , ("usdot"  , "u\803")
  , ("lsdot"  , "l\803")
  , ("csdot"  , "c\803")
  , ("zsdot"  , "z\803")
  , ("xsdot"  , "x\803")
  , ("ecr"    , "ĕ")
  , ("icr"    , "ĭ")
  , ("ocr"    , "ŏ")
  , ("ycr"    , "y̆")
  , ("ucr"    , "ŭ")
  , ("acr"    , "ă")
  , ("xcr"    , "x\774")
  , ("OE"     , "Œ")
  , ("oe"     , "œ")
  , ("omac"   , "ō")
  , ("umac"   , "ū")
  , ("xmac"   , "x\772")
  , ("ocar"   , "ǒ")
  , ("xcar"   , "x\780")
  , ("aemac"  , "ǣ")
  , ("oemac"  , "ō")
  , ("cre"    , "˘")
  , ("ymac"   , "ȳ")
  , ("adot"   , "ȧ")
  , ("mdot"   , "m\775")
  , ("zdot"   , "z\775")
  , ("2dot"   , "2\775")
  , ("3dot"   , "3\775")
  , ("xdot"   , "x\775")
  , ("edh"    , "ð")
  , ("thorn"  , "þ")
  , ("ndot"   , "ṅ")
  , ("rsdot"  , "ṛ")
  , ("yogh"   , "ȝ")
  , ("mdash"  , "—")
  , ("divide" , "÷")
  , ("deg"    , "°")
  , ("middot" , "•")
  , ("root"   , "√")
  , ("th"     , "ᵺ")
  , ("asl"    , "a")
  , ("esl"    , "e")
  , ("isl"    , "i")
  , ("usl"    , "u")
  , ("osl"    , "o")
  , ("adot"   , "ȧ")
  , ( "br"
    , "\n"
    ) -- FIXME replace with "" or "\n"?
  , ("add"    , "a")
  , ("ait"    , "a")
  , ("eit"    , "e")
  , ("iit"    , "i")
  , ("oit"    , "o")
  , ("uit"    , "u")
  , ("hsdot"  , "h\803")
  , ("msdot"  , "m\803")
  , ("etil"   , "ẽ")
  , ("itil"   , "ĩ")
  , ("otil"   , "õ")
  , ("util"   , "ũ")
  , ("ntil"   , "ñ")
  , ("Atil"   , "Ã")
  , ("Etil"   , "Ẽ")
  , ("Itil"   , "Ĩ")
  , ("Otil"   , "Õ")
  , ("Util"   , "Ũ")
  , ("atil"   , "ã")
  , ("stil"   , "s\771")
  , ("mtil"   , "m\771")
  , ("ltil"   , "l\771")
  , ("xtil"   , "x\771")
  , ("Ntil"   , "Ñ")
  , ("ssmile" , "˘")
  , ("digamma", "ϝ")
  , ("oocr"   , "o\861o")
  , ( "oomac"
    , "o\862o"
    )
-- FIXME oomcr is not listed in webfont.txt: is it meant to be macron or crescent (breve)?
  , ("oomcr"        , "o\862o")
  , ("ounceap"      , "℥")
  , ("min"          , "\8242")
  , ("asper"        , "a\787")
  , ("spasp"        , "\8190")
  , ("udd"          , "u")
  , ("frac1x2500"   , "1/25000")
  , ("frac1x10000"  , "1/10000")
  , ("frac1x1000"   , "1/1000")
  , ("frac2x10"     , "2/10")
  , ("frac17x175"   , "17/175")
  , ("frac1x24"     , "1/24")
  , ("frac25x100"   , "25/100")
  , ("frac1x100"    , "1/100")
  , ("frac1x10"     , "1/10")
  , ("frac1x20"     , "1/20")
  , ("frac1x60"     , "1/60")
  , ("frac1x6000"   , "1/6000")
  , ("frac3x16"     , "3/16")
  , ("frac12x13"    , "12/13")
  , ("frac1x3600"   , "1/3600")
  , ("frac1x216000" , "1/216000")
  , ("frac1x50000"  , "1/50000")
  , ("frac925x1000" , "925/1000")
  , ("frac35"       , "3/5")
  , ("frac25"       , "2/5")
  , ("frac43"       , "4/3")
  , ("frac12"       , "1/2")
  , ("frac67"       , "6/7")
  , ("frac16"       , "1/6")
  , ("frac18"       , "1/8")
  , ("frac19"       , "1/9")
  , ("frac95"       , "9/5")
  , ("frac59"       , "5/9")
  , ("frac58"       , "5/8")
  , ("frac32"       , "3/2")
  , ("frac36"       , "3/6")
  , ("ncir"         , "n\770")
  , ("xcir"         , "x\770")
  , ("pause"        , "𝄑")
  , ("umlaut"       , "\252")
  , ("divby"        , "/")
  , ("EDH"          , "Ð")
  , ("ocrl"         , "o")
  , ("ldquo"        , "“")
  , ("ldqu"         , "“")
  , ("integral2l"   , "∫")
  , ("colret"       , "\n")
  , ("colbreak"     , "\n")
  , ("frac1x108719" , "1/108719")
  , ("frac1x8000"   , "1/8000")
  , ("frac1x6400"   , "1/6400")
  , ("frac1x29966"  , "1/29966")
  , ("frac00"       , "0/0")
  , ("frac17"       , "1/7")
  , ("frac38"       , "1/7")
  , ("fllig"        , "ﬂ")
  , ("ffllig"       , "ﬄ")
  , ("filig"        , "ﬁ")
  , ("ccaron"       , "c\780")
  , ("cacute"       , "c\769")
  , ("natural"      , "♮")
  , ("schwa"        , "\399")
  , ("astascending" , "☊")
  , ("astdescending", "\9739")
  , ("ai"           , "ai")
  , ("Jupiter"      , "♃")
  , ("Sun"          , "☉")
  , ("lbrace2"      , "[")
  , ("rbrace2"      , "]")
  , ("als"          , "a")
  , ("cuberoot"     , "∛")
  , ("sigmat"       , "\963")
  , ("Aries"        , "\9800")
  , ("Taurus"       , "9801")
  , ("taurus"       , "9801")
  , ("Gemini"       , "♊")
  , ("Cancer"       , "♋")
  , ("Leo"          , "♌")
  , ("Sagittarius"  , "♐")
  , ("Capricornus"  , "♑")
  , ("Capricorn"    , "♑")
  , ("Aquarius"     , "♒")
  , ("Pisces"       , "♓")
  , ("pisces"       , "♓")
  , ("Virgo"        , "♍")
  , ("Libra"        , "♎")
  , ("libra"        , "♎")
  , ("Scorpio"      , "♏")
  , ("scorpio"      , "♏")
  , ("Crev"         , "<Crev/")
  , ("asterism"     , "⁂")
  , ("sb"           , "")
  , ("par"          , "\182")
  , ("8star"        , "\10037")
  , ("mercury"      , "\9791")
  , ("ect"          , "e")
  , ("dagger"       , "\8224")
  , ("dag"          , "\8224")
  , ("Dagger"       , "\8225")
  , ("male"         , "♂")
  , ("ecrl"         , "e")
  , ("ddag"         , "\8225")
  , ("ecer"         , "e")
  , ("xsl"          , "x")
  , ("upslur"       , "upslur")
  , ("downslur"     , "downslur")
  , ("dele", "dele/deleatur (Edit: similar to the German pfennig symbol (₰).)")
  , ("breve"        , "\728")
  , ( "nsc"
    , "\628"
    ) -- N small capital
  , ("bar", "|")
  ]



-- TODO 
-- oomcr
-- 8star
-- fract (bare)
-- mercury
-- ect ? mispelling of 
-- dag 
-- male
-- xcar
-- ecrl
-- iques
-- ddag
-- xcr
-- xtil
-- ecer
-- xsmac (unused; only in webfont)
-- xmac
-- xsl
-- downslur (no unicode character)
-- upslur
-- xum
-- dele
-- WORD
-- eacut
-- breve
-- nsc
-- bar
