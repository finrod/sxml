structure XmlParser =
struct
  open ParserCombinators
  open XmlTypes

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || <|> ??

  type word = Word32.word
  fun uc c = (Word32.fromInt o ord) c
  fun u s = map uc (String.explode s)
  val us = string o u
  val wchr = chr o Word32.toInt
  fun const x = (fn _ => x)
  fun ++ (u, v) = Vector.concat [u, v]
  infixr 7 ++
  fun somes xs =
      let fun aux ([], acc) = rev acc
            | aux (NONE :: xs, acc) = aux (xs, acc)
            | aux (SOME x :: xs, acc)  = aux (xs, x :: acc)
      in aux (xs, [])
      end

  fun match rng = any suchthat Ranges.match rng
  val char = match Ranges.chars

  val ws = match Ranges.ws
  val space = literal (uc #" ")

  fun sepBy1 f g = g && repeat (f >> g) wth op::
  fun notIn xs = try (char suchthat (fn x => List.all (fn y => x <> y) xs))
  fun oneOf xs = try (char suchthat (fn x => List.exists (fn y => x = y) xs))
  val ows = opt ws
  fun owop p = ows >> p << ows
  fun parens p = us"(" >> owop p << us")"

  val nameStart = match Ranges.nameStart
  val nameChar = match Ranges.nameChar
  val name = nameStart && repeat nameChar wth Vector.fromList o op::
  val names = sepBy1 space name
  val nmtoken = repeat1 nameChar wth Vector.fromList
  val nmtokens = sepBy1 space nmtoken

  fun isDig n = (n >= uc #"0" andalso n <= uc #"9")
  fun isHex n = isDig n orelse
                (n >= uc #"A" andalso n <= uc #"F") orelse
                (n >= uc #"a" andalso n <= uc #"f")
  fun hexTrans s = Word32.fromString ("0wx" ^ String.implode s)
  fun decTrans s = Word32.fromString ("0w" ^ String.implode s)
  val hexdig = any suchthat isHex
  val dig = any suchthat isDig
  (* TODO: fix *)
  fun nonXml xs = true

  val charRef = us"#" >> (us"x" >> repeat1 (hexdig wth wchr) << us";" when hexTrans
                             <|> repeat1 (dig wth wchr) << us";" when decTrans)
  val entityRef = name << us";"
  val reference = us "&" >> (entityRef wth RefEnt <|> charRef wth RefChar)
  val peRef = us"%" >> name << us";"

  fun quote p = us"\"" >> p (u"\"") << us "\""
            <|> us"'" >> p (u"'") << us"'"

  val attValue =
      quote (fn s => repeat (repeat1 (notIn (s @ u"<&")) wth Sum.INL o Vector.fromList
                        <|> reference wth Sum.INR))
  val systemLiteral = quote (repeat o notIn) wth Vector.fromList
  val pubidChar = ws suchthat (fn x => x <> 0wx9) <|> match Ranges.pubidChar
  val pubidLiteral = quote (fn s => repeat (not (oneOf s) >> pubidChar)) wth Vector.fromList

  val charData' = repeat1 (not (us"]]>") >> notIn (u"<&")) wth Vector.fromList
  val charData = !! (charData') wth (fn (x, p) => CStr (false, x, p))

  val comment = us"<!--" >> repeat (not (us"--") >> char) << us"-->"
                  wth Comment o Vector.fromList

  val piTarg = name suchthat nonXml
  fun piarg p = (case p of SOME s => Vector.fromList s | NONE => #[])
  val pi = us"<?" >> piTarg && (opt (ws >> repeat (not (us"?>") >> char)) wth piarg) << us"?>" wth PI

  val cdsect' = us"<![CDATA[" >> repeat (not (us"]]>") >> char) << us"]]>" wth Vector.fromList
  val cdsect = !! (cdsect') wth (fn (x, p) => CStr (true, x, p))

  val misc = (!! (try pi) <|> !! comment) wth CMisc
  val miscWS = repeat (ws wth const NONE <|> (try pi <|> comment) wth SOME) wth somes
  val eq = owop (us"=")

  val attribute = name << eq && attValue wth Attr
  val startTag = us"<" >> name && repeat (ws >> attribute) << ows << us">"
  val endTag = us "</" >> name << ows << us">"
  val emptyElemTag = us"<" >> name && repeat (ws >> attribute) << ows << us"/>"
  fun elem () =
      let fun mkElem ((sname, attrs), (cntnt, ename)) =
              if sname = ename then SOME (Element (sname, attrs, cntnt)) else NONE
      in try emptyElemTag wth (fn (name, attrs) => Element (name, attrs, []))
             <|> (startTag && $cont && endTag) when mkElem
      end
  and cont () = repeat (charData <|> !! reference wth CRef <|> !! ($elem) wth CElem || misc || cdsect)
  val element = $elem
  val content = $cont

  (* xml declarations *)
  val verNum = us"1." && repeat dig wth String.implode o map wchr o op@
  val bar = owop (us"|")
  val verInf = ws >> us"version" >> eq >> (quote (const verNum))
  val sdDecl = ws >> us"standalone" >> eq >>
                  quote (const (us"yes" return true <|> us"no" return false))
  val encName = match Ranges.encStart && repeat (match Ranges.encChar)
                  wth String.implode o map wchr o op::
  val encDecl = ws >> us"encoding" >> eq >> quote (const encName)
  val xmlDecl = us"<?xml" >> verInf && opt encDecl && opt sdDecl << ows << us"?>"
                  wth XMLDecl o flat3

  (* DTD part *)
  val entityVal =
      let fun aux s = repeat (repeat (notIn (s @ u"%&")) wth DTD.EVVal o Vector.fromList
                             <|> reference wth DTD.EVRef (* <|> peRef wth ?? *))
      in quote aux wth DTD.EntVal
      end
  val nDataDecl = ws >> us"NDATA" >> ws >> name
  val externalID = us"SYSTEM" >> ws >> systemLiteral wth DTD.SYSTEM
               <|> us"PUBLIC" >> ws >> pubidLiteral && ws >> systemLiteral wth DTD.PUBLIC
  val geDecl = name && ws >> ((entityVal wth DTD.DefEntVal
                           <|> externalID && opt nDataDecl wth DTD.DefExtId) wth DTD.GEDecl)
  val peDecl = us"%" >> name && ws >> ((entityVal wth DTD.PEDEntVal
                                    <|> externalID wth DTD.PEDExtId) wth DTD.PEDecl)
  val entityDecl = us"<!ENTITY" >> ws >> (geDecl <|> peDecl) << ows << us">" wth DTD.EntityDecl

  fun mkChT (nd, NONE)   = (nd, DTD.ONE)
    | mkChT (nd, SOME c) = if c = uc#"?" then (nd, DTD.OPT) else
                           if c = uc#"*" then (nd, DTD.MANY) else (nd, DTD.MANY1)
  fun cp () = (name wth DTD.TagName <|> $choiceSeq) && opt (oneOf (u"?*+")) wth mkChT
  and choiceSeq () = parens ($cp && ($choiceRst <|> $seqRst)) wth (fn (x, f) => f x)
  and choiceRst () = repeat1 (bar >> $cp) wth (fn xs => fn x => DTD.Choice (x :: xs))
  and seqRst () = repeat (ows >> us"," >> ows >> $cp) wth (fn xs => fn x => DTD.Seq (x :: xs))

  val children = $choiceSeq && opt (oneOf (u"?*+")) wth mkChT

  val mixed = parens (us"#PCDATA" >> repeat (bar >> name))<< us"*" wth DTD.PCDATAplus
          <|> parens (us"#PCDATA") return DTD.PCDATA

  val tokType = us"IDREFS" return DTD.IDREFS || us"IDREF" return DTD.IDREF
             || us"ID" return DTD.ID || us"ENTITIES" return DTD.ENTITIES
             || us"ENTITY" return DTD.ENTITY || us"NMTOKENS" return DTD.NMTOKENS
             || us"NMTOKEN" return DTD.NMTOKEN
  val enumType = us"NOTATION" >> ws >> parens (name && repeat (bar >> name)) wth DTD.NotationType o op::
              <|> parens (nmtoken && repeat (bar >> nmtoken)) wth DTD.Enumeration o op::
  val attType = us"CDATA" return DTD.StringTy <|> tokType wth DTD.TokTy <|> enumType wth DTD.EnumTy

  val defaultDecl = us"#REQUIRED" return DTD.REQUIRED <|> us"#IMPLIED" return DTD.IMPLIED
                <|> (opt (us"#FIXED" << ws) wth isSome) && attValue wth DTD.DefaultsTo


  val attDef = ws >> name << ws && attType << ws && defaultDecl wth DTD.AttrDef o flat3
  val attlistDecl = us"<!ATTLIST" >> ws >> name && repeat attDef << ows << us">"
                      wth DTD.AttlistDecl

  val contentspec = us"EMPTY" return DTD.EMPTY <|> us"ANY" return DTD.ANY
                <|> mixed wth DTD.Mixed <|> children wth DTD.Children
  val elementdecl = us"<!ELEMENT" >> ws >> name && ws >> contentspec << ows << us">"
                      wth DTD.ElemDecl
  val publicID = us"PUBLIC" >> ws >> pubidLiteral wth DTD.PublicID
  val notationdecl = us"<!NOTATION" >> ws >> name << ws &&
                       (externalID wth DTD.NotExt <|> publicID wth DTD.NotPub)
                     << ows << us">" wth DTD.NotationDecl
  val markupdecl = elementdecl <|> attlistDecl <|> entityDecl
               <|> notationdecl <|> (try pi <|> comment) wth DTD.MarkupMisc
  val intSubset = repeat (markupdecl wth SOME o DTD.Markup
                      <|> peRef wth SOME o DTD.PERef <|> ws return NONE) wth somes

  val doctypedecl = us"<!DOCTYPE" >> ws >> name && opt (ws >> externalID) << ows
                    && (opt (us"[" >> intSubset << us"]" << ows)
                            wth (fn NONE => [] | SOME xs => xs)) << us ">"
                    wth DTD.DTD o flat3

  val prolog = opt xmlDecl && miscWS && opt (doctypedecl && miscWS) wth Prolog o flat3

  val ignore = repeat (not (us"<![" <|> us"]]>") >> char) wth Vector.fromList
  fun ignoreSC () = ignore && repeat (us"<![" >> $ignoreSC << us"]]>" && ignore)
                    wth DTD.Ignore

  fun conditionalSect () =
      us"<![" >> owop (us"INCLUDE") >> us"[" >> $extSubsetDecl << us"]]>" wth DTD.IncludeSect
   || us"<![" >> owop (us"IGNORE") >> us"[" >> repeat ($ignoreSC) << us"]]>" wth DTD.IgnoreSect
  and extSubsetDecl () = repeat (markupdecl wth SOME o DTD.ExtMarkup
                             <|> $conditionalSect wth SOME o DTD.ExtCondSect
                             <|> peRef wth SOME o DTD.ExtPERef <|> ws return NONE) wth somes

  val textDecl = us"<?xml" >> opt verInf && encDecl << ows << us"?>" wth DTD.TextDecl
  val extSubset = opt textDecl && $extSubsetDecl wth DTD.ExtSubset

  val extParsedEnt = textDecl && content
  val document = prolog && element && miscWS wth Document o flat3

  exception UnknownEncoding
  exception UnsupportedEncoding of string
  exception EncodingMismatch of string * string
  datatype enc = U16 | U8 | U8_ISO | U8_NONSPEC

  local
      open Stream
      fun matchEnc (U16, s) =
          if s = "UTF-16" then () else raise EncodingMismatch ("UTF-16", s)
        | matchEnc (U8, s) =
          if s = "UTF-8" then () else raise EncodingMismatch ("UTF-8", s)
        | matchEnc (U8_ISO, s) =
          if s = "UTF-8" then ()
          else if substring (s, 0, 9) = "iso-8859-" then raise UnsupportedEncoding s
          else raise EncodingMismatch ("utf-8 or iso", s)
        | matchEnc _ = raise UnknownEncoding
      fun eol s = (case front s of
                       Cons (0wxA : uchar, _) => true
                     | _ => false)
  in
  fun coord file = CoordinatedStream.coordinate eol (Coord.init file)
  fun normalizeWS s : uchar Stream.stream =
      (case front s of
           Nil => eager Nil
         | Cons (0wxD, s') => (case front s' of
                                   Cons (0wxA, _) => s'
                                 | _ => eager (Cons (0wxA, s')))
         | Cons (c, s') => lazy (fn () => Cons (c, normalizeWS s')))
  fun detectEncoding (s : char stream) =
      let val xs = List.map ord (take (s, 4))
          val (codec, ds, enc) =
              (case xs of
                   (* w/ BOM *)
                   [0x00, 0x00, 0xFE, 0xFF] => raise UnsupportedEncoding "UCS-4"
                 | [0xFF, 0xFE, 0x00, 0x00] => raise UnsupportedEncoding "UCS-4"
                 | [0x00, 0x00, 0xFF, 0xFE] => raise UnsupportedEncoding "UCS-4"
                 | [0xFE, 0xFF, 0x00, 0x00] => raise UnsupportedEncoding "UCS-4"
                 | [0xFE, 0xFF, _, _] => (Utf16BE.decodeStr, drop (s, 2), U16)
                 | [0xFF, 0xFE, _, _] => (Utf16LE.decodeStr, drop (s, 2), U16)
                 | [0xEF, 0xBB, 0xBF, _] => (Utf8.decodeStr, drop (s, 3), U8)
                 (* w/o BOM *)
                 | [0x00, 0x00, 0x00, 0x3C] => raise UnsupportedEncoding "UCS-4"
                 | [0x3C, 0x00, 0x00, 0x00] => raise UnsupportedEncoding "UCS-4"
                 | [0x00, 0x3C, 0x00, 0x00] => raise UnsupportedEncoding "UCS-4"
                 | [0x00, 0x00, 0x3C, 0x00] => raise UnsupportedEncoding "UCS-4"
                 | [0x00, 0x3C, 0x00, 0x3F] => (Utf16BE.decodeStr, s, U16)
                 | [0x00, 0x3F, 0x00, 0x3C] => (Utf16LE.decodeStr, s, U16)
                 | [0x3C, 0x3F, 0x78, 0x6D] => (Utf8.decodeStr, s, U8_ISO)
                 | [0x4C, 0x6F, 0xA7, 0x94] => raise UnsupportedEncoding "EBCDIC"
                 | _ => (Utf8.decodeStr, s, U8_NONSPEC))
          val us = codec ds
          val tus = coord "-" us
          val _ = (case simpleParse xmlDecl tus of
                       Sum.INR (XMLDecl (v, SOME edec, sa)) => matchEnc (enc, edec)
                     | _ => ())
      in us
      end
  end

  fun parseString p =
      simpleParse p o coord "-" o normalizeWS o detectEncoding o Stream.fromString

  fun parseStrEnc p enc =
      simpleParse p o coord "-" o normalizeWS o enc o Stream.fromString

  fun parseStringDef p = parseStrEnc p Utf8.decodeStr

end
