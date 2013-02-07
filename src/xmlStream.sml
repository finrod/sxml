structure XmlStream =
struct

  datatype token = StartTag of XmlTypes.name * XmlTypes.attribute list
                 | EndTag of XmlTypes.name
                 | EmptyTag of XmlTypes.name * XmlTypes.attribute list
                 | Reference of XmlTypes.reference
                 | CharData of XmlTypes.ustr
                 | Misc of XmlTypes.misc

  local
      open XmlParser
      open ParserCombinators
      infix  2 wth suchthat return guard when
      infixr 1 || <|> ??
  in

  val token = charData' wth CharData <|> reference wth Reference
            <|> startTag wth StartTag || endTag wth EndTag || emptyElemTag wth EmptyTag
             || (pi || comment) wth Misc || cdsect' wth CharData
  val parseContent = transform (!! token)

  end



end
