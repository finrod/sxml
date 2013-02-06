structure XmlTypes =
struct

  type uchar = Word32.word
  type ustr = uchar vector
  type name = ustr

  datatype reference = RefChar of uchar | RefEnt of ustr
  datatype misc = Comment of ustr | PI of name * ustr
  type attrVal = (name, reference) Sum.sum list
  datatype attribute = Attr of name * attrVal
  datatype 'a element = Element of name * attribute list * 'a content list
       and 'a content = CStr of bool * ustr * 'a | CElem of 'a element * 'a
                      | CRef of reference * 'a | CMisc of misc * 'a

  datatype xmlDecl = XMLDecl of string * string option * bool option

  structure DTD =
  struct

    datatype externalID = SYSTEM of name | PUBLIC of name * name
    datatype ev = EVRef of reference | EVVal of name
    datatype entVal = EntVal of ev list
    datatype geDef = DefEntVal of entVal | DefExtId of externalID * name option
    datatype peDef = PEDEntVal of entVal | PEDExtId of externalID
    datatype entityDecl = GEDecl of geDef | PEDecl of peDef

    datatype publicID = PublicID of name
    datatype notation = NotExt of externalID | NotPub of publicID

    datatype arity  = ONE | OPT | MANY | MANY1
    datatype cpn = Choice of cp list | Seq of cp list | TagName of name
    withtype cp  = cpn * arity
    datatype mixed = PCDATA | PCDATAplus of name list
    datatype contentSpec = EMPTY | ANY | Mixed of mixed | Children of cp

    datatype tokenType = ID | IDREF | IDREFS | ENTITIES | ENTITY | NMTOKENS | NMTOKEN
    datatype enumType  = NotationType of name list | Enumeration of ustr list
    datatype attrType  = StringTy | EnumTy of enumType | TokTy of tokenType

    datatype defaultDecl = REQUIRED | IMPLIED | DefaultsTo of bool * attrVal
    datatype attrDef = AttrDef of name * attrType * defaultDecl

    datatype markupDecl = ElemDecl of name * contentSpec
                        | AttlistDecl of name * attrDef list
                        | NotationDecl of name * notation
                        | EntityDecl of name * entityDecl
                        | MarkupMisc of misc
    datatype intSubsetElem = Markup of markupDecl | PERef of name

    datatype ignoreCts = Ignore of ustr * (ignoreCts * ustr) list
    datatype extSubsetElem = ExtMarkup of markupDecl | ExtCondSect of condSect
                           | ExtPERef of name
         and condSect = IncludeSect of extSubsetElem list | IgnoreSect of ignoreCts list

    datatype textDecl = TextDecl of string option * string
    datatype extSubset = ExtSubset of textDecl option * extSubsetElem list

    datatype docTypeDecl = DTD of name * externalID option * intSubsetElem list

  end

  datatype prolog = Prolog of xmlDecl option * misc list * (DTD.docTypeDecl * misc list) option
  datatype 'a document = Document of prolog * 'a element * misc list

end
