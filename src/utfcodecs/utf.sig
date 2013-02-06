signature UTF_CODEC =
sig

    type uchar = Word32.word
    type ustring = uchar vector

    exception Malformed

    val encode : ustring -> string
    val decode : string -> ustring

    val encodeStr : uchar Stream.stream -> char Stream.stream
    val decodeStr : char Stream.stream -> uchar Stream.stream

end
