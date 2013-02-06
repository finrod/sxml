structure Utf8 : UTF_CODEC =
struct

    type uchar = Word32.word
    type ustring = uchar vector

    exception Malformed

    val && = Word32.andb
    val || = Word32.orb
    val >> = Word32.>>
    val << = Word32.<<
    infix 5 &&
    infix 1 ||
    infix 7 >>
    infix 7 <<
    val & = Word8.andb
    infix 5 &

    val wc = chr o Word32.toInt
    val bw = Word32.fromInt o Word8.toInt

    (* Encode a follow-up byte from the lowest 6 bits of u *)
    fun l6p u = wc (u && 0wx3f || 0wx80)

    fun encodeChar u =
        if u <= 0wx7f then [wc u]
        else if u <= 0wx7ff then [wc (u >> 0w6 || 0wxc0 ), l6p u]
        else if u <= 0wxffff then [wc (u >> 0w12 || 0wxe0), l6p (u >> 0w6), l6p u]
        else if u <= 0wx1fffff then [wc (u >> 0w18 || 0wxf0),
                                     l6p (u >> 0w12), l6p (u >> 0w6), l6p u]
        else if u <= 0wx3ffffff then [wc (u >> 0w24 || 0wxf8), l6p (u >> 0w18),
                                      l6p (u >> 0w12), l6p (u >> 0w6), l6p u]
        else if u <= 0wx7fffffff then [wc (u >> 0w30 || 0wxfc), l6p (u >> 0w24),
                                       l6p (u >> 0w18), l6p (u >> 0w12), l6p (u >> 0w6), l6p u]
        else raise Overflow

    fun encode us = Vector.foldr (fn (u, s) => String.implode (encodeChar u) ^ s) "" us


    (* Get the meaningful part of a follow-up byte *)
    fun rnrm k =
        let val b = Word8Vector.sub k
        in if b = b & 0wxbf then bw (b & 0wx3f) else raise Malformed
        end

    fun decodeChar cs i =
        let val tb = Word8Vector.sub (cs, i)
        in if tb = tb & 0wx7F then (bw tb, i+1)
           else if tb = tb & 0wxDF then (bw (tb & 0wx1f) << 0w6 || rnrm (cs, i+1), i+2)
           else if tb = tb & 0wxEF then
               (bw (tb & 0wx0f) << 0w12 || rnrm (cs, i+1) << 0w6 || rnrm (cs, i+2), i+3)
           else if tb = tb & 0wxF7 then
               (bw (tb & 0wx07) << 0w18 || rnrm (cs, i+1) << 0w12 || rnrm (cs, i+2) << 0w6
                   || rnrm (cs, i+3), i+4)
           else if tb = tb & 0wxFB then
               (bw (tb & 0wx03) << 0w24 || rnrm (cs, i+1) << 0w18 || rnrm (cs, i+2) << 0w12
                   || rnrm (cs, i+3) << 0w6 || rnrm (cs, i+4), i+5)
           else if tb = tb & 0wxFD then
               (bw (tb & 0wx01) << 0w30 || rnrm (cs, i+1) << 0w24 || rnrm (cs, i+2) << 0w18
                   || rnrm (cs, i+3) << 0w12 || rnrm (cs, i+4) << 0w6 || rnrm (cs, i+5), i+6)
           else raise Malformed
        end

    fun decode s =
        let val cs = Byte.stringToBytes s
            fun aux i acc = if i = Word8Vector.length cs then Vector.fromList (rev acc)
                            else let val (u, j) = decodeChar cs i in aux j (u :: acc) end
        in aux 0 []
        end

    local open Stream
          fun maybe m n s = case m of NONE => n | SOME v => s v
          val cw = Word32.fromInt o ord
    in
    fun encFU (u, n, s) =
        if n <= 0 then s else encFU (u >> 0w6, n-1, eager (Cons (l6p u, s)))
    fun encOne (u, s) =
        if u <= 0wx7f then Cons (wc u, s)
        else if u <= 0wx7ff then Cons (wc (u >> 0w6 || 0wxc0), encFU (u, 1, s))
        else if u <= 0wxffff then Cons (wc (u >> 0w12 || 0wxe0), encFU (u, 2, s))
        else if u <= 0wx1fffff then Cons (wc (u >> 0w18 || 0wxf0), encFU (u, 3, s))
        else if u <= 0wx3ffffff then Cons (wc (u >> 0w24 || 0wxf8), encFU (u, 4, s))
        else if u <= 0wx7fffffff then Cons (wc (u >> 0w30 || 0wxfc), encFU (u, 5, s))
        else raise Overflow
    fun encodeStr us = lazy (fn () => case front us of Nil => Nil | Cons (u, us) => encOne (u, encodeStr us))
    fun decFU (n, s, t) =
        if n <= 0 then (t, s)
        else case front s of
                 Nil => raise Malformed
               | Cons (c, cs) => let val b = cw c
                                 in if b = b && 0wxbf then decFU (n-1, cs, t << 0w6 || b && 0wx3f)
                                    else raise Malformed
                                 end
    fun decOne Nil = Nil
      | decOne (Cons (c, cs)) =
        let val tb = cw c
	    val (u, cs) = if tb = tb && 0wx7f then (tb, cs)
			  else if tb = tb && 0wxdf then (decFU (1, cs, tb && 0wx1f))
			  else if tb = tb && 0wxef then (decFU (2, cs, tb && 0wx0f))
			  else if tb = tb && 0wxf7 then (decFU (3, cs, tb && 0wx07))
			  else if tb = tb && 0wxfb then (decFU (4, cs, tb && 0wx03))
			  else if tb = tb && 0wxfd then (decFU (5, cs, tb && 0wx01))
			  else raise Malformed
	in Cons (u, decodeStr cs)
        end
    and decodeStr cs = lazy (fn () => decOne (front cs))
    end

    val pchk = StringCvt.padLeft #"0" 8 o Word32.fmt StringCvt.BIN
    val rchk = StringCvt.scanString (Word32.scan StringCvt.BIN)

end

signature U16_BYTE_ORDER =
sig

    val dec16 : char * char -> Word32.word
    val enc16 : Word32.word -> char * char

end

(* This encodes & decodes Utf16 in big endian *)
functor Utf16 (BO : U16_BYTE_ORDER) : UTF_CODEC =
struct

    type uchar = Word32.word
    type ustring = uchar vector

    exception Malformed

    val && = Word32.andb
    val || = Word32.orb
    val >> = Word32.>>
    val << = Word32.<<
    infix 5 &&
    infix 1 ||
    infix 7 >>
    infix 7 <<

    open Stream

    fun enc16 cons w s =
        let val (fst, snd) = BO.enc16 w
        in cons (fst, cons (snd, s))
        end
    val enc16s = enc16 (eager o Cons)
    val enc16l = enc16 op::

    fun dec16s s =
        (case front s of
             Nil => NONE
           | Cons (fst, s') =>
             (case front s' of
                  Nil => raise Malformed
                | Cons (snd, s'') => SOME (BO.dec16 (fst, snd), s'')))
    fun dec16l (fst :: snd :: xs) = SOME (BO.dec16 (fst, snd), xs)
      | dec16l _ = NONE

    fun encodeOne enc (u, s) =
	if u <= 0wx10000 then
	    if u <= 0wxdfff andalso u >= 0wxd800 then raise Malformed
	    else enc u s
	else if u <= 0wx10ffff
        then enc (0wxd800 || u >> 0w10) (enc (0wxdc00 || u && 0wx3ff) s)
	else raise Malformed
    fun encodeStr us = lazy (fn () => case front us of Nil => Nil | Cons (u, us) => front (encodeOne enc16s (u, encodeStr us)))
    fun encode s = (String.implode o rev o Vector.foldl (encodeOne enc16l) []) s


    fun decodeOne dec s =
        (case dec s of
             NONE => NONE
           | SOME (x, xs) =>
             if x < 0wxd800 orelse x > 0wxdfff then SOME (x, xs)
	     else if x < 0wxdc00
	     then case dec xs of
                      NONE => raise Malformed
                    | SOME (y, ys) =>
                      if y >= 0wxdc00 andalso y <= 0wxdfff
		      then SOME ((x && 0wx3ff) << 0w10 || (y && 0wx3ff), ys)
		      else raise Malformed
	     else raise Malformed)
    fun decodeStr cs =
        lazy (fn () => case decodeOne dec16s cs of
                           NONE => Nil
                         | SOME (x, cs') => Cons (x, decodeStr cs'))
    fun decode s =
        let fun aux xs acc = (case decodeOne dec16l xs of
                                  NONE => Vector.fromList (rev acc)
                                | SOME (x, xs) => aux xs (x :: acc))
        in aux (String.explode s) []
        end

end

structure BigEndian : U16_BYTE_ORDER =
struct
    val && = Word32.andb
    val || = Word32.orb
    val >> = Word32.>>
    val << = Word32.<<
    infix 5 &&
    infix 1 ||
    infix 7 >>
    infix 7 <<

    val toChr   = chr o Word32.toInt
    val fromChr = Word32.fromInt o ord

    fun dec16 (f, s) = fromChr f << 0w8 || fromChr s 
    fun enc16 w = (toChr (w >> 0w8), toChr (w && 0wxFF))
end

structure LittleEndian : U16_BYTE_ORDER =
struct
    val && = Word32.andb
    val || = Word32.orb
    val >> = Word32.>>
    val << = Word32.<<
    infix 5 &&
    infix 1 ||
    infix 7 >>
    infix 7 <<

    val toChr   = chr o Word32.toInt
    val fromChr = Word32.fromInt o ord

    fun dec16 (s, f) = fromChr f << 0w8 || fromChr s 
    fun enc16 w = (toChr (w && 0wxFF), toChr (w >> 0w8))
end

structure Utf16BE = Utf16 (BigEndian)
structure Utf16LE = Utf16 (LittleEndian)

fun toString cs =
    let fun aux (cs, acc) =
	    case Stream.front cs of
		Stream.Nil => acc
	      | Stream.Cons (c, cs) => aux (cs, c :: acc)
    in String.implode (rev (aux (cs, [])))
    end

val test = toString o Utf8.encodeStr o Utf8.decodeStr o Stream.fromString

