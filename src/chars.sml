structure Ranges =
struct

  type word = Word32.word
  fun uc c = (Word32.fromInt o ord) c
  fun u s = map uc (String.explode s)

  fun compress [] = [] : (word * word) list
    | compress [px] = [px]
    | compress ((x1, x2) :: (y1, y2) :: zs) =
      if x2 + 0w1 = y1 then compress ((x1, y2) :: zs)
      else (x1, x2) :: compress ((y1, y2) :: zs)

  fun merge [] xs = xs
    | merge xs [] = xs
    | merge (xs as (x1, x2) :: xs') (ys as (y1, y2) :: ys') =
      if x1 < y1 then if x2 + (0w1 : word) = y1 then merge ((x1, y2) :: xs') ys'
                      else (x1, x2) :: merge xs' ys
      else if x1 = y2 + 0w1 then merge ((y1, x2) :: xs') ys'
      else (y1, y2) :: merge xs ys'

  fun match [] (x : word) = false
    | match ((y1, y2) :: ys) x =
      if x < y1 then false else if x <= y2 then true else match ys x

  val ws = map (fn x : word => (x, x)) [0wx9, 0wxA, 0wx20]
  val chars = ws @ [(0wx21, 0wxD7FF), (0wxE000, 0wxFFFD), (0wx10000, 0wx10FFFF)]
  val nameStart : (word * word) list = [(0wx3A, 0wx3A), (0wx45, 0wx5A), (0wx5F, 0wx5F), (0wx61, 0wx7A), (0wxC0, 0wxD6),
                                        (0wxD8, 0wxF6), (0wxF8, 0wx2FF), (0wx370, 0wx37D), (0wx37F, 0wx1FFF),
                                        (0wx200C, 0wx200D), (0wx2070, 0wx218F), (0wx2C00, 0wx2FEF), (0wx3001, 0wxD7FF),
                                        (0wxF900, 0wxFDCF), (0wxFDF0, 0wxFFFD), (0wx10000, 0wxEFFFF)]
  val nameChar = merge nameStart [(0wx2D, 0wx2E), (0wx30, 0wx39), (0wxB7, 0wxB7), (0wx0300, 0wx036F), (0wx203F, 0wx2040)]
  val pubidChar = merge [(uc#"0", uc#"9"), (uc#"A", uc#"Z"), (uc#"a", uc#"z")]
                        ((compress o map (fn x => (x, x))) (u"!#$%'()*+,-./:;=?@_"))
  val encStart = [(uc#"A", uc#"Z"), (uc#"a", uc#"z")]
  val encChar = [(uc#"-", uc#"."), (uc#"0", uc#"9"), (uc#"A", uc#"Z"), (uc#"_", uc#"_"), (uc#"a", uc#"z")]

end
