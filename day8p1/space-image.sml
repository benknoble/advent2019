structure Image = struct
  type pixel = int
  type row = pixel list
  type layer = row list
  type image = layer list
  fun fromString rows columns str : image =
    let
      val tokens = String.explode str
      val pixels = List.mapPartial Int.fromString (map Char.toString tokens)
      fun readRow cols pix = (List.take (pix, cols), List.drop (pix, cols))
      fun readLayer rows cols pix =
        let
          fun readLayer' rows cols pix acc =
            if rows = 0 then (rev acc,pix)
            else
              let val (row, rest) = readRow cols pix
              in readLayer' (rows-1) cols rest (row::acc)
              end
        in
          readLayer' rows cols pix []
        end
      fun readLayers rows cols pix =
        let
          fun readLayers' rows cols pix acc =
            if null pix then rev acc
            else
              let val (layer, rest) = readLayer rows cols pix
              in readLayers' rows cols rest (layer::acc)
              end
        in
          readLayers' rows cols pix []
        end
    in
      readLayers rows columns pixels
    end

  fun count (p : pixel) lay =
    foldl (fn (r, s) =>
    s + foldl (fn (p', ac) => if p = p' then ac+1 else ac) 0 r)
    0 lay
end

structure Solution = struct
  val readString = TextIO.inputAll o TextIO.openIn
  fun solve s =
    let
      val image = Image.fromString 6 25 (readString s)
      val counts = ListPair.zip (map (Image.count 0) image, image)
      val (_,smallest) =
        foldl (fn ((c,l), (m,l')) => if c < m then (c,l) else (m,l'))
        (6*25 + 1, []) counts (* at most all the pixels are 0 *)
    in
      Image.count 1 smallest * Image.count 2 smallest
    end
end
