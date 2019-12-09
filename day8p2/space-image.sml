structure Image = struct
  type pixel = int
  type row = pixel list
  type layer = row list
  type encoding = layer list * int * int

  datatype color = BLACK
                 | WHITE
                 | TRANSPARENT
                 | UNKNOWN
  type image = int list list

  fun pixelToColor p = case p of
                            0 => BLACK
                          | 1 => WHITE
                          | 2 => TRANSPARENT
                          | _ => UNKNOWN
  fun colorToPixel c = case c of
                            BLACK => 0
                          | WHITE => 1
                          (* shouldn't have these *)
                          | TRANSPARENT => 2
                          | UNKNOWN => 999

  fun fromString rows columns str : encoding =
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
      (readLayers rows columns pixels, rows, columns)
    end

  fun get (x,y) =
    let
      fun getter n =
        let fun getter' n f = if n = 1 then hd o f
                              else getter' (n-1) (tl o f)
        in getter' n (fn id => id)
        end
    in
      getter x o getter y
    end

  fun decode (e,rows,cols) : image =
    let
      val rows' = List.tabulate (rows, fn i => i+1)
      val cols' = List.tabulate (cols, fn i => i+1)
      val colors = map (fn lay => map (fn row => map pixelToColor row) lay) e
      val color = foldl (fn (c, cur) => if cur = TRANSPARENT then c else cur) TRANSPARENT
      val colorStacks = map (fn row =>
                        map (fn col =>
                        map (get (col,row))
                        colors)
                        cols')
                        rows'
      val colorsFinal = map (fn row =>
                        map (fn col =>
                        color (get (col,row) colorStacks))
                        cols')
                        rows'
    in
      map (fn row =>
      map (fn col =>
      colorToPixel (get (col,row) colorsFinal))
      cols')
      rows'
    end

  fun pixelToString p = case p of
                             0 => " "
                           | 1 => "*"
                           | _ => "?"

  fun toString image =
    String.concat
    (map (fn row =>
      let val row' = map pixelToString row
      in String.concat (row' @ ["\n"])
      end) image)
end

structure Solution = struct
  val readString = TextIO.inputAll o TextIO.openIn
  fun solve s =
    let
      val encoding = Image.fromString 6 25 (readString s)
      val image = Image.decode encoding
    in
      print (Image.toString image)
    end
end
