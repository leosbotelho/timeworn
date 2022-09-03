function themeUpdate(desc)
  Default        = { Colour="#000000" }
  Canvas         = { Colour="#d9d9d9" }
  Number         = { Colour="#000000" }
  Escape         = { Colour="#000000" }
  String         = { Colour="#8a8a8a", Italic=true }
  PreProcessor   = { Colour="#000000" }
  StringPreProc  = { Colour="#8a8a8a", Italic=true }
  BlockComment   = { Colour="#808080", Italic=true }
  LineComment    = BlockComment
  LineNum        = { Colour="#808080" }
  Operator       = { Colour="#000000" }
  Interpolation  = { Colour="#000000" }

  Keywords = {
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
    { Colour = "#000000" },
  }
end

Plugins = {
  { Type = "theme", Chunk = themeUpdate }
}
