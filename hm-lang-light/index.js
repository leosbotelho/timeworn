import P       from "parsimmon"
import tokens  from "./tokens"
import parsers from "./parsers"

const HMLang = P.createLanguage({
  /* Simple token parsers */
  ...tokens,

  /* Parsers */
  ...parsers
})

export default HMLang