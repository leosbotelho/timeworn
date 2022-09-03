const runP = lang => parserId => s =>
  lang[parserId].tryParse(s)

export default runP