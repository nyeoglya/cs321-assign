exception TypeError
type context
val createEmptyContext : unit -> context
val typing : context -> Tml.exp -> Tml.tp
val typeOf : Tml.exp -> Tml.tp
val typeOpt : Tml.exp -> Tml.tp option
