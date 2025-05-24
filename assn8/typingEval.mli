exception TypeError
exception Stuck

val typeOf : Fjava.program -> Fjava.typ
val typeOpt : Fjava.program -> Fjava.typ option
val step : Fjava.program -> Fjava.program
val stepOpt : Fjava.program -> Fjava.program option
val multiStep : Fjava.program -> Fjava.program
val stepStream : Fjava.program -> Fjava.program Stream.t
