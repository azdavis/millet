signature REACTIVE = sig
  type machine
  type instruction
  type signal = Atom.atom
  type config
  type in_signal
  type out_signal
  val machine : { inputs : signal list, outputs : signal list, body : instruction } -> machine
  val run : machine -> bool
  val reset : machine -> unit
  val inputsOf : machine -> in_signal list
  val outputsOf : machine -> out_signal list
  val inputSignal : in_signal -> signal
  val outputSignal : out_signal -> signal
  val setInSignal : (in_signal * bool) -> unit
  val getInSignal : in_signal -> bool
  val getOutSignal : out_signal -> bool
  val || : (instruction * instruction) -> instruction
  val & : (instruction * instruction) -> instruction
  val nothing : instruction
  val stop : instruction
  val suspend : instruction
  val action : (machine -> unit) -> instruction
  val exec : (machine -> {stop : unit -> unit, done : unit -> bool}) -> instruction
  val ifThenElse : ((machine -> bool) * instruction * instruction) -> instruction
  val repeat : (int * instruction) -> instruction
  val loop : instruction -> instruction
  val close : instruction -> instruction
  val signal : (signal * instruction) -> instruction
  val rebind : (signal * signal * instruction) -> instruction
  val when : (config * instruction * instruction) -> instruction
  val trap : (config * instruction) -> instruction
  val trapWith : (config * instruction * instruction) -> instruction
  val emit : signal -> instruction
  val await : config -> instruction
  val posConfig : signal -> config
  val negConfig : signal -> config
  val orConfig : (config * config) -> config
  val andConfig : (config * config) -> config
end

structure Reactive : REACTIVE = struct end
