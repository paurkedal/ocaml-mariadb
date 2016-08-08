type mode = [`Blocking | `Nonblocking]

module type S = sig
  type error = int * string
  type 'a result = ('a, error) Pervasives.result

  module Res : sig
    type t

    type time = Common.Res.time =
      { year : int
      ; month : int
      ; day : int
      ; hour : int
      ; minute : int
      ; second : int
      }

    type value =
      [ `Int of int
      | `Float of float
      | `String of string
      | `Bytes of bytes
      | `Time of time
      | `Null
      ]

    val fetch : t -> value array option result
    val num_rows : t -> int
  end

  module Stmt : sig
    type state = [`Prepared | `Executed]
    type 's t constraint 's = [< state]

    type param =
      [ `Tiny of int
      | `Short of int
      | `Int of int
      | `Float of float
      | `Double of float
      | `String of string
      | `Blob of bytes
      ]

    val execute : [`Prepared] t -> param array -> Res.t result

    val execute' : [`Prepared] t -> param array
                -> ([`Executed] t * Res.t) result

    val close : [< state] t -> unit result

    (*val reset : [`Executed] t -> [`Prepared] t result*)
  end

  type state = [`Unconnected | `Connected | `Tx]
  type 's t constraint 's = [< state]

  type flag
  type server_option =
    | Multi_statements of bool

  val connect : ?host:string
             -> ?user:string
             -> ?pass:string
             -> ?db:string -> ?port:int -> ?socket:string
             -> ?flags:flag list -> unit
             -> [`Connected] t result

  val close : [< `Connected | `Tx] t -> unit

  val set_character_set : [`Connected] t -> string -> unit result
  val select_db : [`Connected] t -> string -> unit result
  val change_user : [`Connected] t -> string -> string -> string option
                 -> unit result
  val dump_debug_info : [`Connected] t -> unit result
  val set_server_option : [`Connected] t -> server_option
                       -> unit result
  val ping : [`Connected] t -> unit result
  val prepare : [`Connected] t -> string -> [`Prepared] Stmt.t result

  module Tx : sig
    val commit : [`Connected] t -> [`Tx] t result
    val rollback : [`Tx] t -> [`Connected] t result
    val autocommit : [`Connected] t -> bool -> [`Connected] t result
  end
end
