(* A single-use option represents a command-line option that can be
   set at most once -- as opposed to options that it makes sense to
   call several times. For example, the option '-targetpath <path>'
   sets the single, global target path, we want to prevent the user
   from passing it several times, as they may not expect that some of
   the uses will be ignored.
*)


type 'a t
(** ['a t] represents a single-use option that records a setting of
    type ['a].  For example, the [-targetpath] option mentioned
    earlier would be a [string t], it stores a string
    (a filesystem path). By "single-use", we mean that setting an option
    several times, with a different value, fails with an error. *)

val make : unit -> 'a t
(** [make] creates a new single-use option, that is initially unset. *)

val set : 'a t -> string -> 'a -> unit
(** [set option cmd v] records that the command [cmd] sets the value
    of [option] to [v]. An error is raised if the option was already
    set to a different value.

    A "command" is any text that the user will identify as part of the
    input they gave. For example it could be ["-targetpath foo"] if they
    entered this in the command-line, or [TARGETPATH=foo] if we record it
    from an environment variable. It is only used in the error message
    shown to the user.

    For example, if you call [set targetpath "-targetpath foo" "foo"]
    and then [set targetpath "-targetpath bar" "bar"], then the second
    [set] will faill with the error:
      Command-line options: options '-targetpath foo'
      and '-targetpath bar' are incompatible.
*)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f option] will call [f v] if [option] is set to [v],
    and do nothing if [option] is unset. *)

