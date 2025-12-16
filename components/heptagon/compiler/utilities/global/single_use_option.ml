type 'a state =
  | Unset
  | Set of { cmd : string; value : 'a }
type 'a t = 'a state ref

let make () : 'a t = ref Unset
      
let set option cmd v =
  match !option with
    | Unset -> option := Set { cmd; value = v }
    | Set { cmd = old_cmd; value = v' } ->
       if v = v' then ()
       else
         Compiler_utils.error
           "Command-line options: options '%s' and '%s' are incompatible."
           old_cmd cmd

let iter f option =
  match !option with
    | Unset -> ()
    | Set { cmd = _; value } -> f value
