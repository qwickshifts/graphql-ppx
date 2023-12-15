module Json = struct
  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list ]

  exception Type_error of string * t

  let typeof = function
    | `Assoc _ -> "object"
    | `Bool _ -> "bool"
    | `Float _ -> "float"
    | `Int _ -> "int"
    | `List _ -> "array"
    | `Null -> "null"
    | `String _ -> "string"

  let typerr msg js = raise (Type_error (msg ^ typeof js, js))
  let assoc name obj = try List.assoc name obj with Not_found -> `Null

  let member name = function
    | `Assoc obj -> assoc name obj
    | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

  let to_string = function
    | `String s -> s
    | js -> typerr "Expected string, got " js

  let to_float = function
    | `Float f -> f
    | js -> typerr "Expected float, got " js

  let to_int = function `Int i -> i | js -> typerr "Expected int, got " js
  let to_bool = function `Bool b -> b | js -> typerr "Expected bool, got " js
end

let assign_typename : Js.Json.t -> string -> Js.Json.t =
  [%raw {| (obj, typename) => { obj.__typename = typename; return obj } |}]

[%%private
let clone =
  (fun a ->
     Obj.magic (Js.Obj.assign (Obj.magic (Js.Obj.empty ())) (Obj.magic a))
    : 'a Js.Dict.t -> 'a Js.Dict.t)]

let rec deepMerge (json1 : Js.Json.t) (json2 : Js.Json.t) =
  match
    ( ( Obj.magic json1 = Js.null,
        Js.Array2.isArray json1,
        Js.typeof json1 = "object" ),
      ( Obj.magic json2 = Js.null,
        Js.Array2.isArray json2,
        Js.typeof json2 = "object" ) )
  with
  | (_, true, _), (_, true, _) ->
    (Obj.magic
       (Js.Array2.mapi (Obj.magic json1) (fun el1 idx ->
            let el2 = Js.Array2.unsafe_get (Obj.magic json2) idx in
            match Js.typeof el2 = "object" with
            | true -> deepMerge el1 el2
            | false -> el2))
      : Js.Json.t)
  | (false, false, true), (false, false, true) ->
    let obj1 = clone (Obj.magic json1) in
    let obj2 = Obj.magic json2 in
    Js.Array2.forEach (Js.Dict.keys obj2) (fun key ->
        let existingVal = (Js.Dict.unsafeGet obj1 key : Js.Json.t) in
        let newVal = (Js.Dict.unsafeGet obj2 key : Js.Json.t) in
        Js.Dict.set obj1 key
          (match Js.typeof existingVal <> "object" with
          | true -> newVal
          | false -> Obj.magic (deepMerge existingVal newVal)));
    Obj.magic obj1
  | (_, _, _), (_, _, _) -> json2
