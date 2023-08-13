let rec value_to_json (y : Yaml.value) =
  Jsont_none.(
    match y with
    | `A a -> array ~map:value_to_json a
    | `Bool b -> bool b
    | `Float f -> number f
    | `Null -> null
    | `String s -> string s
    | `O l -> object' ~map:value_to_json l)

let rec value_of_json j : Yaml.value =
  Jsont.(
    match j with
    | Array (a, _) ->
        let a = List.rev @@ List.rev_map value_of_json a in
        `A a
    | Bool (b, _) -> `Bool b
    | Number (f, _) -> `Float f
    | Null (_, _) -> `Null
    | String (s, _) -> `String s
    | Object (l, _) ->
        let l = List.rev @@ List.rev_map (fun (name, j) -> (fst name, value_of_json j)) l in
        `O l)

let map_error v = Result.map_error (function s -> `Msg s) v

let of_yaml t yaml =
  let json = Result.map value_to_json yaml in
  Result.bind json (fun json -> Jsont.Json.decode t json |> map_error)

let to_yaml t v = Jsont.Json.encode t v |> Result.map value_of_json |> map_error
let of_string t s = of_yaml t @@ Yaml.of_string s

let to_string ?len ?encoding ?scalar_style ?layout_style t v =
  Result.bind (to_yaml t v) (Yaml.to_string ?len ?encoding ?scalar_style ?layout_style)
