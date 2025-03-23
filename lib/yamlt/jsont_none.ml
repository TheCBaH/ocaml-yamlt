open Jsont

let node a = (a, Meta.none)
let array ~map a = Array (node @@ List.rev @@ List.rev_map map a)
let bool b = Bool (node b)
let number b = Number (node b)
let null = Null (node ())
let string s = String (node s)

let object' ~map o =
  let o =
    List.rev
    @@ List.rev_map
         (fun (name, v) ->
           let name = node name in
           let m = (name, map v) in
           m)
         o
  in
  Object (node o)
