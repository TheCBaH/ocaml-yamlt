module M = struct
  type t = { foo : string; bar : string }

  let make foo bar = { foo; bar }
  let foo t = t.foo
  let bar t = t.bar

  let jsont =
    Jsont.Object.map make
    |> Jsont.Object.mem "foo" Jsont.string ~enc:foo
    |> Jsont.Object.mem "bar" Jsont.string ~enc:bar
    |> Jsont.Object.finish

  let pp fmt t = Format.fprintf fmt "@[{foo:%s,@ bar:%s}@]" t.foo t.bar
end

module U = struct
  type t = { unknown : Jsont.json }

  let make unknown = { unknown }
  let unknown t = t.unknown
  let jsont = Jsont.Object.map make |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:unknown |> Jsont.Object.finish
end

let print fmt = Format.printf "%a%!" fmt

let%expect_test "pp" =
  print M.pp @@ M.make "bar" "foo";
  [%expect "{foo:bar, bar:foo}"]

let%expect_test "jsont_decode" =
  let m = Result.get_ok @@ Jsont_bytesrw.decode_string M.jsont {|{"foo": "bar", "bar": "foo"} |} in
  print M.pp m;
  [%expect "{foo:bar, bar:foo}"]

let%expect_test "jsont_none_decode" =
  let j = Jsont_none.(object' ~map:Fun.id [ ("foo", string "bar"); ("bar", string "foo") ]) in
  let m = Result.get_ok @@ Jsont.Json.decode M.jsont j in
  print M.pp m;
  [%expect "{foo:bar, bar:foo}"]

let%expect_test "jsont_encode" =
  let s = Jsont_bytesrw.encode_string M.jsont @@ M.make "bar" "foo" in
  print (Format.pp_print_result ~ok:Format.pp_print_string ~error:Format.pp_print_string) s;
  [%expect {| {"foo":"bar","bar":"foo"} |}]

let%expect_test "jsont_passthrough" =
  let u = Result.get_ok @@ Jsont_bytesrw.decode_string U.jsont {|{"foo-x": "bar", "bar": "foo-x"} |} in
  let s = Jsont_bytesrw.encode_string U.jsont u in
  print (Format.pp_print_result ~ok:Format.pp_print_string ~error:Format.pp_print_string) s;
  [%expect {| {"foo-x":"bar","bar":"foo-x"} |}]
