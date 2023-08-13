let print fmt = Format.printf "%a%!" fmt

let%expect_test "yamlt_decode" =
  let m = Result.get_ok @@ Yamlt.of_string Jsont_test.M.jsont "{foo: bar, bar: foo}" in
  print Jsont_test.M.pp m;
  [%expect "{foo:bar, bar:foo}"]

let%expect_test "yamlt_encode" =
  let s = Result.get_ok @@ Yamlt.to_string ~layout_style:`Flow Jsont_test.M.jsont @@ Jsont_test.M.make "bar" "foo" in
  print Format.pp_print_string s;
  [%expect "{foo: bar, bar: foo}"]

let%expect_test "yamlt_passthrough" =
  let u = Result.get_ok @@ Yamlt.of_string Jsont_test.U.jsont "{foo-x: bar, bar: foo-x}" in
  let s = Result.get_ok @@ Yamlt.to_string ~layout_style:`Flow Jsont_test.U.jsont u in
  print Format.pp_print_string s;
  [%expect "{foo-x: bar, bar: foo-x}"]

let yaml =
  {|
# Please see the documentation for more information:
# https://docs.github.com/github/administering-a-repository/configuration-options-for-dependency-updates
version: 2
updates:
 - package-ecosystem: "devcontainers"
   directory: "/"
   schedule:
     interval: weekly
 - package-ecosystem: "github-actions"
   directory: "/"
   schedule:
     interval: weekly
|}

let%expect_test "yaml_file" =
  let tmp = "tmp.yaml" in
  let fout = Stdlib.open_out tmp in
  output_string fout yaml;
  close_out fout;
  let u = Result.get_ok @@ Yamlt_unix.of_file Jsont_test.U.jsont Fpath.(v tmp) in
  let s = Result.get_ok @@ Yamlt.to_string Jsont_test.U.jsont u in
  print Format.pp_print_string s;
  [%expect
    {|
    version: 2
    updates:
    - package-ecosystem: devcontainers
      directory: /
      schedule:
        interval: weekly
    - package-ecosystem: github-actions
      directory: /
      schedule:
        interval: weekly
    |}]
