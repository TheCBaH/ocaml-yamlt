let of_file t s = Yamlt.of_yaml t @@ Yaml_unix.of_file s
let to_file p t v = Result.bind (Yamlt.to_yaml t v) (Yaml_unix.to_file p)
