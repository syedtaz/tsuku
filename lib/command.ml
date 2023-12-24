module Command = struct
  open Core

  let a =
    let open Command.Param in
    anon ("filename" %: string)
  ;;

  let command =
    Command.basic
      ~summary:"Create the default location for Koka packages"
      ~readme:(fun () -> "Creates a path under ~/.tsuku")
      (Command.Param.map a ~f:(fun filename () -> Format.print_string filename))
  ;;
end

include Command
