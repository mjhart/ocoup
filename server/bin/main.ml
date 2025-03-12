open! Core
open! Async

let command =
  Command.async ~summary:"Run ocoup on the command line"
    (Command.Param.return (fun () -> Ocoup.run_game ()))

let run_server_command =
  Command.async ~summary:"Run the ocoup server"
    (let%map_open.Command port =
       flag "port"
         (optional_with_default 8080 int)
         ~doc:"INT where to run the server"
     in
     fun () -> Ocoup.run_server ~port)

let group_command =
  Command.group
    ~summary:"Ocoup - an ocaml implementation of the board game coup"
    [ ("run", command); ("server", run_server_command) ]

let () = Command_unix.run group_command
