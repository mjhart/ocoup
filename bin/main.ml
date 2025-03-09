open! Core
open! Async

let command =
  Command.async ~summary:"TODO"
    (Command.Param.return (fun () -> Ocoup.run_game ()))

let make_http_request_command =
  Command.async ~summary:"TODO" (Command.Param.return (fun () -> return ()))

let run_server_command =
  Command.async ~summary:"TODO"
    (Command.Param.return (fun () -> Ocoup.run_server ()))

let group_command =
  Command.group ~summary:"TODO"
    [
      ("run", command);
      ("http", make_http_request_command);
      ("server", run_server_command);
    ]

let () = Command_unix.run group_command
