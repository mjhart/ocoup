open! Core

let command =
  Async.Command.async ~summary:"TODO"
    (Command.Param.return (fun () -> Ocoup.run_game ()))

let make_http_request_command =
  Async.Command.async ~summary:"TODO"
    (Command.Param.return (fun () -> Ocoup.make_http_request ()))

let group_command =
  Async.Command.group ~summary:"TODO"
    [ ("run", command); ("http", make_http_request_command) ]

let () = Command_unix.run group_command
