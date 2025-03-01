open! Core

let command =
  Async.Command.async ~summary:"TODO"
    (Command.Param.return (fun () -> Ocoup.run_game ()))

let () = Command_unix.run command
