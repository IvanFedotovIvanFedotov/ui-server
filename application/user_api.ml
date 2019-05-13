type t = User.passwd

let set_password (users:t) _user body _env _state =
  let open User in
  let open Lwt.Infix in
  match pass_change_of_yojson body with
  | Error e -> Lwt.return (`Error e)
  | Ok pass ->
     Lwt.catch (* TODO remove catch *)
       (fun () ->
         get_pass users pass.user >>= fun passwd ->
         if passwd.pass = pass.old_pass
         then
           set_pass users { user = pass.user; password = pass.new_pass } >>= fun () ->
           Lwt.return `Unit
         else
           Lwt.return `Wrong_password)
       (fun _ -> Lwt.return (`Error "internal password db error, please report"))

let logout _user _body _env _state =
  Lwt.return `Need_auth
