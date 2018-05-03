open Api.Interaction   
open Lwt.Infix

let set_password (users : User.entries) body () =
  let open User in
  yojson_of_body body >>= fun js ->
  match pass_change_of_yojson js with
  | Error e -> respond_error e ()
  | Ok pass -> (try if (get_pass users pass.user).pass = pass.old_pass
                    then (set_pass users { user = pass.user; password = pass.new_pass };
                          respond_ok ())
                    else respond_error "bad pass" ()
                with _ -> respond_error "pass db err" ())
  
let user_handle users id meth args _ headers body = (*headers body =*)
  let open Api.Redirect in
  let open User in
  let not_root = not @@ User.eq id `Root in
  match meth, args with
  | `POST,   ["password"] -> redirect_if not_root @@ set_password users body
  | `GET,    ["logout"]   -> respond_need_auth ~headers:headers ~auth:(`Basic "User Visible Realm") ()
  | _ -> not_found ()

module Api_handler = Api.Handler.Make(Common.User)
  
let handlers users =
  [ (module struct
       let domain = "user"
       let handle = user_handle users
     end : Api_handler.HANDLER); ]