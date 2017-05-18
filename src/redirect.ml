let redirect_auth dbs headers good bad =
  Authorize.auth_needed dbs headers
  |> function
    | Some id -> good id
    | None    -> bad ()

let home_redirect ?headers =
  match headers with
  | Some headers -> (fun () -> Cohttp_lwt_unix.Server.respond_redirect ~headers ~uri:(Uri.with_path Uri.empty "/") ())
  | None         -> (fun () -> Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/") ())
                  
let login_redirect = Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.with_path Uri.empty "/login")

let not_found = Cohttp_lwt_unix.Server.respond_not_found
