open Pc_control_types
open Netlib.Uri

module Api_http = Api_js.Http.Make(Application_types.Body)

module Event = struct
  let get_config sock =
    let of_yojson = Network_config.of_yojson in
    Api_js.Websocket.JSON.subscribe
      ~path:Path.Format.("network/config" @/ empty)
      ~query:Query.empty
      of_yojson sock
end

let get_config () =
  Api_http.perform
    ~meth:`GET
    ~path:Path.Format.("api/network/config" @/ empty)
    ~query:Query.empty
    (fun _env -> function
       | Error _ as e -> Lwt.return e
       | Ok x ->
         match Network_config.of_yojson x with
         | Error e -> Lwt.return_error (`Conv_error e)
         | Ok _ as x -> Lwt.return x)

let set_config conf =
  Api_http.perform_unit
    ~meth:`POST
    ~body:(Network_config.to_yojson conf)
    ~path:Path.Format.("api/network/config" @/ empty)
    ~query:Query.empty
    (fun _env res -> Lwt.return res)
