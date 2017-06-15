class type handle =
  object
    method getId  : Js.number Js.t Js.prop
  end

type parameters =
  { plugin    : Js.js_string Js.opt
  ; success   : handle Js.t -> unit
  }

class type janus = 
  object
    method getServer     : Js.js_string Js.t Js.prop
    method isConnected   : bool Js.t Js.prop
    method getSessionId  : Js.number Js.t Js.prop
    method attach        : parameters -> unit Js.meth
    method destroy       : parameters -> unit Js.meth
  end

val init : ?debug:bool -> ?callback:(unit -> unit) -> unit -> unit

val isWebrtcSupported : unit -> bool

val create : server:string            ->
             ?iceServers:string list  ->
             ?ipv6:bool               ->
             ?withCredentials:bool    ->
             ?max_poll_events:float   ->
             ?destroyOnUnload:bool    ->
             ?token:string            ->
             ?apisecret:string        ->
             ?success:(unit -> unit)       ->
             ?error:(Js.js_string -> unit) ->
             ?destroyed:(unit -> unit)     ->
             unit -> janus Js.t
